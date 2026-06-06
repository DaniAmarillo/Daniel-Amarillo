"""
scraper.py — Búsqueda y almacenamiento de artículos nuevos (requisito 2.1.7).

Ofrece DOS modos intercambiables:

  • MODO NAVEGADOR  — Selenium + undetected-chromedriver sobre dl.acm.org.
    Es el scraper del Taller 1, refactorizado. Obtiene la metadata completa
    de ACM (citas, descargas, referencias, CCS tags). Requiere Chrome y, por la
    protección Cloudflare de ACM, conviene ejecutarlo en LOCAL / Docker (donde
    eventualmente se puede resolver un captcha en la ventana del navegador).
    Sus dependencias se importan de forma PEREZOSA, por lo que este archivo se
    puede importar sin tener Selenium instalado.

  • MODO API (Crossref) — usa la API pública de Crossref (ISSN de ACM TOG).
    Solo necesita la librería estándar de Python; no abre navegador y no choca
    con Cloudflare, así que FUNCIONA en Streamlit Cloud. Obtiene título, autores,
    fecha, DOI, conteo de referencias y conteo de citas de Crossref. La métrica
    de descargas no está disponible por API (queda en 0 para esos artículos).

Ambos modos producen el mismo formato de dict y delegan la inserción en
`db.insertar_papers`, de modo que la lógica de almacenamiento es única.
"""

import re
import json
import time
import html
import urllib.parse
import urllib.request

import db

# ISSN de ACM Transactions on Graphics (print).  E-ISSN: 1557-7368
TOG_ISSN = "0730-0301"
ACM_BASE = "https://dl.acm.org"
_CONTACTO = "taller-mineria-datos@unal.edu.co"  # para el 'polite pool' de Crossref

_MES_NOMBRE = {
    1: "January", 2: "February", 3: "March", 4: "April", 5: "May", 6: "June",
    7: "July", 8: "August", 9: "September", 10: "October", 11: "November",
    12: "December",
}


# ═════════════════════════════════════════════════════════════════════════════
#  Disponibilidad de Selenium
# ═════════════════════════════════════════════════════════════════════════════
def selenium_disponible():
    """True si el entorno tiene Selenium + undetected-chromedriver + bs4."""
    try:
        import undetected_chromedriver  # noqa: F401
        import selenium                 # noqa: F401
        from bs4 import BeautifulSoup   # noqa: F401
        return True
    except Exception:
        return False


def detectar_chrome_version():
    """
    Intenta detectar la versión MAYOR de Google Chrome instalada en el equipo.
    Devuelve un int (p. ej. 149) o None si no se puede determinar.
    Todo va envuelto en try/except: ante cualquier duda devuelve None y la app
    se limita a mostrar un aviso.
    """
    import os
    import subprocess

    def _mayor(texto):
        m = re.search(r"(\d+)\.\d+\.\d+", texto or "")
        return int(m.group(1)) if m else None

    # ── Windows: primero el registro (rápido y fiable), luego el .exe ──
    if os.name == "nt":
        try:
            import winreg
            for hive in (winreg.HKEY_CURRENT_USER, winreg.HKEY_LOCAL_MACHINE):
                try:
                    k = winreg.OpenKey(hive, r"Software\Google\Chrome\BLBeacon")
                    valor, _ = winreg.QueryValueEx(k, "version")
                    winreg.CloseKey(k)
                    mj = _mayor(valor)
                    if mj:
                        return mj
                except OSError:
                    continue
        except Exception:
            pass
        for ruta in (
            os.path.expandvars(r"%ProgramFiles%\Google\Chrome\Application\chrome.exe"),
            os.path.expandvars(r"%ProgramFiles(x86)%\Google\Chrome\Application\chrome.exe"),
            os.path.expandvars(r"%LocalAppData%\Google\Chrome\Application\chrome.exe"),
        ):
            try:
                if os.path.exists(ruta):
                    out = subprocess.run(
                        ["powershell", "-NoProfile", "-Command",
                         f"(Get-Item '{ruta}').VersionInfo.ProductVersion"],
                        capture_output=True, text=True, timeout=10,
                    )
                    mj = _mayor(out.stdout)
                    if mj:
                        return mj
            except Exception:
                continue
        return None

    # ── macOS / Linux: ejecutar el binario con --version ──
    for binario in (
        "google-chrome", "google-chrome-stable", "chrome", "chromium",
        "chromium-browser",
        "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    ):
        try:
            out = subprocess.run([binario, "--version"],
                                 capture_output=True, text=True, timeout=10)
            mj = _mayor(out.stdout)
            if mj:
                return mj
        except Exception:
            continue
    return None


# ═════════════════════════════════════════════════════════════════════════════
#  MODO API  (Crossref)  —  sin navegador, apto para la nube
# ═════════════════════════════════════════════════════════════════════════════
def _strip_jats(texto):
    """Limpia el abstract de Crossref (viene con etiquetas JATS/XML)."""
    if not texto:
        return ""
    t = re.sub(r"<[^>]+>", " ", str(texto))
    t = html.unescape(t)
    return re.sub(r"\s+", " ", t).strip()


def _fecha_desde_partes(item):
    """Construye 'DD Month YYYY' a partir de los campos de fecha de Crossref."""
    for clave in ("published", "published-online", "published-print", "issued"):
        partes = (item.get(clave) or {}).get("date-parts") or []
        if partes and partes[0]:
            dp = partes[0]
            y = dp[0]
            m = dp[1] if len(dp) > 1 else 1
            d = dp[2] if len(dp) > 2 else 1
            mes = _MES_NOMBRE.get(m, "January")
            return f"{d:02d} {mes} {y}"
    return ""


def _autores_crossref(item):
    nombres = []
    for a in item.get("author", []) or []:
        nombre = " ".join(p for p in (a.get("given"), a.get("family")) if p).strip()
        if nombre:
            nombres.append(nombre)
    return "; ".join(nombres) or "N/A"


def _parse_crossref_item(item):
    """Convierte un registro de Crossref al formato común de `datos`."""
    titulos = item.get("title") or []
    titulo = titulos[0].strip() if titulos else "N/A"
    doi = (item.get("DOI") or "").strip()
    url = item.get("URL") or (f"{ACM_BASE}/doi/{doi}" if doi else None)
    subjects = "; ".join(item.get("subject", []) or [])
    n_refs = item.get("reference-count", 0) or len(item.get("reference", []) or [])

    return {
        "titulo": titulo,
        "doi": doi,
        "url_paper": url,
        "fecha": _fecha_desde_partes(item),
        "autores": _autores_crossref(item),
        "resumen": _strip_jats(item.get("abstract", "")),
        "num_citas": item.get("is-referenced-by-count", 0),
        "num_descargas": 0,                      # no disponible vía API
        "tags": subjects,
        "ccs_tags": "",                          # Crossref no expone CCS
        "num_referencias": n_refs,
        "referencias": "",                       # se omiten en modo API
    }


def _crossref_get(url, progress):
    req = urllib.request.Request(
        url, headers={"User-Agent": f"TallerMineriaDatos/1.0 (mailto:{_CONTACTO})"}
    )
    with urllib.request.urlopen(req, timeout=30) as resp:
        return json.loads(resp.read().decode("utf-8"))


def _crossref_recientes(rows, progress):
    """Trae los artículos más recientes de ACM TOG ordenados por fecha desc."""
    params = {
        "filter": "from-pub-date:2025-01-01",
        "sort": "published",
        "order": "desc",
        "rows": str(rows),
        "select": ("DOI,title,author,published,published-online,published-print,"
                   "issued,is-referenced-by-count,reference-count,subject,URL,abstract"),
        "mailto": _CONTACTO,
    }
    url = f"https://api.crossref.org/journals/{TOG_ISSN}/works?" + urllib.parse.urlencode(params)
    data = _crossref_get(url, progress)
    return data.get("message", {}).get("items", [])


def _crossref_por_doi(doi, progress):
    try:
        url = "https://api.crossref.org/works/" + urllib.parse.quote(doi)
        data = _crossref_get(url, progress)
        return data.get("message")
    except Exception as e:
        progress(f"  · no se pudo consultar {doi}: {e}")
        return None


def _reconsultar_api(db_path, progress):
    ult = db.ultimos_papers(db_path, 5)
    actualizados = []
    for p in ult:
        progress(f"  · reconsultando {p['doi']} …")
        item = _crossref_por_doi(p["doi"], progress) if p["doi"] else None
        cit_new = item.get("is-referenced-by-count", p["citations"]) if item else p["citations"]
        dl_new = p["downloads"]  # descargas no disponibles por API → se conservan
        db.actualizar_metricas(db_path, p["paper_id"], cit_new, dl_new)
        actualizados.append({
            "paper_id": p["paper_id"], "title": p["title"], "doi": p["doi"],
            "citas_antes": p["citations"], "citas_ahora": db.to_int(cit_new),
            "descargas_antes": p["downloads"], "descargas_ahora": dl_new,
        })
    return actualizados


def actualizar_api(db_path, max_nuevos=60, progress=print):
    progress("Modo API (Crossref): consultando artículos recientes de ACM TOG…")
    dois = db.cargar_dois(db_path)
    try:
        items = _crossref_recientes(max_nuevos, progress)
    except Exception as e:
        raise RuntimeError(f"No se pudo consultar Crossref: {e}")
    progress(f"Crossref devolvió {len(items)} registros recientes.")

    nuevos = []
    for it in items:
        doi = (it.get("DOI") or "").strip().lower()
        if not doi or doi in dois:
            continue
        nuevos.append(_parse_crossref_item(it))
        dois.add(doi)

    if nuevos:
        n = db.insertar_papers(db_path, nuevos)
        progress(f"✓ {n} artículos nuevos almacenados en SQLite.")
        return {"modo": "nuevos", "metodo": "api", "nuevos": nuevos, "actualizados": []}

    progress("No se encontraron artículos nuevos. Reconsultando los últimos 5…")
    act = _reconsultar_api(db_path, progress)
    return {"modo": "reconsulta", "metodo": "api", "nuevos": [], "actualizados": act}


# ═════════════════════════════════════════════════════════════════════════════
#  MODO NAVEGADOR  (Selenium + undetected-chromedriver)  —  fiel al Taller 1
# ═════════════════════════════════════════════════════════════════════════════
def _import_selenium():
    import undetected_chromedriver as uc
    from selenium.webdriver.common.by import By
    from bs4 import BeautifulSoup
    return uc, By, BeautifulSoup


def iniciar_driver(headless=False, version_main=None):
    uc, _, _ = _import_selenium()
    opts = uc.ChromeOptions()
    opts.add_argument("--window-size=1920,1080")
    opts.add_argument("--no-sandbox")
    opts.add_argument("--disable-dev-shm-usage")
    if headless:
        opts.add_argument("--headless=new")
    kwargs = {"options": opts}
    if version_main:
        kwargs["version_main"] = version_main
    return uc.Chrome(**kwargs)


def _esperar_pagina_lista(driver, timeout=30, progress=print):
    """Espera a que la página cargue y no sea un challenge de Cloudflare."""
    tiempo, intervalo = 0, 2
    while tiempo < timeout:
        titulo = (driver.title or "").lower()
        url = (driver.current_url or "").lower()
        bloqueado = any(x in titulo for x in [
            "just a moment", "attention required", "cloudflare",
            "checking your browser", "please wait",
        ])
        valido = (not bloqueado) and (
            "/toc/tog/" in url or "/doi/" in url
            or any(x in titulo for x in ["acm", "tog", "transactions"])
        )
        if valido:
            return True
        if bloqueado:
            progress(f"  Cloudflare detectado, esperando… ({tiempo}s). "
                     f"Si hay captcha, resuélvelo en la ventana de Chrome.")
        time.sleep(intervalo)
        tiempo += intervalo
    progress(f"  Timeout — título: {driver.title}")
    return False


def _navegar_con_reintento(driver, url, intentos=3, progress=print):
    for intento in range(1, intentos + 1):
        progress(f"  Cargando (intento {intento}/{intentos}): {url}")
        driver.get(url)
        time.sleep(5 if intento == 1 else 10)
        if _esperar_pagina_lista(driver, progress=progress):
            return True
        if intento < intentos:
            driver.get(ACM_BASE)      # warm-up intermedio
            time.sleep(8)
    progress(f"  ✗ No se pudo cargar: {url}")
    return False


def warming_up(driver, progress=print):
    """Visita la home de ACM para establecer cookies y pasar el challenge."""
    progress("Warming up: visitando la home de ACM…")
    driver.get(ACM_BASE)
    time.sleep(12)
    if _esperar_pagina_lista(driver, timeout=45, progress=progress):
        progress("Warming up completado.")
    else:
        progress("Si aparece un captcha, resuélvelo en la ventana de Chrome; "
                 "se reintentará automáticamente.")


def _expandir_acordeones(driver, By):
    try:
        elems = driver.find_elements(
            By.CSS_SELECTOR,
            "a.accordion-tabbed__control, button.accordion-tabbed__control",
        )
        n = 0
        for e in elems:
            if e.get_attribute("aria-expanded") != "true":
                try:
                    driver.execute_script("arguments[0].click();", e)
                    time.sleep(0.4)
                    n += 1
                except Exception:
                    pass
        if n:
            time.sleep(1.2)
        return n
    except Exception:
        return 0


def _doi_de_url(url):
    return url.replace(f"{ACM_BASE}/doi/", "").strip().lower()


def extraer_doi_urls_de_issue(driver, url_issue, BS, By, progress=print):
    if not _navegar_con_reintento(driver, url_issue, progress=progress):
        return [], BS(driver.page_source, "html.parser")
    n = _expandir_acordeones(driver, By)
    if n:
        progress(f"    (expandidos {n} acordeones)")
    soup = BS(driver.page_source, "html.parser")
    links = soup.select(
        "h3.issue-item__title a, h5.issue-item__title a, "
        "div.issue-item__content h5 a, div.issue-item__content h3 a"
    )
    doi_urls, vistos = [], set()
    for l in links:
        href = l.get("href", "")
        if href.startswith("/doi/") and href not in vistos:
            doi_urls.append(ACM_BASE + href)
            vistos.add(href)
    return doi_urls, soup


def extraer_datos_paper(driver, url_paper, BS, progress=print):
    """Extrae todos los campos de un paper individual (igual que el Taller 1)."""
    driver.get(url_paper)
    time.sleep(2.5)
    soup = BS(driver.page_source, "html.parser")

    doi = _doi_de_url(url_paper)
    t = soup.find("h1", property="name")
    titulo = t.get_text(strip=True) if t else "N/A"

    f = soup.select_one("span.core-date-published")
    fecha = f.get_text(strip=True) if f else "N/A"

    spans = soup.select(
        "div.contributors span[property='givenName'], "
        "div.contributors span[property='familyName']"
    )
    nombres = [s.get_text(strip=True) for s in spans]
    autores = [f"{nombres[i]} {nombres[i + 1]}"
               for i in range(0, len(nombres) - 1, 2)]
    if not autores:
        autores = [a.get_text(strip=True)
                   for a in soup.select("ul.loa span.author-name")]
    autores_str = "; ".join(autores) or "N/A"

    ab = soup.select_one("section#abstract div[role='paragraph']")
    resumen = ab.get_text(" ", strip=True) if ab else "N/A"

    num_citas = num_descargas = "N/A"
    mb = soup.select_one("button.metrics-toggle")
    if mb:
        cs = mb.select_one("span.citation span")
        ds = mb.select_one("span.metric span")
        if cs:
            num_citas = cs.get_text(strip=True).replace(",", "")
        if ds:
            num_descargas = ds.get_text(strip=True).replace(",", "")
    if num_citas == "N/A":
        ca = soup.select_one("div.article-metric.citation div.metric-value span")
        da = soup.select_one("div.article-metric.download div.metric-value span")
        if ca:
            num_citas = ca.get_text(strip=True).replace(",", "")
        if da:
            num_descargas = da.get_text(strip=True).replace(",", "")

    author_tags = [a.get_text(strip=True)
                   for a in soup.select("section[property='keywords'] li a")]
    badge_tags = [a.get_text(strip=True) for a in soup.select("a.badge-type")]
    vistos = set(author_tags)
    for tag in badge_tags:
        if tag not in vistos:
            author_tags.append(tag)
            vistos.add(tag)
    tags_str = "; ".join(author_tags) or "N/A"

    ccs = [a.get_text(strip=True)
           for a in soup.select("section#sec-terms a") if a.get_text(strip=True)]
    ccs_str = "; ".join(ccs) or "N/A"

    refs = [r.get_text(" ", strip=True)
            for r in soup.select(
                "section#bibliography div[role='listitem'] div.citation-content")
            if r.get_text(strip=True)]
    refs_str = " ||| ".join(refs) or "N/A"

    return {
        "titulo": titulo, "doi": doi, "url_paper": url_paper, "fecha": fecha,
        "autores": autores_str, "resumen": resumen, "num_citas": num_citas,
        "num_descargas": num_descargas, "tags": tags_str, "ccs_tags": ccs_str,
        "num_referencias": len(refs), "referencias": refs_str,
    }


def _url_ultimo_issue(driver, BS, progress):
    """Detecta el issue más reciente del journal (mayor volumen/número)."""
    _navegar_con_reintento(driver, f"{ACM_BASE}/journal/tog", progress=progress)
    soup = BS(driver.page_source, "html.parser")
    candidatos = []
    for a in soup.select("a[href*='/toc/tog/']"):
        m = re.search(r"/toc/tog/(\d+)/(\d+)/(\d+)", a.get("href", ""))
        if m:
            y, v, i = map(int, m.groups())
            candidatos.append((v, i, f"{ACM_BASE}{a['href'].split('?')[0]}"))
    if not candidatos:
        return None
    candidatos.sort(reverse=True)
    progress(f"  Issue más reciente detectado: {candidatos[0][2]}")
    return candidatos[0][2]


def _url_issue_anterior(soup, current_url):
    """URL del issue inmediatamente anterior (más antiguo)."""
    btn = soup.select_one("a.content-navigation__btn--prev")
    if btn and btn.get("href", "").startswith("/toc/tog/"):
        return ACM_BASE + btn["href"].split("?")[0]
    cur = re.search(r"/toc/tog/(\d+)/(\d+)/(\d+)", current_url or "")
    if cur:
        _, cv, ci = map(int, cur.groups())
        mejor = None
        for a in soup.select("a[href*='/toc/tog/']"):
            m = re.search(r"/toc/tog/(\d+)/(\d+)/(\d+)", a.get("href", ""))
            if m:
                _, v, i = map(int, m.groups())
                if (v, i) < (cv, ci) and (mejor is None or (v, i) > (mejor[0], mejor[1])):
                    mejor = (v, i, ACM_BASE + a["href"].split("?")[0])
        if mejor:
            return mejor[2]
    return None


def _buscar_nuevos_navegador(driver, db_path, dois, max_issues, progress, BS, By):
    """Recorre los issues recientes hacia atrás hasta alcanzar la BD."""
    nuevos = []
    url_issue = _url_ultimo_issue(driver, BS, progress)
    revisados = 0
    while url_issue and revisados < max_issues:
        revisados += 1
        progress(f"\n[Issue #{revisados}] {url_issue}")
        doi_urls, soup = extraer_doi_urls_de_issue(driver, url_issue, BS, By, progress)
        nuevos_issue = [u for u in doi_urls if _doi_de_url(u) not in dois]
        progress(f"  {len(doi_urls)} papers · {len(nuevos_issue)} nuevos")
        if not doi_urls:
            break
        if not nuevos_issue:
            progress("  Este issue ya está completo en la BD → búsqueda al día.")
            break
        for j, u in enumerate(nuevos_issue, 1):
            progress(f"    [{j}/{len(nuevos_issue)}] {u.split('/')[-1]}")
            try:
                datos = extraer_datos_paper(driver, u, BS, progress)
                nuevos.append(datos)
                dois.add(_doi_de_url(u))
            except Exception as e:
                progress(f"      ✗ error: {e}")
        url_issue = _url_issue_anterior(soup, url_issue)
    return nuevos


def _reconsultar_navegador(driver, db_path, progress, BS):
    ult = db.ultimos_papers(db_path, 5)
    actualizados = []
    for p in ult:
        url = p["url"] or f"{ACM_BASE}/doi/{p['doi']}"
        progress(f"  · reconsultando {p['doi']} …")
        try:
            datos = extraer_datos_paper(driver, url, BS, progress)
            cit_new = db.to_int(datos.get("num_citas"), p["citations"])
            dl_new = db.to_int(datos.get("num_descargas"), p["downloads"])
        except Exception as e:
            progress(f"      ✗ error: {e}")
            cit_new, dl_new = p["citations"], p["downloads"]
        db.actualizar_metricas(db_path, p["paper_id"], cit_new, dl_new)
        actualizados.append({
            "paper_id": p["paper_id"], "title": p["title"], "doi": p["doi"],
            "citas_antes": p["citations"], "citas_ahora": cit_new,
            "descargas_antes": p["downloads"], "descargas_ahora": dl_new,
        })
    return actualizados


def actualizar_navegador(db_path, headless=False, version_main=None,
                         max_issues=6, progress=print):
    uc, By, BS = _import_selenium()
    dois = db.cargar_dois(db_path)
    progress("Modo Navegador (Selenium + undetected-chromedriver).")
    driver = iniciar_driver(headless, version_main)
    try:
        warming_up(driver, progress)
        nuevos = _buscar_nuevos_navegador(driver, db_path, dois, max_issues,
                                          progress, BS, By)
        if nuevos:
            n = db.insertar_papers(db_path, nuevos)
            progress(f"✓ {n} artículos nuevos almacenados en SQLite.")
            return {"modo": "nuevos", "metodo": "navegador",
                    "nuevos": nuevos, "actualizados": []}
        progress("No se encontraron artículos nuevos. Reconsultando los últimos 5…")
        act = _reconsultar_navegador(driver, db_path, progress, BS)
        return {"modo": "reconsulta", "metodo": "navegador",
                "nuevos": [], "actualizados": act}
    finally:
        try:
            driver.quit()
        except Exception:
            pass
        progress("Navegador cerrado.")


# ═════════════════════════════════════════════════════════════════════════════
#  Despachador
# ═════════════════════════════════════════════════════════════════════════════
def actualizar(db_path, metodo="auto", headless=False, version_main=None,
               max_issues=6, max_api=60, progress=print):
    """
    Punto de entrada único usado por la app.
      metodo = "auto" → navegador si Selenium está disponible, si no API.
    """
    if metodo == "auto":
        metodo = "navegador" if selenium_disponible() else "api"

    if metodo == "navegador":
        if not selenium_disponible():
            raise RuntimeError(
                "Selenium / undetected-chromedriver no está instalado en este "
                "entorno (típico en Streamlit Cloud). Usa el modo API, o ejecuta "
                "la app en local / con Docker:  pip install -r requirements-scraper.txt"
            )
        return actualizar_navegador(db_path, headless, version_main, max_issues, progress)

    return actualizar_api(db_path, max_nuevos=max_api, progress=progress)
