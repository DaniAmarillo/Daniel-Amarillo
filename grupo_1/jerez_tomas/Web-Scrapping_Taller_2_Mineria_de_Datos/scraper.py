
import re
import time
import random
from typing import Optional

import requests
from bs4 import BeautifulSoup

NATURE_BASE = "https://www.nature.com"
JOURNAL_SLUG = "natmachintell"
OPENALEX_BASE = "https://api.openalex.org"
NATURE_MI_OPENALEX_ID = "S4210184325" 

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/131.0.0.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.9,es;q=0.8",
}

PAUSE_MIN = 1.0
PAUSE_MAX = 2.5
TIMEOUT = 25


GEN_AI_PATTERNS = [
    r"\b(generative\s+(model|ai|adversarial|network)s?)\b",
    r"\b(diffusion\s+(model|process)s?)\b",
    r"\b(large\s+language\s+model|llm|llms)\b",
    r"\b(foundation\s+model|chatgpt|gpt-?\d|bert\b|llama)\b",
    r"\b(text-?to-?(image|video|3d|protein))\b",
    r"\b(stable\s+diffusion|dall-?e|midjourney)\b",
    r"\b(variational\s+autoencoder|vae)\b",
    r"\bgan\b|\bgans\b",
    r"\b(autoregressive\s+(model|generation))\b",
    r"\b(image|text|video|protein|molecule|sequence)\s+generation\b",
    r"\b(prompt(ing)?|in-context\s+learning)\b",
]
ML_PATTERNS = [
    r"\b(machine\s+learning|ml\s+model)\b",
    r"\b(deep\s+learning|neural\s+network)s?\b",
    r"\b(reinforcement\s+learning|rl\s+agent)\b",
    r"\b(self-supervised|supervised|unsupervised)\s+learning\b",
    r"\b(transfer\s+learning|federated\s+learning|meta-?learning)\b",
    r"\b(transformer|attention\s+mechanism)s?\b",
    r"\b(convolutional|recurrent|graph)\s+neural\b",
    r"\b(cnn|rnn|lstm|gnn|mlp)\b",
    r"\b(embedding|representation\s+learning)s?\b",
    r"\b(pretrain|fine-?tun|backbone\s+model)",
    r"\b(artificial\s+intelligence|ai\s+(model|system|agent))\b",
]
STATS_PATTERNS = [
    r"\b(bayesian\s+(inference|model|method|network))\b",
    r"\b(statistical\s+(method|model|test|inference|analysis))\b",
    r"\b(monte\s+carlo|markov\s+chain|mcmc)\b",
    r"\b(maximum\s+likelihood|likelihood\s+estimation)\b",
    r"\b(uncertainty\s+quantification)\b",
    r"\b(probabilistic\s+(model|inference|programming))\b",
    r"\b(stochastic\s+process|gaussian\s+process)\b",
]


def classify_paper(paper: dict) -> str:
    """Clasifica un paper en una de las 4 categorías del taller."""
    text = " ".join([
        paper.get("title", "") or "",
        paper.get("abstract", "") or "",
        " ".join(paper.get("subjects", []) or []),
    ]).lower()
    for pat in GEN_AI_PATTERNS:
        if re.search(pat, text, re.IGNORECASE):
            return "IA Generativa"
    for pat in ML_PATTERNS:
        if re.search(pat, text, re.IGNORECASE):
            return "Machine Learning"
    for pat in STATS_PATTERNS:
        if re.search(pat, text, re.IGNORECASE):
            return "Estadística"
    return "Otros"


def _polite_sleep():
    time.sleep(random.uniform(PAUSE_MIN, PAUSE_MAX))


def _fetch(url: str, timeout: int = TIMEOUT) -> Optional[requests.Response]:
    """Devuelve la respuesta HTTP o None si falla. NO lanza excepción."""
    try:
        return requests.get(url, headers=HEADERS, timeout=timeout)
    except requests.RequestException:
        return None


def _extract_count(text: Optional[str]) -> Optional[int]:
    if not text:
        return None
    text = text.strip().lower().replace(",", "")
    m = re.match(r"([\d.]+)\s*([km]?)", text)
    if not m:
        return None
    num = float(m.group(1))
    suffix = m.group(2)
    if suffix == "k":
        num *= 1_000
    elif suffix == "m":
        num *= 1_000_000
    return int(num)


def _meta(soup, name) -> Optional[str]:
    t = soup.find("meta", attrs={"name": name}) or soup.find(
        "meta", attrs={"property": name}
    )
    if t and t.get("content"):
        return t["content"].strip()
    return None


def _meta_list(soup, name) -> list[str]:
    return [
        t["content"].strip()
        for t in soup.find_all("meta", attrs={"name": name})
        if t.get("content")
    ]


def _article_id(url: str) -> str:
    m = re.search(r"/articles/(s\d+-\d+-\d+-[\dxz]+)", url)
    return m.group(1) if m else url.split("/")[-1].split("?")[0]



class NatureBlocked(Exception):
    """Excepción: Nature retornó 403, hay que usar fallback."""
    pass


def _nature_listing_url(year: int, page: int = 1) -> str:
    if page == 1:
        return f"{NATURE_BASE}/{JOURNAL_SLUG}/research-articles?year={year}"
    return (
        f"{NATURE_BASE}/{JOURNAL_SLUG}/research-articles"
        f"?searchType=journalSearch&sort=PubDate&year={year}&page={page}"
    )


def _list_nature_year(year: int, max_pages: int = 10) -> list[dict]:
    articles = []
    for page in range(1, max_pages + 1):
        url = _nature_listing_url(year, page)
        r = _fetch(url)
        if r is None:
            break
        if r.status_code == 403:
            raise NatureBlocked(f"Nature bloqueó la petición (403): {url}")
        if r.status_code != 200:
            break

        soup = BeautifulSoup(r.text, "lxml")
        page_articles = []
        for h3 in soup.find_all("h3"):
            a = h3.find("a", href=True)
            if not a or "/articles/" not in a["href"]:
                continue
            full = a["href"] if a["href"].startswith("http") else NATURE_BASE + a["href"]
            full = full.split("?")[0]
            container = h3.find_parent("article") or h3.find_parent("li")
            date_text = None
            if container:
                t = container.find("time")
                if t:
                    date_text = t.get("datetime") or t.get_text(strip=True)
            page_articles.append({
                "url": full,
                "paper_id": _article_id(full),
                "title": a.get_text(strip=True),
                "date": date_text,
            })

        if not page_articles:
            break
        articles.extend(page_articles)
        _polite_sleep()

    seen = set()
    unique = []
    for a in articles:
        if a["paper_id"] not in seen:
            seen.add(a["paper_id"])
            unique.append(a)
    return unique


def _scrape_nature_article(url: str) -> dict:
    r = _fetch(url)
    if r is None:
        raise RuntimeError(f"No se pudo descargar {url}")
    if r.status_code == 403:
        raise NatureBlocked(f"Nature 403: {url}")
    if r.status_code != 200:
        raise RuntimeError(f"HTTP {r.status_code} en {url}")

    soup = BeautifulSoup(r.text, "lxml")
    data = {
        "paper_id": _article_id(url),
        "url": url,
        "source": "nature",
        "title": _meta(soup, "citation_title"),
        "doi": _meta(soup, "citation_doi") or _meta(soup, "DC.identifier"),
        "publication_date": _meta(soup, "citation_publication_date")
                            or _meta(soup, "prism.publicationDate"),
        "online_date": _meta(soup, "citation_online_date"),
        "journal": _meta(soup, "citation_journal_title") or "Nature Machine Intelligence",
        "volume": _meta(soup, "citation_volume"),
        "abstract": _meta(soup, "dc.description") or _meta(soup, "description"),
        "authors": _meta_list(soup, "citation_author"),
    }
    data["n_authors"] = len(data["authors"])

    subjects = []
    for a in soup.find_all("a", attrs={"data-track-action": "view subject"}):
        s = a.get_text(strip=True)
        if s:
            subjects.append(s)
    if not subjects:
        subjects = _meta_list(soup, "dc.subject")
    data["subjects"] = subjects

    accesses = altmetric = citations = None
    for li in soup.find_all(["li", "p"]):
        txt = li.get_text(" ", strip=True).lower()
        m_acc = re.search(r"([\d.,]+\s*[km]?)\s*accesses?", txt)
        m_alt = re.search(r"([\d.,]+\s*[km]?)\s*altmetric", txt)
        m_cit = re.search(r"([\d.,]+\s*[km]?)\s*citations?", txt)
        if m_acc and accesses is None:
            accesses = _extract_count(m_acc.group(1))
        if m_alt and altmetric is None:
            altmetric = _extract_count(m_alt.group(1))
        if m_cit and citations is None:
            citations = _extract_count(m_cit.group(1))
    data["accesses"] = accesses
    data["altmetric"] = altmetric
    data["citations_nature"] = citations

    references = []
    refs_ol = soup.find("ol", class_=re.compile("c-article-references|references", re.I))
    if refs_ol:
        for li in refs_ol.find_all("li"):
            ref_text = li.get_text(" ", strip=True)
            ref_text = re.sub(
                r"\s*(Article|Google Scholar|MathSciNet|PubMed|CAS|ADS|Chapter|MATH)\s*",
                " ", ref_text,
            )
            ref_text = re.sub(r"\s+", " ", ref_text).strip()
            if ref_text:
                references.append(ref_text)
    data["references"] = references
    data["n_references"] = len(references)
    return data


def _list_openalex_year(year: int, max_results: int = 200) -> list[dict]:
    """Lista los artículos de Nature Machine Intelligence en OpenAlex para un año.
    Retorna lista en formato compatible con el listado de Nature."""
    url = (
        f"{OPENALEX_BASE}/works"
        f"?filter=primary_location.source.id:S{NATURE_MI_OPENALEX_ID[1:]},"
        f"publication_year:{year},type:article"
        f"&per-page={min(max_results, 200)}"
        f"&select=id,doi,title,publication_date,authorships,abstract_inverted_index,"
        f"cited_by_count,referenced_works_count,concepts,ids"
    )
    r = _fetch(url)
    if r is None or r.status_code != 200:
        return []
    data = r.json()
    out = []
    for w in data.get("results", []):
        doi = (w.get("doi") or "").replace("https://doi.org/", "")
        pid = doi.split("/")[-1] if "/" in doi else None
        nature_url = f"{NATURE_BASE}/articles/{pid}" if pid else w.get("id")
        out.append({
            "url": nature_url,
            "paper_id": pid or _article_id(w.get("id", "")),
            "title": w.get("title"),
            "date": w.get("publication_date"),
            "_openalex_raw": w,
        })
    return out


def _abstract_from_inverted(inv: dict) -> Optional[str]:
    if not inv:
        return None
    positions = []
    for word, idx_list in inv.items():
        for i in idx_list:
            positions.append((i, word))
    positions.sort()
    return " ".join(w for _, w in positions)


def _build_paper_from_openalex(raw: dict, url_fallback: str) -> dict:
    doi = (raw.get("doi") or "").replace("https://doi.org/", "")
    pid = doi.split("/")[-1] if "/" in doi else _article_id(url_fallback)
    authors = [
        a.get("author", {}).get("display_name")
        for a in raw.get("authorships", [])
        if a.get("author", {}).get("display_name")
    ]
    concepts = [c.get("display_name") for c in raw.get("concepts", [])[:8]]

    return {
        "paper_id": pid,
        "url": url_fallback,
        "source": "openalex",
        "title": raw.get("title"),
        "doi": doi,
        "publication_date": raw.get("publication_date"),
        "journal": "Nature Machine Intelligence",
        "abstract": _abstract_from_inverted(raw.get("abstract_inverted_index")),
        "authors": authors,
        "n_authors": len(authors),
        "subjects": concepts,
        "accesses": None,           
        "altmetric": None,
        "citations_nature": raw.get("cited_by_count"),
        "references": [],           
        "n_references": raw.get("referenced_works_count", 0),
    }


def list_articles_in_year(year: int) -> tuple[list[dict], str]:
    """Lista artículos publicados en un año. Devuelve (lista, fuente_usada).
    fuente_usada in {'nature', 'openalex'}."""
    try:
        articles = _list_nature_year(year)
        return articles, "nature"
    except NatureBlocked:
        articles = _list_openalex_year(year)
        return articles, "openalex"


def scrape_article(article_meta: dict) -> Optional[dict]:
    """Scrapea metadatos completos de un artículo.
    Recibe el dict del listado (con url o _openalex_raw)."""
    if "_openalex_raw" in article_meta:
        return _build_paper_from_openalex(
            article_meta["_openalex_raw"], article_meta["url"]
        )
    try:
        return _scrape_nature_article(article_meta["url"])
    except NatureBlocked:
        # Fallback: pedir el work por DOI a OpenAlex
        return _scrape_via_openalex_by_doi(article_meta.get("paper_id"))
    except Exception:
        return None


def _scrape_via_openalex_by_doi(paper_id: str) -> Optional[dict]:
    if not paper_id:
        return None
    full_doi = f"10.1038/{paper_id}"
    url = f"{OPENALEX_BASE}/works/doi:{full_doi}"
    r = _fetch(url)
    if r is None or r.status_code != 200:
        return None
    raw = r.json()
    return _build_paper_from_openalex(raw, f"{NATURE_BASE}/articles/{paper_id}")


def scrape_article_by_id(paper_id: str) -> Optional[dict]:
    """Scrappea por ID directamente (para actualizar métricas de papers existentes)."""
    nature_url = f"{NATURE_BASE}/articles/{paper_id}"
    try:
        return _scrape_nature_article(nature_url)
    except NatureBlocked:
        return _scrape_via_openalex_by_doi(paper_id)
    except Exception:
        return None
