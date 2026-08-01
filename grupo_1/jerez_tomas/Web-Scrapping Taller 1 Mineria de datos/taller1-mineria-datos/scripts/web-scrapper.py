import os
import re
import json
import time
import random
from pathlib import Path
from datetime import datetime

import requests
from bs4 import BeautifulSoup
from tqdm import tqdm

BASE_URL = "https://www.nature.com"
JOURNAL_SLUG = "natmachintell"
YEAR = 2025
TOTAL_PAGES = 7 


HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/131.0.0.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.9,es;q=0.8",
    "Accept-Encoding": "gzip, deflate, br",
    "Connection": "keep-alive",
    "Upgrade-Insecure-Requests": "1",
}


PAUSE_MIN = 1.5  
PAUSE_MAX = 3.5
TIMEOUT = 30


SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_DIR = SCRIPT_DIR.parent
DATA_DIR = PROJECT_DIR / "data"
ARTICLES_DIR = DATA_DIR / "articles"
DATA_DIR.mkdir(parents=True, exist_ok=True)
ARTICLES_DIR.mkdir(parents=True, exist_ok=True)

LISTING_FILE = DATA_DIR / "listing.json"
PAPERS_FILE = DATA_DIR / "papers_raw.json"
CHECKPOINT_FILE = DATA_DIR / "checkpoint.json"



def polite_sleep():
    time.sleep(random.uniform(PAUSE_MIN, PAUSE_MAX))


def fetch(url, retries=3):
    for attempt in range(retries):
        try:
            r = requests.get(url, headers=HEADERS, timeout=TIMEOUT)
            if r.status_code == 200:
                return r.text
            if r.status_code == 429:  
                wait = (attempt + 1) * 10
                print(f"\n  [429] Rate limited. Esperando {wait}s...")
                time.sleep(wait)
                continue
            print(f"\n  [{r.status_code}] {url}")
        except requests.RequestException as e:
            print(f"\n  [ERROR] {e} en {url}")
            time.sleep((attempt + 1) * 5)
    return None


def parse_meta(soup, name):
    tag = soup.find("meta", attrs={"name": name})
    if tag and tag.get("content"):
        return tag["content"].strip()
    tag = soup.find("meta", attrs={"property": name})
    if tag and tag.get("content"):
        return tag["content"].strip()
    return None


def parse_meta_list(soup, name):
    return [
        t["content"].strip()
        for t in soup.find_all("meta", attrs={"name": name})
        if t.get("content")
    ]


def extract_count(text):
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



def get_listing_url(page):
    if page == 1:
        return f"{BASE_URL}/{JOURNAL_SLUG}/research-articles?year={YEAR}"
    return (
        f"{BASE_URL}/{JOURNAL_SLUG}/research-articles"
        f"?searchType=journalSearch&sort=PubDate&year={YEAR}&page={page}"
    )


def parse_listing_page(html):
    soup = BeautifulSoup(html, "lxml")
    articles = []

    for h3 in soup.find_all("h3"):
        a = h3.find("a", href=True)
        if not a:
            continue
        href = a["href"]
        if "/articles/" not in href:
            continue
        full_url = href if href.startswith("http") else BASE_URL + href

        container = h3.find_parent("article") or h3.find_parent("li")
        date_text = None
        article_type = None
        if container:
            time_tag = container.find("time")
            if time_tag:
                date_text = time_tag.get("datetime") or time_tag.get_text(strip=True)
            type_span = container.find(attrs={"data-test": "article.type"})
            if type_span:
                article_type = type_span.get_text(strip=True)

        articles.append({
            "url": full_url,
            "title_listing": a.get_text(strip=True),
            "date_listing": date_text,
            "article_type": article_type,
        })
    return articles


def scrape_listing():
    if LISTING_FILE.exists():
        print(f"[FASE A] Listado ya existe en {LISTING_FILE}, lo reutilizo.")
        with open(LISTING_FILE) as f:
            return json.load(f)

    print(f"[FASE A] Descargando listado de {TOTAL_PAGES} páginas...")
    all_articles = []
    for page in range(1, TOTAL_PAGES + 1):
        url = get_listing_url(page)
        print(f"  Página {page}/{TOTAL_PAGES}: {url}")
        html = fetch(url)
        if not html:
            print(f"  [WARN] Página {page} no se pudo descargar")
            continue
        page_articles = parse_listing_page(html)
        print(f"    -> {len(page_articles)} artículos encontrados")
        all_articles.extend(page_articles)
        polite_sleep()

    seen = set()
    unique = []
    for a in all_articles:
        if a["url"] not in seen:
            seen.add(a["url"])
            unique.append(a)

    with open(LISTING_FILE, "w") as f:
        json.dump(unique, f, indent=2, ensure_ascii=False)
    print(f"[FASE A] Total: {len(unique)} artículos únicos guardados en {LISTING_FILE}")
    return unique


def parse_article(html, source_url):
    soup = BeautifulSoup(html, "lxml")
    data = {"url": source_url}

    data["title"] = parse_meta(soup, "citation_title")
    data["doi"] = parse_meta(soup, "citation_doi") or parse_meta(soup, "DC.identifier")
    data["publication_date"] = parse_meta(soup, "citation_publication_date") or \
                                parse_meta(soup, "prism.publicationDate")
    data["online_date"] = parse_meta(soup, "citation_online_date")
    data["journal"] = parse_meta(soup, "citation_journal_title") or "Nature Machine Intelligence"
    data["volume"] = parse_meta(soup, "citation_volume")
    data["issue"] = parse_meta(soup, "citation_issue")
    data["language"] = parse_meta(soup, "citation_language")
    data["abstract"] = parse_meta(soup, "dc.description") or parse_meta(soup, "description")

    authors = parse_meta_list(soup, "citation_author")
    data["authors"] = authors
    data["n_authors"] = len(authors)

    subjects = []
    for a in soup.find_all("a", attrs={"data-track-action": "view subject"}):
        s = a.get_text(strip=True)
        if s:
            subjects.append(s)
    if not subjects:
        subjects = parse_meta_list(soup, "dc.subject")
    data["subjects"] = subjects

    accesses = altmetric = citations = None
    metrics_section = soup.find("ul", class_=re.compile("metrics|article-metrics", re.I))
    if not metrics_section:
        metrics_section = soup

    for li in metrics_section.find_all(["li", "p"]):
        txt = li.get_text(" ", strip=True).lower()
        m_acc = re.search(r"([\d.,]+\s*[km]?)\s*accesses?", txt)
        m_alt = re.search(r"([\d.,]+\s*[km]?)\s*altmetric", txt)
        m_cit = re.search(r"([\d.,]+\s*[km]?)\s*citations?", txt)
        if m_acc and accesses is None:
            accesses = extract_count(m_acc.group(1))
        if m_alt and altmetric is None:
            altmetric = extract_count(m_alt.group(1))
        if m_cit and citations is None:
            citations = extract_count(m_cit.group(1))

    data["accesses"] = accesses
    data["altmetric"] = altmetric
    data["citations_nature"] = citations 

    references = []
    refs_ol = soup.find("ol", class_=re.compile("c-article-references|references", re.I))
    if not refs_ol:
        refs_section = soup.find(id=re.compile("references", re.I))
        if refs_section:
            refs_ol = refs_section.find("ol")

    if refs_ol:
        for li in refs_ol.find_all("li"):
            ref_text = li.get_text(" ", strip=True)
            ref_text = re.sub(
                r"\s*(Article|Google Scholar|MathSciNet|PubMed|CAS|ADS|Chapter|MATH)\s*",
                " ",
                ref_text
            ).strip()
            ref_text = re.sub(r"\s+", " ", ref_text)
            if ref_text:
                references.append(ref_text)

    data["references"] = references
    data["n_references"] = len(references)

    return data


def article_id_from_url(url):
    m = re.search(r"/articles/(s\d+-\d+-\d+-[\dxz]+)", url)
    return m.group(1) if m else url.split("/")[-1].split("?")[0]


def load_checkpoint():
    if CHECKPOINT_FILE.exists():
        with open(CHECKPOINT_FILE) as f:
            return json.load(f)
    return {"completed": [], "failed": []}


def save_checkpoint(cp):
    with open(CHECKPOINT_FILE, "w") as f:
        json.dump(cp, f, indent=2)


def scrape_articles(listing):
    cp = load_checkpoint()
    completed = set(cp["completed"])
    failed = set(cp["failed"])

    pending = [a for a in listing if article_id_from_url(a["url"]) not in completed]
    print(f"\n[FASE B] {len(completed)} ya completados, {len(pending)} pendientes")

    pbar = tqdm(pending, desc="Scrapeando artículos")
    for art in pbar:
        url = art["url"].split("?")[0]  
        aid = article_id_from_url(url)
        out_file = ARTICLES_DIR / f"{aid}.json"

        if out_file.exists():
            completed.add(aid)
            continue

        pbar.set_postfix_str(aid)
        html = fetch(url)
        if not html:
            failed.add(aid)
            cp["failed"] = sorted(failed)
            save_checkpoint(cp)
            polite_sleep()
            continue

        try:
            data = parse_article(html, url)
            data["paper_id"] = aid
            data["title_listing"] = art.get("title_listing")
            data["date_listing"] = art.get("date_listing")
            data["article_type"] = art.get("article_type")
            data["scraped_at"] = datetime.utcnow().isoformat() + "Z"

            with open(out_file, "w") as f:
                json.dump(data, f, indent=2, ensure_ascii=False)

            completed.add(aid)
            cp["completed"] = sorted(completed)
            cp["failed"] = sorted(failed - {aid})
            save_checkpoint(cp)
        except Exception as e:
            print(f"\n  [PARSE ERROR] {aid}: {e}")
            failed.add(aid)
            cp["failed"] = sorted(failed)
            save_checkpoint(cp)

        polite_sleep()

    print(f"\n[FASE B] Total completados: {len(completed)}")
    print(f"[FASE B] Total fallidos:    {len(failed)}")
    if failed:
        print(f"  Fallidos: {sorted(failed)}")


def consolidate():
    print(f"\n[CONSOLIDACIÓN] Juntando todos los artículos...")
    papers = []
    for jf in sorted(ARTICLES_DIR.glob("*.json")):
        with open(jf) as f:
            papers.append(json.load(f))
    with open(PAPERS_FILE, "w") as f:
        json.dump(papers, f, indent=2, ensure_ascii=False)
    print(f"  -> {len(papers)} artículos en {PAPERS_FILE}")
    print(f"  -> Tamaño del archivo: {PAPERS_FILE.stat().st_size / 1024:.1f} KB")
    return papers


if __name__ == "__main__":
    print("=" * 70)
    print(" SCRAPER NATURE MACHINE INTELLIGENCE - 2025")
    print("=" * 70)
    print(f" Carpeta de salida: {DATA_DIR}")
    print(f" Pausa entre requests: {PAUSE_MIN}-{PAUSE_MAX} segundos")
    print()


    listing = scrape_listing()


    scrape_articles(listing)

    papers = consolidate()

    print("\n" + "=" * 70)
    print(" REPORTE FINAL")
    print("=" * 70)
    n_with_abstract = sum(1 for p in papers if p.get("abstract"))
    n_with_refs = sum(1 for p in papers if p.get("references"))
    n_with_acc = sum(1 for p in papers if p.get("accesses"))
    n_with_alt = sum(1 for p in papers if p.get("altmetric"))
    print(f" Total artículos:              {len(papers)}")
    print(f" Con abstract:                 {n_with_abstract}")
    print(f" Con lista de referencias:     {n_with_refs}")
    print(f" Con métrica de accesses:      {n_with_acc}")
    print(f" Con altmetric score:          {n_with_alt}")
    if papers:
        avg_refs = sum(p.get("n_references", 0) for p in papers) / len(papers)
        print(f" Promedio referencias/paper:   {avg_refs:.1f}")
    print()