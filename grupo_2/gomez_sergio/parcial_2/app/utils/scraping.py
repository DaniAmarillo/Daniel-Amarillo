import logging
import re
import sqlite3
import sys
import uuid
from datetime import datetime

import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait

# ─────────────────────────────────────────────
# Configuración del logger
# ─────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler(sys.stdout)],
)
logger = logging.getLogger(__name__)
 
 
# ─────────────────────────────────────────────
# 1. Utilidades de texto
# ─────────────────────────────────────────────
class TextUtils:
    """Funciones estáticas de normalización y clasificación de texto."""
 
    KEYWORDS: dict[str, list[str]] = {
        "Machine Learning": [
            " machine learning ", " ml ", " deep learning ",
            " neural network ", " cnn ", " rnn ",
            " random forest ", " svm ", " xgboost ",
            " clustering ", " feature selection ",
            " predictive model ", " prediction model ",
            " training dataset ", " validation dataset ",
        ],
        "IA Generativa": [
            " generative ai ", " llm ", " gpt ",
            " transformer ", " diffusion ",
            " text generation ", " image generation ",
            " large language model ",
        ],
        "Estadística": [
            " statistical ", " statistics ",
            " hypothesis ", " p-value ",
            " confidence interval ",
            " anova ", " chi-square ",
            " logistic regression ",
            " regression analysis ",
            " cox regression ",
            " survival analysis ",
            " kaplan-meier ",
            " hazard ratio ",
            " odds ratio ",
            " bayesian ",
            " probability ",
            " distribution ",
        ],
    }
 
    @staticmethod
    def normalize(text: str) -> str:
        return text.lower()
 
    @staticmethod
    def parse_number(value) -> int:
        """Convierte valores como '2.3k' → 2300."""
        value = str(value).strip().lower()
        if value.endswith("k"):
            return int(float(value[:-1]) * 1000)
        return int(value)
 
    @classmethod
    def classify_abstract(cls, abstract: str) -> str:
        """Clasifica el abstract en una categoría temática."""
        if not abstract or (isinstance(abstract, str) and abstract.strip() == ""):
            return "no se pudo clasificar"
 
        text = cls.normalize(abstract)
        scores = {k: 0 for k in cls.KEYWORDS}
        for category, words in cls.KEYWORDS.items():
            for w in words:
                if w in text:
                    scores[category] += 1
 
        best = max(scores, key=scores.get)
        return best if scores[best] > 0 else "Otros"
 
 
# ─────────────────────────────────────────────
# 2. Interacción con el navegador (Selenium)
# ─────────────────────────────────────────────
class BrowserHelper:
    """Encapsula las interacciones de bajo nivel con Selenium."""
 
    def __init__(self, driver: webdriver.Chrome, timeout: int = 10):
        self.driver = driver
        self.wait = WebDriverWait(driver, timeout)
 
    def accept_cookies(self) -> None:
        try:
            btn = self.wait.until(
                EC.element_to_be_clickable(
                    (By.CSS_SELECTOR, "button[data-cc-action='accept']")
                )
            )
            btn.click()
            logger.info("Cookies aceptadas")
        except Exception:
            logger.info("No apareció banner de cookies")
 
    def get_total_articles(self) -> int:
        span = self.driver.find_element(
            By.CSS_SELECTOR, "span[data-test='results-data-total']"
        )
        return int(re.search(r"\d+", span.text).group())
 
    def get_last_article_number(self) -> int:
        metas = self.driver.find_elements(By.CSS_SELECTOR, "div.c-meta")
        last_num = 0
        for meta in metas:
            items = meta.find_elements(By.CSS_SELECTOR, "span.c-meta__item")
            for item in items:
                match = re.search(r"Article:\s*(\d+)", item.text)
                if match:
                    last_num = max(last_num, int(match.group(1)))
        return last_num
 
    def get_article_links(self, desde_articulo: int | None = None) -> list[str]:
        all_links = []

        while True:
            cards = self.driver.find_elements(By.CSS_SELECTOR, "div.app-card-open__main")

            for card in cards:
                num_art = None
                items = card.find_elements(By.CSS_SELECTOR, "span.c-meta__item")
                for item in items:
                    match = re.search(r"Article:\s*(\d+)", item.text)
                    if match:
                        num_art = int(match.group(1))

                if desde_articulo is not None and desde_articulo != 0:
                    if num_art is None or num_art < desde_articulo:
                        continue

                try:
                    href = card.find_element(
                        By.CSS_SELECTOR, "h2.app-card-open__heading a"
                    ).get_attribute("href")
                    if href and href not in all_links:
                        all_links.append(href)
                except Exception:
                    pass

            # Si desde_articulo != 0, no necesitamos paginar
            if desde_articulo is not None and desde_articulo != 0:
                break

            # Buscar botón "Next" y continuar si existe
            try:
                next_item = self.driver.find_element(
                    By.CSS_SELECTOR, "li.eds-c-pagination__item--next"
                )
                next_link = next_item.find_element(By.CSS_SELECTOR, "a.eds-c-pagination__link")
                next_url = next_link.get_attribute("href")

                if not next_url:
                    break

                self.driver.get(next_url)
                # Esperar a que carguen las cards
                WebDriverWait(self.driver, 10).until(
                    EC.presence_of_element_located((By.CSS_SELECTOR, "div.app-card-open__main"))
                )
            except Exception:
                # No hay botón next o no es clickeable
                break

        return all_links
 
    def navigate_and_wait(self, url: str, locator) -> None:
        self.driver.get(url)
        self.wait.until(EC.presence_of_element_located(locator))
 
 
# ─────────────────────────────────────────────
# 3. Scraper de artículos individuales
# ─────────────────────────────────────────────
class ArticleScraper:
    """Extrae los datos de un artículo individual."""
 
    def __init__(self, browser: BrowserHelper):
        self.browser = browser
        self.driver = browser.driver
        self.wait = browser.wait
 
    def scrape(self, links: list[str]) -> list[dict]:
        data = []
        for link in links:
            self.driver.get(link)
            self.wait.until(EC.presence_of_element_located((By.TAG_NAME, "body")))
            logger.info("Visitando: %s", link)
 
            title_el = self.driver.find_element(
                By.XPATH, '//*[@id="main"]/section/div/div/div[1]/h1'
            )
            title = title_el.text
 
            if title.startswith("Retraction Note:") or title.startswith("Correction:"):
                logger.info("Omitido (corrección/retractación): %s", title)
                continue
 
            record = self._extract_record(link, title)
            data.append(record)
 
        return data
 
    def _extract_record(self, link: str, title: str) -> dict:
        driver = self.driver
 
        journal_el = driver.find_element(
            By.XPATH, '//*[@id="main"]/section/div/div/div[2]/a[1]/span'
        )
        date_el = driver.find_element(
            By.XPATH, '//*[@id="main"]/section/div/div/div[1]/ul[1]/li[3]/time'
        )
        parsed_date = datetime.strptime(date_el.text, "%d %B %Y")
 
        # DOI
        doi = None
        for item in driver.find_elements(
            By.CSS_SELECTOR, "li.c-bibliographic-information__list-item"
        ):
            if item.find_elements(By.CSS_SELECTOR, 'abbr[title="Digital Object Identifier"]'):
                doi = item.find_element(
                    By.CSS_SELECTOR, "span.c-bibliographic-information__value"
                ).text
                break
 
        # Abstract
        abstract_el = driver.find_elements(By.CSS_SELECTOR, "#Abs1-content")
        abstract = abstract_el[0].text if abstract_el else ""
 
        # Autores
        authors_list = []
        for a in driver.find_elements(By.CSS_SELECTOR, "li.c-article-author-list__item"):
            try:
                tag = a.find_element(By.CSS_SELECTOR, "a[data-test='author-name']")
                authors_list.append(tag.text.strip())
            except Exception:
                pass
 
        # Citas y descargas
        citations_el = driver.find_elements(
            By.CSS_SELECTOR, 'li[data-test="citation-count"] p'
        )
        downloads_el = driver.find_elements(
            By.CSS_SELECTOR, 'li[data-test="access-count"] p'
        )
 
        citations = TextUtils.parse_number(
            citations_el[0].text.split()[0] if citations_el else "0"
        )
        downloads = TextUtils.parse_number(
            downloads_el[0].text.split()[0] if downloads_el else "0"
        )
 
        # Referencias
        refs_list = [
            r.text.strip()
            for r in driver.find_elements(By.CSS_SELECTOR, "p.c-article-references__text")
        ]
 
        return {
            "paper_id": str(uuid.uuid4()),
            "journal_name": journal_el.text,
            "title": title,
            "publication_date": parsed_date.strftime("%Y-%m-%d"),
            "year": parsed_date.year,
            "doi": doi,
            "url": link,
            "abstract": abstract,
            "authors": ",".join(authors_list),
            "n_authors": len(authors_list),
            "citations": citations,
            "downloads": downloads,
            "references": "<REF>".join(refs_list),
            "n_references": len(refs_list),
            "topic_label": TextUtils.classify_abstract(abstract),
        }
 
 
# ─────────────────────────────────────────────
# 4. Procesador de volúmenes
# ─────────────────────────────────────────────
class VolumeProcessor:
    """Decide la estrategia de scraping por volumen (nuevo / completo / parcial)."""
 
    def __init__(self, browser: BrowserHelper, scraper: ArticleScraper):
        self.browser = browser
        self.scraper = scraper
        self.driver = browser.driver
 
    def process(
        self,
        volume_filtrado: list[dict],
        articulos: pd.DataFrame,
    ) -> tuple[pd.DataFrame, list[dict], dict]:
        all_data: list[dict] = []
        article_links_last: list[str] = []
 
        for vol in volume_filtrado:
            vol_num = vol["volume"]
            vol_en_df = articulos[articulos["volume"] == vol_num]
 
            for link in vol["links"]:
                self.driver.get(link)
                total_actuales = self.browser.get_total_articles()
 
                # ── CASO 1: Volumen nuevo ──────────────────────────────
                if vol_en_df.empty:
                    logger.info("[NUEVO] Volumen %s → scraping completo", vol_num)
                    article_links_last = self.browser.get_article_links()
                    nueva_fila = pd.DataFrame(
                        [{"volume": vol_num, "articles": self.browser.get_last_article_number()}]
                    )
                    articulos = pd.concat([articulos, nueva_fila], ignore_index=True)
                    data = self.scraper.scrape(article_links_last)
                    all_data.extend(data)
                    logger.info("[NUEVO] %d artículos extraídos", len(article_links_last))
 
                else:
                    total_df = vol_en_df["articles"].values[0]
 
                    # ── CASO 2: Al día ─────────────────────────────────
                    if total_actuales == total_df:
                        logger.info("[OK] Volumen %s - link al día, siguiente...", vol_num)
                        continue
 
                    # ── CASO 3: Hay artículos nuevos ───────────────────
                    elif total_actuales > total_df:
                        logger.info(
                            "[ACTUALIZAR] Volumen %s: web=%d BD=%d → desde artículo %d",
                            vol_num, total_actuales, total_df, total_df + 1,
                        )
                        articulos.loc[
                            articulos["volume"] == vol_num, "articles"
                        ] = self.browser.get_last_article_number()
                        article_links_last = self.browser.get_article_links(
                            desde_articulo=total_df + 1
                        )
                        data = self.scraper.scrape(article_links_last)
                        all_data.extend(data)
                        logger.info(
                            "[ACTUALIZAR] %d artículos nuevos desde %d",
                            len(article_links_last), total_df + 1,
                        )
 
            logger.info("[FIN] Volumen %s procesado", vol_num)
 
        return articulos, all_data, {"agregados": len(article_links_last)}
 
 
# ─────────────────────────────────────────────
# 5. Persistencia en SQLite
# ─────────────────────────────────────────────
class DatabaseManager:
    """Maneja la lectura y escritura en SQLite."""
 
    def __init__(self, db_path: str):
        self.db_path = db_path
 
    def load_articles(self) -> pd.DataFrame:
        with sqlite3.connect(self.db_path) as conn:
            return pd.read_sql_query("SELECT * FROM articles", conn)
 
    def save_results(
        self,
        papers: list[dict],
        articulos: pd.DataFrame,
        notification: dict,
    ) -> None:
        with sqlite3.connect(self.db_path) as conn:
            if papers:
                pd.DataFrame(papers).to_sql("papers", conn, if_exists="append", index=False)
            pd.DataFrame(articulos).to_sql("articles", conn, if_exists="replace", index=False)
            pd.DataFrame([notification]).to_sql(
                "notification", conn, if_exists="replace", index=False
            )
        logger.info("Resultados guardados en %s", self.db_path)
 
 
# ─────────────────────────────────────────────
# 6. Job principal
# ─────────────────────────────────────────────
class ScrapingJob:
    """
    Orquesta todo el flujo end-to-end.
    Instanciar y llamar a run() desde R con reticulate:
 
        py <- reticulate::import_from_path("scraping_job", path = ".")
        job <- py$ScrapingJob(
            db_path  = "revista_q1_2025.sqlite",
            base_url = "https://link.springer.com/journal/13045/volumes-and-issues"
        )
        job$run()
    """
 
    def __init__(
        self,
        db_path: str = "./revista_q1_2025.sqlite",
        base_url: str = "https://link.springer.com/journal/13045/volumes-and-issues",
        timeout: int = 10,
    ):
        self.db_path = db_path
        self.base_url = base_url
        self.timeout = timeout
 
    def run(self) -> dict:
        logger.info("=== ScrapingJob iniciado ===")
 
        # Inicializar driver y helpers
        driver = webdriver.Chrome()
        browser = BrowserHelper(driver, self.timeout)
        scraper = ArticleScraper(browser)
        processor = VolumeProcessor(browser, scraper)
        db = DatabaseManager(self.db_path)
 
        try:
            # Cargar estado previo
            articulos = db.load_articles()
 
            # Navegar y aceptar cookies
            browser.navigate_and_wait(
                self.base_url,
                (By.CSS_SELECTOR, "div[class='c-list-bullets']"),
            )
            browser.accept_cookies()
 
            # Obtener lista de volúmenes/issues
            volumes_issues = driver.find_elements(
                By.CSS_SELECTOR, "li.app-vol-and-issues-item"
            )
            volumes = [
                {
                    "volume": int(
                        a.find_element(By.CSS_SELECTOR, "h2 span:first-child")
                        .text.split()[-1]
                    ),
                    "links": [
                        lnk.get_attribute("href")
                        for lnk in a.find_elements(By.CSS_SELECTOR, "a.c-list-group__link")
                    ],
                }
                for a in volumes_issues
            ]
 
            # Filtrar desde el volumen de referencia
            volumen_ref = articulos["volume"].max()
            volume_filtrado = [v for v in volumes if v["volume"] >= volumen_ref]
 
            # Procesar
            articulos_upd, datos, notification = processor.process(
                volume_filtrado, articulos
            )
 
            # Persistir
            db.save_results(datos, articulos_upd, notification)
 
        finally:
            driver.quit()
            logger.info("=== ScrapingJob finalizado ===")
 
        return notification
 
 
# ─────────────────────────────────────────────
# Punto de entrada directo
# ─────────────────────────────────────────────
if __name__ == "__main__":
    job = ScrapingJob()
    result = job.run()
    logger.info("Resumen: %s", result)