
import sqlite3
import re
from pathlib import Path
from typing import Optional
from contextlib import contextmanager

import pandas as pd
import streamlit as st

DB_PATH = Path(__file__).resolve().parent / "revista_q1_2025.sqlite"


@contextmanager
def get_conn():
    conn = sqlite3.connect(DB_PATH, check_same_thread=False)
    conn.row_factory = sqlite3.Row
    try:
        yield conn
    finally:
        conn.close()


def db_exists() -> bool:
    return DB_PATH.exists()


@st.cache_data(ttl=60, show_spinner=False)
def get_all_papers() -> pd.DataFrame:
    with get_conn() as conn:
        df = pd.read_sql_query(
            """
            SELECT paper_id, journal_name, title, publication_date, year,
                   doi, url, abstract, authors_raw, n_authors,
                   citations, downloads, altmetric, n_references,
                   topic_label, article_type
            FROM papers
            ORDER BY publication_date DESC
            """,
            conn,
        )
    df["publication_date"] = pd.to_datetime(df["publication_date"], errors="coerce")
    return df


@st.cache_data(ttl=60, show_spinner=False)
def get_topic_list() -> list[str]:
    with get_conn() as conn:
        rows = conn.execute(
            "SELECT DISTINCT topic_label FROM papers ORDER BY topic_label"
        ).fetchall()
    return [r["topic_label"] for r in rows if r["topic_label"]]


@st.cache_data(ttl=60, show_spinner=False)
def get_author_list() -> list[str]:
    with get_conn() as conn:
        rows = conn.execute(
            "SELECT author_name FROM authors ORDER BY author_name"
        ).fetchall()
    return [r["author_name"] for r in rows]


@st.cache_data(ttl=60, show_spinner=False)
def get_year_range() -> tuple[int, int]:
    with get_conn() as conn:
        row = conn.execute(
            "SELECT MIN(year) AS mn, MAX(year) AS mx FROM papers"
        ).fetchone()
    return (row["mn"] or 2025), (row["mx"] or 2025)


def clear_all_caches():
    get_all_papers.clear()
    get_topic_list.clear()
    get_author_list.clear()
    get_year_range.clear()


def filter_papers(
    df: pd.DataFrame,
    date_range: Optional[tuple] = None,
    topics: Optional[list[str]] = None,
    author: Optional[str] = None,
    doi: Optional[str] = None,
    keyword: Optional[str] = None,
) -> pd.DataFrame:
    result = df.copy()

    if date_range and len(date_range) == 2:
        d0, d1 = pd.to_datetime(date_range[0]), pd.to_datetime(date_range[1])
        result = result[
            (result["publication_date"] >= d0) & (result["publication_date"] <= d1)
        ]

    if topics:
        result = result[result["topic_label"].isin(topics)]

    if author:
        result = result[
            result["authors_raw"].fillna("").str.contains(
                re.escape(author), case=False, regex=True
            )
        ]

    if doi:
        result = result[
            result["doi"].fillna("").str.contains(re.escape(doi), case=False, regex=True)
        ]

    if keyword:
        kw = re.escape(keyword)
        in_title = result["title"].fillna("").str.contains(kw, case=False, regex=True)
        in_abs = result["abstract"].fillna("").str.contains(kw, case=False, regex=True)
        result = result[in_title | in_abs]

    return result.reset_index(drop=True)


def compute_kpis(df: pd.DataFrame) -> dict:
    if df.empty:
        return {
            "total_papers": 0, "avg_authors": 0, "avg_citations": 0,
            "avg_references": 0, "total_downloads": 0,
            "most_cited": None, "most_downloaded": None,
            "by_topic": {},
        }

    most_cited_idx = df["citations"].fillna(-1).idxmax()
    most_dl_idx = df["downloads"].fillna(-1).idxmax()

    return {
        "total_papers": len(df),
        "avg_authors": float(df["n_authors"].mean()),
        "avg_citations": float(df["citations"].dropna().mean() or 0),
        "avg_references": float(df["n_references"].mean()),
        "total_downloads": int(df["downloads"].fillna(0).sum()),
        "most_cited": df.loc[most_cited_idx, ["title", "citations", "doi"]].to_dict(),
        "most_downloaded": df.loc[most_dl_idx, ["title", "downloads", "doi"]].to_dict(),
        "by_topic": df["topic_label"].value_counts().to_dict(),
    }


def get_existing_dois() -> set[str]:
    with get_conn() as conn:
        rows = conn.execute("SELECT doi FROM papers WHERE doi IS NOT NULL").fetchall()
    return {r["doi"] for r in rows}


def get_existing_paper_ids() -> set[str]:
    with get_conn() as conn:
        rows = conn.execute("SELECT paper_id FROM papers").fetchall()
    return {r["paper_id"] for r in rows}


def get_recent_paper_ids(n: int = 5) -> list[str]:
    with get_conn() as conn:
        rows = conn.execute(
            "SELECT paper_id FROM papers ORDER BY publication_date DESC LIMIT ?",
            (n,),
        ).fetchall()
    return [r["paper_id"] for r in rows]


def insert_paper(paper: dict, topic_label: str) -> bool:
    if not paper.get("paper_id"):
        return False
    with get_conn() as conn:
        cur = conn.cursor()
        exists = cur.execute(
            "SELECT 1 FROM papers WHERE paper_id = ?", (paper["paper_id"],)
        ).fetchone()
        if exists:
            return False

        year = _parse_year(paper.get("publication_date") or paper.get("online_date"))

        cur.execute(
            """
            INSERT INTO papers (
                paper_id, journal_name, title, publication_date, year,
                doi, url, abstract, authors_raw, n_authors,
                citations, downloads, altmetric, n_references,
                topic_label, article_type
            ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
            """,
            (
                paper["paper_id"],
                paper.get("journal", "Nature Machine Intelligence"),
                paper.get("title"),
                paper.get("publication_date"),
                year,
                paper.get("doi"),
                paper.get("url"),
                paper.get("abstract"),
                "; ".join(paper.get("authors", [])),
                paper.get("n_authors", len(paper.get("authors", []))),
                paper.get("citations_nature") or paper.get("citations"),
                paper.get("accesses") or paper.get("downloads"),
                paper.get("altmetric"),
                paper.get("n_references", len(paper.get("references", []))),
                topic_label,
                paper.get("article_type"),
            ),
        )

        for order, name in enumerate(paper.get("authors", []), start=1):
            name = (name or "").strip()
            if not name:
                continue
            cur.execute(
                "INSERT OR IGNORE INTO authors (author_name) VALUES (?)", (name,)
            )
            aid = cur.execute(
                "SELECT author_id FROM authors WHERE author_name = ?", (name,)
            ).fetchone()[0]
            cur.execute(
                "INSERT OR IGNORE INTO paper_authors VALUES (?, ?, ?)",
                (paper["paper_id"], aid, order),
            )

        for order, ref in enumerate(paper.get("references", []), start=1):
            ref = (ref or "").strip()
            if not ref:
                continue
            norm = _normalize_ref(ref)
            if not norm:
                continue
            cur.execute(
                "INSERT OR IGNORE INTO refs (reference_text, reference_text_normalized) VALUES (?, ?)",
                (ref, norm),
            )
            rid = cur.execute(
                "SELECT reference_id FROM refs WHERE reference_text_normalized = ?",
                (norm,),
            ).fetchone()[0]
            cur.execute(
                "INSERT OR IGNORE INTO paper_refs VALUES (?, ?, ?)",
                (paper["paper_id"], rid, order),
            )

        conn.commit()
    return True


def update_paper_metrics(paper_id: str, paper: dict) -> bool:
    with get_conn() as conn:
        cur = conn.cursor()
        cur.execute(
            """
            UPDATE papers
            SET citations = COALESCE(?, citations),
                downloads = COALESCE(?, downloads),
                altmetric = COALESCE(?, altmetric)
            WHERE paper_id = ?
            """,
            (
                paper.get("citations_nature") or paper.get("citations"),
                paper.get("accesses") or paper.get("downloads"),
                paper.get("altmetric"),
                paper_id,
            ),
        )
        conn.commit()
        return cur.rowcount > 0


def _parse_year(date_str):
    if not date_str:
        return None
    m = re.search(r"(\d{4})", date_str)
    return int(m.group(1)) if m else None


def _normalize_ref(t):
    if not t:
        return ""
    s = t.lower()
    s = re.sub(r"https?://\S+", "", s)
    s = re.sub(r"doi[: ]\S+", "", s)
    s = re.sub(r"[^\w\s]", " ", s)
    return re.sub(r"\s+", " ", s).strip()
