from __future__ import annotations

import re
import sqlite3
import unicodedata
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable, Sequence

import numpy as np
import scipy.sparse as sp
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import (
    ENGLISH_STOP_WORDS,
    CountVectorizer,
    TfidfVectorizer,
)


ACADEMIC_STOPWORDS = frozenset({
    "paper", "papers", "article", "articles", "study", "studies",
    "work", "works", "present", "presents", "presented",
    "propose", "proposes", "proposed", "propose", "proposal",
    "show", "shows", "shown", "demonstrate", "demonstrates", "demonstrated",
    "result", "results", "finding", "findings",
    "approach", "approaches", "framework", "frameworks",
    "here", "however", "thus", "therefore", "moreover", "furthermore",
    "recent", "recently", "novel", "new",
    "use", "used", "uses", "using", "usage",
    "based", "via", "toward", "towards",
})

STOPWORDS = frozenset(ENGLISH_STOP_WORDS) | ACADEMIC_STOPWORDS


_TOKEN_RE = re.compile(r"[a-z][a-z\-']{2,}")

_STEMMER = None


def _get_stemmer():
    global _STEMMER
    if _STEMMER is None:
        from nltk.stem.snowball import SnowballStemmer
        _STEMMER = SnowballStemmer("english")
    return _STEMMER


def normalize_text(text: str) -> str:
    if not text:
        return ""
    text = unicodedata.normalize("NFKD", text)
    text = "".join(c for c in text if not unicodedata.combining(c))
    return text.lower()


def tokenize(text: str, *, use_stemming: bool = True,
             remove_stopwords: bool = True) -> list[str]:
    tokens = _TOKEN_RE.findall(normalize_text(text))
    expanded: list[str] = []
    for tok in tokens:
        expanded.append(tok)
        if "-" in tok:
            expanded.extend(p for p in tok.split("-") if len(p) >= 3)
    if remove_stopwords:
        expanded = [t for t in expanded if t not in STOPWORDS]
    if use_stemming:
        st = _get_stemmer()
        expanded = [st.stem(t) for t in expanded]
    return expanded


class Analyzer:
    def __init__(self, use_stemming: bool = True, remove_stopwords: bool = True):
        self.use_stemming = use_stemming
        self.remove_stopwords = remove_stopwords

    def __call__(self, doc: str) -> list[str]:
        return tokenize(doc, use_stemming=self.use_stemming,
                        remove_stopwords=self.remove_stopwords)

    def __repr__(self) -> str:
        return (f"Analyzer(use_stemming={self.use_stemming}, "
                f"remove_stopwords={self.remove_stopwords})")


def make_analyzer(use_stemming: bool = True) -> Analyzer:
    """Devuelve un analizador picklable."""
    return Analyzer(use_stemming=use_stemming)

TITLE_BOOST = 3
SUBJECTS_BOOST = 2


@dataclass
class Corpus:
    paper_ids: list[str]
    texts: list[str]
    meta: list[dict] = field(default_factory=list)

    def __len__(self) -> int:
        return len(self.texts)


def load_corpus(db_path: str | Path) -> Corpus:
    conn = sqlite3.connect(str(db_path))
    conn.row_factory = sqlite3.Row
    has_subjects = any(
        r["name"] == "subjects_raw"
        for r in conn.execute("PRAGMA table_info(papers)").fetchall()
    )
    cols = ("paper_id, title, abstract, authors_raw, publication_date, doi, url, "
            "topic_label, citations, downloads, n_authors")
    if has_subjects:
        cols += ", subjects_raw"
    rows = conn.execute(
        f"SELECT {cols} FROM papers ORDER BY paper_id"
    ).fetchall()
    conn.close()

    paper_ids, texts, meta = [], [], []
    for r in rows:
        title = (r["title"] or "").strip()
        abstract = (r["abstract"] or "").strip()
        subjects = (r["subjects_raw"] or "").strip() if has_subjects else ""

        parts = [title] * TITLE_BOOST + [abstract] + [subjects] * SUBJECTS_BOOST
        texts.append(" ".join(p for p in parts if p))

        paper_ids.append(r["paper_id"])
        meta.append({
            "paper_id": r["paper_id"],
            "title": title,
            "abstract": abstract,
            "subjects": subjects,
            "authors": r["authors_raw"] or "",
            "publication_date": r["publication_date"] or "",
            "doi": r["doi"] or "",
            "url": r["url"] or "",
            "topic_label": r["topic_label"] or "",
            "citations": r["citations"],
            "downloads": r["downloads"],
        })
    return Corpus(paper_ids=paper_ids, texts=texts, meta=meta)


class BM25Index:

    def __init__(self, k1: float = 1.5, b: float = 0.75):
        self.k1 = k1
        self.b = b
        self.vectorizer: CountVectorizer | None = None
        self.W: sp.csc_matrix | None = None
        self.idf: np.ndarray | None = None
        self.doc_lengths: np.ndarray | None = None
        self.avgdl: float = 0.0

    def fit(self, texts: Sequence[str], analyzer, min_df: int = 1,
            max_df: float = 1.0) -> "BM25Index":
        self.vectorizer = CountVectorizer(
            analyzer=analyzer, min_df=min_df, max_df=max_df, dtype=np.float32
        )
        counts = self.vectorizer.fit_transform(texts).tocsr()  
        n_docs, _ = counts.shape

        self.doc_lengths = np.asarray(counts.sum(axis=1)).ravel()
        self.avgdl = float(self.doc_lengths.mean())

        df = np.asarray((counts > 0).sum(axis=0)).ravel()
        self.idf = np.log(1.0 + (n_docs - df + 0.5) / (df + 0.5)).astype(np.float32)


        norm = (self.k1 * (1.0 - self.b + self.b * self.doc_lengths / self.avgdl)
                ).astype(np.float32)


        W = counts.copy()
        rows = np.repeat(np.arange(n_docs), np.diff(W.indptr))
        tf = W.data
        W.data = (tf * (self.k1 + 1.0)) / (tf + norm[rows])
        # Multiplicar por el IDF de la columna correspondiente.
        W.data = W.data * self.idf[W.indices]

        self.W = W.tocsc() 
        return self

    @property
    def vocabulary_size(self) -> int:
        return 0 if self.vectorizer is None else len(self.vectorizer.vocabulary_)

    def score(self, query: str) -> np.ndarray:
        """Puntaje BM25 de cada documento frente a la consulta."""
        assert self.vectorizer is not None and self.W is not None
        tokens = self.vectorizer.build_analyzer()(query)
        vocab = self.vectorizer.vocabulary_
        idxs = [vocab[t] for t in tokens if t in vocab]
        if not idxs:
            return np.zeros(self.W.shape[0], dtype=np.float32)
        # Sumar una columna por cada ocurrencia del término en la consulta
        # (equivale a usar la frecuencia del término en la query, qtf).
        scores = np.asarray(self.W[:, idxs].sum(axis=1)).ravel()
        return scores.astype(np.float32)



class LSAIndex:


    def __init__(self, n_components: int = 80, random_state: int = 42):
        self.n_components = n_components
        self.random_state = random_state
        self.vectorizer: TfidfVectorizer | None = None
        self.svd: TruncatedSVD | None = None
        self.doc_vectors: np.ndarray | None = None   
        self.explained_variance_: float = 0.0

    def fit(self, texts: Sequence[str], analyzer, min_df: int = 2,
            max_df: float = 0.6) -> "LSAIndex":
        self.vectorizer = TfidfVectorizer(
            analyzer=analyzer, min_df=min_df, max_df=max_df,
            sublinear_tf=True, dtype=np.float32,
        )
        X = self.vectorizer.fit_transform(texts)          
        self.tfidf_shape_ = X.shape
        self.tfidf_density_ = X.nnz / (X.shape[0] * X.shape[1])

        max_k = min(X.shape) - 1
        k = min(self.n_components, max_k)
        if k != self.n_components:
            self.n_components = k

        self.svd = TruncatedSVD(n_components=k, random_state=self.random_state,
                                algorithm="randomized", n_iter=7)
        reduced = self.svd.fit_transform(X)               
        self.explained_variance_ = float(self.svd.explained_variance_ratio_.sum())
        self.doc_vectors = _l2_normalize(reduced).astype(np.float32)
        return self

    @property
    def vocabulary_size(self) -> int:
        return 0 if self.vectorizer is None else len(self.vectorizer.vocabulary_)

    def project_query(self, query: str) -> np.ndarray:
        """Proyecta la consulta al espacio latente de k dimensiones."""
        assert self.vectorizer is not None and self.svd is not None
        q_tfidf = self.vectorizer.transform([query])       
        q_red = self.svd.transform(q_tfidf)                
        return _l2_normalize(q_red)[0].astype(np.float32)

    def score(self, query: str) -> np.ndarray:
        assert self.doc_vectors is not None
        q = self.project_query(query)
        if not np.any(q):
            return np.zeros(self.doc_vectors.shape[0], dtype=np.float32)
        return (self.doc_vectors @ q).astype(np.float32)


def _l2_normalize(mat: np.ndarray) -> np.ndarray:
    norms = np.linalg.norm(mat, axis=1, keepdims=True)
    norms[norms == 0] = 1.0
    return mat / norms


def reciprocal_rank_fusion(score_arrays: Sequence[np.ndarray],
                           k: int = 60,
                           weights: Sequence[float] | None = None) -> np.ndarray:
    if weights is None:
        weights = [1.0] * len(score_arrays)
    n = len(score_arrays[0])
    fused = np.zeros(n, dtype=np.float32)
    for scores, w in zip(score_arrays, weights):
        order = np.argsort(-scores, kind="stable")
        ranks = np.empty(n, dtype=np.int32)
        ranks[order] = np.arange(1, n + 1)
        fused += (w / (k + ranks)).astype(np.float32)
    return fused


STRATEGIES = {
    "bm25":   "BM25 (léxica)",
    "lsa":    "LSA · TF-IDF + SVD (semántica)",
    "hybrid": "Híbrida RRF (BM25 + LSA)",
}


@dataclass
class SearchResult:
    rank: int
    paper_id: str
    title: str
    authors: str
    publication_date: str
    topic_label: str
    doi: str
    url: str
    score: float
    snippet: str
    abstract: str
    citations: int | None = None
    downloads: int | None = None


class SearchEngine:

    def __init__(self, corpus: Corpus, bm25: BM25Index, lsa: LSAIndex,
                 config: dict | None = None):
        self.corpus = corpus
        self.bm25 = bm25
        self.lsa = lsa
        self.config = config or {}

    @classmethod
    def build(cls, db_path: str | Path, n_components: int = 80,
              use_stemming: bool = True, bm25_k1: float = 1.5,
              bm25_b: float = 0.75, tfidf_min_df: int = 2,
              tfidf_max_df: float = 0.6) -> "SearchEngine":
        corpus = load_corpus(db_path)
        analyzer = make_analyzer(use_stemming=use_stemming)

        bm25 = BM25Index(k1=bm25_k1, b=bm25_b).fit(
            corpus.texts, analyzer, min_df=1, max_df=1.0
        )
        lsa = LSAIndex(n_components=n_components).fit(
            corpus.texts, analyzer, min_df=tfidf_min_df, max_df=tfidf_max_df
        )
        config = {
            "n_documents": len(corpus),
            "use_stemming": use_stemming,
            "bm25_k1": bm25_k1,
            "bm25_b": bm25_b,
            "bm25_vocabulary": bm25.vocabulary_size,
            "tfidf_min_df": tfidf_min_df,
            "tfidf_max_df": tfidf_max_df,
            "tfidf_vocabulary": lsa.vocabulary_size,
            "tfidf_shape": lsa.tfidf_shape_,
            "tfidf_density": lsa.tfidf_density_,
            "lsa_components": lsa.n_components,
            "lsa_explained_variance": lsa.explained_variance_,
            "title_boost": TITLE_BOOST,
            "subjects_boost": SUBJECTS_BOOST,
        }
        return cls(corpus, bm25, lsa, config)

    def query_has_signal(self, query: str) -> bool:
        tokens = self.bm25.vectorizer.build_analyzer()(query)
        if not tokens:
            return False
        bm25_vocab = self.bm25.vectorizer.vocabulary_
        tfidf_vocab = self.lsa.vectorizer.vocabulary_
        return any(t in bm25_vocab for t in tokens) or \
               any(t in tfidf_vocab for t in tokens)

    def raw_scores(self, query: str, strategy: str) -> np.ndarray:
        if strategy == "bm25":
            return self.bm25.score(query)
        if strategy == "lsa":
            return self.lsa.score(query)
        if strategy == "hybrid":
            bm25_s = self.bm25.score(query)
            lsa_s = self.lsa.score(query)
            if not np.any(bm25_s) and not np.any(lsa_s):
                return np.zeros_like(bm25_s)
            return reciprocal_rank_fusion([bm25_s, lsa_s])
        raise ValueError(f"Estrategia desconocida: {strategy!r}")

    def search(self, query: str, strategy: str = "hybrid", top_k: int = 10,
               min_score: float = 0.0) -> list[SearchResult]:
        query = (query or "").strip()
        if not query or not self.query_has_signal(query):
            return []

        scores = self.raw_scores(query, strategy)

        cites = np.array(
            [(m["citations"] or 0) for m in self.corpus.meta], dtype=np.float64
        )
        ids = np.array(self.corpus.paper_ids)
        order = np.lexsort((ids, -cites, -scores))

        results: list[SearchResult] = []
        for pos, idx in enumerate(order[:top_k], start=1):
            s = float(scores[idx])
            if s <= min_score:
                continue
            m = self.corpus.meta[idx]
            results.append(SearchResult(
                rank=pos,
                paper_id=m["paper_id"],
                title=m["title"],
                authors=m["authors"],
                publication_date=m["publication_date"],
                topic_label=m["topic_label"],
                doi=m["doi"],
                url=m["url"],
                score=s,
                snippet=make_snippet(m["abstract"], query),
                abstract=m["abstract"],
                citations=m["citations"],
                downloads=m["downloads"],
            ))
        return results

    def search_all(self, query: str, top_k: int = 10) -> dict[str, list[SearchResult]]:
        return {s: self.search(query, s, top_k) for s in STRATEGIES}

    def top_ids(self, query: str, strategy: str, k: int = 5) -> list[str]:
        return [r.paper_id for r in self.search(query, strategy, top_k=k)]

    def save(self, path: str | Path) -> Path:
        import joblib
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        joblib.dump(
            {"corpus": self.corpus, "bm25": self.bm25,
             "lsa": self.lsa, "config": self.config},
            path, compress=3,
        )
        return path

    @classmethod
    def load(cls, path: str | Path) -> "SearchEngine":
        import joblib
        obj = joblib.load(str(path))
        return cls(obj["corpus"], obj["bm25"], obj["lsa"], obj["config"])



def make_snippet(abstract: str, query: str, width: int = 320) -> str:
    if not abstract:
        return ""
    if len(abstract) <= width:
        return abstract

    q_stems = set(tokenize(query))
    if not q_stems:
        return abstract[:width].rsplit(" ", 1)[0] + "…"

    sentences = re.split(r"(?<=[.!?])\s+", abstract)
    best_i, best_hits = 0, -1
    for i, sent in enumerate(sentences):
        hits = sum(1 for t in tokenize(sent) if t in q_stems)
        if hits > best_hits:
            best_i, best_hits = i, hits

    snippet, i, j = sentences[best_i], best_i, best_i
    while len(snippet) < width and (i > 0 or j < len(sentences) - 1):
        if j < len(sentences) - 1:
            j += 1
            snippet = snippet + " " + sentences[j]
        elif i > 0:
            i -= 1
            snippet = sentences[i] + " " + snippet
    prefix = "…" if best_i > 0 and i > 0 else ""
    suffix = "…" if j < len(sentences) - 1 else ""
    return prefix + snippet[:width].rsplit(" ", 1)[0] + suffix