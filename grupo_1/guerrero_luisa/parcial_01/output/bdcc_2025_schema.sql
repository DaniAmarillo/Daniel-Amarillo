
DROP TABLE IF EXISTS papers;
DROP TABLE IF EXISTS references_table;

CREATE TABLE papers (
    paper_id INTEGER PRIMARY KEY AUTOINCREMENT,
    journal_name TEXT,
    title TEXT NOT NULL,
    publication_date TEXT,
    year INTEGER,
    doi TEXT,
    url TEXT,
    abstract TEXT,
    authors_raw TEXT,
    n_authors INTEGER,
    citations INTEGER,
    downloads INTEGER,
    n_references INTEGER,
    topic_label TEXT
);

CREATE TABLE references_table (
    reference_id INTEGER PRIMARY KEY AUTOINCREMENT,
    doi TEXT,
    title TEXT,
    reference_order INTEGER,
    reference_text TEXT,
    reference_text_normalized TEXT
);

