
PRAGMA foreign_keys = ON;

CREATE TABLE papers (
  paper_id INTEGER PRIMARY KEY AUTOINCREMENT,
  journal_name TEXT,
  title TEXT NOT NULL,
  authors_raw TEXT,
  publication_date TEXT,
  year INTEGER,
  doi TEXT UNIQUE,
  url TEXT,
  abstract TEXT,
  n_authors INTEGER,
  citations INTEGER,
  views INTEGER,
  downloads INTEGER,
  n_references INTEGER,
  topic_label TEXT
);

CREATE TABLE authors (
  author_id INTEGER PRIMARY KEY AUTOINCREMENT,
  author_name TEXT NOT NULL UNIQUE
);

CREATE TABLE paper_authors (
  paper_id INTEGER,
  author_id INTEGER,
  author_order INTEGER,
  FOREIGN KEY (paper_id) REFERENCES papers(paper_id),
  FOREIGN KEY (author_id) REFERENCES authors(author_id)
);

CREATE TABLE references_table (
  reference_id INTEGER PRIMARY KEY AUTOINCREMENT,
  reference_text_normalized TEXT NOT NULL UNIQUE
);

CREATE TABLE paper_references (
  paper_id INTEGER,
  reference_id INTEGER,
  reference_order INTEGER,
  FOREIGN KEY (paper_id) REFERENCES papers(paper_id),
  FOREIGN KEY (reference_id) REFERENCES references_table(reference_id)
);

