# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a **student submission repository** for the Minería de Datos (Data Mining) course, Universidad Nacional de Colombia. It is not a single application — it is a collection of independent, per-student workshop submissions (`taller_XX`) organized by group. There is no shared build system, package manifest, or test suite at the repo root; each student's subfolder is its own self-contained project (R, Python, SQL, Shiny, Streamlit, etc.) with its own dependencies.

Because of this, "commonly used commands" are determined per-submission, not globally. Look inside the specific `taller_XX` or project folder for a `requirements.txt`, `.Rmd`, `deploy.R`/`global.R`/`server.R`/`ui.R` (Shiny app), or Python script to know how to run it.

## Repository Structure

```
dm_2016325/
├── README.md              # Submission rules (Spanish) — read this for the PR/folder contract
├── grupo_1/
│   └── <apellido_nombre>/
│       ├── taller_01/ .. taller_XX/   # one folder per class activity
│       └── (occasionally extra project folders outside the taller_XX pattern)
├── grupo_2/
│   └── <apellido_nombre>/
│       └── taller_01/ .. taller_XX/
└── parcial2/               # instructor's own exam/example Shiny app (annual_reviews_2025.db, server.R, ui.R, global.R, deploy.R)
```

- Each student has one folder per group (`grupo_1` or `grupo_2`), named `apellido_nombre`.
- Inside each student folder, work is split into `taller_XX` subfolders, one per class activity, per `README.md`.
- Some student folders deviate from the strict `taller_XX` pattern (e.g. `Web-Scrapping_Taller_2_Mineria_de_Datos`, `Web-Scrapping Taller 1 Mineria de datos`) — these are student-created project folders for larger deliverables (Streamlit/Shiny apps) rather than the plain `taller_XX/` convention.
- `.gitkeep` files preserve empty `taller_XX` folders before a student has submitted; do not delete them.
- Submissions are heterogeneous by design: expect a mix of Jupyter notebooks (`.ipynb`), R Markdown (`.Rmd`), plain scripts (`.py`, `.R`), SQL/SQLite databases (`.db`, `.sqlite`), CSV datasets, and small Shiny/Streamlit apps with their own `requirements.txt`.

## Working in this repository

- **Scope discipline is the core rule.** Per `README.md`, any change should touch only one student's folder (or, for course-admin work, root-level files). Do not modify another student's folder, rename existing folders, delete classmates' work, or restructure `grupo_1`/`grupo_2`/`taller_XX` conventions.
- **Branch/PR convention**: submissions are made via a branch named `<apellido_nombre>/taller_XX` and a PR titled like `Taller 01 - Nombre Apellido`, touching only `grupo_X/apellido_nombre/taller_XX/`.
- **No global lint/build/test step** — validate a submission by running/opening it with whatever tool matches its files (e.g. `Rscript -e "rmarkdown::render('taller.Rmd')"` for `.Rmd`, `python script.py` for scripts, `jupyter nbconvert --execute` for notebooks, `Rscript app.R` / shiny run for Shiny apps).
- When asked to inspect or fix a specific submission, treat that student's subfolder as the entire project root — do not assume conventions from one student's folder apply to another's.
