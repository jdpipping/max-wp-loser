# Extreme-Path Benchmarks for Sequential Probability Forecasts

Benchmark distributions for how often sequential forecasts hit extreme values on paths that ultimately fail. Theory, simulations, and ESPN win-probability analysis for NFL and NBA.

## Quick Links

- **Manuscript:** `writing/manuscript/main.pdf`
- **AOAS version:** `writing/aoas/main.pdf`
- **NESSIS 2025 slides:** `presentations/nessis2025/slides.pdf`
- **WSABI research note:** `writing/research-note/research-note.pdf`
- **Poster:** `presentations/poster/poster.pdf`

## Directory Structure

```text
.
├── R/                      # reusable plotting/style helpers
├── scripts/                # runnable analysis and figure-generation entry points
├── data/
│   ├── raw/                # place raw source pulls here
│   └── derived/            # canonical derived NFL/NBA analysis data
├── results/
│   ├── figures/            # generated shared figures
│   ├── tables/             # generated tables
│   └── logs/               # optional run logs
├── writing/                # manuscript, AOAS version, arXiv, research note
├── presentations/          # slides, poster, lecture assets
└── archive/                # historical drafts and retired project variants
```

## Code

Run scripts from the repository root.

- **`scripts/manuscript/`** Shared theory, simulation, and NFL/NBA diagnostic scripts used by both `writing/manuscript` and `writing/aoas`.
- **`scripts/presentations/`** Figure-generation scripts for talks and poster assets.
- **`scripts/writing/research-note.R`** Research-note simulation and empirical graphics.
- **`scripts/analyses/ravens/analysis.R`** Ravens-specific side analysis.
- **`R/plot-style.R`** Shared plotting theme and palette helper.

## Data And Results

- **`data/derived/nfl/`** and **`data/derived/nba/`** are the canonical empirical inputs for the current manuscript and AOAS analysis.
- **`results/figures/manuscript/`** stores the shared figures consumed by both manuscript variants.
- Deliverable-specific local figures still live under their writing or presentation folders when they are not yet shared across outputs.
