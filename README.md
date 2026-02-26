# Extreme-Path Benchmarks for Sequential Probability Forecasts

Benchmark distributions for how often sequential forecasts hit extreme values on paths that ultimately fail (e.g., 90% and still wrong). Theory, code, and ESPN win-probability analysis for NFL and NBA.

## Quick Links

- **Manuscript:** `paper/main.pdf`
- **NESSIS 2025 slides:** `nessis2025/slides.pdf`
- **WSABI research note:** `research-note/research-note.pdf`
- **2nd year poster:** `poster/poster.pdf`

## Directory Structure

```
.
├── nessis2025/       # talk slides, simulations, figures
├── paper/            # manuscript and appendix
│   ├── code/         # R scripts for theory, data, analysis
│   ├── data/         # NBA/NFL game-level data and summaries
│   └── figures/      # generated figures
├── poster/           # poster source and figures
└── research-note/    # short research note
```

## Code

All scripts are in R. Run from the repository root or the corresponding `.Rproj`.

- **`paper/code/`** Scrape ESPN win-probability data, compute loser's peak by game, run PIT diagnostics, generate figures. Plus two-team/*n*-team theory scripts.
- **`nessis2025/`** Talk simulations and figures.
- **`poster/`** Poster figures.
- **`research-note/`** Research-note analyses.
