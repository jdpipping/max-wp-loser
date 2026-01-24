# The Blown Lead Paradox

Work on the distribution of the maximum win probability attained by losing teams, with theoretical results, applied examples, and empirical validation. This repository contains manuscript sources, code, data, and presentation materials.

## Quick Links

- Full Manuscript: `paper/main.pdf`
- Manuscript Supplement: `paper/supp.pdf`
- WSABI Research Note: `research-note/research-note.pdf`
- 2nd Year Poster: `poster/poster.pdf`
- NESSIS 2025 Slides: `nessis2025/slides.pdf`

## Directory Structure

```
.
├── nessis2025/
├── paper/
│   ├── code/
│   ├── data/
│   └── figures/
├── poster/
└── research-note/
```

### Directory Descriptions

- `nessis2025/` talk slides, simulation scripts, and figures
- `paper/` manuscript sources, supplement, and bibliography
  - `paper/code/` R scripts for theory, simulations, scraping, and validation
  - `paper/data/` NBA/NFL game-level CSVs and binned summaries
  - `paper/figures/` generated figures for the paper
- `poster/` poster source, figures, and compiled PDF
- `research-note/` short research note (code + figures)

## Code Descriptions

- `nessis2025/` runs the grid simulations and empirical summaries for the slides and saves figures in `nessis2025/figures/`, including the Super Bowl LVII win-probability chart.
- `paper/code/` builds the manuscript results: scrape/import NBA/NFL win-probability data, bin games by starting win probability, compute the distribution of the losing team's maximum win probability, and generate summary tables and figures. It also includes theoretical/simulation scripts (`two-player`, `n-player`, `finance`) and validation checks.
- `poster/` generates the poster-specific simulation and empirical figures in `poster/figures/`.
- `research-note/` runs simulations and empirical analyses used in the research note and writes figures (and a GIF) to `research-note/figures/`.

## Running the Code

All scripts are in R. Run all code from the repository root directory or the corresponding `.Rproj`.