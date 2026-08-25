# The Blown Lead Paradox

Benchmarks for calibrated win-probability paths, with regular-season NFL and NBA applications.

## Quick Links

- **Manuscript:** `writing/manuscript/main.pdf`
- **Manuscript supplement:** `writing/manuscript/supp.pdf`
- **AOAS article:** `writing/aoas/article.pdf`
- **AOAS supplement:** `writing/aoas/supplement.pdf`
- **AOAS cover letter:** `writing/aoas/cover-letter.pdf`
- **arXiv article with appendices:** `writing/arxiv/main.pdf`
- **arXiv upload ZIP:** `writing/arxiv/upload.zip`
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
├── writing/                # working manuscript, submission prep, arXiv, research note
├── presentations/          # slides, poster, lecture assets
└── archive/                # historical drafts and retired project variants
```

## Code

Run scripts from the repository root.

- **`scripts/manuscript/`** Theory, simulation, and NFL/NBA analysis scripts for the working manuscript and later submission packages.
- **`scripts/presentations/`** Figure-generation scripts for talks and poster assets.
- **`scripts/writing/research-note.R`** Research-note simulation and empirical graphics.
- **`scripts/analyses/ravens/analysis.R`** Ravens-specific side analysis.
- **`R/plot-style.R`** Shared plotting theme and palette helper.
- **`R/dyadic-bootstrap.R`** Reusable season-stratified pigeonhole weights, tie-aware upper K--S statistics, clustered means, and calendar-block sensitivity helpers.

### Canonical manuscript run

From the repository root, reproduce the frozen-data analysis and compile the working, AOAS, and arXiv documents with:

```sh
Rscript scripts/manuscript/run-full-analysis.R --skip-build=true --replicates=9999 --seed=20260815 --validation-outer=250 --validation-inner=399 --compile-pdf=true
```

To reacquire the public feeds before analysis, omit `--skip-build=true`; NFL acquisition queries August through February and preserves ESPN's actual regular-season identifier. The data contracts enforce NFL starting-year labels with January carryover games, NBA ending-year labels, and NBA franchise matchups so ESPN-tagged All-Star exhibitions cannot enter the regular-season sample.

The frozen-data run requires the R packages listed in `results/tables/manuscript/session-info.txt`, including `arrow` for the compressed path files. It uses 9,999 common season-stratified dyadic draws with seed 20260815 for PIT, tail, first-crossing, and fixed-clock analyses. The finite-schedule null simulation uses 250 Monte Carlo samples with 399 inner draws. `build-manuscript-artifacts.R` is the sole writer of `manuscript-results.csv` and `inference-results.tex`, which are consumed by every manuscript package.

## Data And Results

- **`data/derived/nfl/`** and **`data/derived/nba/`** contain compact game summaries and frozen, Zstandard-compressed `*_paths.parquet` files for the public analysis.
- **`results/figures/manuscript/`** stores the figures consumed by the working manuscript and future submission packages.
- **`results/tables/manuscript/frozen-data-manifest.csv`** records SHA-256 hashes for every canonical game and path input.
- **`results/tables/manuscript/session-info.txt`** records the R runtime and package versions used by the canonical run.
- **`writing/aoas/`** and **`writing/arxiv/`** are regenerated from the working manuscript by `build-submission-packages.R`. The arXiv source keeps its figures organized in subdirectories for local use.
- Upload **`writing/arxiv/upload.zip`** directly and select `main.tex` as the top-level TeX file. The ZIP contains a flat twelve-file copy of the TeX source, bibliography, and required figures; `main.pdf` and local build files are excluded.
- Deliverable-specific local figures still live under their writing or presentation folders when they are not yet shared across outputs.

## Extreme-Value Correction In Enriched Paths

The acquisition pipeline writes unmodified ESPN feed archives as local `*_wp_paths.csv` files and never edits them in place. Those large duplicate CSVs are excluded from the public repository. The frozen `*_paths.parquet` files retain the original published probability columns alongside the correction layer used by the analysis.

The enriched path pipeline adds a conservative correction layer for exact eventual-loser `1` spikes:

- Exact loser-side `1` runs are always corrected.
- The contaminated run is replaced with the last valid pre-extreme loser win probability. If no prior valid value exists, the first later valid value is used instead.

Corrected enriched path files include:

- `home_wp_corrected`
- `away_wp_corrected`
- `loser_wp_corrected`
- `winner_wp_corrected`
- `was_1`

The raw probability columns remain alongside these corrected columns. Main derived analyses use the corrected enriched paths when those columns are available, including peak summaries, first-attainment metadata, fixed-clock calibration outputs, and case-study path figures. Raw paths and exclusion of every patched game are reported as sensitivities using identical bootstrap draws.

The artifact audit also reports strict and weak thresholds, probability-rounding bounds, terminal-inclusive crossings, calendar-block resampling, franchise-linked resampling, feed coverage, and leave-one-season-out results.

## License And Data Provenance

The analysis software is available under the MIT License in `LICENSE.md`. The empirical files are derived from publicly accessible ESPN feeds and are included for research reproducibility; users remain responsible for complying with the source provider's terms. Citation metadata are provided in `CITATION.cff`.
