# Data Layout

- `raw/` is reserved for source pulls or unmodified exports.
- `derived/nfl/` and `derived/nba/` contain the canonical regular-season game summaries and one frozen `*_paths.parquet` file per season.
- NFL season labels are starting years and include January/February carryover games; NBA labels are season-ending years.
- The Parquet files contain the published ESPN probability columns, corrected analysis columns, game state, teams, and outcomes needed by the complete frozen-data run.
- Large local acquisition outputs (`*_wp_paths.csv` and `*_paths.csv`) are intentionally ignored because they duplicate the frozen Parquet files.
- `results/tables/manuscript/frozen-data-manifest.csv` provides byte counts and SHA-256 hashes for every released analytical input.
