# Data Layout

- `raw/` is reserved for source pulls or unmodified exports.
- `derived/nfl/` and `derived/nba/` are the canonical derived datasets used by the current manuscript and AOAS draft.
- If a script creates downstream summaries, prefer writing them back into `derived/` only when they are reusable across deliverables.
