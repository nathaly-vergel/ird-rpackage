## [Unreleased]

### 2026-02-26 — Stable with paradox 0.11.0

#### CI / Checks
- `devtools::check()` passes without errors (commit 3fa42b7).
- Uses `paradox` 0.11.0 and older `mlr3` versions.
- `credit_example.R` works under these versions.
  - However, it fails with newer versions because the latest `mlr3` requires `paradox >= 1.0.1`, and the package code is not yet refactored for that API.
  
  Old versions installed via renv:
  - renv::install("mlr3@0.18.0")
  - renv::install("mlr3learners@0.6.0")
  - renv::install("mlr3pipelines@0.5.1")

#### Dependencies
- `paradox` = 0.11.0
- `mlr3` = 0.18.0
- `mlr3learners` = 0.6.0
- `mlr3pipelines` = 0.5.1

