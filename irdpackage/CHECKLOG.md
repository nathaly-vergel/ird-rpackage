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

## [Unreleased]

### 2026-03-09 — Migration to paradox 1.0.1

#### CI / Checks

- `devtools::check()` passes without errors under the current dependency stack (commit 69efec8)
- The package was updated to work with recent versions of `paradox` and the `mlr3` ecosystem.
- The example `credit_example.R` runs successfully with the current CRAN versions.

To verify the new behaviour was consistent, results were compared against the previous implementation (using `paradox 0.11.0`). The main algorithms (PRIM, MaxBox, etc) produce the same results.

One difference appears in the post-processing step for integer-valued features. The previous implementation evaluated many candidate bounds that differed only by very small numeric amounts but ultimately corresponded to the same integer bound after rounding. The updated implementation avoids evaluating such redundant candidates. As a result, the resulting post-processed boxes can differ slightly, but the search becomes more efficient and avoids spending time evaluating changes that have no real effect on the integer domain.

This adjustment to the post-processing logic was reviewed and approved by Prof. Giuseppe.

Note: rebuilding ParamSet objects rather than mutating them in place was necessary for the migration to paradox = 1.0.1. Routines that update boxes frequently (like PRIM and PostProcessing) now have more overhead as a result.

#### Dependencies (current CRAN versions)

* `mlr3` = 1.5.0
* `mlr3learners` = 0.14.0
* `mlr3pipelines` = 0.11.0
* `paradox` = 1.0.1
