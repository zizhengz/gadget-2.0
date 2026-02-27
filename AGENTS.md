# AGENTS.md

## Cursor Cloud specific instructions

This is a single R package (`gadget`) with C++ extensions via Rcpp/RcppArmadillo. No databases, Docker, or external services are needed.

### Prerequisites (installed by VM snapshot)

- R 4.5+ from CRAN Ubuntu repo (`noble-cran40`)
- System libraries: `r-base-dev`, `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libfontconfig1-dev`, `libfreetype6-dev`, `libpng-dev`, `libtiff5-dev`, `libjpeg-dev`, `libharfbuzz-dev`, `libfribidi-dev`, `libgmp-dev`, `libmpfr-dev`
- R packages: all `Imports` + `Suggests` from `DESCRIPTION`, plus `devtools`, `remotes`, `rcmdcheck`

### Key commands

| Task | Command |
|------|---------|
| Load package (dev) | `R -e 'devtools::load_all()'` |
| Run tests | `R -e 'devtools::test()'` |
| R CMD check (lint) | `R -e 'rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")'` |
| Install package | `R CMD INSTALL .` |
| Rebuild C++ code | `R -e 'devtools::load_all()'` (triggers recompilation) |

### Gotchas

- The package is **not** installed to the system R library by default. Use `devtools::load_all()` for development or `R CMD INSTALL .` to install globally before using `library(gadget)`.
- C++ source lives in `src/`; any changes require recompilation via `devtools::load_all()`.
- `R CMD check` produces 2 pre-existing NOTEs (missing LICENSE file template, NSE variable bindings) — these are not errors.
- `.Rprofile` sets a `styler` option for `mlr` style; this is harmless and can be ignored.
- Tests (115 total) run quickly (~10s). Two deprecation warnings from `tidyselect` are expected.
