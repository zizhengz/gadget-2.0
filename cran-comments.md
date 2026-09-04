# CRAN comments for xplaineff

This is a resubmission of `xplaineff` 0.1.0.

## Resubmission

This resubmission addresses the reviewer comments by:

- adding a reference for the method to `DESCRIPTION`;
- removing examples for the unexported functions `extract_split_info()`, `mean_center_ice()`,
  `plot_tree_structure()`, and `prepare_split_data_ale()`;
- replacing unnecessary `\dontrun{}` blocks with short executable examples; and
- removing commented-out example code.

The package vignette was removed because it is not needed for this submission.

## Test results

Local checks were run on September 4, 2026, with:

```sh
LC_ALL=C OMP_NUM_THREADS=1 \
  R CMD check --as-cran --no-manual --no-vignettes xplaineff_0.1.0.tar.gz
```

- macOS Ventura 13.1, R 4.5.0: `0 errors | 0 warnings | 4 notes`.
- `checking CRAN incoming feasibility ... NOTE`: external URL checks could not be completed because the local environment could not resolve external hosts.
- `checking for future file timestamps ... NOTE`: unable to verify the current time in the local check environment.
- `checking top-level files ... NOTE`: `pandoc` is unavailable in the local check environment.
- `checking for detritus in the temp directory ... NOTE`: local check found `xcrun_db`.

## Package contents

The package tarball excludes non-package directories such as `simulation/`, `scripts/`, `paper/`, and the root
`figures/` directory. The eight images used by the README are included under `man/figures/`; `.r-lib/` and local
virtual environments such as `.venv-effector050/` remain excluded.

## Dependencies

`xplaineff` depends on R (>= 4.3.0) and uses `Rcpp`/`RcppArmadillo` for compiled code.
