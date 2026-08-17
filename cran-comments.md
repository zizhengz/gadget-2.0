# CRAN comments for xplaineff

This is the first CRAN submission of `xplaineff`.

## Test results

Local checks were run with:

```sh
env LC_ALL=C OMP_NUM_THREADS=1 \
  PATH=/Applications/quarto/bin/tools:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin \
  R_LIBS=/Users/zzz/Downloads/LMU_Work/GADGET/.r-lib \
  R CMD check --as-cran xplaineff_0.1.0.tar.gz
```

- macOS Ventura 13.1, R 4.5.0: `0 errors | 0 warnings | 4 notes`.
- Windows Server 2022, R-release 4.6.1 (win-builder): `0 errors | 0 warnings | 1 note`.
- Windows Server 2022, R-devel r90242 (win-builder): `0 errors | 0 warnings | 1 note`.
- `checking CRAN incoming feasibility ... NOTE`: new submission; external URL checks could not be completed because the local environment could not resolve external hosts.
- `checking for future file timestamps ... NOTE`: unable to verify the current time in the local check environment.
- `checking HTML version of manual ... NOTE`: package `V8` is unavailable in the local check environment.
- `checking for detritus in the temp directory ... NOTE`: local check found `xcrun_db`.

## Package contents

The package tarball excludes non-package directories such as `simulation/`, `scripts/`, `paper/`, and the root
`figures/` directory. The eight images used by the README are included under `man/figures/`; `.r-lib/` and local
virtual environments such as `.venv-effector050/` remain excluded.

## Dependencies

`xplaineff` depends on R (>= 4.3.0) and uses `Rcpp`/`RcppArmadillo` for compiled code.
