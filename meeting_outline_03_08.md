# Meeting Outline

## Purpose
- Recap the main paper changes since the last review.
- Check whether the current benchmark setup is fair and easy to communicate.
- Confirm whether the current figures are enough for the paper.
- Make sure the experiments are fine before the Thursday reading session, so anything
  that is still off can be fixed in time.

## 1. What changed since the last review
- Section 00, Abstract:
  - keyword “interaction”
  - Changed the implementation claim from "fast" to "efficient" to avoid overclaiming.
  - Separated the global benchmark claim from the regional split-search claim.
  - Updated the evaluation summary so it matches the current paper contents.
- Section 01, Introduction:
  - Added a concrete forward reference to the bike-sharing regional hour curves.
  - Moved the first use of FOI to the sentence about identifying the interacting feature.
  - Fix RHALE name abbv. error.
- Section 02, Methodology:
  - Fix ALE condition notation error.
  - Unified inline math notation and fixed small notation issues such as \(i\)-th observation.
  - Clarified the ALE interval convention so the leftmost interval includes the minimum.
  - Added labels for the main PD/ALE observation-effect, risk, and split-improvement equations.
  - Made the categorical split candidates explicit:
    one-versus-rest for default PD, ordered-prefix splits for default ALE, and exhaustive
    level-set search as an optional guarded mode.
  - Clarified the algorithm steps, including how the max split depth stopping rule is applied.
- Section 03, Related work:
  - Corrected the package descriptions for effectplots and iml, including their categorical
    ALE behavior.
  - Replaced the long package-computation table in the main text with a short summary paragraph.
  - Moved the detailed package-computation comparison to the appendix.
- Section 04, xplaineff:
  - Made the C++ claim more precise: C++ is used for heterogeneity aggregation and split-search
    kernels, while effect precomputation can use R, C++, and model-specific prediction helpers.
  - Added a compact explanation of the main PdStrategy/AleStrategy differences directly in the
    package section.
  - Explained the categorical defaults in user-facing terms:
    PD isolates one level against the rest, while ALE orders levels and splits prefixes from suffixes.
  - Mentioned the optional exhaustive categorical split mode and its level guard.
  - Added the note that character split features are converted to factors, while numeric binary
    indicators stay numeric unless the user encodes them as factors.
  - Removed the Section 4 API table and the separate "PD vs ALE" subsection.
- Re-ran the runtime benchmarks against effector 0.4 and effector 0.5; the results remain
  favorable for the current benchmark story.

## 2. Benchmark setup recap
- Global benchmark:
  - Task: compute univariate global PD/ICE and ALE for all `p` features.
  - Baselines: `pdp`, `iml`, `DALEX`/`ingredients`, `ale`, and `effectplots`.
  - Fairness controls:
    - all packages use the same generated data files with seed 21;
    - all packages are run on the same `n`, `p`, and `K_j` cells;
    - all packages evaluate all `p` features;
    - all packages use full-data effect computation, without package-side subsampling or
      bootstrap resampling;
    - all PD calls use `K_j` grid points, and all ALE calls use `K_j` intervals when the
      package interface supports this setting;
    - package-internal parallelism is disabled:
      `pdp` uses `parallel = FALSE`, `ale` uses `parallel = 0`, and `iml` is run without
      a `future` parallel plan;
    - thread counts are fixed to one for model prediction and low-level numerical backends;
    - each cell uses one untimed warm-up run and 20 timed repetitions, with medians and IQRs
      reported.
  - Prediction settings:
    - bagged-tree setting: all R packages receive the same fitted `ranger` model.
      The model has 100 trees, variance splitting, `mtry = p`, `sample.fraction = 1`,
      seed 21, and `num.threads = 1`;
    - toy setting: all packages use the same deterministic analytic prediction function,
      so prediction is cheap and package-side data construction/aggregation becomes visible.
  - Common cell: `n = 10000`, `p = 20`, and `K_j = 20`.
  - Sweep design: one axis is varied at a time, while the other axes stay fixed at the
    common-cell values.
    - `n` sweep: `n = {1000, 5000, 10000, 20000}`, with `p = 20` and `K_j = 20`.
    - `p` sweep: `p = {10, 20, 50, 100}`, with `n = 10000` and `K_j = 20`.
    - grid sweep: `K_j = {10, 20, 50}`, with `n = 10000` and `p = 20`.
  - For PD, `K_j` is the number of grid points; for ALE, `K_j` is the number of intervals.
  - Runtime object: full package call for global effect computation, including input
    construction, model prediction, aggregation, and returned object construction.
- Regional benchmark:
  - Task: compute regional PD/ALE trees after the global effect information is available.
  - Baseline: `effector`, with the current paper comparison using effector 0.5.
  - Fairness controls:
    - both packages use the same generated data files with seed 21;
    - both packages are run on the same `n`, `p`, `K_j`, and `n_split` cells;
    - both packages use all `p` features as FOI and all `p` features as candidate split features;
    - both packages grow binary trees with `min_node_size = 50`;
    - xplaineff's `n_split` is matched to effector's `max_depth`;
    - both packages use 19 numeric split candidates per feature
      (`n_quantiles = 19` in xplaineff, `numerical_features_grid_size = 20` in effector);
    - both packages use one untimed warm-up run and 20 timed repetitions;
    - thread counts are fixed to one for model prediction and low-level numerical backends.
  - Model interface: for the bagged-tree setting, both sides use `scikit-learn`
    `RandomForestRegressor`; xplaineff calls it from R through `reticulate`, and effector
    calls it directly in Python.
    The RF configuration is matched: 100 trees, squared-error splitting, `max_features = 1.0`,
    unrestricted depth, bootstrap sampling, minimum leaf size 1, seed 21, and `n_jobs = 1`.
  - Toy model: both packages use the same deterministic analytic prediction function.
  - Common cell: `n = 10000`, `p = 20`, `K_j = 20`, and `n_split = 2`.
  - Sweep design: one axis is varied at a time, while the other axes stay fixed at the
    common-cell values.
    - `n` sweep: `n = {1000, 5000, 10000, 20000}`, with `p = 20`, `K_j = 20`,
      and `n_split = 2`.
    - `p` sweep: `p = {10, 20, 50, 100}`, with `n = 10000`, `K_j = 20`,
      and `n_split = 2`.
    - grid sweep: `K_j = {10, 20, 50}`, with `n = 10000`, `p = 20`,
      and `n_split = 2`.
    - depth sweep: `n_split = {2, 5, 8, 10}`, with `n = 10000`, `p = 20`,
      and `K_j = 20`.
  - For PD, `K_j` is the number of grid points; for ALE, `K_j` is the number of intervals.
    `n_split` is the maximum split depth / maximum number of splits along a root-to-leaf path.
  - Timing split:
    - split-search runtime excludes global precomputation and measures only tree partitioning;
    - total runtime includes both global effect computation and regional split search.
  - Runtime protocol: same warm-up/repetition structure as the global benchmark.

## 3. What I want to confirm
- Are the current benchmark settings fair and easy to communicate?
- Are the current figures enough for the paper?
- If anything still looks off, what should I fix before Thursday?
