/**
 * @file ale_sweep.cpp
 * @brief Fast ALE main sweep for tree splitting - C++ implementation
 *
 * Implements the core loop: move rows from right to left node and evaluate
 * split objectives at candidate positions. Replaces the R-level for-loop
 * in search_best_split_point_ale and search_best_split_point_ale_with.
 */

#include <Rcpp.h>
#include <set>
#include <map>
#include <cmath>

using namespace Rcpp;

// SSE from sufficient statistics: risk = s2 - s1^2/n
inline double risk_from_stats(double n, double s1, double s2) {
  if (n <= 1.0) return 0.0;
  return s2 - (s1 * s1) / n;
}

// Boundary stabilizer for numeric split feature (when use_stabilizer=true)
// Replicates adjust_side_for_feature logic from R
double adjust_side_cpp(
    const NumericVector& node_dL,
    const IntegerVector& node_int,
    double original_risk,
    bool is_left,
    int t,
    int n_obs
) {
  int idx_start = is_left ? 0 : t;
  int idx_end   = is_left ? t - 1 : n_obs - 1;
  int n_side = idx_end - idx_start + 1;
  if (n_side <= 20) return original_risk;

  int w = std::max(static_cast<int>(0.1 * n_side + 0.5), 10);
  if (w > n_side) w = n_side;

  // Window indices (0-based within node_dL/node_int)
  IntegerVector window_indices(w);
  if (is_left) {
    for (int i = 0; i < w; ++i) window_indices[i] = n_side - w + i;
  } else {
    for (int i = 0; i < w; ++i) window_indices[i] = i;
  }

  // Unique intervals in window
  std::set<int> target_intervals;
  for (int i = 0; i < w; ++i) {
    target_intervals.insert(node_int[window_indices[i]]);
  }

  // Near/Far split
  std::vector<double> vals_near, vals_far;
  for (int i = 0; i < n_side; ++i) {
    if (target_intervals.count(node_int[i]))
      vals_near.push_back(node_dL[i]);
    else
      vals_far.push_back(node_dL[i]);
  }
  int n_near = vals_near.size();
  int n_far = vals_far.size();
  if (n_near < 2 || n_far < 2) return original_risk;

  // Mean and SD of far
  double sum_far = 0.0, sum2_far = 0.0;
  for (double v : vals_far) {
    sum_far += v;
    sum2_far += v * v;
  }
  double mean_far = sum_far / n_far;
  double var_far = (sum2_far - sum_far * sum_far / n_far) / std::max(n_far - 1, 1);
  double sd_far = (var_far > 0) ? std::sqrt(var_far) : 0.0;

  // SD of near
  double sum_near = 0.0, sum2_near = 0.0;
  for (double v : vals_near) {
    sum_near += v;
    sum2_near += v * v;
  }
  double var_near = (sum2_near - sum_near * sum_near / n_near) / std::max(n_near - 1, 1);
  double sd_near = (var_near > 0) ? std::sqrt(var_near) : 0.0;

  if (R_IsNA(sd_near) || R_IsNA(sd_far) || sd_far <= 0 || sd_near <= 2.0 * sd_far)
    return original_risk;

  // Replace near with rnorm(mean_far, sd_far), recompute SSE
  std::map<int, double> s1_map, s2_map, n_map;
  for (int i = 0; i < n_side; ++i) {
    int k = node_int[i];
    double v = node_dL[i];
    if (target_intervals.count(k))
      v = R::rnorm(mean_far, sd_far);
    s1_map[k] += v;
    s2_map[k] += v * v;
    n_map[k] += 1.0;
  }
  double sse = 0.0;
  for (auto& it : n_map) {
    int k = it.first;
    double nk = n_map[k];
    if (nk <= 1.0) continue;
    double s1 = s1_map[k], s2 = s2_map[k];
    sse += s2 - (s1 * s1) / nk;
  }
  return sse;
}

/**
 * @brief ALE main sweep: find best split by moving rows left-to-right
 *
 * @param ord_idx 1-based row indices in sorted order (R convention)
 * @param dL_mat p x N matrix, dL_mat(j,i) = dL for feature j, sample i
 * @param interval_idx_mat p x N matrix, interval index per feature per sample
 * @param offsets IntegerVector, length p, start offset per feature in flattened arrays
 * @param tot_n, tot_s1, tot_s2 Initial totals (right node = full); will be modified
 * @param r_risks Initial right risks per feature; will be modified
 * @param is_cand LogicalVector, length n_obs-1, which positions are candidates
 * @param min_node_size Minimum observations per child
 * @param split_feat_j 1-based index of split feature in ALE, or 0 if !has_self_ale
 * @param use_stabilizer Whether to use boundary stabilizer (numeric + has_self_ale only)
 * @param z_sorted NumericVector, z values in ord order (for numeric); used by stabilizer
 * @param n_obs Number of observations in node
 *
 * @return List with best_t (1-based), best_left_risks, best_right_risks, best_risks_sum
 */
// [[Rcpp::export]]
List ale_sweep_cpp(
    IntegerVector ord_idx,
    NumericMatrix dL_mat,
    IntegerMatrix interval_idx_mat,
    IntegerVector offsets,
    NumericVector tot_n,
    NumericVector tot_s1,
    NumericVector tot_s2,
    NumericVector r_risks,
    LogicalVector is_cand,
    int min_node_size,
    int split_feat_j,
    bool use_stabilizer,
    NumericVector z_sorted,
    int n_obs
) {
  const int p = dL_mat.nrow();
  const int M = tot_n.size();

  // Working copies (R passes by value for NumericVector, so we need to copy)
  std::vector<double> r_n(tot_n.begin(), tot_n.end());
  std::vector<double> r_s1(tot_s1.begin(), tot_s1.end());
  std::vector<double> r_s2(tot_s2.begin(), tot_s2.end());
  std::vector<double> l_risks(p, 0.0);
  std::vector<double> r_risks_vec(r_risks.begin(), r_risks.end());

  double risks_sum = 0.0;
  for (int j = 0; j < p; ++j) risks_sum += r_risks_vec[j];

  double best_risks_sum = R_PosInf;
  int best_t = -1;
  std::vector<double> best_left_risks(p), best_right_risks(p);

  bool has_self_ale = (split_feat_j >= 1 && split_feat_j <= p);

  // Precompute for stabilizer (only when use_stabilizer && has_self_ale)
  NumericVector dL_j_sorted(n_obs);
  IntegerVector interval_idx_sorted(n_obs);
  if (use_stabilizer && has_self_ale && z_sorted.size() >= (size_t)n_obs) {
    int j0 = split_feat_j - 1;  // 0-based
    for (int i = 0; i < n_obs; ++i) {
      int row = ord_idx[i] - 1;  // 1-based to 0-based
      dL_j_sorted[i] = dL_mat(j0, row);
      interval_idx_sorted[i] = interval_idx_mat(j0, row);
    }
  }

  for (int t = 1; t <= n_obs - 1; ++t) {
    // move_row_all: move ord_idx[t] from right to left
    int row = ord_idx[t - 1] - 1;  // ord_idx is 1-based, t is 1-based so t-1 is index
    if (row < 0) continue;

    for (int j = 0; j < p; ++j) {
      double d = dL_mat(j, row);
      int interval_idx_val = interval_idx_mat(j, row);
      int m = offsets[j] + interval_idx_val - 1;  // 1-based to 0-based
      if (m < 0 || m >= M) continue;

      double r_n_old = r_n[m];
      double r_s1_old = r_s1[m];
      double r_s2_old = r_s2[m];
      double r_risk_old = risk_from_stats(r_n_old, r_s1_old, r_s2_old);

      double r_n_new = r_n_old - 1.0;
      double r_s1_new = r_s1_old - d;
      double r_s2_new = r_s2_old - d * d;
      double r_risk_new = risk_from_stats(r_n_new, r_s1_new, r_s2_new);

      r_n[m] = r_n_new;
      r_s1[m] = r_s1_new;
      r_s2[m] = r_s2_new;
      r_risks_vec[j] -= r_risk_old;
      r_risks_vec[j] += r_risk_new;

      double l_n_old = tot_n[m] - r_n_old;
      double l_s1_old = tot_s1[m] - r_s1_old;
      double l_s2_old = tot_s2[m] - r_s2_old;
      double l_risk_old = risk_from_stats(l_n_old, l_s1_old, l_s2_old);

      double l_n_new = tot_n[m] - r_n_new;
      double l_s1_new = tot_s1[m] - r_s1_new;
      double l_s2_new = tot_s2[m] - r_s2_new;
      double l_risk_new = risk_from_stats(l_n_new, l_s1_new, l_s2_new);

      l_risks[j] -= l_risk_old;
      l_risks[j] += l_risk_new;
      risks_sum += (-l_risk_old - r_risk_old + l_risk_new + r_risk_new);
    }

    if (!is_cand[t - 1] || t < min_node_size || (n_obs - t) < min_node_size)
      continue;

    bool l_const = (z_sorted.size() > 0 && std::abs(z_sorted[0] - z_sorted[t - 1]) < 1e-15);
    bool r_const = (z_sorted.size() >= (size_t)n_obs && std::abs(z_sorted[t] - z_sorted[n_obs - 1]) < 1e-15);
    bool do_stab = use_stabilizer && has_self_ale && !l_const && !r_const && t > 20 && (n_obs - t) > 20;

    // Compute total objective using already-maintained risks_sum,
    // avoiding per-candidate copies and full re-sum of risk vectors.
    double total = R_PosInf;
    double adj_l = 0.0, adj_r = 0.0;
    int j0 = split_feat_j - 1; // 0-based index of split feature (only valid if has_self_ale)

    if (!has_self_ale) {
      // No self ALE: objective is simply the current total risk.
      total = risks_sum;
    } else if (!use_stabilizer) {
      // Exclude self ALE from objective (interaction-focused splitting).
      double self_risk = l_risks[j0] + r_risks_vec[j0];
      total = risks_sum - self_risk;
    } else if (!do_stab) {
      // Stabilizer enabled globally but not used at this t:
      // if one side is constant in z, drop that side's self ALE.
      double drop = 0.0;
      if (l_const) drop += l_risks[j0];
      if (r_const) drop += r_risks_vec[j0];
      total = risks_sum - drop;
    } else {
      // Full boundary stabilizer on both sides for self ALE.
      NumericVector node_dL_left(t);
      IntegerVector node_int_left(t);
      for (int i = 0; i < t; ++i) {
        node_dL_left[i] = dL_j_sorted[i];
        node_int_left[i] = interval_idx_sorted[i];
      }
      NumericVector node_dL_right(n_obs - t);
      IntegerVector node_int_right(n_obs - t);
      for (int i = 0; i < n_obs - t; ++i) {
        node_dL_right[i] = dL_j_sorted[t + i];
        node_int_right[i] = interval_idx_sorted[t + i];
      }
      adj_l = adjust_side_cpp(node_dL_left, node_int_left,
          l_risks[j0], true, t, t);
      adj_r = adjust_side_cpp(node_dL_right, node_int_right,
          r_risks_vec[j0], false, 0, n_obs - t);
      double risk_t_j = l_risks[j0] + r_risks_vec[j0];
      total = risks_sum - risk_t_j + adj_l + adj_r;
    }

    if (!R_FINITE(total) || total >= best_risks_sum) continue;

    // Update best solution and store full left/right risk vectors,
    // applying any necessary self-ALE adjustments only once.
    best_risks_sum = total;
    best_t = t;
    best_left_risks = l_risks;
    best_right_risks = r_risks_vec;

    if (has_self_ale) {
      if (!use_stabilizer) {
        best_left_risks[j0] = 0.0;
        best_right_risks[j0] = 0.0;
      } else if (!do_stab) {
        if (l_const) best_left_risks[j0] = 0.0;
        if (r_const) best_right_risks[j0] = 0.0;
      } else {
        best_left_risks[j0] = adj_l;
        best_right_risks[j0] = adj_r;
      }
    }
  }

  if (best_t < 0) {
    return List::create(
      _["best_t"] = NA_INTEGER,
      _["best_risks_sum"] = R_PosInf,
      _["best_left_risks"] = NumericVector(p, NA_REAL),
      _["best_right_risks"] = NumericVector(p, NA_REAL)
    );
  }

  return List::create(
    _["best_t"] = best_t + 0,  // keep 1-based for R
    _["best_risks_sum"] = best_risks_sum,
    _["best_left_risks"] = NumericVector(best_left_risks.begin(), best_left_risks.end()),
    _["best_right_risks"] = NumericVector(best_right_risks.begin(), best_right_risks.end())
  );
}
