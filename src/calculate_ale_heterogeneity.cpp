/**
 * @file calculate_ale_heterogeneity.cpp
 * @brief ALE heterogeneity (sum of squared dL residuals) per feature - C++ implementation.
 */

#include <Rcpp.h>
#include <unordered_map>

// [[Rcpp::depends(Rcpp)]]
using namespace Rcpp;

// -----------------------------------------------------------------------------
// calculate_ale_heterogeneity_single_cpp
// Purpose:
//   Compute ALE heterogeneity for a single feature as the sum of squared
//   residuals of dL from its interval-wise mean. Formally:
//     sum_i ( dL_i - mean_{interval(i)}(dL) )^2
// Inputs:
//   - dL: numeric vector of local effects (finite differences) per sample
//   - interval_index: integer vector, same length as dL, giving the interval id
//     for each sample (used to group samples and compute per-interval means)
// Notes:
//   - NA/NaN values in dL are ignored in both mean and SSE accumulation.
//   - If input is empty, returns 0.0.
// Output:
//   Scalar heterogeneity (sum of squared residuals).
// -----------------------------------------------------------------------------
// [[Rcpp::export]]
double calculate_ale_heterogeneity_single_cpp(NumericVector dL, IntegerVector interval_index) {
  int n = dL.length();
  if (n == 0) return 0.0;
  if (interval_index.length() != static_cast<R_xlen_t>(n)) {
    stop("dL and interval_index must have the same length");
  }

  /* Pass 1: accumulate sum and count per interval (skip NA/NaN). */
  std::unordered_map<int, double> interval_means;
  std::unordered_map<int, int> interval_counts;

  for (int i = 0; i < n; ++i) {
    int interval = interval_index[i];
    double value = dL[i];

    if (NumericVector::is_na(value)) continue;

    interval_means[interval] += value;
    interval_counts[interval]++;
  }
  
  /* Convert sums to means. */
  for (auto& pair : interval_means) {
    int interval = pair.first;
    int count = interval_counts[interval];
    if (count > 0) {
      pair.second /= count;
    }
  }
  
  /* Pass 2: sum of squared residuals (dL - interval_mean)^2, skip NA/NaN. */
  double total_heterogeneity = 0.0;
  for (int i = 0; i < n; ++i) {
    int interval = interval_index[i];
    double value = dL[i];

    if (NumericVector::is_na(value)) continue;
    
    double interval_mean = interval_means[interval];
    double diff = value - interval_mean;
    total_heterogeneity += diff * diff;
  }
  
  return total_heterogeneity;
}

// -----------------------------------------------------------------------------
// calculate_ale_heterogeneity_list_cpp
// Purpose:
//   Vectorized interface over a list of features. For each element in Y (a
//   DataFrame with columns dL and interval.index), compute the single-feature
//   heterogeneity and return a named list of numeric values.
// Inputs:
//   - Y: List of DataFrame; each DataFrame must contain columns "dL" (Numeric)
//        and "interval.index" (Integer).
// Notes:
//   - Names of Y (feature names) are propagated to the result, if present.
// Output:
//   Named List of numeric heterogeneity values, one per feature.
// -----------------------------------------------------------------------------
// [[Rcpp::export]]
List calculate_ale_heterogeneity_list_cpp(List Y) {
  int n_feat = Y.size();
  List result(n_feat);

  for (int i = 0; i < n_feat; ++i) {
    DataFrame data = as<DataFrame>(Y[i]);
    NumericVector dL = data["dL"];
    IntegerVector interval_index = data["interval.index"];
    
    double heterogeneity = calculate_ale_heterogeneity_single_cpp(dL, interval_index);
    result[i] = heterogeneity;
  }
  
  /* Preserve names if provided on input list. */
  CharacterVector names = Y.names();
  if (names.length() > 0) {
    result.names() = names;
  }
  
  return result;
}
