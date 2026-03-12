#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// -----------------------------------------------------------------------------
// node_heterogeneity_cpp
// Purpose:
//   Compute heterogeneity (sum of variances) per feature from effect matrices.
//   For each matrix in Y: sum over columns of (sum of squares - sum^2 / n),
//   i.e., sum_j [ sum_i Y[i,j]^2 - (sum_i Y[i,j])^2 / n ].
//   Used by pdStrategy$heterogeneity().
// Inputs:
//   - Y: List of numeric matrices (e.g., ICE effect matrices per feature).
// Notes:
//   - NA values are excluded (na.rm = TRUE) per column.
//   - Empty or all-NA columns contribute 0.
// Output:
//   NumericVector of length length(Y).
// -----------------------------------------------------------------------------
// [[Rcpp::export]]
NumericVector node_heterogeneity_cpp(List Y) {
  int n_mat = Y.length();
  NumericVector result(n_mat);

  for (int m = 0; m < n_mat; ++m) {
    NumericMatrix M = as<NumericMatrix>(Y[m]);
    int nrow = M.nrow();
    int ncol = M.ncol();
    double total = 0.0;

    for (int j = 0; j < ncol; ++j) {
      double sum_val = 0.0;
      double sum_sq = 0.0;
      int n_valid = 0;

      for (int i = 0; i < nrow; ++i) {
        double v = M(i, j);
        if (!std::isnan(v)) {
          sum_val += v;
          sum_sq += v * v;
          n_valid++;
        }
      }

      if (n_valid > 0) {
        total += sum_sq - (sum_val * sum_val) / static_cast<double>(n_valid);
      }
    }

    result[m] = total;
  }

  return result;
}
