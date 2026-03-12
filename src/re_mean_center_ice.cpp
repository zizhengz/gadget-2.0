/**
 * @file re_mean_center_ice.cpp
 * @brief Fast mean-centering of ICE matrices - C++ implementation.
 */

#include <Rcpp.h>
#include <unordered_set>
#include <vector>
#include <string>

// [[Rcpp::depends(Rcpp)]]
using namespace Rcpp;

// -----------------------------------------------------------------------------
// charvec_to_set (internal)
// Purpose:
//   Convert Rcpp CharacterVector to unordered_set<string> for O(1) membership
//   lookup. Used to check if column names are in the grid.
// Notes:
//   NA values are skipped.
// -----------------------------------------------------------------------------
inline std::unordered_set<std::string> charvec_to_set(const CharacterVector& vec) {
  std::unordered_set<std::string> s;
  for (int i = 0; i < vec.size(); ++i) {
    if (!CharacterVector::is_na(vec[i])) {
      s.insert(Rcpp::as<std::string>(vec[i]));
    }
  }
  return s;
}

// -----------------------------------------------------------------------------
// re_mean_center_ice_cpp
// Purpose:
//   Mean-center ICE matrices per feature, only on grid columns. For each matrix:
//   select rows by idx, set non-grid columns to NA, center each row on its mean.
// Inputs:
//   Y: List of NumericMatrix with dimnames (one per feature)
//   grid: List of CharacterVector, valid grid column names per feature
//   idx: IntegerVector of row indices, 1-based (R convention)
// Output:
//   List of mean-centered NumericMatrix, column names preserved.
// -----------------------------------------------------------------------------
// [[Rcpp::export]]
List re_mean_center_ice_cpp(List Y, List grid, IntegerVector idx) {
  int L = Y.size();
  List result(L);
  CharacterVector namesY = Y.names();

  for (int i = 0; i < L; ++i) {
    NumericMatrix mat = as<NumericMatrix>(Y[i]);
    List dimnames = mat.attr("dimnames");
    CharacterVector colnames = as<CharacterVector>(dimnames[1]);
    CharacterVector grid_i = grid[i];
    std::unordered_set<std::string> keep_cols = charvec_to_set(grid_i);
    int ncols = mat.ncol();
    int nrows = idx.size();

    /* Precompute column mask: avoids unordered_set lookup in the inner loop. */
    std::vector<bool> in_grid(ncols);
    for (int c = 0; c < ncols; ++c) {
      in_grid[c] = (!CharacterVector::is_na(colnames[c])) &&
        (keep_cols.count(Rcpp::as<std::string>(colnames[c])) > 0);
    }

    NumericMatrix centered(nrows, ncols);
    centered.attr("dimnames") = List::create(R_NilValue, colnames);

    int nrows_mat = mat.nrow();
    for (int r = 0; r < nrows; ++r) {
      int row_idx = idx[r] - 1;  /* idx is 1-based (R convention) */
      if (row_idx < 0 || row_idx >= nrows_mat) {
        for (int c = 0; c < ncols; ++c) centered(r, c) = NA_REAL;
        continue;
      }

      /* Pass 1: copy grid columns (non-grid -> NA), accumulate sum/count for mean. */
      double sum = 0.0;
      int count = 0;
      for (int c = 0; c < ncols; ++c) {
        if (in_grid[c]) {
          double v = mat(row_idx, c);
          centered(r, c) = v;
          if (!NumericMatrix::is_na(v)) {
            sum += v;
            count++;
          }
        } else {
          centered(r, c) = NA_REAL;
        }
      }
      double mean = (count > 0) ? sum / count : NA_REAL;

      /* Pass 2: subtract row mean from non-NA values (center the ICE curves). */
      for (int c = 0; c < ncols; ++c) {
        double v = centered(r, c);
        if (!NumericMatrix::is_na(v)) {
          centered(r, c) = v - mean;
        }
      }
    }
    result[i] = centered;
  }
  result.attr("names") = namesY;
  return result;
}
