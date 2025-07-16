/**
 * @file search_best_split.cpp
 * @brief Fast tree splitting implementation using C++ and Armadillo
 *
 * This file implements efficient tree splitting algorithms for the gadget package.
 * It provides both categorical and numerical splitting with optimized performance.
 *
 * @author gadget Development Team
 * @version 1.0
 */

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <chrono>
#include <algorithm>          // std::sort, std::upper_bound

using namespace Rcpp;

/* ------------------------------------------------------------------ *
 *  Helper Functions                                                   *
 * ------------------------------------------------------------------ */

/**
 * @brief Remove duplicates from a numeric vector efficiently
 *
 * Uses std::set for O(n log n) complexity without tree allocations.
 * This is more efficient than the commented alternative below.
 *
 * @param x Input numeric vector
 * @return NumericVector with unique values in sorted order
 */
inline NumericVector unique_cpp(NumericVector x) {
  std::set<double> s(x.begin(), x.end());
  return NumericVector(s.begin(), s.end());
}
// inline NumericVector unique_cpp(NumericVector x) {
//   std::sort(x.begin(), x.end());
//   x.erase(std::unique(x.begin(), x.end()), x.end());
//   return x;
// }

/**
 * @brief Calculate R type-7 quantile for sorted vector
 *
 * Implements R's type-7 quantile algorithm for sorted vectors.
 * This is the default quantile type in R and provides good
 * statistical properties for continuous data.
 *
 * @param x Sorted numeric vector - passed by reference (&) to avoid copying
 * @param p Probability (0 <= p <= 1)
 * @return Quantile value
 */
inline double quantile_type7(const NumericVector& x, double p) {  // & = reference, const = read-only, avoids copying vector
  const int n = x.size();
  if (n == 0) return NA_REAL;
  if (p <= 0.0) return x[0];
  if (p >= 1.0) return x[n - 1];

  const double h = (n - 1) * p;
  const int lo   = static_cast<int>(h);
  const double f = h - lo;

  return x[lo] + f * (x[lo + 1] - x[lo]);
}

/**
 * @brief Convert R object to Armadillo matrix with zero-copy when possible
 *
 * Attempts to create an Armadillo view of the R matrix without copying data.
 * Falls back to conversion if the object is not a numeric matrix.
 *
 * @param obj R object (matrix or convertible to matrix)
 * @return Armadillo matrix (zero copy)
 */
inline arma::mat arma_view(SEXP obj) {
  if (!Rf_isMatrix(obj) || TYPEOF(obj) != REALSXP) {
    static Rcpp::Function as_matrix("as.matrix");
    obj = as_matrix(obj);
  }
  Rcpp::NumericMatrix M(obj);
  return arma::mat(M.begin(), M.nrow(), M.ncol(), /*copy*/ false);
}


/* ------------------------------------------------------------------ *
 *  Core Splitting Algorithm                                          *
 * ------------------------------------------------------------------ */

/**
 * @brief Internal function for finding the best split point for a single feature
 *
 * This is the core splitting algorithm that handles both categorical and numerical
 * variables. It accepts preprocessed data for maximum efficiency.
 *
 * @param z Feature vector (numeric or categorical)
 * @param Ym Vector of effect matrices (preprocessed) - passed by reference (&) to avoid copying
 * @param S_tot Vector of total sums for each effect matrix - passed by reference (&) to avoid copying
 * @param n_quantiles Number of quantiles for numerical splitting (optional)
 * @param is_categorical Whether the feature is categorical
 * @param min_node_size Minimum number of observations per node
 * @return List with split.point and split.objective
 */
List search_best_split_point_cpp_internal(
    SEXP              z,
    const std::vector<arma::mat>& Ym,        // & = reference, const = read-only, avoids copying large matrices
    const std::vector<arma::rowvec>& S_tot,  // & = reference, const = read-only, avoids copying large vectors
    Nullable<int>     n_quantiles   = R_NilValue,
    bool              is_categorical = false,
    int               min_node_size  = 1)
{
  const int Ly = Ym.size(); // p
  const int N = Ym[0].n_rows; // n

  double best_obj = R_PosInf, best_split = NA_REAL;
  std::string best_level;

  /* ================= Categorical Variable Splitting ================= */
  if (is_categorical) {
    // Extract factor levels and convert to integer vector
    IntegerVector z_fac(z);
    CharacterVector lev = z_fac.attr("levels");
    const int K = lev.size();

    // If only one level, no valid split possible
    if (K <= 1)
      return List::create(_["split.point"] = R_NaString,
        _["split.objective"] = R_PosInf);

    // Calculate per-level left sums for each effect matrix
    std::vector< std::vector<arma::rowvec> > SumL(Ly);
    std::vector<int> countL(K, 0);
    for (int l = 0; l < Ly; ++l)
      SumL[l].assign(K, arma::rowvec(Ym[l].n_cols, arma::fill::zeros));

    // Accumulate sums for each level
    for (int i = 0; i < N; ++i) {
      int k = z_fac[i] - 1;                  // Convert to 0-based indexing
      ++countL[k];
      for (int l = 0; l < Ly; ++l) {
        SumL[l][k] += Ym[l].row(i);  // Direct accumulation, NaN already processed in preprocessing
      }
    }

    // Evaluate each level as potential split (last level is reference)
    for (int k = 0; k < K - 1; ++k) {
      int NL = countL[k], NR = N - NL;

      // Skip if either child node would be too small
      if (NL < min_node_size || NR < min_node_size) continue;

      // Calculate objective function for this split
      double obj = 0.0;
      for (int l = 0; l < Ly; ++l) {
        const arma::rowvec SL = SumL[l][k];        // const = read-only reference to avoid copying
        const arma::rowvec SR = S_tot[l] - SL;     // const = read-only reference to avoid copying
        obj += arma::accu( - SL%SL / NL - SR%SR / NR );
      }

      // Update best split if this one is better
      if (obj < best_obj) {
        best_obj = obj;
        best_level = as<std::string>(lev[k]);
      }
    }

    return List::create(_["split.point"]     = best_level,
      _["split.objective"] = best_obj);
  }

  /* ================= Numerical Variable Splitting =================== */

  // Convert to numeric and sort the feature vector once
  NumericVector z_num;
  if (TYPEOF(z) == STRSXP) {
    // Convert character to numeric if possible
    CharacterVector z_char(z);
    z_num = NumericVector(z_char.size());
    for (int i = 0; i < z_char.size(); ++i) {
      if (z_char[i] == NA_STRING) {
        z_num[i] = NA_REAL;
      } else {
        z_num[i] = std::stod(as<std::string>(z_char[i]));
      }
    }
  } else {
    z_num = NumericVector(z);
  }

  // Create sorted index and sorted values
  IntegerVector ord = Rcpp::seq(0, N - 1);
  std::sort(ord.begin(), ord.end(),
      // Custom comparator: sorts indices so that their corresponding feature values are in ascending order
      [&](int i, int j){ return z_num[i] < z_num[j]; });

  NumericVector z_sorted(N);
  for (int i = 0; i < N; ++i) z_sorted[i] = z_num[ord[i]];

  // Generate candidate split points
  NumericVector splits;
  if (n_quantiles.isNotNull()) {
    // Use quantile-based grid
    int nq = Rcpp::as<int>(n_quantiles);
    NumericVector uniq = unique_cpp(z_sorted);
    if (uniq.size() < nq) {
      splits = uniq;
    } else {
      // Calculate quantile positions
      NumericVector q(nq);
      for (int i = 0; i < nq; ++i) q[i] = (i + 1.0)/(nq + 1.0);
      splits = NumericVector(nq);
      for (int i = 0; i < nq; ++i)
        splits[i] = quantile_type7(z_sorted, q[i]);
      splits = unique_cpp(splits);
    }
  } else {
    // Use all unique values as candidates
    splits = unique_cpp(z_sorted);
  }

  // Check if we have any valid split candidates
  if (splits.size() == 0)
    return List::create(_["split.point"]     = NA_REAL,
      _["split.objective"] = R_PosInf);

  // Stream through split candidates and accumulate left sums
  std::vector<arma::rowvec> SL(Ly);
  for (int l = 0; l < Ly; ++l)
    SL[l] = arma::rowvec(Ym[l].n_cols, arma::fill::zeros);

  int idx = 0;
  for (double sp : splits) {  // Range-based for loop, sp is a copy of each split value
    // Move observations to left node until split point
    while (idx < N && z_sorted[idx] <= sp) {
      int r = ord[idx++];
      for (int l = 0; l < Ly; ++l) {
        SL[l] += Ym[l].row(r);  // Direct accumulation, NaN already processed in preprocessing
      }
    }
    int NL = idx, NR = N - NL;

    // Skip if either child node would be too small
    if (NL < min_node_size || NR < min_node_size) continue;

    // Calculate objective function for this split
    double obj = 0.0;
    for (int l = 0; l < Ly; ++l) {
      const arma::rowvec SR = S_tot[l] - SL[l];
      obj += arma::accu( - SL[l]%SL[l] / NL - SR%SR / NR );
    }
    if (obj < best_obj) { best_obj = obj; best_split = sp; }
  }

  // Refine split point to midpoint between adjacent values
  double Lft = -std::numeric_limits<double>::infinity(),
    Rgt =  std::numeric_limits<double>::infinity();
  for (int i = 0; i < N; ++i) {
    double v = z_num[i];
    if (v <= best_split && v > Lft) Lft = v;
    if (v >  best_split && v < Rgt) Rgt = v;
  }
  double mid = (Lft + Rgt) / 2.0;

  return List::create(_["split.point"]     = mid,
    _["split.objective"] = best_obj);
}


/* ------------------------------------------------------------------ *
 *  Main Interface Functions                                           *
 * ------------------------------------------------------------------ */

/**
 * @brief Find the best split for all features in a dataset
 *
 * This is the main exported function that evaluates all features and finds
 * the best split point for each one. It preprocesses the data once for
 * efficiency and returns a DataFrame with results for all features.
 *
 * @param Z DataFrame of features
 * @param Y List of effect matrices
 * @param min_node_size Minimum number of observations per node
 * @param n_quantiles Number of quantiles for numerical splitting (optional)
 * @return DataFrame with split results for all features
 */
// [[Rcpp::export]]
DataFrame search_best_split_cpp(
    DataFrame       Z,
    List            Y,
    int             min_node_size,
    Nullable<int>   n_quantiles = R_NilValue)
{
  // Initialize output vectors
  const int p = Z.size();
  CharacterVector feat_names = Z.names();

  CharacterVector split_feature(p);
  LogicalVector   is_cat_vec(p);
  CharacterVector split_point_out(p);
  NumericVector   split_obj(p);
  NumericVector   split_runtime(p);

  // Preprocess all Y matrices' NaN values to avoid repeated processing
  const int Ly = Y.size();
  std::vector<arma::mat> Ym(Ly);
  std::vector<arma::rowvec> S_tot(Ly);
  for (int l = 0; l < Ly; ++l) {
    Ym[l] = arma_view(Y[l]);
    Ym[l].replace(arma::datum::nan, 0.0);  // Process all NaN values once
    S_tot[l] = arma::sum(Ym[l], 0);        // Precompute column sums
  }

  // Start timing
  auto t0 = std::chrono::high_resolution_clock::now();

  // Evaluate each feature
  for (int j = 0; j < p; ++j) {
    SEXP z_j  = Z[j];
    bool is_c = Rf_isFactor(z_j);

    // Call internal function directly, passing preprocessed data
    List res = search_best_split_point_cpp_internal(
      z_j, Ym, S_tot, n_quantiles, is_c, min_node_size);

    // Store results
    split_feature[j]   = feat_names[j];
    is_cat_vec[j]      = is_c;
    split_obj[j]       = res["split.objective"];
    split_point_out[j] = as<CharacterVector>(wrap(res["split.point"]))[0];
  }

  // Calculate total runtime and identify best split
  auto t1 = std::chrono::high_resolution_clock::now();
  split_runtime.fill(std::chrono::duration<double>(t1 - t0).count());

  double best_val = min(split_obj);
  LogicalVector best_split = (split_obj == best_val);

  // Return results as DataFrame
  return DataFrame::create(
    _["split.feature"]   = split_feature,
    _["is.categorical"]  = is_cat_vec,
    _["split.point"]     = split_point_out,
    _["split.objective"] = split_obj,
    _["split.runtime"]   = split_runtime,
    _["best.split"]      = best_split
  );
}

/**
 * @brief Find the best split point for a single feature
 *
 * This is a wrapper function that preprocesses the data and calls the
 * internal splitting function. It handles NaN values and converts data
 * to the format expected by the core algorithm.
 *
 * @param z Feature vector (numeric or categorical)
 * @param Y List of response matrices
 * @param n_quantiles Number of quantiles for numerical splitting (optional)
 * @param is_categorical Whether the feature is categorical
 * @param min_node_size Minimum number of observations per node
 * @return List with split.point and split.objective
 */
// [[Rcpp::export]]
List search_best_split_point_cpp(
    SEXP              z,
    List              Y,
    Nullable<int>     n_quantiles   = R_NilValue,
    bool              is_categorical = false,
    int               min_node_size  = 1)
{
  // Preprocess Y matrices
  const int Ly = Y.size();
  std::vector<arma::mat> Ym(Ly);
  std::vector<arma::rowvec> S_tot(Ly);

  for (int l = 0; l < Ly; ++l) {
    Ym[l] = arma_view(Y[l]);
    Ym[l].replace(arma::datum::nan, 0.0);  // Handle NaN values
    S_tot[l] = arma::sum(Ym[l], 0);        // Precompute column sums
  }

  return search_best_split_point_cpp_internal(z, Ym, S_tot, n_quantiles, is_categorical, min_node_size);
}
