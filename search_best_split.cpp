// split_search_fast.cpp
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <chrono>
#include <algorithm>          // std::sort, std::upper_bound

using namespace Rcpp;

/* ------------------------------------------------------------------ *
 *  helpers                                                           *
 * ------------------------------------------------------------------ */

// sort-in-place & unique  – O(n log n) without tree allocations
inline NumericVector unique_cpp(NumericVector x) {
  std::set<double> s(x.begin(), x.end());
  return NumericVector(s.begin(), s.end());
}
// inline NumericVector unique_cpp(NumericVector x) {
//   std::sort(x.begin(), x.end());
//   x.erase(std::unique(x.begin(), x.end()), x.end());
//   return x;
// }

// R type-7 quantile for *sorted* vector
inline double quantile_type7(const NumericVector& x, double p) {
  const int n = x.size();
  if (n == 0) return NA_REAL;
  if (p <= 0.0) return x[0];
  if (p >= 1.0) return x[n - 1];

  const double h = (n - 1) * p;
  const int lo   = static_cast<int>(h);
  const double f = h - lo;

  return x[lo] + f * (x[lo + 1] - x[lo]);
}

// double quantile_type7(const NumericVector &x, const double p) {
//   const int n = x.size();
//   if (n == 0) return NA_REAL;
//   if (p <= 0.0) return x[0];
//   if (p >= 1.0) return x[n - 1];
//
//   const double idx   = p * (n - 1);
//   const int    lo    = floor(idx);
//   const double gamma = idx - lo;
//   return (gamma == 0.0) ? x[lo] :
//     (1 - gamma) * x[lo] + gamma * x[lo + 1];
// }

// coercion to numeric matrix → Armadillo view (zero-copy)
inline arma::mat arma_view(SEXP obj) {
  if (!Rf_isMatrix(obj) || TYPEOF(obj) != REALSXP) {
    static Rcpp::Function as_matrix("as.matrix");
    obj = as_matrix(obj);
  }
  Rcpp::NumericMatrix M(obj);
  return arma::mat(M.begin(), M.nrow(), M.ncol(), /*copy*/ false);
}


/* ------------------------------------------------------------------ *
 *  inner splitter                                                    *
 * ------------------------------------------------------------------ */

// [[Rcpp::export]]
List search_best_split_point_cpp(
    SEXP              z,
    List              Y,
    Nullable<int>     n_quantiles   = R_NilValue,
    bool              is_categorical = false,
    int               min_node_size  = 1)
{
  const int Ly = Y.size();
  std::vector<arma::mat> Ym(Ly);
  std::vector<arma::rowvec> S_tot(Ly);       // total col-sums

  // cast Y once and compute colSums once -------------------------------
  for (int l = 0; l < Ly; ++l) {
    Ym[l]   = arma_view(Y[l]);
    S_tot[l]= arma::sum(Ym[l], 0);           // 1×G rowvec
  }
  const int N = Ym[0].n_rows;

  double best_obj = R_PosInf, best_split = NA_REAL;
  std::string best_level;

  /* ================= categorical ==================================== */
  if (is_categorical) {
    IntegerVector z_fac(z);
    CharacterVector lev = z_fac.attr("levels");
    const int K = lev.size();
    if (K <= 1)
      return List::create(_["split.point"] = R_NaString,
        _["split.objective"] = R_PosInf);

    // per-level left sums ------------------------------------------------
    std::vector< std::vector<arma::rowvec> > SumL(Ly);
    std::vector<int> countL(K, 0);
    for (int l = 0; l < Ly; ++l)
      SumL[l].assign(K, arma::rowvec(Ym[l].n_cols, arma::fill::zeros));

    for (int i = 0; i < N; ++i) {
      int k = z_fac[i] - 1;                  // 0-based level
      ++countL[k];
      for (int l = 0; l < Ly; ++l)
        SumL[l][k] += Ym[l].row(i);
    }

    for (int k = 0; k < K - 1; ++k) {        // last level = reference set
      int NL = countL[k], NR = N - NL;
      if (NL < min_node_size || NR < min_node_size) continue;

      double obj = 0.0;
      for (int l = 0; l < Ly; ++l) {
        const arma::rowvec SL = SumL[l][k];
        const arma::rowvec SR = S_tot[l] - SL;
        obj += arma::accu( - SL%SL / NL - SR%SR / NR );
      }
      if (obj < best_obj) { best_obj = obj; best_level = as<std::string>(lev[k]); }
    }

    return List::create(_["split.point"]     = best_level,
      _["split.objective"] = best_obj);
  }

  /* ================= numerical ======================================= */

  // sort z once ---------------------------------------------------------
  NumericVector z_num(z);
  IntegerVector ord = Rcpp::seq(0, N - 1);
  std::sort(ord.begin(), ord.end(),
    [&](int i, int j){ return z_num[i] < z_num[j]; });

  NumericVector z_sorted(N);
  for (int i = 0; i < N; ++i) z_sorted[i] = z_num[ord[i]];

  // candidate grid ------------------------------------------------------
  NumericVector splits;
  if (n_quantiles.isNotNull()) {
    int nq = Rcpp::as<int>(n_quantiles);
    NumericVector uniq = unique_cpp(z_sorted);
    if (uniq.size() < 10) {
      splits = uniq;
    } else {
      NumericVector q(nq);
      for (int i = 0; i < nq; ++i) q[i] = (i + 1.0)/(nq + 1.0);
      splits = NumericVector(nq);
      for (int i = 0; i < nq; ++i)
        splits[i] = quantile_type7(z_sorted, q[i]);
      splits = unique_cpp(splits);
    }
  } else {
    splits = unique_cpp(z_sorted);
  }
  if (splits.size() == 0)
    return List::create(_["split.point"]     = NA_REAL,
      _["split.objective"] = R_PosInf);

  // streaming left sums --------------------------------------------------
  std::vector<arma::rowvec> SL(Ly);
  for (int l = 0; l < Ly; ++l)
    SL[l] = arma::rowvec(Ym[l].n_cols, arma::fill::zeros);

  int idx = 0;
  for (double sp : splits) {
    while (idx < N && z_sorted[idx] <= sp) {
      int r = ord[idx++];
      for (int l = 0; l < Ly; ++l)
        SL[l] += Ym[l].row(r);
    }
    int NL = idx, NR = N - NL;
    if (NL < min_node_size || NR < min_node_size) continue;

    double obj = 0.0;
    for (int l = 0; l < Ly; ++l) {
      const arma::rowvec SR = S_tot[l] - SL[l];
      obj += arma::accu( - SL[l]%SL[l] / NL - SR%SR / NR );
    }
    if (obj < best_obj) { best_obj = obj; best_split = sp; }
  }

  // midpoint refinement ---------------------------------------------------
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
 *  outer wrapper                                                     *
 * ------------------------------------------------------------------ */

// [[Rcpp::export]]
DataFrame search_best_split_cpp(
    DataFrame       Z,
    List            Y,
    int             min_node_size,
    Nullable<int>   n_quantiles = R_NilValue)
{
  const int p = Z.size();
  CharacterVector feat_names = Z.names();

  CharacterVector split_feature(p);
  LogicalVector   is_cat_vec(p);
  CharacterVector split_point_out(p);
  NumericVector   split_obj(p);
  NumericVector   split_runtime(p);

  auto t0 = std::chrono::high_resolution_clock::now();

  for (int j = 0; j < p; ++j) {
    SEXP z_j  = Z[j];
    bool is_c = Rf_isFactor(z_j);

    List res = search_best_split_point_cpp(
      z_j, Y, n_quantiles, is_c, min_node_size);

    split_feature[j]   = feat_names[j];
    is_cat_vec[j]      = is_c;
    split_obj[j]       = res["split.objective"];
    split_point_out[j] = as<CharacterVector>(wrap(res["split.point"]))[0];
  }

  auto t1 = std::chrono::high_resolution_clock::now();
  split_runtime.fill(std::chrono::duration<double>(t1 - t0).count());

  double best_val = min(split_obj);
  LogicalVector best_split = (split_obj == best_val);

  return DataFrame::create(
    _["split.feature"]   = split_feature,
    _["is.categorical"]  = is_cat_vec,
    _["split.point"]     = split_point_out,
    _["split.objective"] = split_obj,
    _["split.runtime"]   = split_runtime,
    _["best.split"]      = best_split
  );
}
