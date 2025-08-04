/**
 * @file re_mean_center_ice.cpp
 * @brief Fast mean-centering of ICE matrices using C++ for the gadget package
 *
 * @author gadget Development Team
 * @version 1.0
 */
 #include <Rcpp.h>
 #include <unordered_set>
 #include <vector>
 #include <string>
 // [[Rcpp::depends(Rcpp)]]
 using namespace Rcpp;
 using namespace std;
 
 /**
  * @brief Convert an Rcpp CharacterVector to a C++ unordered_set<string>
  *
  * This helper function takes an R character vector (e.g., column names)
  * and converts it to a C++ unordered_set of strings. This enables
  * O(1) hash-based lookup for string membership, which is useful for
  * efficiently checking if a column name is present in a set (such as a grid).
  *
  * @param vec Rcpp CharacterVector of strings (e.g., column names)
  * @return std::unordered_set<std::string> containing all unique strings from vec
  */
 std::unordered_set<std::string> charvec_to_set(const CharacterVector& vec) {
   std::unordered_set<std::string> s;
   for (int i = 0; i < vec.size(); ++i) {
     // Check for NA values before conversion
     if (!CharacterVector::is_na(vec[i])) {
       s.insert(Rcpp::as<std::string>(vec[i]));
     }
   }
   return s;
 }
 
 /**
  * @brief Safely convert an SEXP object to NumericMatrix. If not a matrix, automatically calls as.matrix.
  */
 inline NumericMatrix safe_matrix(SEXP obj) {
   if (!Rf_isMatrix(obj) || TYPEOF(obj) != REALSXP) {
     static Rcpp::Function as_matrix("as.matrix");
     obj = as_matrix(obj);
   }
   return NumericMatrix(obj);
 }
 
 // [[Rcpp::export]]
 /**
  * @brief Mean-center ICE matrices for each feature, only on grid columns.
  *
  * For each feature matrix in Y, this function:
  *   - Selects only the rows specified by idx
  *   - Sets columns not in grid to NA
  *   - For each row, mean-centers only the valid grid and non-NA columns
  *   - Returns a list of centered matrices, preserving column names
  *
  * @param Y   List of NumericMatrix, one per feature with dimnames
  * @param grid List of CharacterVector, each specifying valid grid columns for the feature
  * @param idx IntegerVector of row indices, 1-based as in R
  * @return List of mean-centered NumericMatrix, with column names preserved
  */
 List re_mean_center_ice_cpp(List Y, List grid, IntegerVector idx) {
   int L = Y.size();
   List result(L);
   CharacterVector namesY = Y.names();
 
   for (int i = 0; i < L; ++i) {
     // Extract matrix and its column names for this feature
     NumericMatrix mat = safe_matrix(Y[i]);
     List dimnames = mat.attr("dimnames");
     CharacterVector colnames = as<CharacterVector>(dimnames[1]);
     CharacterVector grid_i = grid[i];
     // Convert grid columns to unordered_set for fast lookup
     std::unordered_set<std::string> keep_cols = charvec_to_set(grid_i);
     int ncols = mat.ncol();
     int nrows = idx.size();
 
     // Convert colnames to std::vector<string> for fast comparison
     std::vector<std::string> colnames_str(ncols);
     for (int c = 0; c < ncols; ++c) {
       if (!CharacterVector::is_na(colnames[c])) {
         colnames_str[c] = Rcpp::as<std::string>(colnames[c]);
       } else {
         colnames_str[c] = "";  // Use empty string for NA values
       }
     }
 
     // Prepare output matrix, preserve column names
     NumericMatrix centered(nrows, ncols);
     centered.attr("dimnames") = List::create(R_NilValue, colnames);
 
     // For each selected row
     for (int r = 0; r < nrows; ++r) {
       int row_idx = idx[r] - 1;
       
       // First, set non-grid columns to NA (matching R implementation)
       for (int c = 0; c < ncols; ++c) {
         if (keep_cols.count(colnames_str[c])) {
           centered(r, c) = mat(row_idx, c);
         } else {
           centered(r, c) = NA_REAL;
         }
       }
       
       // Then compute row mean and center (matching R's rowMeans with na.rm=TRUE)
       double sum = 0.0;
       int count = 0;
       for (int c = 0; c < ncols; ++c) {
         if (!NumericMatrix::is_na(centered(r, c))) {
           sum += centered(r, c);
           count++;
         }
       }
       double mean = (count > 0) ? sum / count : NA_REAL;
       
       // Finally, center the values
       for (int c = 0; c < ncols; ++c) {
         if (!NumericMatrix::is_na(centered(r, c))) {
           centered(r, c) = centered(r, c) - mean;
         }
       }
     }
     result[i] = centered;
   }
   result.attr("names") = namesY;
   return result;
 }
 
 