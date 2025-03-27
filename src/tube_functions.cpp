// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubemean_int(const arma::Cube<int>& x, double mis_val) {
  int nr = x.n_rows; 
  int nc = x.n_cols;
  Rcpp::NumericVector ans(nr * nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::ivec tube = x.tube(i, j);
      arma::uvec locs = arma::find(tube != mis_val);
      if (locs.is_empty()) {
        ans[index] = NA_REAL;
        index++;
        continue;
      } 
      ans[index] = arma::mean(arma::conv_to<arma::colvec>::from(tube.elem(locs)));
      index++;
    }
  }
  return ans;
}


// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubemean_num(const arma::Cube<double>& x, bool na_rm) {
  int nr = x.n_rows; 
  int nc = x.n_cols;
  Rcpp::NumericVector ans(nr * nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      if (na_rm == true) {
        arma::vec tube = x.tube(i, j);
        arma::uvec locs = arma::find_finite(tube);
        if (locs.is_empty()) {
          ans[index] = NA_REAL;
          index++;
          continue;
        } 
        ans[index] = arma::mean(tube.elem(locs));
        index++;
      } else {
        if (x.tube(i, j).has_nonfinite()) {
          ans[index] = NA_REAL;
          index++;
          continue;
        }
        arma::vec tube = x.tube(i, j);
        ans[index] = arma::mean(tube);
        index++;
      }
    }
  }
  return ans;
}