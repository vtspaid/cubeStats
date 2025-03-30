// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Mean
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemean_int(const arma::Cube<int>& x, double mis_val) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  for (int i = 0; i < ns; i++) {
    arma::uvec sub = arma::find(x.slice(i) != mis_val);
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
      continue;
    } 
    ans[i] = arma::mean(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemean_num(const arma::Cube<double>& x, bool na_rm) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(1).n_elem-1);
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
    arma::uvec sub = arma::find_finite(x.slice(i));
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
      continue;
    } 
    ans[i] = arma::mean(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
    } else {
      ans[i] = arma::mean(arma::conv_to<arma::colvec>::from(x.slice(i).elem(all)));
    }
    
  }
  return ans;
}
/////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Max
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemax_int(const arma::Cube<int>& x, double mis_val) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  for (int i = 0; i < ns; i++) {
    arma::uvec sub = arma::find(x.slice(i) != mis_val);
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
      continue;
    } 
    ans[i] = arma::max(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemax_num(const arma::Cube<double>& x, bool na_rm) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(1).n_elem-1);
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      ans[i] = arma::max(arma::conv_to<arma::colvec>::from(x.slice(i).elem(all)));
    } else if (x.slice(i).has_nonfinite()){
      ans[i] = NA_REAL;
      continue;
    } else {
    ans[i] = arma::max(arma::conv_to<arma::colvec>::from(x.slice(i).elem(all)));
  }
  }
return ans;
}
/////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Min
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemin_int(const arma::Cube<int>& x, double mis_val) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  for (int i = 0; i < ns; i++) {
    arma::uvec sub = arma::find(x.slice(i) != mis_val);
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
      continue;
    } 
    ans[i] = arma::min(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemin_num(const arma::Cube<double>& x, bool na_rm) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(1).n_elem-1);
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      ans[i] = arma::min(arma::conv_to<arma::colvec>::from(x.slice(i).elem(all)));
    } else if (x.slice(i).has_nonfinite()){
      ans[i] = NA_REAL;
      continue;
    } else {
      ans[i] = arma::min(arma::conv_to<arma::colvec>::from(x.slice(i).elem(all)));
    }
  }
return ans;
}
/////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Mean
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemedian_int(const arma::Cube<int>& x, double mis_val) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  for (int i = 0; i < ns; i++) {
    arma::uvec sub = arma::find(x.slice(i) != mis_val);
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
      continue;
    } 
    ans[i] = arma::median(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemedian_num(const arma::Cube<double>& x, bool na_rm) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(1).n_elem-1);
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec sub = arma::find_finite(x.slice(i));
      if (sub.is_empty()) {
        ans[i] = NA_REAL;
        continue;
      } 
      ans[i] = arma::median(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
    } else {
      ans[i] = arma::median(arma::conv_to<arma::colvec>::from(x.slice(i).elem(all)));
    }
    
  }
  return ans;
}
/////////////////////////////////////////////////////////////////////////////////