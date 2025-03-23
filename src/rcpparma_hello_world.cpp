// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

//' Check function.
//'
//' @param x cube this is a test
//' @return  new vector
//' @export
// [[Rcpp::export]]
arma::vec cubemean(const arma::Cube<double>& x) {
  return arma::mean(x);
}

// [[Rcpp::export]]
Rcpp::NumericVector cubemean_int(const arma::Cube<int>& x) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  for (int i = 0; i < ns; i++) {
    arma::uvec sub = arma::find(x.slice(i) != -2147483648);
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
      continue;
    } 
    ans[i] = arma::mean(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cubemean_int2(const arma::Cube<int>& x) {
  int ns = x.n_slices;
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(1).n_elem-1);
  Rcpp::NumericVector ans(ns);
  for (int i = 0; i < ns; i++) {
    ans[i] = arma::mean(arma::conv_to<arma::colvec>::from(x.slice(i).elem(all)));
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cubemean_num(const arma::Cube<double>& x) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  for (int i = 0; i < ns; i++) {
    arma::uvec sub = arma::find_finite(x.slice(i));
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
      continue;
    } 
    ans[i] = arma::mean(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cubemean_user(const arma::Cube<double>& x, Rcpp::Function fun) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  for (int i = 0; i < ns; i++) {
    arma::uvec sub = arma::find_finite(x.slice(i));
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
      continue;
    } 
    arma::vec test = arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub));
    ans[i] = Rcpp::as<double>(fun(test));
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cubemean_user2(const arma::Cube<int>& x, Rcpp::Function fun) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  for (int i = 0; i < ns; i++) {
    arma::uvec sub = arma::find_finite(x.slice(i));
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
      continue;
    } 
    arma::vec test = arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub));
    ans[i] = Rcpp::as<double>(fun(test));
  }
  return ans;
}


// [[Rcpp::export]]
arma::Cube<int> intna(const arma::Cube<int>& x) {
  arma::Cube<int> y = x;
  return y;
}

// [[Rcpp::export]]
double intna2(const arma::Cube<int>& x) {
  return arma::mean(x(1, 1, 1));
}






