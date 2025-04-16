// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]


// [[RCPP::export]]
template <typename T, typename Func>
Rcpp::NumericVector cpp_tubefun(const arma::Cube<T>& x, 
                                 bool na_rm, 
                                 Func armaFunc,
                                 bool auto_na,
                                 double mis_val = -2147483648,
                                 bool conv_int = true) {
  int nr = x.n_rows; 
  int nc = x.n_cols;
  Rcpp::NumericVector ans(nr * nc);
  int index = 0;
  
  //get a uvec containing all indices in a tube
  arma::uvec all = arma::regspace<arma::uvec>(0, x.tube(0, 0).n_elem - 1);
  int all_length = all.n_elem;
  
  // Precompute whether type is integer
  constexpr bool is_int = std::is_same<T, int>::value;
  
  T mis = static_cast<T>(mis_val);
  
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::uvec locs;
      if (is_int) { 
        // Handle integer case
        arma::Col<T> tube = x.tube(i, j);
        locs = arma::find(tube != mis);
        
        // If not removing NAs but missing values exist, return NA
        if (na_rm == false && locs.n_elem != all_length) {
          ans[index] = NA_REAL;
          index++;
          continue;
        } else if (locs.n_elem == 0) {
          ans[index] = NA_REAL;
          index++;
          continue;
        } else if (conv_int){
          ans[index] = armaFunc(arma::conv_to<arma::colvec>::from(tube.elem(locs)));
          index++;
        } else {
          ans[index] = armaFunc(tube.elem(locs));
          index++;
        }
        
        // handle numeric case
      } else { 
       // arma::Col<T> tube = x.tube(i, j);
        if (auto_na == false && na_rm == true) {
          arma::Col<T> tube = x.tube(i, j);
          ans[index] = armaFunc(tube.elem(all));
          index++;
          continue;
        }
        
        
        
        if (na_rm == true) {
          arma::Col<T> tube = x.tube(i, j);
          locs = arma::find_finite(tube);
          if (locs.is_empty()) {
            ans[index] = NA_REAL;
            index++;
            continue;
          } 
          ans[index] = armaFunc(tube.elem(locs));
          index++;
        } else {
          if (x.tube(i, j).has_nonfinite()) {
            ans[index] = NA_REAL;
            index++;
            continue;
          }
          arma::Col<T> tube = x.tube(i, j);
          ans[index] = armaFunc(tube);
          index++;
        }
        
        
        
        
        
        // 
        // else {
        //   //if (na_rm == false && tube.has_nonfinite()) {
        //   if (na_rm == false && tube.has_nonfinite() == false) {
        //     ans[index] = armaFunc(tube.elem(all));
        //     index++;
        //     continue;
        //   } else {
        //     locs = arma::find_finite(tube);
        //   }
        //   if (na_rm == false && locs.n_elem != all_length) {
        //     ans[index] = NA_REAL;
        //     index++;
        //   } else {
        //   ans[index] = armaFunc(tube.elem(locs));
        //   index++;
        //   }
        // } 
      }
      
    }
  }
  return ans;
}

////////////////////////////////////////////////////////////////////////////////
// Mean
// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubemean_int(const arma::Cube<int>& x, 
                                     bool na_rm, 
                                     double mis_val) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::mean(v); }, 
                     true, mis_val);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubemean_num(const arma::Cube<double>& x, 
                                     bool na_rm) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::mean(v); }, 
                     true);
}
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Max
// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubemax_int(const arma::Cube<int>& x, 
                                     bool na_rm, 
                                     double mis_val) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::max(v); }, 
                     false, mis_val, false);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubemax_num(const arma::Cube<double>& x, 
                                     bool na_rm) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::max(v); }, 
                     false, 0, false);
}
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Max
// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubemin_int(const arma::Cube<int>& x, 
                                    bool na_rm, 
                                    double mis_val) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::min(v); }, 
                     false, mis_val, false);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubemin_num(const arma::Cube<double>& x, 
                                    bool na_rm) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::min(v); }, 
                     false, 0, false);
}
////////////////////////////////////////////////////////////////////////////////


// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubemean_int2(const arma::Cube<int>& x, double mis_val) {
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
Rcpp::NumericVector cpp_tubemean_num2(const arma::Cube<double>& x, bool na_rm) {
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