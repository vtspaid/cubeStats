// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]


////////////////////////////////////////////////////////////////////////////////
// Template function to compute by slice
// [[RCPP::export]]
//template <typename T>
template <typename T, typename Func>
Rcpp::NumericVector cpp_slicefun(const arma::Cube<T>& x, 
                                 bool na_rm, 
                                // double (*armaFunc)(const arma::vec&),
                                 Func armaFunc,
                                 bool auto_na,
                                 double mis_val = -2147483648,
                                 bool conv_int = true) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  
  //get a uvec containing all indices in a slice
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(0).n_elem - 1);
  int all_length = all.n_elem;
  
  // Precompute whether type is integer
  constexpr bool is_int = std::is_same<T, int>::value;
  
  T mis = static_cast<T>(mis_val);
  
  for (int i = 0; i < ns; i++) {
    arma::uvec sub;
    if (is_int) { 
      // Handle integer case
      sub = arma::find(x.slice(i) != mis);
      
      // If not removing NAs but missing values exist, return NA
      if (na_rm == false && sub.n_elem != all_length) {
        ans[i] = NA_REAL;
        continue;
      } else if (sub.n_elem == 0) {
        ans[i] = NA_REAL;
      } else if (conv_int){
      ans[i] = armaFunc(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
      } else {
        ans[i] = armaFunc(x.slice(i).elem(sub));
      }
      
    } else { 
      if (na_rm) {
        sub = auto_na ? arma::find_finite(x.slice(i)) : all;
      } else {
        sub = all;
      } 
      if (sub.n_elem == 0) {
        ans[i] = NA_REAL;
      } else {
      ans[i] = armaFunc(x.slice(i).elem(sub));
      }
    }

  }
  return ans;
}

////////////////////////////////////////////////////////////////////////////////
// Function to compute the mean //////
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemean_int(const arma::Cube<int>& x, 
                                      bool na_rm, 
                                      double mis_val) {
 // return cpp_slicefun(x, na_rm, arma::mean, true, mis_val);
 return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::mean(v); }, true, mis_val);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemean_num(const arma::Cube<double>& x, bool na_rm) {
  //return cpp_slicefun(x, na_rm, arma::mean, true);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::mean(v); }, true);
}
////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Max
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemax_int(const arma::Cube<int>& x,
                                      bool na_rm,
                                      double mis_val) {
 // return cpp_slicefun(x, na_rm, arma::max, false, mis_val);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::max(v); }, 
                      true, mis_val, false);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemax_num(const arma::Cube<double>& x, bool na_rm) {
  //return cpp_slicefun(x, na_rm, arma::max, false);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::max(v); }, true);
}

////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Min
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemin_int(const arma::Cube<int>& x,
                                     bool na_rm,
                                     double mis_val) {
 // return cpp_slicefun(x, na_rm, arma::min, false, mis_val);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::min(v); }, 
                      true, mis_val, false);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemin_num(const arma::Cube<double>& x, bool na_rm) {
  //return cpp_slicefun(x, na_rm, arma::min, false);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::min(v); }, true);
}
/////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Median
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemedian_int(const arma::Cube<int>& x,
                                        bool na_rm,
                                        double mis_val) {
  //return cpp_slicefun(x, na_rm, arma::median, true, mis_val);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::median(v); }, true, mis_val);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemedian_num(const arma::Cube<double>& x,
                                        bool na_rm) {
 // return cpp_slicefun(x, na_rm, arma::median, true);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::median(v); }, true);
}
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Functions for finding the sum
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicesum_int(const arma::Cube<int>& x,
                                        bool na_rm,
                                        double mis_val) {
  //return cpp_slicefun(x, na_rm, arma::sum, true, mis_val);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::sum(v); }, true, mis_val);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicesum_num(const arma::Cube<double>& x, 
                                        bool na_rm) {
 // return cpp_slicefun(x, na_rm, arma::sum, true);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::sum(v); }, true);
}
/////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Functions for finding the standard deviation
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicesd_int(const arma::Cube<int>& x,
                                     bool na_rm,
                                     double mis_val) {
  //return cpp_slicefun(x, na_rm, arma::sum, true, mis_val);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::stddev(v); }, true, mis_val);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicesd_num(const arma::Cube<double>& x, 
                                     bool na_rm) {
  // return cpp_slicefun(x, na_rm, arma::sum, true);
  return cpp_slicefun(x, na_rm, [](const auto& v) { return arma::stddev(v); }, true);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_test(const arma::Cube<double>& x) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(0).n_elem-1);
  for (int i = 0; i < ns; i++) {
    ans[i] = arma::stddev(x.slice(i).elem(all));
  }
  return ans;
}

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Evaluation functions
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicegreater_int(const arma::Cube<int>& x,
                                     bool na_rm,
                                     double value,
                                     double mis_val) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  int tot = x.slice(0).n_elem;
    for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec greater = arma::find(x.slice(i) > value);
      ans[i] = greater.n_elem;
    } else {
      arma::uvec sub = arma::find(x.slice(i) != mis_val);
      if (sub.n_elem != tot) {
        ans[i] = NA_REAL;
        continue;
      }
      arma::uvec greater = arma::find(x.slice(i) > value);
      ans[i] = greater.n_elem;
      
    }
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicegreater_num(const arma::Cube<double>& x,
                                         bool na_rm,
                                         double value) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  int tot = x.slice(0).n_elem;
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec greater = arma::find(x.slice(i) > value);
      ans[i] = greater.n_elem;
    } else {
      arma::uvec sub = arma::find_finite(x.slice(i));
      if (sub.n_elem != tot) {
        ans[i] = NA_REAL;
        continue;
      }
      arma::uvec greater = arma::find(x.slice(i) > value);
      ans[i] = greater.n_elem;
      
    }
  }
  return ans;
}


// [[Rcpp::export]]
Rcpp::NumericVector cpp_sliceless_int(const arma::Cube<int>& x,
                                         bool na_rm,
                                         double value,
                                         double mis_val) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  int tot = x.slice(0).n_elem;
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec lesser = arma::find(x.slice(i) < value);
      ans[i] = lesser.n_elem;
    } else {
      arma::uvec sub = arma::find(x.slice(i) != mis_val);
      if (sub.n_elem != tot) {
        ans[i] = NA_REAL;
        continue;
      }
      arma::uvec lesser = arma::find(x.slice(i) < value);
      ans[i] = lesser.n_elem;
      
    }
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_sliceless_num(const arma::Cube<double>& x,
                                         bool na_rm,
                                         double value) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  int tot = x.slice(0).n_elem;
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec lesser = arma::find(x.slice(i) < value);
      ans[i] = lesser.n_elem;
    } else {
      arma::uvec sub = arma::find_finite(x.slice(i));
      if (sub.n_elem != tot) {
        ans[i] = NA_REAL;
        continue;
      }
      arma::uvec lesser = arma::find(x.slice(i) < value);
      ans[i] = lesser.n_elem;
      
    }
  }
  return ans;
}


// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicerange_int(const arma::Cube<int>& x,
                                      bool na_rm,
                                      double value1,
                                      double value2,
                                      double mis_val) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  int tot = x.slice(0).n_elem;
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec match = arma::find((x.slice(i) > value1) && (x.slice(i) < value2));
      ans[i] = match.n_elem;
    } else {
      arma::uvec sub = arma::find(x.slice(i) != mis_val);
      if (sub.n_elem != tot) {
        ans[i] = NA_REAL;
        continue;
      }
      arma::uvec match = arma::find((x.slice(i) > value1) && (x.slice(i) < value2));
      ans[i] = match.n_elem;
      
    }
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicerange_num(const arma::Cube<double>& x,
                                      bool na_rm,
                                      double value1,
                                      double value2) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  int tot = x.slice(0).n_elem;
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec match = arma::find((x.slice(i) > value1) && (x.slice(i) < value2));
      ans[i] = match.n_elem;
    } else {
      arma::uvec sub = arma::find_finite(x.slice(i));
      if (sub.n_elem != tot) {
        ans[i] = NA_REAL;
        continue;
      }
      arma::uvec match = arma::find((x.slice(i) > value1) && (x.slice(i) < value2));
      ans[i] = match.n_elem;
      
    }
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_sliceequal_int(const arma::Cube<int>& x,
                                      bool na_rm,
                                      double value,
                                      double mis_val) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  int tot = x.slice(0).n_elem;
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec greater = arma::find(x.slice(i) == value);
      ans[i] = greater.n_elem;
    } else {
      arma::uvec sub = arma::find(x.slice(i) != mis_val);
      if (sub.n_elem != tot) {
        ans[i] = NA_REAL;
        continue;
      }
      arma::uvec greater = arma::find(x.slice(i) == value);
      ans[i] = greater.n_elem;
      
    }
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_sliceequal_num(const arma::Cube<double>& x,
                                      bool na_rm,
                                      double value) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  int tot = x.slice(0).n_elem;
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec greater = arma::find(x.slice(i) == value);
      ans[i] = greater.n_elem;
    } else {
      arma::uvec sub = arma::find_finite(x.slice(i));
      if (sub.n_elem != tot) {
        ans[i] = NA_REAL;
        continue;
      }
      arma::uvec greater = arma::find(x.slice(i) == value);
      ans[i] = greater.n_elem;
      
    }
  }
  return ans;
}
// End of evaluate functions
///////////////////////////////////////////////////////////////////////////////

