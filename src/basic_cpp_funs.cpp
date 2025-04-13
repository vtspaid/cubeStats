// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]


////////////////////////////////////////////////////////////////////////////////
// Template function to compute by slice
// [[RCPP::export]]
template <typename T>
Rcpp::NumericVector cpp_slicefun(const arma::Cube<T>& x, 
                                 bool na_rm, 
                                 double (*armaFunc)(const arma::colvec&),
                                 bool auto_na,
                                 double mis_val = -2147483648) {
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
      }
      
    } else { 
      if (na_rm) {
        sub = auto_na ? arma::find_finite(x.slice(i)) : all;
      } else {
        if (x.slice(i).has_nonfinite()) {
          ans[i] = NA_REAL;
          continue;
        }
        sub = all;
      }
    }
    
    // Compute mean only if valid values exist
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
    } else {
      ans[i] = armaFunc(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
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
  return cpp_slicefun(x, na_rm, arma::mean, true, mis_val);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemean_num(const arma::Cube<double>& x, bool na_rm) {
  return cpp_slicefun(x, na_rm, arma::mean, true);
}
////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Max
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemax_int(const arma::Cube<int>& x, 
                                      bool na_rm, 
                                      double mis_val) {
  return cpp_slicefun(x, na_rm, arma::max, false, mis_val);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemax_num(const arma::Cube<double>& x, bool na_rm) {
  return cpp_slicefun(x, na_rm, arma::max, false);
}

////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Min
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemin_int(const arma::Cube<int>& x, 
                                     bool na_rm, 
                                     double mis_val) {
  return cpp_slicefun(x, na_rm, arma::min, false, mis_val);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemin_num(const arma::Cube<double>& x, bool na_rm) {
  return cpp_slicefun(x, na_rm, arma::min, false);
}
/////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Functions for finding the Median
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemedian_int(const arma::Cube<int>& x, 
                                        bool na_rm, 
                                        double mis_val) {
  return cpp_slicefun(x, na_rm, arma::median, true, mis_val);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicemedian_num(const arma::Cube<double>& x, 
                                        bool na_rm) {
  return cpp_slicefun(x, na_rm, arma::median, true);
}
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Functions for finding the sum
// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicesum_int(const arma::Cube<int>& x,
                                     bool na_rm,
                                     double mis_val) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(1).n_elem-1);
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec sub = arma::find(x.slice(i) != mis_val);
      if (sub.is_empty()) {
        ans[i] = 0;
        continue;
      } 
    ans[i] = arma::sum(x.slice(i).elem(sub));
    } else {
      ans[i] = arma::sum(x.slice(i).elem(all));
    }
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_slicesum_num(const arma::Cube<double>& x, bool na_rm) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(1).n_elem-1);
  for (int i = 0; i < ns; i++) {
    if (na_rm == true) {
      arma::uvec sub = arma::find_finite(x.slice(i));
      if (sub.is_empty()) {
        ans[i] = 0;
        continue;
      } 
      ans[i] = arma::sum(x.slice(i).elem(sub));
    } else {
      ans[i] = arma::sum(x.slice(i).elem(all));
    }
    
  }
  return ans;
}
/////////////////////////////////////////////////////////////////////////////////



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


// [[RCPP::export]]
template <typename T>
arma::uvec get_sub(const arma::Cube<T>& x, 
                                  bool na_rm, 
                                  double mis_val = -2147483648) {
  int ns = x.n_slices;
  Rcpp::NumericVector ans(ns);
  arma::uvec all = arma::regspace<arma::uvec>(0, x.slice(0).n_elem - 1);
  for (int i = 0; i < ns; i++) {
    arma::uvec sub;
    
    if (std::is_same<T, int>::value) { 
      // Handle integer case
      sub = arma::find(x.slice(i) != static_cast<T>(mis_val));
      if (na_rm == false && sub.n_elem != all.n_elem) {
        ans[i] = NA_REAL;
        continue;
      }
      
    } else { 
      // Handle double case
      if (na_rm) {
        sub = arma::find_finite(x.slice(i));
      } else {
        sub = all;
      }
    }
    
    if (sub.is_empty()) {
      ans[i] = NA_REAL;
    } else {
      ans[i] = arma::mean(arma::conv_to<arma::colvec>::from(x.slice(i).elem(sub)));
    }
  }
  return ans;
}
