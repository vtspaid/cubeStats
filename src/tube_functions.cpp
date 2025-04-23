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
  const int nr = x.n_rows; 
  const int nc = x.n_cols;
  Rcpp::NumericVector ans(nr * nc);
  int index = 0;
  const int ns = x.n_slices;
  bool has_missing;
  //get a uvec containing all indices in a tube
  arma::uvec all = arma::regspace<arma::uvec>(0, ns -1);
  
  arma::Col<T> vals(ns);          // reuse buffer
  
  // Precompute whether type is integer
  constexpr bool is_int = std::is_same<T, int>::value;
  
  T mis = static_cast<T>(mis_val);
  arma::uvec locs;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      if (is_int) { 
        // Handle integer case
        
        vals = x.tube(i, j);
        has_missing = arma::any(vals == mis);
        
        if (!na_rm) {
          if (has_missing) {
            ans[index++] = NA_REAL;
          } else {
            // Use all values
            if (conv_int) {
              ans[index++] = armaFunc(arma::conv_to<arma::vec>::from(vals));
            } else {
              ans[index++] = armaFunc(vals);
            }
          }
        } else {
          if (!has_missing) {
            if (conv_int) {
              ans[index++] = armaFunc(arma::conv_to<arma::vec>::from(vals));
              continue;
            } else {
              ans[index++] = armaFunc(vals);
              continue;
            }
          }
          // Remove missing values
          locs = arma::find(vals != mis);
          if (locs.n_elem == 0) {
            ans[index++] = NA_REAL;
          } else {
            if (conv_int) {
              ans[index++] = armaFunc(arma::conv_to<arma::vec>::from(vals.elem(locs)));
            } else {
              ans[index++] = armaFunc(vals.elem(locs));
            }
          }
        }
        
        // handle numeric case
      } else { 
       // arma::Col<T> tube = x.tube(i, j);
        if (auto_na == false && na_rm == true) {
          arma::Col<T> tube = x.tube(i, j);
          ans[index++] = armaFunc(tube.elem(all));
          continue;
        }
        
        
        
        if (na_rm == true) {
          arma::Col<T> tube = x.tube(i, j);
          locs = arma::find_finite(tube);
          if (locs.is_empty()) {
            ans[index++] = NA_REAL;
            continue;
          } 
          ans[index++] = armaFunc(tube.elem(locs));
        } else {
          if (x.tube(i, j).has_nonfinite()) {
            ans[index++] = NA_REAL;
            continue;
          }
          arma::Col<T> tube = x.tube(i, j);
          ans[index++] = armaFunc(tube);
        }
        
      }
      
    }
  }
  return ans;
}


// [[Rcpp::export]]
Rcpp::NumericVector tube_test(const arma::Cube<int>& x, 
                                     bool na_rm = false, 
                                     double mis_val = -2147483648) {
int nr = x.n_rows; 
int nc = x.n_cols;
Rcpp::NumericVector ans(nr * nc);
int index = 0;

const int ns = x.n_slices;
arma::ivec vals(ns);          // reuse buffer
arma::vec tube_conv(ns);   

//get a uvec containing all indices in a tube
for (int j = 0; j < nc; j++) {
  for (int i = 0; i < nr; i++) {
    arma::uvec locs;

        vals = x.tube(i, j);
            ans[index++] = arma::mean(arma::conv_to<arma::vec>::from(vals));
  }
}
return ans;
}
////////////////////////////////////////////////////////////////////////////////


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
// Min
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

////////////////////////////////////////////////////////////////////////////////
// sd
// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubesd_int(const arma::Cube<int>& x, 
                                    bool na_rm, 
                                    double mis_val) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::stddev(v); }, 
                     true, mis_val, true);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubesd_num(const arma::Cube<double>& x, 
                                    bool na_rm) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::stddev(v); }, 
                     true, 0, true);
}
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// sum
// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubesum_int(const arma::Cube<int>& x, 
                                   bool na_rm, 
                                   double mis_val) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::sum(v); }, 
                     true, mis_val, true);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubesum_num(const arma::Cube<double>& x, 
                                   bool na_rm) {
  return cpp_tubefun(x, na_rm = na_rm, [](const auto& v) { return arma::sum(v); }, 
                     true, 0, true);
}
////////////////////////////////////////////////////////////////////////////////

// // [[Rcpp::export]]
// Rcpp::NumericVector cpp_tubemean_int2(const arma::Cube<int>& x, double mis_val) {
//   int nr = x.n_rows; 
//   int nc = x.n_cols;
//   Rcpp::NumericVector ans(nr * nc);
//   int index = 0;
//   for (int j = 0; j < nc; j++) {
//     for (int i = 0; i < nr; i++) {
//       arma::ivec tube = x.tube(i, j);
//       arma::uvec locs = arma::find(tube != mis_val);
//       if (locs.is_empty()) {
//         ans[index] = NA_REAL;
//         index++;
//         continue;
//       } 
//       ans[index] = arma::mean(arma::conv_to<arma::colvec>::from(tube.elem(locs)));
//       index++;
//     }
//   }
//   return ans;
// }
// 
// 
// // [[Rcpp::export]]
// Rcpp::NumericVector cpp_tubemean_num2(const arma::Cube<double>& x, bool na_rm) {
//   int nr = x.n_rows; 
//   int nc = x.n_cols;
//   Rcpp::NumericVector ans(nr * nc);
//   int index = 0;
//   for (int j = 0; j < nc; j++) {
//     for (int i = 0; i < nr; i++) {
//       if (na_rm == true) {
//         arma::vec tube = x.tube(i, j);
//         arma::uvec locs = arma::find_finite(tube);
//         if (locs.is_empty()) {
//           ans[index] = NA_REAL;
//           index++;
//           continue;
//         } 
//         ans[index] = arma::mean(tube.elem(locs));
//         index++;
//       } else {
//         if (x.tube(i, j).has_nonfinite()) {
//           ans[index] = NA_REAL;
//           index++;
//           continue;
//         }
//         arma::vec tube = x.tube(i, j);
//         ans[index] = arma::mean(tube);
//         index++;
//       }
//     }
//   }
//   return ans;
// }



////////////////////////////////////////////////////////////////////////////////
// Evaluation functions
// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubegreater_int(const arma::Cube<int>& x,
                                        bool na_rm,
                                        double value,
                                        double mis_val) {
  int nr = x.n_rows;
  int nc = x.n_cols;
  int tot = x.n_slices;
  Rcpp::NumericVector ans(nr* nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::ivec tube = x.tube(i, j);
      arma::uvec sub = arma::find(tube != mis_val);
      if (na_rm == true) {
        arma::uvec greater = arma::find(tube.elem(sub) > value);
        ans[index++] = greater.n_elem;
      } else if (na_rm == false) {
        if (sub.n_elem != tot) {
          ans[index++] = NA_REAL;
        } else {
          arma::uvec greater = arma::find(tube.elem(sub) > value);
          ans[index++] = greater.n_elem;
        }
        
      }
    }
  }
  return ans;
}
    
// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubegreater_num(const arma::Cube<double>& x,
                                        bool na_rm,
                                        double value) {
  int nr = x.n_rows;
  int nc = x.n_cols;
  int tot = x.n_slices;
  Rcpp::NumericVector ans(nr* nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::vec tube = x.tube(i, j);
      if (na_rm == true) {
        arma::uvec greater = arma::find(tube > value);
        ans[index++] = greater.n_elem;
      } else {
        arma::uvec sub = arma::find_finite(tube);
        if (sub.n_elem != tot) {
          ans[index++] = NA_REAL;
          continue;
        }
        arma::uvec greater = arma::find(tube > value);
        ans[index++] = greater.n_elem;
        
      }
    }
  }
    return ans;
}


// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubeless_int(const arma::Cube<int>& x,
                                        bool na_rm,
                                        double value,
                                        double mis_val) {
  int nr = x.n_rows;
  int nc = x.n_cols;
  int tot = x.n_slices;
  Rcpp::NumericVector ans(nr* nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::ivec tube = x.tube(i, j);
      arma::uvec sub = arma::find(tube != mis_val);
      if (na_rm == true) {
        arma::uvec greater = arma::find(tube.elem(sub) < value);
        ans[index++] = greater.n_elem;
      } else if (na_rm == false) {
        if (sub.n_elem != tot) {
          ans[index++] = NA_REAL;
        } else {
          arma::uvec greater = arma::find(tube.elem(sub) < value);
          ans[index++] = greater.n_elem;
        }
        
      }
    }
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubeless_num(const arma::Cube<double>& x,
                                        bool na_rm,
                                        double value) {
  int nr = x.n_rows;
  int nc = x.n_cols;
  int tot = x.n_slices;
  Rcpp::NumericVector ans(nr* nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::vec tube = x.tube(i, j);
      if (na_rm == true) {
        arma::uvec greater = arma::find(tube < value);
        ans[index++] = greater.n_elem;
      } else {
        arma::uvec sub = arma::find_finite(tube);
        if (sub.n_elem != tot) {
          ans[index++] = NA_REAL;
          continue;
        }
        arma::uvec greater = arma::find(tube < value);
        ans[index++] = greater.n_elem;
        
      }
    }
  }
  return ans;
}


// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubeequal_int(const arma::Cube<int>& x,
                                     bool na_rm,
                                     double value,
                                     double mis_val) {
  int nr = x.n_rows;
  int nc = x.n_cols;
  int tot = x.n_slices;
  Rcpp::NumericVector ans(nr* nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::ivec tube = x.tube(i, j);
      arma::uvec sub = arma::find(tube != mis_val);
      if (na_rm == true) {
        arma::uvec greater = arma::find(tube.elem(sub) == value);
        ans[index++] = greater.n_elem;
      } else if (na_rm == false) {
        if (sub.n_elem != tot) {
          ans[index++] = NA_REAL;
        } else {
          arma::uvec greater = arma::find(tube.elem(sub) == value);
          ans[index++] = greater.n_elem;
        }
        
      }
    }
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_tubeequal_num(const arma::Cube<double>& x,
                                     bool na_rm,
                                     double value) {
  int nr = x.n_rows;
  int nc = x.n_cols;
  int tot = x.n_slices;
  Rcpp::NumericVector ans(nr* nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::vec tube = x.tube(i, j);
      if (na_rm == true) {
        arma::uvec greater = arma::find(tube == value);
        ans[index++] = greater.n_elem;
      } else {
        arma::uvec sub = arma::find_finite(tube);
        if (sub.n_elem != tot) {
          ans[index++] = NA_REAL;
          continue;
        }
        arma::uvec greater = arma::find(tube == value);
        ans[index++] = greater.n_elem;
        
      }
    }
  }
  return ans;
}


// [[Rcpp::export]]
Rcpp::NumericVector cpp_tuberange_int(const arma::Cube<int>& x,
                                      bool na_rm,
                                      double value1,
                                      double value2,
                                      double mis_val) {
  int nr = x.n_rows;
  int nc = x.n_cols;
  int tot = x.n_slices;
  Rcpp::NumericVector ans(nr* nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::ivec tube = x.tube(i, j);
      arma::uvec sub = arma::find(tube != mis_val);
      if (na_rm == true) {
        arma::uvec greater = arma::find((tube.elem(sub) > value1) && (tube.elem(sub) < value2));
        ans[index++] = greater.n_elem;
      } else if (na_rm == false) {
        if (sub.n_elem != tot) {
          ans[index++] = NA_REAL;
        } else {
          arma::uvec greater = arma::find((tube.elem(sub) > value1) && (tube.elem(sub) < value2));
          ans[index++] = greater.n_elem;
        }
        
      }
    }
  }
  return ans;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_tuberange_num(const arma::Cube<double>& x,
                                      bool na_rm,
                                      double value1,
                                      double value2) {
  int nr = x.n_rows;
  int nc = x.n_cols;
  int tot = x.n_slices;
  Rcpp::NumericVector ans(nr* nc);
  int index = 0;
  for (int j = 0; j < nc; j++) {
    for (int i = 0; i < nr; i++) {
      arma::vec tube = x.tube(i, j);
      if (na_rm == true) {
        arma::uvec greater = arma::find((tube > value1) && (tube < value2));
        ans[index++] = greater.n_elem;
      } else {
        arma::uvec sub = arma::find_finite(tube);
        if (sub.n_elem != tot) {
          ans[index++] = NA_REAL;
          continue;
        }
        arma::uvec greater = arma::find((tube > value1) && (tube < value2));
        ans[index++] = greater.n_elem;
        
      }
    }
  }
  return ans;
}