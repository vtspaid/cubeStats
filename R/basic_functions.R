################################################################################
# Wrappers for our Rcpp functions to control which functions are used


#' Get the mean value of each layer in a 3d cube
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer means.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceMean(small_matrix)
#' @export
sliceMean <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if (is.integer(x)) {
    cpp_slicemean_int(x, na_rm = na.rm, mis_val = mis_val)
  } else {
    cpp_slicemean_num(x, na_rm = na.rm)
  }
}

# SliceMax ---------------------------------------------------------------------
#' Get the max value of each layer in a 3d cube
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer/slice maximums.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceMax(small_matrix)
#' @export
sliceMax <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if(is.integer(x)) {
    cpp_slicemax_int(x, na_rm = na.rm, mis_val = mis_val)
  } else {
    cpp_slicemax_num(x, na_rm = na.rm)
  }
}

# sliceMin --------------------------------------
#' Get the min value of each layer in a 3d cube
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer/slice minimums.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceMin(small_matrix)
#' @export
sliceMin <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if(is.integer(x)) {
    cpp_slicemin_int(x, na_rm = na.rm, mis_val = mis_val)
  } else {
    cpp_slicemin_num(x, na_rm = na.rm)
  }
}


# sliceMedian ----------------------------------------------
#' Get the median value of each layer in a 3d cube
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer/slice maximums.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceMedian(small_matrix)
#' @export
sliceMedian <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if(is.integer(x)) {
    cpp_slicemedian_int(x, na_rm = na.rm, mis_val = mis_val)
  } else {
    cpp_slicemedian_num(x, na_rm = na.rm)
  }
}


# sliceSum -------------------------
#' Get the sum of each layer in a 3d cube
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer/slice sums.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceSum(small_matrix)
#' @export
sliceSum <- function(x,
                     na.rm = FALSE, 
                     mis_val = -2147483648) {
  if(is.integer(x) | is.logical(x)) {
    out <- cpp_slicesum_int(x, na_rm = na.rm, mis_val = mis_val)
  } else  if (is.numeric(x)) {
    out <- cpp_slicesum_num(x, na_rm = na.rm)
  } 
  return(out)
} 

# sliceSd -------------------------
#' Get the standard deviation of each layer in a 3d cube
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer/slice standard deviations.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceSd(small_matrix)
#' @export
sliceSd <- function(x,
                    na.rm = FALSE, 
                    mis_val = -2147483648) {
  if(is.integer(x) | is.logical(x)) {
    out <- cpp_slicesd_int(x, na_rm = na.rm, mis_val = mis_val)
  } else  if (is.numeric(x)) {
    out <- cpp_slicesd_num(x, na_rm = na.rm)
  } 
  return(out)
} 

# sliceEval --------------------------------------------------------
#' Evaluate simple expressions
#' 
#' @param x A 3d matrix/array.
#' @param expression Character: one of c(">", "<", "==", "within").
#' @param value Numeric Vector of length 1 in most cases or length 2 when 
#' `expression = "within"`
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer/slice sums based on evaluation.
#' 
#' @details
#' Within uses x > value[1] && x < value[2]
#' 
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceEval(small_matrix)
#' @export
sliceEval <- function(x,
                      expression = c(">", "<", "==", "within"),
                      value,
                      na.rm = FALSE, 
                      mis_val = -2147483648) {
  if (is.integer(x)) {
    if (expression == ">") {
      out <- cpp_slicegreater_int(x, na_rm = na.rm, value = value, mis_val = mis_val)
    } else if (expression == "<") {
      out <- cpp_sliceless_int(x, na_rm = na.rm, value = value, mis_val = mis_val)
    } else if (expression == "==") {
      out <- cpp_sliceequal_int(x, na_rm = na.rm, value = value, mis_val = mis_val)
    } else if (expression == "within") {
      out <- cpp_slicerange_int(x, na_rm = na.rm, value[1], value[2], mis_val = mis_val)
    } else {
      stop("expression = ", expression, "; when it should be one of '>', '<', '==', or 'within'")
    }
  } else {
    if (expression == ">") {
      out <- cpp_slicegreater_num(x, na_rm = na.rm, value = value)
    } else if (expression == "<") {
      out <- cpp_sliceless_num(x, na_rm = na.rm, value = value)
    } else if (expression == "==") {
      out <- cpp_sliceequal_num(x, na_rm = na.rm, value = value)
    } else if (expression == "within") {
      out <- cpp_slicerange_num(x, na_rm = na.rm, value[1], value[2])
    } else{
      stop("expression = ", expression, "; when it should be one of '>', '<', '==', or 'within'")
    }
  }
  return(out)
} 



# sliceFinite ----------------------
#' Check if there are any Finite values in each slice
#' 
#' @param x A 3d matrix/array.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer/slice standard deviations.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceAllFinite(small_matrix)
#' @export
sliceAllFinite <- function(x,
                           mis_val = -2147483648) {
  if(is.integer(x) | is.logical(x)) {
    out <- cpp_slicefinite_int(x, mis_val = mis_val)
  } else  if (is.numeric(x)) {
    out <- cpp_slicefinite_num(x)
  } 
  return(out)
} 


# sliceEval --------------------------------------------------------
#' Evaluate simple expressions
#' 
#' @param x A 3d matrix/array.
#' @param expression Character: one of c(">", "<", "==", "within").
#' @param value Numeric Vector of length 1 in most cases or length 2 when 
#' `expression = "within"`
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer/slice sums based on evaluation.
#' 
#' @details
#' Within uses x > value[1] && x < value[2]
#' 
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceEval(small_matrix)
#' @export
sliceEval <- function(x,
                      expression = c(">", "<", "==", "within"),
                      value,
                      na.rm = FALSE, 
                      mis_val = -2147483648) {
  if (is.integer(x)) {
    if (expression == ">") {
      out <- cpp_slicegreater_int(x, na_rm = na.rm, value = value, mis_val = mis_val)
    } else if (expression == "<") {
      out <- cpp_sliceless_int(x, na_rm = na.rm, value = value, mis_val = mis_val)
    } else if (expression == "==") {
      out <- cpp_sliceequal_int(x, na_rm = na.rm, value = value, mis_val = mis_val)
    } else if (expression == "within") {
      out <- cpp_slicerange_int(x, na_rm = na.rm, value[1], value[2], mis_val = mis_val)
    } else {
      stop("expression = ", expression, "; when it should be one of '>', '<', '==', or 'within'")
    }
  } else {
    if (expression == ">") {
      out <- cpp_slicegreater_num(x, na_rm = na.rm, value = value)
    } else if (expression == "<") {
      out <- cpp_sliceless_num(x, na_rm = na.rm, value = value)
    } else if (expression == "==") {
      out <- cpp_sliceequal_num(x, na_rm = na.rm, value = value)
    } else if (expression == "within") {
      out <- cpp_slicerange_num(x, na_rm = na.rm, value[1], value[2])
    } else{
      stop("expression = ", expression, "; when it should be one of '>', '<', '==', or 'within'")
    }
  }
  return(out)
} 



# sliceAllFinite ----------------------
#' Check if there are any Finite values in each slice
#' 
#' @param x A 3d matrix/array.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of 1 and 0s. 1s indicates all values are Finite
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceAllFinite(small_matrix)
#' @export
sliceAllFinite <- function(x,
                           mis_val = -2147483648) {
  if(is.integer(x) | is.logical(x)) {
    out <- cpp_slicefinite_int(x, mis_val = mis_val)
  } else  if (is.numeric(x)) {
    out <- cpp_slicefinite_num(x)
  } 
  return(out)
} 

# sliceAllNA ----------------------
#' Check if there are any Finite values in each slice
#' 
#' @param x A 3d matrix/array.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of 1 and 0s. 1 indicates all values are NA
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' sliceAllNA(small_matrix)
#' @export
sliceAllNA <- function(x,
                       mis_val = -2147483648) {
  if(is.integer(x) | is.logical(x)) {
    out <- cpp_slicena_int(x, mis_val = mis_val)
  } else  if (is.numeric(x)) {
    out <- cpp_slicena_num(x)
  } 
  return(out)
} 