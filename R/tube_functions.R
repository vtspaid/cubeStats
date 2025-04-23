
# tubeMean -------------------
#' Get the mean value of each cross-sectional cell (tube) in a 3d matrix
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of layer/slice maximums.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' tubeMean(small_matrix)
#' @export
tubeMean <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if(is.integer(x)) {
    cpp_tubemean_int(x, na_rm = na.rm, mis_val = mis_val)
  } else {
    cpp_tubemean_num(x, na_rm = na.rm)
  }
}


# tubeMax -------------------------------------
#' Get the mean value of each cross-sectional cell (tube) in a 3d matrix
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of tube maximums.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' tubeMax(small_matrix)
#' @export
tubeMax <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if(is.integer(x)) {
    cpp_tubemax_int(x, na_rm = na.rm, mis_val = mis_val)
  } else {
    cpp_tubemax_num(x, na_rm = na.rm)
  }
}

# tubeMin -------------------------------------
#' Get the mean value of each cross-sectional cell (tube) in a 3d matrix
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of tube minimums.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' tubeMin(small_matrix)
#' @export
tubeMin <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if(is.integer(x)) {
    cpp_tubemin_int(x, na_rm = na.rm, mis_val = mis_val)
  } else {
    cpp_tubemin_num(x, na_rm = na.rm)
  }
}


# tubeSd -------------------------------------
#' Get the mean value of each cross-sectional cell (tube) in a 3d matrix
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of tube maximums.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' tubeSd(small_matrix)
#' @export
tubeSd <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if(is.integer(x)) {
    cpp_tubesd_int(x, na_rm = na.rm, mis_val = mis_val)
  } else {
    cpp_tubesd_num(x, na_rm = na.rm)
  }
}


# tubeSum -------------------------------------
#' Get the mean value of each cross-sectional cell (tube) in a 3d matrix
#' 
#' @param x A 3d matrix/array.
#' @param na.rm True or false, should NAs be removed before calculating the mean.
#' @param mis_val An integer to use as the missing value if the input matrix
#' is an integer type. Argument is ignored if the input array is numeric.
#' @returns A vector of tube maximums.
#' @examples
#' small_matrix <- array(1:625, c(5, 5, 5))
#' tubeSum(small_matrix)
#' @export
tubeSum <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if(is.integer(x)) {
    cpp_tubesum_int(x, na_rm = na.rm, mis_val = mis_val)
  } else {
    cpp_tubesum_num(x, na_rm = na.rm)
  }
}




# tubeEval --------------------------------------------------------
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
#' tubeEval(small_matrix)
#' @export
tubeEval <- function(x,
                      expression = c(">", "<", "==", "within"),
                      value,
                      na.rm = FALSE, 
                      mis_val = -2147483648) {
  if (is.integer(x)) {
    if (expression == ">") {
      out <- cpp_tubegreater_int(x, na_rm = na.rm, value = value, mis_val = mis_val)
    } else if (expression == "<") {
      out <- cpp_tubeless_int(x, na_rm = na.rm, value = value, mis_val = mis_val)
    } else if (expression == "==") {
      out <- cpp_tubeequal_int(x, na_rm = na.rm, value = value, mis_val = mis_val)
    } else if (expression == "within") {
      out <- cpp_tuberange_int(x, na_rm = na.rm, value[1], value[2], mis_val = mis_val)
    } else {
      stop("expression = ", expression, "; when it should be one of '>', '<', '==', or 'within'")
    }
  } else {
    if (expression == ">") {
      out <- cpp_tubegreater_num(x, na_rm = na.rm, value = value)
    } else if (expression == "<") {
      out <- cpp_tubeless_num(x, na_rm = na.rm, value = value)
    } else if (expression == "==") {
      out <- cpp_tubeequal_num(x, na_rm = na.rm, value = value)
    } else if (expression == "within") {
      out <- cpp_tuberange_num(x, na_rm = na.rm, value[1], value[2])
    } else{
      stop("expression = ", expression, "; when it should be one of '>', '<', '==', or 'within'")
    }
  }
  return(out)
} 


