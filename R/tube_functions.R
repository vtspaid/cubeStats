
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
