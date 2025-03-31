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
  if(is.integer(x)) {
    cpp_slicemean_int(x, mis_val = mis_val)
  } else {
    cpp_slicemean_num(x, na_rm = na.rm)
  }
}


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
    cpp_slicemax_int(x, mis_val = mis_val)
  } else {
    cpp_slicemax_num(x, na_rm = na.rm)
  }
}


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
    cpp_slicemin_int(x, mis_val = mis_val)
  } else {
    cpp_slicemin_num(x, na_rm = na.rm)
  }
}



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
    cpp_slicemedian_int(x, mis_val = mis_val)
  } else {
    cpp_slicemedian_num(x, na_rm = na.rm)
  }
}



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
                     data = parent.frame(), 
                     na.rm = FALSE, 
                     mis_val = -2147483648) {
  if(is.integer(x) | is.logical(x)) {
    out <- cpp_slicesum_int(x, na_rm = na.rm, mis_val = mis_val)
  } else  if (is.numeric(x)) {
    out <- cpp_slicesum_num(x, na_rm = na.rm)
  } 
  return(out)
} 


