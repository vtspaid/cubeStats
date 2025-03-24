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
#' lyrMean(small_matrix)
#' @export
lyrMean <- function(x, na.rm = FALSE, mis_val = -2147483648) {
  if(is.integer(x)) {
    cpp_cubemean_int(x, mis_val = mis_val)
  } else {
    cpp_cubemean_num(x, na_rm = na.rm)
  }
}
