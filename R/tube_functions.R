

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
    cpp_tubemean_int(x, mis_val = mis_val)
  } else {
    cpp_tubemean_num(x, na_rm = na.rm)
  }
}