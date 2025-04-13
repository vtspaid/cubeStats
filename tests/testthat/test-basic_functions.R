
# create a simple matrix to test ---------------
test_mat <- array(1:12, c(5, 5, 5))
test_mat_int <- test_mat
mode(test_mat) <- "numeric"

# Create a matrix where the first layer is all NA's
test_mat_na_1 <- test_mat      
test_mat_na_1[, , 1] <- NA
test_mat_na_1_int <- test_mat_na_1
mode(test_mat_na_1_int) <- "integer"

# Test that random NA's are removed when na.rm = TRUE
test_mat_na <- test_mat
test_mat_na[c(1, 4), c(1, 5), c(3, 4, 5)] <- NA
test_mat_na_int <- test_mat_na
mode(test_mat_na_int) <- "integer"
flat_mat_na <- test_mat_na
dim(flat_mat_na) <- c(25, 5) 
flat_mat_na_int <- test_mat_na_int
dim(flat_mat_na_int) <- c(25, 5) 

# SliceMean ----------------------------------------------
test_that("sliceMean works", {
  expect_equal(sliceMean(test_mat), c(6.28, 6.32, 6.36, 6.40, 6.44))
  expect_equal(sliceMean(test_mat_na_1), c(NA, 6.32, 6.36, 6.40, 6.44))
  expect_equal(sliceMean(test_mat_na), c(6.28, 6.32, NA, NA, NA))
  expect_equal(sliceMean(test_mat_na, na.rm = TRUE), colMeans(flat_mat_na, na.rm = TRUE))
  
})

test_that("sliceMean works on integers", {
  expect_equal(sliceMean(test_mat_int), c(6.28, 6.32, 6.36, 6.40, 6.44))
  expect_equal(sliceMean(test_mat_na_1_int), c(NA, 6.32, 6.36, 6.40, 6.44))
  expect_equal(sliceMean(test_mat_na_int), c(6.28, 6.32, NA, NA, NA))
  expect_equal(sliceMean(test_mat_na_int, na.rm = TRUE), colMeans(flat_mat_na_int, na.rm = TRUE))
  
})

# sliceMin ---------------------------------------
test_that("sliceMin works", {
  expect_equal(sliceMin(test_mat_int), sapply(1:5, function(x) min(test_mat_int[, , x])))
  expect_equal(sliceMin(test_mat_na_1_int, na.rm = TRUE), 
               sapply(1:5, function(x) min(test_mat_na_1_int[, , x], na.rm = TRUE)))
  expect_equal(sliceMin(test_mat_na, na.rm = TRUE), 
               sapply(1:5, function(x) min(test_mat_na[, , x], na.rm = TRUE)))
  expect_equal(sliceMin(test_mat), sapply(1:5, function(x) min(test_mat[, , x])))
  expect_equal(sliceMin(test_mat_na_int, na.rm = TRUE), 
               sapply(1:5, function(x) min(test_mat_na_int[, , x], na.rm = TRUE)))
  expect_equal(sliceMin(test_mat_na_1, na.rm = TRUE), 
               sapply(1:5, function(x) min(test_mat_na_1[, , x], na.rm = TRUE)))
  
})

# sliceMax -------------------------------------
test_that("sliceMax works", {
  expect_equal(sliceMax(test_mat_int), sapply(1:5, function(x) max(test_mat_int[, , x])))
  expect_equal(sliceMax(test_mat_na_1_int, na.rm = TRUE), 
               sapply(1:5, function(x) max(test_mat_na_1_int[, , x], na.rm = TRUE)))
  expect_equal(sliceMax(test_mat_na, na.rm = TRUE), 
               sapply(1:5, function(x) max(test_mat_na[, , x], na.rm = TRUE)))
  expect_equal(sliceMax(test_mat), sapply(1:5, function(x) max(test_mat[, , x])))
  expect_equal(sliceMax(test_mat_na_int, na.rm = TRUE), 
               sapply(1:5, function(x) max(test_mat_na_int[, , x], na.rm = TRUE)))
  expect_equal(sliceMax(test_mat_na_1, na.rm = TRUE), 
               sapply(1:5, function(x) max(test_mat_na_1[, , x], na.rm = TRUE)))
  
})

# sliceMedian --------
test_that("sliceMedian works", {
  expect_equal(sliceMedian(test_mat_int), sapply(1:5, function(x) median(test_mat_int[, , x])))
  expect_equal(sliceMedian(test_mat_na_1_int, na.rm = TRUE), 
               sapply(1:5, function(x) median(test_mat_na_1_int[, , x], na.rm = TRUE)))
  expect_equal(sliceMedian(test_mat_na, na.rm = TRUE), 
               sapply(1:5, function(x) median(test_mat_na[, , x], na.rm = TRUE)))
  expect_equal(sliceMedian(test_mat), sapply(1:5, function(x) median(test_mat[, , x])))
  expect_equal(sliceMedian(test_mat_na_int, na.rm = TRUE), 
               sapply(1:5, function(x) median(test_mat_na_int[, , x], na.rm = TRUE)))
  expect_equal(sliceMedian(test_mat_na_1, na.rm = TRUE), 
               sapply(1:5, function(x) median(test_mat_na_1[, , x], na.rm = TRUE)))
  
})

# sliceSum --------------
test_that("sliceSum works", {
  expect_equal(sliceSum(test_mat_int), sapply(1:5, function(x) sum(test_mat_int[, , x])))
  expect_equal(sliceSum(test_mat_na_1_int, na.rm = TRUE), 
               sapply(1:5, function(x) sum(test_mat_na_1_int[, , x], na.rm = TRUE)))
  expect_equal(sliceSum(test_mat_na, na.rm = TRUE), 
               sapply(1:5, function(x) sum(test_mat_na[, , x], na.rm = TRUE)))
  expect_equal(sliceSum(test_mat), sapply(1:5, function(x) sum(test_mat[, , x])))
  expect_equal(sliceSum(test_mat_na_int, na.rm = TRUE), 
               sapply(1:5, function(x) sum(test_mat_na_int[, , x], na.rm = TRUE)))
  expect_equal(sliceSum(test_mat_na_1, na.rm = TRUE), 
               sapply(1:5, function(x) sum(test_mat_na_1[, , x], na.rm = TRUE)))
  
})
