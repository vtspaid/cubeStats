
# create a simple matrix to test ---------------
test_mat <- array(1:12L, c(5, 5, 5))
test_mat_int <- test_mat
mode(test_mat) <- "numeric"
mode(test_mat_int) <- "integer"

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
flat_mat_int <- test_mat_int
dim(flat_mat_int) <- c(25, 5)
flat_mat_num <- test_mat
dim(flat_mat_num) <- c(25, 5)

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

# sliceSd ------------
test_that("sliceSd works", {
  expect_equal(sliceSd(test_mat_int), sapply(1:5, function(x) sd(test_mat_int[, , x])))
  expect_equal(sliceSd(test_mat_na_1_int, na.rm = TRUE), 
               sapply(1:5, function(x) sd(test_mat_na_1_int[, , x], na.rm = TRUE)))
  expect_equal(sliceSd(test_mat_na, na.rm = TRUE), 
               sapply(1:5, function(x) sd(test_mat_na[, , x], na.rm = TRUE)))
  expect_equal(sliceSd(test_mat), sapply(1:5, function(x) sd(test_mat[, , x])))
  expect_equal(sliceSd(test_mat_na_int, na.rm = TRUE), 
               sapply(1:5, function(x) sd(test_mat_na_int[, , x], na.rm = TRUE)))
  expect_equal(sliceSd(test_mat_na_1, na.rm = TRUE), 
               sapply(1:5, function(x) sd(test_mat_na_1[, , x], na.rm = TRUE)))
  
})

# sliceEval --------------
test_that("sliceEval works", {
  expect_equal(sliceEval(test_mat, ">", 5), 
               sapply(1:5, function(x) sum(test_mat[, , x] > 5)))
  expect_equal(sliceEval(test_mat_int, ">", 5), 
               sapply(1:5, function(x) sum(test_mat_int[, , x] > 5)))
  expect_equal(sliceEval(test_mat, "<", 5), 
               sapply(1:5, function(x) sum(test_mat[, , x] < 5)))
  expect_equal(sliceEval(test_mat, "within", c(5, 10)), 
               sapply(1:5, function(x) sum(test_mat[, , x] > 5 & 
                                             test_mat[, , x] < 10)))
  expect_equal(sliceEval(test_mat_int, "==", 5), 
               sapply(1:5, function(x) sum(test_mat_int[, , x] == 5)))
  expect_equal(sliceEval(test_mat, "==", 5), 
               sapply(1:5, function(x) sum(test_mat[, , x] == 5)))
  expect_equal(sliceEval(test_mat_na, "==", 5), 
               sapply(1:5, function(x) sum(test_mat_na[, , x] == 5)))
  expect_equal(sliceEval(test_mat_na_int, ">", 5), 
               sapply(1:5, function(x) sum(test_mat_na_int[, , x] > 5)))
})

# sliceAllFinite ----------------
test_that("sliceAllFinite works", {
  expect_equal(sliceAllFinite(test_mat), 
               ifelse(sapply(1:5, function(x) sum(is.na(test_mat[, , x]))) == 0,
                      1, 0))
})

# sliceAllNA ----------------
test_that("sliceAllNA works", {
  expect_equal(sliceAllNA(test_mat_na_1), 
               ifelse(sapply(1:5, function(x) sum(is.na(test_mat_na_1[, , x]))) == 25,
                      1, 0))
})


# TubeMean ------------
test_that("tubeMean works", {
  expect_equal(tubeMean(test_mat_int), rowMeans2(flat_mat_int))
  expect_equal(tubeMean(test_mat), rowMeans2(flat_mat_num))
  expect_equal(tubeMean(test_mat_na_int), rowMeans2(flat_mat_na_int))
  expect_equal(tubeMean(test_mat_na_int, na.rm = TRUE), rowMeans2(flat_mat_na_int, na.rm = TRUE))
  expect_equal(tubeMean(test_mat_na), rowMeans2(flat_mat_na))
  expect_equal(tubeMean(test_mat_na, na.rm = TRUE), rowMeans2(flat_mat_na, na.rm = TRUE))
})

# TubeMax ------------
test_that("tubeMax works", {
  expect_equal(tubeMax(test_mat_int), rowMaxs(flat_mat_int))
  expect_equal(tubeMax(test_mat), rowMaxs(flat_mat_num))
  expect_equal(tubeMax(test_mat_na_int), rowMaxs(flat_mat_na_int))
  expect_equal(tubeMax(test_mat_na_int, na.rm = TRUE), rowMaxs(flat_mat_na_int, na.rm = TRUE))
  expect_equal(tubeMax(test_mat_na), rowMaxs(flat_mat_na))
  expect_equal(tubeMax(test_mat_na, na.rm = TRUE), rowMaxs(flat_mat_na, na.rm = TRUE))
})

# TubeMin ------------
test_that("tubeMin works", {
  expect_equal(tubeMin(test_mat_int), rowMins(flat_mat_int))
  expect_equal(tubeMin(test_mat), rowMins(flat_mat_num))
  expect_equal(tubeMin(test_mat_na_int), rowMins(flat_mat_na_int))
  expect_equal(tubeMin(test_mat_na_int, na.rm = TRUE), rowMins(flat_mat_na_int, na.rm = TRUE))
  expect_equal(tubeMin(test_mat_na), rowMins(flat_mat_na))
  expect_equal(tubeMin(test_mat_na, na.rm = TRUE), rowMins(flat_mat_na, na.rm = TRUE))
})


# TubeSD ------------
test_that("tubeSd works", {
  expect_equal(tubeSd(test_mat_int), rowSds(flat_mat_int))
  expect_equal(tubeSd(test_mat), rowSds(flat_mat_num))
  expect_equal(tubeSd(test_mat_na_int), rowSds(flat_mat_na_int))
  expect_equal(tubeSd(test_mat_na_int, na.rm = TRUE), rowSds(flat_mat_na_int, na.rm = TRUE))
  expect_equal(tubeSd(test_mat_na), rowSds(flat_mat_na))
  expect_equal(tubeSd(test_mat_na, na.rm = TRUE), rowSds(flat_mat_na, na.rm = TRUE))
})


# TubeSum ------------
test_that("tubeSum works", {
  expect_equal(tubeSum(test_mat_int), rowSums(flat_mat_int))
  expect_equal(tubeSum(test_mat), rowSums(flat_mat_num))
  expect_equal(tubeSum(test_mat_na_int), rowSums(flat_mat_na_int))
  expect_equal(tubeSum(test_mat_na_int, na.rm = TRUE), rowSums(flat_mat_na_int, na.rm = TRUE))
  expect_equal(tubeSum(test_mat_na), rowSums(flat_mat_na))
  expect_equal(tubeSum(test_mat_na, na.rm = TRUE), rowSums(flat_mat_na, na.rm = TRUE))
})

# tubeEval ------------------------
test_that("tubeEval works", {
  expect_equal(tubeEval(test_mat, ">", 5), 
               rowSums2(flat_mat_num > 5))
  expect_equal(tubeEval(test_mat_int, ">", 5), 
               rowSums2(flat_mat_int > 5))
  expect_equal(tubeEval(test_mat, "<", 5), 
               rowSums2(flat_mat_num < 5))
  expect_equal(tubeEval(test_mat, "within", c(5, 10)), 
               rowSums2(flat_mat_num > 5 & flat_mat_num < 10))
  expect_equal(tubeEval(test_mat_int, "==", 5), 
               rowSums2(flat_mat_int == 5))
  expect_equal(tubeEval(test_mat, "==", 5), 
               rowSums2(flat_mat_num == 5))
  expect_equal(tubeEval(test_mat_na, "==", 5), 
               rowSums2(flat_mat_na == 5))
  expect_equal(tubeEval(test_mat_na_int, ">", 5), 
               rowSums2(flat_mat_na_int > 5))
})

# tubeAllNA ---------------
test_that("tubeAllNA works", {
  expect_equal(tubeAllNA(test_mat_na), 
               rowAlls(flat_mat_na, value = NA))
  expect_equal(tubeAllNA(test_mat_na_int), 
               rowAlls(flat_mat_na_int, value = NA))
})

# tubeAllFinite ---------------
test_that("tubeAllFinite works", {
  expect_equal(tubeAllFinite(test_mat_na), 
               !rowAnyNAs(flat_mat_na))
  expect_equal(tubeAllFinite(test_mat_na_int), 
               !rowAnyNAs(flat_mat_na_int))
})
