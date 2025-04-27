
# cubeStats: Functions for performing basic calculations on slices and tubes of 3d matrices

## Introduction
cubeStats aims to provided fast and efficient functions for calculating statistics
across slices/layers and tubes of a 3d matrix or cube. cubeStats also allows for
the use of missing values in integer matrices. e.g. treating -9999 as NA. The
underlying code to these functions was written in c++ using armadillo and the
Rcpp package.

## Current status
This package is currently under development and not yet available on CRAN. I hope
to have to have it on CRAN by 2026.

## Contribute
Anyone is free to contribute to this project. If you are aware of a similar project
that already exists or a better way of performing these calculations please reach out to me. 

## Installation
To install use `remotes::install_github("https://github.com/vtspaid/cubeStats")`

## Examples
Create an integer matrix ranging from 1 to 100, with -9999 as a missing value.
100 slices, and each slice is 1000 rows and 50 columns
```r
 x <- array(sample(c(-9999, 1:100),, 500000), c(1000, 50, 100))
 mode(x) <- "integer"
```

Find the mean value for each slice and each tube
```r
lyr_means <- sliceMean(x, na.rm = TRUE, mis_val = -9999)
tube_means <- tubeMean(x, na.rm = TRUE, mis_val = -9999)
```

## Notes
mis_val only works for integer matrices. Missing values can not be used for
numeric matrices, missing values of numeric matrices must be represented with
NA'. If an integer array has NA's then no missing value needs to be supplied.