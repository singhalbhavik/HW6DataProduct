
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparseNumeric

<!-- badges: start -->

[![R-CMD-check](https://github.com/singhalbhavik/HW6DataProduct/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/singhalbhavik/HW6DataProduct/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`sparseNumeric` provides an S4 class (`sparse_numeric`) for representing
sparse numeric vectors along with efficient operations such as addition,
subtraction, elementwise multiplication, cross-products, mean, norm, and
standardization. The package is useful when working with numeric vectors
that contain many zeros, allowing you to store only the non-zero
entries.

## Installation

You can install the development version of **sparseNumeric** from
GitHub:

``` r
# install.packages("pak")
pak::pak("singhalbhavik/HW6DataProduct")
```

## Example

Below is a basic example showing how to construct sparse vectors and
carry out arithmetic operations:

``` r
library(sparseNumeric)

# Create two numeric vectors
x <- c(0, 5, 0, 3)
y <- c(2, 0, 0, 7)

# Convert to sparse_numeric
sx <- as(x, "sparse_numeric")
sy <- as(y, "sparse_numeric")

# Addition
sparse_add(sx, sy)
#> An object of class sparse_numeric ...

# Elementwise multiplication
sparse_mult(sx, sy)

# Compute the Euclidean norm
norm(sx)

# Standardize the vector
standardize(sx)
```

## What the package provides

- A compact S4 representation of sparse numeric vectors  
- Vectorized arithmetic: `+`, `-`, `*`  
- Generic functions:
  - `sparse_add()`
  - `sparse_sub()`
  - `sparse_mult()`
  - `sparse_crossprod()`
  - `nnzero()`
  - `standardize()`
  - `norm()`  
- A custom `plot()` method for visualizing overlapping non-zero
  entries  
- Full documentation and examples on the package website (built with
  pkgdown)

## Development

This package was built for the UT Austin SDS 375 Data Product course and
includes:

- Test coverage using **testthat**
- Automated documentation using **roxygen2**
- A pkgdown website
- GitHub Actions CI (R CMD check)
