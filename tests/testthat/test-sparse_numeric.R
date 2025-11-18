test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", { 
  expect_true({
    x <- new("sparse_numeric",
             value  = c(1, 2, 0, 1),
             pos    = as.integer(c(1, 2, 3, 5)),
             length = as.integer(5))
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value  = c(1, 2, 0, 1),
             pos    = as.integer(c(1, 2, 3, 5)),
             length = as.integer(5))
    x@length <- as.integer(2)
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", {
  expect_true(isGeneric("sparse_add"))
})

test_that("sparse mult generic", {
  expect_true(isGeneric("sparse_mult"))
})

test_that("sparse sub generic", {
  expect_true(isGeneric("sparse_sub"))
})

test_that("sparse crossprod generic", {
  expect_true(isGeneric("sparse_crossprod"))
})


test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

#### ------------------------------------------------------------------------
#### Minimal extra HW6 tests to hit new code paths
#### ------------------------------------------------------------------------

test_that("nnzero counts non-zero entries", {
  x <- as(c(0, 1, 0, 2, 3), "sparse_numeric")
  expect_equal(nnzero(x), 3L)
  
  z <- as(rep(0, 5), "sparse_numeric")
  expect_equal(nnzero(z), 0L)
})

test_that("sparse_mult and sparse_crossprod agree with dense operations", {
  xd <- c(0, 1, 0, 2, 3)
  yd <- c(1, 0, 2, 0, 3)
  x  <- as(xd, "sparse_numeric")
  y  <- as(yd, "sparse_numeric")
  
  expect_equal(as(sparse_mult(x, y), "numeric"), xd * yd)
  expect_equal(sparse_crossprod(x, y), sum(xd * yd))
})

test_that("mean for sparse_numeric includes zeros", {
  xd <- c(0, 1, 0, 2, 3)
  x  <- as(xd, "sparse_numeric")
  expect_equal(mean(x), mean(xd))
  
  empty <- new("sparse_numeric",
               value  = numeric(0),
               pos    = integer(0),
               length = as.integer(0))
  expect_true(is.na(mean(empty)))
})

test_that("norm for sparse_numeric gives Euclidean norm", {
  x <- as(c(3, 4, 0), "sparse_numeric")
  expect_equal(norm(x), 5)
  
  z <- as(rep(0, 4), "sparse_numeric")
  expect_equal(norm(z), 0)
})

test_that("standardize matches dense scale for a non-constant vector", {
  xd <- c(0, 1, 0, 2, 3)
  x  <- as(xd, "sparse_numeric")
  
  x_std <- standardize(x)
  expect_equal(
    as(x_std, "numeric"),
    as.numeric(scale(xd)),
    tolerance = 1e-8
  )
})

test_that("standardize returns zeros for a constant vector", {
  const <- rep(5, 4)
  sc    <- as(const, "sparse_numeric")
  sc_std <- standardize(sc)
  
  expect_equal(as(sc_std, "numeric"), rep(0, 4))
})

test_that("plot and show run without error", {
  x <- as(c(0, 1, 0, 2, 3), "sparse_numeric")
  y <- as(c(1, 0, 2, 0, 3), "sparse_numeric")
  
  expect_no_error(show(x))
  expect_no_error(plot(x, y))
})

test_that("sparse_mult with disjoint non-zeros returns all zeros", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 1, 0), "sparse_numeric")
  
  m <- sparse_mult(x, y)
  
  expect_s4_class(m, "sparse_numeric")
  expect_equal(nnzero(m), 0L)
  expect_equal(as(m, "numeric"), c(0, 0, 0))
})

test_that("standardize handles empty sparse_numeric object", {
  empty <- new("sparse_numeric",
               value  = numeric(0),
               pos    = integer(0),
               length = as.integer(0))
  
  out <- standardize(empty)
  
  expect_s4_class(out, "sparse_numeric")
  expect_equal(out@length, as.integer(0))
  expect_equal(nnzero(out), 0L)
})

test_that("sparse_mult errors for different lengths", {
  x <- as(rep(1, 5), "sparse_numeric")
  y <- as(rep(1, 4), "sparse_numeric")
  
  expect_error(sparse_mult(x, y), "must have the same length")
})



test_that("plot warns when there is no overlap in non-zero entries", {
  a <- as(c(1, 0, 0), "sparse_numeric")
  b <- as(c(0, 0, 1), "sparse_numeric")
  
  expect_warning(plot(a, b), "no overlapping non-zero entries")
})


test_that("arithmetic operators +, -, * work for sparse_numeric", {
  xd <- c(0, 1, 0, 2, 3)
  yd <- c(1, 0, 2, 0, 3)
  x  <- as(xd, "sparse_numeric")
  y  <- as(yd, "sparse_numeric")
  
  # + operator
  sum_op  <- as(x + y, "numeric")
  sum_ref <- xd + yd
  expect_equal(sum_op, sum_ref)
  
  # - operator
  diff_op  <- as(x - y, "numeric")
  diff_ref <- xd - yd
  expect_equal(diff_op, diff_ref)
  
  # * operator (elementwise)
  prod_op  <- as(x * y, "numeric")
  prod_ref <- xd * yd
  expect_equal(prod_op, prod_ref)
})

