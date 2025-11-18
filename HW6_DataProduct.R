#' sparseNumeric: Sparse numeric vector S4 class and utilities
#'
#' The `sparseNumeric` package provides an S4 class
#' `sparse_numeric` for representing sparse numeric vectors, along
#' with operations such as addition, subtraction, elementwise
#' multiplication, cross-products, and utilities like mean, norm, and
#' standardization that can be applied to sparse vectors.
#'
#' @keywords internal
"_PACKAGE"

library(methods)

#' Sparse numeric vector S4 class
#'
#' The \code{sparse_numeric} class stores a numeric vector in sparse
#' format using three slots:
#' \describe{
#'   \item{value}{Numeric vector of non-zero entries.}
#'   \item{pos}{Integer vector of positions (1-based) of non-zero entries.}
#'   \item{length}{Single non-negative integer giving the total length of
#'   the dense vector.}
#' }
#'
#' @slot value Numeric vector of non-zero entries.
#' @slot pos Integer vector of 1-based positions of non-zero entries.
#' @slot length Single non-negative integer giving the full vector length.
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric", 
    pos = "integer",  
    length = "integer"   
  )
)

setValidity("sparse_numeric", function(object) {
  errs = character()
  
  if (length(object@length) != 1L || object@length < 0L) {
    errs = c(errs, "length need to be a single non-negative integer")
  }
  
  if (length(object@value) != length(object@pos)) {
    errs = c(errs, "value and pos need to have same length")
  }
  
  if (length(object@pos) > 0L) {
    if (any(object@pos < 1L) || any(object@pos > object@length)) {
      errs = c(errs, "pos must be in between 1 and length")
    }
  }
  
  if (length(errs) == 0L) TRUE else errs
})


setAs("numeric", "sparse_numeric", function(from) {
  idx = which(from != 0)
  if (length(idx) == 0L) {
    new("sparse_numeric", value = numeric(0), pos = integer(0), length = as.integer(length(from)))
  } else {
    new("sparse_numeric", value = from[idx], pos = as.integer(idx), length = as.integer(length(from)))
  }
})


setAs("sparse_numeric", "numeric", function(from) {
  out = numeric(from@length)
  if (length(from@pos) > 0L) {
    out[from@pos] = from@value
  }
  out
})

#' Show method for sparse_numeric
#'
#' Displays a compact representation of a \code{sparse_numeric} object.
#'
#' @param object A \code{sparse_numeric} object.
#'
#' @export
setMethod("show", "sparse_numeric", function(object) {
  cat("An object of class sparse_numeric \n")
  cat(" Length:", object@length, "\n")
  cat(" Non-zero entries:", length(object@value), "\n")
  if (length(object@value) > 0L) {
    cat(" pos value\n")
    for (k in seq_along(object@value)) {
      cat(sprintf("%4d %5g\n", object@pos[k], object@value[k]))
    }
  }
})

#' Add two sparse_numeric vectors
#'
#' Computes the elementwise sum of two \code{sparse_numeric} vectors that
#' have the same length.
#'
#' @param x,y Objects of class \code{sparse_numeric}.
#'
#' @return A \code{sparse_numeric} object representing \code{x + y}.
#' @export
setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))

#' Multiply two sparse_numeric vectors elementwise
#'
#' Computes the elementwise product of two \code{sparse_numeric} vectors
#' that have the same length.
#'
#' @param x,y Objects of class \code{sparse_numeric}.
#'
#' @return A \code{sparse_numeric} object representing \code{x * y}.
#' @export
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))

#' Subtract two sparse_numeric vectors
#'
#' Computes the elementwise difference of two \code{sparse_numeric}
#' vectors that have the same length.
#'
#' @param x,y Objects of class \code{sparse_numeric}.
#'
#' @return A \code{sparse_numeric} object representing \code{x - y}.
#' @export
setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))

#' Sparse cross-product
#'
#' Computes the inner product of two \code{sparse_numeric} vectors.
#'
#' @param x,y Objects of class \code{sparse_numeric}.
#'
#' @return A numeric scalar giving the cross-product.
#' @export
setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))

#' Number of non-zero entries
#'
#' Returns the number of non-zero entries in a \code{sparse_numeric}
#' vector.
#'
#' @param x A \code{sparse_numeric} object.
#'
#' @return An integer count of non-zero entries.
#' @export
setGeneric("nnzero", function(x) standardGeneric("nnzero"))


#' Norm of a sparse_numeric vector
#'
#' Computes the Euclidean norm of a \code{sparse_numeric} vector, defined
#' as the square root of the sum of squared entries.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return A numeric scalar giving the Euclidean norm of \code{x}.
#' @name norm_sparse_numeric
#' @export
if (!isGeneric("norm")) {
  setGeneric("norm", function(x, ...) standardGeneric("norm"))
}


#' Standardize a sparse_numeric vector
#'
#' Centers and scales a \code{sparse_numeric} vector by subtracting the
#' mean and dividing by the standard deviation, where both statistics are
#' computed over the full vector including implicit zeros.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return A \code{sparse_numeric} object representing the standardized
#' vector.
#' @name standardize_sparse_numeric
#' @export
setGeneric(
  "standardize",
  function(x, ...) standardGeneric("standardize")
)


###ADDDDDDD COMMENTS HERE
.check_same_length = function(x, y) {
  if (x@length != y@length) {
    stop("sparse_numeric objects must have the same length")
  }
}


#' @rdname sparse_add
#' @export
setMethod(
  "sparse_add",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    .check_same_length(x, y)
    
    pos_x = x@pos; val_x = x@value
    pos_y = y@pos; val_y = y@value
    
    i = j = 1L
    len_x = length(pos_x)
    len_y = length(pos_y)
    
    res_pos = integer(0)
    res_val = numeric(0)
    
    while (i <= len_x && j <= len_y) {
      if (pos_x[i] == pos_y[j]) {
        s = val_x[i] + val_y[j]
        if (s != 0) {
          res_pos = c(res_pos, pos_x[i])
          res_val = c(res_val, s)
        }
        i = i + 1L
        j = j + 1L
      } 
      else if (pos_x[i] < pos_y[j]) {
        res_pos = c(res_pos, pos_x[i])
        res_val = c(res_val, val_x[i])
        i = i + 1L
      } 
      else {
        res_pos = c(res_pos, pos_y[j])
        res_val = c(res_val, val_y[j])
        j = j + 1L
      }
    }
    
    if (i <= len_x) {
      res_pos = c(res_pos, pos_x[i:len_x])
      res_val = c(res_val, val_x[i:len_x])
    }
    if (j <= len_y) {
      res_pos = c(res_pos, pos_y[j:len_y])
      res_val = c(res_val, val_y[j:len_y])
    }
    
    new("sparse_numeric", value = res_val, pos = as.integer(res_pos), length = x@length)
  }
)


#' @rdname sparse_sub
#' @export
setMethod(
  "sparse_sub",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    .check_same_length(x, y)
    y_neg = new("sparse_numeric", value = -y@value, pos = y@pos, length = y@length)
    sparse_add(x, y_neg)
  }
)


#' @rdname sparse_mult
#' @export
setMethod(
  "sparse_mult",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    .check_same_length(x, y)
    common_pos = intersect(x@pos, y@pos)
    if (length(common_pos) == 0L) {
      return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
    }
    
    vx = x@value[match(common_pos, x@pos)]
    vy = y@value[match(common_pos, y@pos)]
    v_prod = vx * vy
    keep = which(v_prod != 0)
    
    new("sparse_numeric", value = v_prod[keep], pos = as.integer(common_pos[keep]), length = x@length)
  }
)

#' @rdname sparse_crossprod
#' @export
setMethod(
  "sparse_crossprod",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    .check_same_length(x, y)
    
    common_pos = intersect(x@pos, y@pos)
    if (length(common_pos) == 0L) return(0)
    
    vx = x@value[match(common_pos, x@pos)]
    vy = y@value[match(common_pos, y@pos)]
    sum(vx * vy)
  }
)

#' @rdname nnzero
#' @export
setMethod("nnzero", signature(x = "sparse_numeric"), function(x) length(x@value))

setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))

setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))

setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))

if (!isGeneric("plot")) {
  setGeneric("plot", function(x, y) standardGeneric("plot"))
}

#' Plot overlapping non-zero entries of two sparse_numeric vectors
#'
#' Plots the values of two \code{sparse_numeric} vectors at positions
#' where they both have non-zero entries.
#'
#' @param x,y Objects of class \code{sparse_numeric}.
#' @param ... Additional arguments passed on to \code{plot()}.
#'
#' @return Invisibly returns \code{NULL}.
#' @export
setMethod(
  "plot", signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    .check_same_length(x, y)
    
    common_pos = intersect(x@pos, y@pos)
    
    if (length(common_pos) == 0L) {
      warning("Vectors have no overlapping non-zero entries")
      return(invisible(NULL))
    }
    
    xv = x@value[match(common_pos, x@pos)]
    yv = y@value[match(common_pos, y@pos)]
    
    plot(common_pos, xv, xlab = "Position", ylab = "Value", pch  = 16)
    
    points(common_pos, yv, pch = 1)
    
    invisible(NULL)
  }
)


#' Mean of a sparse_numeric vector
#'
#' Computes the mean of a \code{sparse_numeric} vector including the
#' implicit zeros, without converting to a dense representation.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return A numeric scalar giving the mean of \code{x}.
#' @export
setMethod(
  "mean",
  signature(x = "sparse_numeric"),
  function(x, ...) {
    n <- as.integer(x@length)
    if (n == 0L) {
      return(NA_real_)
    }
    sum(x@value) / n
  }
)

#' @rdname norm_sparse_numeric
#' @export
setMethod(
  "norm",
  signature(x = "sparse_numeric"),
  function(x, ...) {
    sqrt(sum(x@value^2))
  }
)


#' @rdname standardize_sparse_numeric
#' @export
setMethod(
  "standardize",
  signature(x = "sparse_numeric"),
  function(x, ...) {
    n <- as.integer(x@length)
    if (n == 0L) {
      return(x)
    }
    
    # mean over full vector (implicit zeros)
    m <- mean(x)
    
    # compute sample variance efficiently using sparse representation
    # s^2 = (1 / (n - 1)) * (sum(x_i^2) - n * m^2)
    sum_sq <- sum(x@value^2)
    var <- if (n > 1L) (sum_sq - n * m^2) / (n - 1L) else 0
    if (var < 0 && abs(var) < 1e-12) {
      var <- 0  # numerical safeguard
    }
    sd <- sqrt(var)
    
    if (sd == 0) {
      # All entries identical; standardized vector is all zeros
      return(new("sparse_numeric",
                 value  = numeric(0),
                 pos    = integer(0),
                 length = x@length))
    }
    
    # Materialize standardized dense vector then re-sparsify
    dense <- numeric(n)
    if (length(x@pos) > 0L) {
      dense[x@pos] <- x@value
    }
    z <- (dense - m) / sd
    
    as(z, "sparse_numeric")
  }
)