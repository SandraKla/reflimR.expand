#' Calculate standardized logarithmic values
#'
#' Converts positive laboratory measurements into standardized logarithmic
#' values using lower and upper reference limits.
#'
#' @param x A numeric vector containing positive laboratory measurements.
#' @param L A positive numeric value specifying the lower reference limit.
#' @param U A positive numeric value specifying the upper reference limit.
#'   `U` must be greater than `L`.
#'
#' @return A numeric vector containing the calculated zlog values.
#'   Invalid measurements are returned as `NA`.
#'
#' @examples
#' zlog(4, L = 2, U = 8)
#' zlog(c(2, 4, 8), L = 2, U = 8)
#'
#' @export
zlog <- function(x, L = 0, U = 0) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  if (
    length(L) != 1L ||
    length(U) != 1L ||
    !is.finite(L) ||
    !is.finite(U) ||
    L <= 0 ||
    U <= 0 ||
    U <= L
  ) {
    return(rep(NA_real_, length(x)))
  }

  result <- rep(NA_real_, length(x))
  valid <- is.finite(x) & x > 0

  logl <- log(L)
  logu <- log(U)
  mu.log <- (logl + logu) / 2
  sigma.log <- (logu - logl) / 3.919928

  result[valid] <- (log(x[valid]) - mu.log) / sigma.log

  result
}
