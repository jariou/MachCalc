## Examples of C code

#' Reverse elements
#'
#' This function takes a numeric vector and returns a reversed version of this
#' vector. The internal computation is via C.
#' @param x a numeric vector
#' @return a numeric vector (reversed \code{x})
#' @author Jacques Rioux <\url{http://jariou.github.io/}>
#' @examples reverse(c(1, 9.3, 8.2, 4.33)); reverse(1:10)
#' @export
#' @useDynLib machcalc
reverse = function(x) {
  n = length(x)
  res = .C('reverse', as.numeric(x), n, numeric(n), package = 'rmini')
  res[[3L]]
}
