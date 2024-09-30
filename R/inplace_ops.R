#' @title Add and assign
#' @description
#' This operator adds the right-hand side value to the left-hand side variable and assigns the result to the left-hand side variable.
#' @param t Target variable (numeric).
#' @param s Increment (numeric).
#' @return The result of adding `s` to `t` and assigning it to `t`.
#' @examples
#' x <- 1
#' x %+=% 10
#' x
#' @export
`%+=%` <- function(t, s) eval.parent(substitute(t <- t + s))

#' @title Minus and assign
#' @description
#' A useful function maybe.
#' @param t A number.
#' @param m Another number.
#' @return The result of x - y.
#' @export
`%-=%` <- function(t, m) eval.parent(substitute(t <- t - m))

`%c=%` <- function(t, a) eval.parent(substitute(t <- c(t, a)))

`%union=%` <- function(t, a) eval.parent(substitute(t <- union(t, a)))

assignPlus <- function(t, s, env = parent.frame()) {
  assertString(t)
  assign(t, get(t, env) + s, env)
}

assignMinus <- function(t, m, env = parent.frame()) {
  assertString(t)
  assign(t, get(t, env) - m, env)
}

assignC <- function(t, a, env = parent.frame()) {
  assertString(t)
  assign(t, c(get(t, env), a), env)
}

assignUnion <- function(t, a, env = parent.frame()) {
  assertString(t)
  assign(t, union(get(t, env), a), env)
}
