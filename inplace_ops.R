
`%+=%` <- function(t, s) eval.parent(substitute(t <- t + s))

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
