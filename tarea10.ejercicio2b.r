library(invLT)

gamma_lt <- function(s, shape, rate) {
    return((rate^shape) / ((s + rate)^shape))
}


gamma_lt_pow <- function(s, shape, rate, pow = 1) { # LT elevada de gamma
    return((gamma_lt(s, shape, rate))^pow)
}

ex_2b_gamma_tail <- function(s) { # LT del ejercicio 2.b de la tarea 10 y cor 2.3
    return((1 - gamma_lt_pow(s, shape = 1.2, rate = 1, pow = 10)) / s)
}

vals <- seq(from = 1, to = 120, by = 1)

inverted_lt_vals <- vapply(
    vals,
    iv.opC,
    complex(1),
    L.FUN = ex_2b_gamma_tail,
    nterms = 21L
)

plot(vals, inverted_lt_vals, type = "l")
