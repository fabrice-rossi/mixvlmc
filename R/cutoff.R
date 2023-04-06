to_native <- function(quantiles, space_size) {
  stats::qchisq(quantiles, df = space_size - 1, lower.tail = FALSE) / 2
}

to_quantile <- function(natives, space_size) {
  stats::pchisq(2 * natives, df = space_size - 1, lower.tail = FALSE)
}

