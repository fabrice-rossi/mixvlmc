to_multi_dts <- function(xs, vals = NULL) {
  if (is.null(vals)) {
    nx <- to_dts(xs[[1]])
    vals <- nx$vals
    if (length(xs) > 1) {
      ixs <- c(list(nx$ix), lapply(xs[-1], \(x) to_dts(x, vals = vals)$ix))
    } else {
      ixs <- list(nx$ix)
    }
    list(ixs = ixs, vals = vals)
  } else {
    list(ixs = lapply(xs, \(x) to_dts(x, vals = vals)$ix), vals = vals)
  }
}

weighted_table <- function(xs, weights = NULL) {
  counts <- sapply(xs, table)
  if (is.null(weights)) {
    res <- as.integer(rowSums(counts))
  } else {
    res <- rowSums(sweep(counts, 2, weights, "*"))
  }
  res
}

weighted_tabulate <- function(xs, nbins, weights = NULL) {
  counts <- sapply(xs, \(x) tabulate(x + 1, nbins = nbins))
  if (is.null(weights)) {
    res <- as.integer(rowSums(counts))
  } else {
    res <- rowSums(sweep(counts, 2, weights, "*"))
  }
  res
}
