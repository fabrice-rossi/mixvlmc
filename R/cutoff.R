to_native <- function(quantiles, space_size) {
  stats::qchisq(quantiles, df = space_size - 1, lower.tail = FALSE) / 2
}

to_quantile <- function(natives, space_size) {
  stats::pchisq(2 * natives, df = space_size - 1, lower.tail = FALSE)
}

guaranteed_pruning <- function(raw_cutoff, space_size, mode, raw) {
  if (!raw) {
    if (length(raw_cutoff) > 1) {
      pre_res <- sqrt(raw_cutoff[-length(raw_cutoff)] * raw_cutoff[-1])
      last_val <- 2 * raw_cutoff[length(raw_cutoff)] - pre_res[length(pre_res)]
      cutoff <- c(pre_res, last_val)
    } else {
      cutoff <- after(raw_cutoff)
    }
  } else {
    cutoff <- raw_cutoff
  }
  if (mode == "native") {
    cutoff
  } else {
    to_quantile(cutoff, space_size)
  }
}
