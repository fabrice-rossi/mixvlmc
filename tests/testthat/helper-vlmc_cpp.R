compare_vlmc_cpp <- function(m1, m2, verbose = TRUE) {
  if (length(m1) != length(m2)) {
    if (verbose) {
      cat("Different number of attributes\n")
    }
    return(FALSE)
  }
  for (field in c("vals", "depth", "nb_ctx", "data_size", "alpha", "cutoff", "ix", "extended_ll", "keep_match", "max_depth")) {
    if (!isTRUE(all.equal(m1[[field]], m2[[field]]))) {
      if (verbose) {
        cat("In field", field, "\n")
        print(waldo::compare(m1[[field]], m2[[field]]))
      }
      return(FALSE)
    }
  }
  ctx_m1 <- contexts(m1, frequency = "detailed")
  ctx_m2 <- contexts(m2, frequency = "detailed")
  if (!compare_ctx(ctx_m1, ctx_m2, verbose = verbose)) {
    return(FALSE)
  }
  TRUE
}
