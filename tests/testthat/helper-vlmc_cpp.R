compare_vlmc <- function(m1, m2) {
  if (length(m1) != length(m2)) {
    return(FALSE)
  }
  for (field in c("vals", "depth", "nb_ctx", "data_size", "alpha", "cutoff", "ix", "extended_ll")) {
    if (!all.equal(m1[[field]], m2[[field]])) {
      return(FALSE)
    }
  }
  ctx_m1 <- contexts(m1, frequency = "detailed")
  ctx_m2 <- contexts(m2, frequency = "detailed")
  if(!identical(dim(ctx_m1), dim(ctx_m2))) {
    return(FALSE)
  }
  m2_match <- c()
  for(i in seq_along(ctx_m1$context)) {
    in_m2 <- Position(\(x) identical(x, ctx_m1$context[[i]]), ctx_m2$context, nomatch = 0)
    if(in_m2==0) {
      return(FALSE)
    }
    m2_match <- c(m2_match, in_m2)
    c_m1 <- ctx_m1[2:ncol(ctx_m1)][i, ]
    c_m2 <- ctx_m2[2:ncol(ctx_m2)][in_m2, ]
    row.names(c_m1) <- NULL
    row.names(c_m2) <- NULL
    if(!identical(c_m1, c_m2)) {
      return(FALSE)
    }
  }
  if(!all.equal(sort(m2_match), 1:nrow(ctx_m2))) {
    return(FALSE)
  }
  TRUE
}
