do_chisq <- function(sample_size, repetition, freq, sampler) {
  prob <- freq/sum(freq)
  res <- rep(NA, repetition)
  for(k in seq_along(res)) {
    res[k] <- chisq.test(table(sampler(freq, sample_size)),
                         p =prob)$statistic
  }
  res
}
