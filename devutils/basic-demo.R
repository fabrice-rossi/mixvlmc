set.seed(0)
x <- sample(c("A", "B", "C"), 1000, replace=TRUE)
x_tree <- vlmc(x, alpha=0.05)
