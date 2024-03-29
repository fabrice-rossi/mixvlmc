% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctx_tree.R
\name{context_number}
\alias{context_number}
\title{Number of contexts of a context tree}
\usage{
context_number(ct)
}
\arguments{
\item{ct}{a context tree.}
}
\value{
the number of contexts of the tree.
}
\description{
This function returns the number of distinct contexts in a context tree.
}
\examples{
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
# should be 8
context_number(rdts_ctree)
}
