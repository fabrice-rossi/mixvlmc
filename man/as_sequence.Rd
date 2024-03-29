% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctx_node.R
\name{as_sequence}
\alias{as_sequence}
\title{Extract the sequence encoded by a node}
\usage{
as_sequence(node, reverse)
}
\arguments{
\item{node}{a \code{ctx_node} object as returned by \code{\link[=find_sequence]{find_sequence()}}}

\item{reverse}{specifies whether the sequence should be reported in reverse
temporal order (\code{TRUE}) or in the temporal order (\code{FALSE}). Defaults to the
order associated to the \code{ctx_node} which is determined by the parameters of
the call to \code{\link[=contexts]{contexts()}} or \code{\link[=find_sequence]{find_sequence()}}.}
}
\value{
the sequence represented by the \code{node} object, a vector
}
\description{
This function returns the sequence represented by the \code{node} object.
}
\examples{
rdts <- c("A", "B", "C", "A", "A", "B", "B", "C", "C", "A")
rdts_tree <- ctx_tree(rdts, max_depth = 3)
res <- find_sequence(rdts_tree, "A")
as_sequence(res)
}
