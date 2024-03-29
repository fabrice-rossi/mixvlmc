% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctx_node_covlmc.R
\name{covariate_memory}
\alias{covariate_memory}
\title{Covariate memory length for a COVLMC context}
\usage{
covariate_memory(node)
}
\arguments{
\item{node}{A \code{ctx_node_covlmc} object as returned by \code{\link[=find_sequence]{find_sequence()}} or
\code{\link[=contexts.covlmc]{contexts.covlmc()}}}
}
\value{
the memory length, an integer
}
\description{
This function returns the length of the memory of a COVLMC context represented
by a \code{ctx_node_covlmc} object.
}
\examples{
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 10)
ctxs <- contexts(m_cov)
## get all the memory lengths
sapply(ctxs, covariate_memory)
}
