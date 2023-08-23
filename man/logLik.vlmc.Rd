% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vlmc_likelihood.R
\name{logLik.vlmc}
\alias{logLik.vlmc}
\title{Log-Likelihood of a VLMC}
\usage{
\method{logLik}{vlmc}(object, initial = c("truncated", "specific", "extended"), ...)
}
\arguments{
\item{object}{the vlmc representation.}

\item{initial}{specifies the likelihood function, more precisely the way the
first few observations for which contexts cannot be calculated are integrated
in the likelihood. See \code{\link[=loglikelihood]{loglikelihood()}} for details.}

\item{...}{additional parameters for logLik.}
}
\value{
an object of class \code{logLik}. This is a number,
the log-likelihood of the (CO)VLMC with the following attributes:
\itemize{
\item \code{df}: the number of parameters used by the VLMC for this likelihood calculation
\item \code{nobs}: the number of observations included in this likelihood calculation
\item \code{initial}: the value of the \code{initial} parameter used to compute this likelihood
}
}
\description{
This function evaluates the log-likelihood of a VLMC fitted on a discrete time
series.
}
\examples{
pc <- powerconsumption[powerconsumption$week == 5, ]
breaks <- c(
  0,
  median(powerconsumption$active_power, na.rm = TRUE),
  max(powerconsumption$active_power, na.rm = TRUE)
)
labels <- c(0, 1)
dts <- cut(pc$active_power, breaks = breaks, labels = labels)
m_nocovariate <- vlmc(dts)
ll <- logLik(m_nocovariate)
ll
attributes(ll)
}
\seealso{
\code{\link[=loglikelihood]{loglikelihood()}}
}