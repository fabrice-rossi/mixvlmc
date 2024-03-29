% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covlmc_predict.R
\name{predict.covlmc}
\alias{predict.covlmc}
\title{Next state prediction in a discrete time series for a VLMC with covariates}
\usage{
\method{predict}{covlmc}(
  object,
  newdata,
  newcov,
  type = c("raw", "probs"),
  final_pred = TRUE,
  ...
)
}
\arguments{
\item{object}{a fitted covlmc object.}

\item{newdata}{a time series adapted to the covlmc object.}

\item{newcov}{a data frame with the new values for the covariates.}

\item{type}{character indicating the type of prediction required. The default
\code{"raw"} returns actual predictions in the form of a new time series. The
alternative \code{"probs"} returns a matrix of prediction probabilities (see
details).}

\item{final_pred}{if \code{TRUE} (default value), the predictions include a final
prediction step, made by computing the context of the full time series.
When \code{FALSE} this final prediction is not included.}

\item{...}{additional arguments.}
}
\value{
A vector of predictions if \code{type="raw"} or a matrix of state
probabilities if \code{type="probs"}.
}
\description{
This function computes one step ahead predictions for a discrete time series
based on a VLMC with covariates.
}
\details{
Given a time series \code{X}, at time step \code{t}, a context is computed using
observations from \code{X[1]} to \code{X[t-1]} (see the dedicated section). The
prediction is then the most probable state for \code{X[t]} given this logistic
model of the context and the corresponding values of the covariates. The time
series of predictions is returned by the function when \code{type="raw"} (default
case).

When \code{type="probs"}, the function returns of the probabilities of each state
for \code{X[t]} as estimated by the logistic models. Those probabilities are
returned as a matrix of probabilities with column names given by the state
names.
}
\section{Extended contexts}{


As explained in details in \code{\link[=loglikelihood.covlmc]{loglikelihood.covlmc()}} documentation and in
the dedicated \code{vignette("likelihood", package = "mixvlmc")}, the first
initial values of a time series do not in general have a proper context for
a COVLMC with a non zero order. In order to predict something meaningful
for those values, we rely on the notion of extended context defined in the
documents mentioned above. This follows the same logic as using
\code{\link[=loglikelihood.covlmc]{loglikelihood.covlmc()}} with the parameter \code{initial="extended"}. All
covlmc functions that need to manipulate initial values with no proper
context use the same approach.
}

\examples{
pc <- powerconsumption[powerconsumption$week == 10, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.2, 0.7, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5, alpha = 0.5)
rdts_probs <- predict(m_cov, rdts[1:144], rdts_cov[1:144, , drop = FALSE], type = "probs")
rdts_preds <- predict(m_cov, rdts[1:144], rdts_cov[1:144, , drop = FALSE],
  type = "raw", final_pred = FALSE
)
}
