% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tune.R
\name{plot.tune_vlmc}
\alias{plot.tune_vlmc}
\alias{plot.tune_covlmc}
\title{Plot the results of automatic (CO)VLMC complexity selection}
\usage{
\method{plot}{tune_vlmc}(
  x,
  value = c("criterion", "likelihood"),
  cutoff = c("quantile", "native"),
  ...
)

\method{plot}{tune_covlmc}(
  x,
  value = c("criterion", "likelihood"),
  cutoff = c("quantile", "native"),
  ...
)
}
\arguments{
\item{x}{a \code{tune_vlmc} object}

\item{value}{the criterion to plot (default "criterion").}

\item{cutoff}{the scale used for the cut off criterion (default "quantile")}

\item{...}{additional parameters passed to \code{\link[base:plot]{base::plot()}}}
}
\value{
the \code{tune_vlmc} object invisibly
}
\description{
This function plots the results of \code{\link[=tune_vlmc]{tune_vlmc()}} or \code{\link[=tune_covlmc]{tune_covlmc()}}.
}
\details{
The standard plot consists in showing the evolution of the criterion
used to select the model (\code{\link[=AIC]{AIC()}} or \code{\link[=BIC]{BIC()}}) as a function of the
cut off criterion expressed in the quantile scale (the quantile is used
by default to offer a common default behaviour between \code{\link[=vlmc]{vlmc()}} and
\code{\link[=covlmc]{covlmc()}}). Parameters can be used to display instead the \code{\link[=loglikelihood]{loglikelihood()}}
of the model (by setting \code{value="likelihood"}) and to use the native
scale for the cut off when available (by setting \code{cutoff="native"}).
}
\section{Customisation}{

The function sets several default before calling \code{\link[base:plot]{base::plot()}}, namely:
\itemize{
\item \code{type}: "l" by default to use a line representation;
\item \code{xlab}: "Cut off (quantile scale)" by default, adapted to the actual
scale;
\item \code{ylab}: the name of the criterion or "Log likelihood".
}

These parameters can be overridden by specifying other values when calling
the function. All parameters specified in addition to \code{x}, \code{value} and
\code{cutoff} are passed to \code{\link[base:plot]{base::plot()}}.
}

\examples{
rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
tune_result <- tune_vlmc(rdts)
## default plot
plot(tune_result)
## likelihood
plot(tune_result, value = "likelihood")
## parameters overriding
plot(tune_result,
  value = "likelihood",
  xlab = "Cut off", type = "b"
)
pc <- powerconsumption[powerconsumption$week \%in\% 10:12, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
rdts_best_model_tune <- tune_covlmc(rdts, rdts_cov, criterion = "AIC")
plot(rdts_best_model_tune)
plot(rdts_best_model_tune, value = "likelihood")

}
