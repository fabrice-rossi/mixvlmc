% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot_tune.R
\name{autoplot.tune_covlmc}
\alias{autoplot.tune_covlmc}
\title{Create a complete ggplot for the results of automatic COVLMC complexity
selection}
\usage{
\method{autoplot}{tune_covlmc}(object, ...)
}
\arguments{
\item{object}{a \code{tune_covlmc} object}

\item{...}{additional parameters (not used currently)}
}
\value{
a ggplot object
}
\description{
This function prepares a plot of the results of \code{\link[=tune_covlmc]{tune_covlmc()}} using
ggplot2. The result can be passed to \code{\link[=print]{print()}} to display the result.
}
\details{
The graphical representation proposed by this function is complete, while the
one produced by \code{\link[=plot.tune_covlmc]{plot.tune_covlmc()}} is minimalistic. We use here the
faceting capabilities of ggplot2 to combine on a single graphical
representation the evolution of multiple characteristics of the VLMC during
the pruning process, while \code{\link[=plot.tune_covlmc]{plot.tune_covlmc()}} shows only the selection
criterion or the log likelihood. Each facet of the resulting plot shows a
quantity as a function of the cut off expressed in quantile or native scale.
}
\examples{
pc <- powerconsumption[powerconsumption$week \%in\% 10:12, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
rdts_best_model_tune <- tune_covlmc(rdts, rdts_cov, criterion = "AIC")
covlmc_plot <- ggplot2::autoplot(rdts_best_model_tune)
print(covlmc_plot)

}
