% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixvlmc-package.R
\docType{package}
\name{mixvlmc-package}
\alias{mixvlmc}
\alias{mixvlmc-package}
\title{mixvlmc: Variable Length Markov Chains with Covariates}
\description{
Estimates Variable Length Markov Chains (VLMC) models and VLMC with covariates models from discrete sequences. Supports model selection via information criteria and simulation of new sequences from an estimated model. See Bühlmann, P. and Wyner, A. J. (1999) \doi{10.1214/aos/1018031204} for VLMC and Zanin Zambom, A., Kim, S. and Lopes Garcia, N. (2022) \doi{10.1111/jtsa.12615} for VLMC with covariates.
}
\section{Package options}{


Mixvlmc uses the following \code{\link[=options]{options()}}:
\itemize{
\item \code{mixvlmc.maxit}: maximum number of iterations in model fitting for \code{\link[=covlmc]{covlmc()}}
\item \code{mixvlmc.predictive}: specifies the computing engine used for model fitting
for \code{\link[=covlmc]{covlmc()}}. Two values are supported:
\itemize{
\item \code{"glm"} (default value): \code{\link[=covlmc]{covlmc()}} uses \code{\link[stats:glm]{stats::glm()}} with a binomial
link (\code{\link[stats:family]{stats::binomial()}}) for a two values state space, and \code{\link[VGAM:vglm]{VGAM::vglm()}}
with a multinomial link (\code{\link[VGAM:multinomial]{VGAM::multinomial()}}) for a state space with
three or more values;
\item \code{"multinom"}: \code{\link[=covlmc]{covlmc()}} uses \code{\link[nnet:multinom]{nnet::multinom()}} in all cases.
}

The first option \code{"glm"} is recommended as both \code{\link[stats:glm]{stats::glm()}} and \code{\link[VGAM:vglm]{VGAM::vglm()}}
are able to detect and deal with degeneracy in the data set.
\item \code{mixvlmc.backend}: specifies the implementation used for the context tree
construction in \code{\link[=ctx_tree]{ctx_tree()}}, \code{\link[=vlmc]{vlmc()}} and \code{\link[=tune_vlmc]{tune_vlmc()}}. Two values are
supported:
\itemize{
\item \code{"R"} (default value): this corresponds to the original almost pure R
implementation.
\item \code{"C++"}: this corresponds to the experimental C++ implementation. This
version is significantly faster than the R version, but is still
considered experimental.
}
\item \code{mixvlmc.charset}: specifies the collection of characters used to display
context trees in "ascii art" when using the \code{"text"} format for \code{\link[=draw]{draw()}}
and related functions. Two values are supported:
\itemize{
\item \code{"ascii"}: the collection uses only standard ASCII characters and
should be compatible with all environments;
\item \code{"utf8"}: the collection uses UTF-8 symbols and needs a compatible display.
At loading the option is set based on a call to \code{\link[cli:is_utf8_output]{cli::is_utf8_output()}}.
It defaults to \code{"utf8"} is this encoding is supported.
}
}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/fabrice-rossi/mixvlmc}
  \item \url{https://fabrice-rossi.github.io/mixvlmc/}
  \item Report bugs at \url{https://github.com/fabrice-rossi/mixvlmc/issues}
}

}
\author{
\strong{Maintainer}: Fabrice Rossi \email{Fabrice.Rossi@apiacoa.org} (\href{https://orcid.org/0000-0003-4638-1286}{ORCID}) [copyright holder]

Other contributors:
\itemize{
  \item Hugo Le Picard \email{lepicardhugo@gmail.com} (\href{https://orcid.org/0000-0002-7023-2996}{ORCID}) [contributor]
  \item Guénolé Joubioux \email{guenole.joubioux@gmail.com} [contributor]
}

}
