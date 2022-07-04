\name{systematic_fit_position}
\alias{systematic_fit_position}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to fit one additional variable into a base model for the position from a vector of variables}
\description{A function to fit one additional variable into a base model for the position of a branch from a set vector of variables based on the AIC criteria.}
\usage{systematic_fit_position(base, k, vars, data, trace = F)}
\arguments{
  \item{base}{The base model formula.}
  \item{k}{Number of components in the mixture of of the beta regression model for position.}
  \item{vars}{A vector of candidate variables.}
  \item{data}{Data used for model selection.}
  \item{trace}{Whether to show the steps of the model selection process.}
}
\author{Nishan Mudalige}
\examples{
## To be added.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.