\name{fit_position}
\alias{fit_position}
\title{A function to fit a variable to the model for the position of a branch}
\description{A function which will fit a single new variable to the model for the position of a branch.}
\usage{fit_position(base, k = 1, var, data, trace = F)}
\arguments{
  \item{base}{Formula of base model.}
  \item{k}{Number of components in the mixture of of the beta regression model for position.}
  \item{var}{Variable to add to the model.}
  \item{data}{Data used for parameter estimation.}
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
