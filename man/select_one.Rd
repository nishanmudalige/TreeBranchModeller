\name{select_one}
\alias{select_one}
\title{A function to select the best new variable for the model for direction.}
\description{A function that will select the single best variable to add to a base model based on the AIC criteria. The variable that will be added will be the variable that provides the lowest AIC.}
\usage{select_one(data, include_names, u, v, trace = F)}
\arguments{
  \item{data}{Data used for model selection.}
  \item{include_names}{The names of the variables that must be included in the model selection.}
  \item{u}{Directional cosine.}
  \item{v}{Directional cosine.}
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
