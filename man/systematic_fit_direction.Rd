\name{systematic_fit_direction}
\alias{systematic_fit_direction}
\title{A function to systematically fit the best linear and quadratic terms in the model for direction}
\description{A function that will systematically fit the best linear and quadratic terms in the model for direction. The function will first fit the best linear terms and if any linear terms are selected by the model, then the function will determine whether any of the linear terms may also be added into the model as quadratic terms and interaction terms based on AIC criteria.}
\usage{systematic_fit_direction(data, u, v, trace = F)}
\arguments{
  \item{data}{Data used for model selection.}
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
