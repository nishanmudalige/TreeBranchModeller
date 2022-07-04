\name{systematic_fit_full_length}
\alias{systematic_fit_full_length}
\title{A function to systematically fit the best linear and quadratic terms in the model for length}
\description{A function that will systematically fit the best linear and quadratic terms in the model for the length of a branch. The function will first fit the best linear terms and if any linear terms are selected by the model, then the function will determine whether any of the linear terms may also be added into the model as quadratic terms and interaction terms based on AIC criteria.}
\usage{systematic_fit_full_length(base, vars, data, 
linear = T, quad_int = T, hierarchical = T, trace = F)}
\arguments{
  \item{base}{The base model formula.}
  \item{vars}{A vector of candidate variables.}
  \item{data}{Data used for model selection.}
  \item{linear}{Logical argument. If set to TRUE, then determine the linear terms that should be included.}
  \item{quad_int}{Logical argument. If set to TRUE, then determine the quadratic and interaction terms That should be included.}
  \item{hierarchical}{Logical argument. If set to TRUE, then only use the linear terms added as candidates for the quadratic and interaction terms.}
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
