\name{select_multiple}
\alias{select_multiple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function that will select the best set of variables for the model for direction.}
\description{A function that will select the best set of varialbes to include in the model based on AIC criteria, based on variables that must be included and a set of one or more candidate variables.
The function will sequentiall add new variables into the model based on the variable that provides the lowest AIC, until the AIC does not decrease with the addition of a new variable.}
\usage{select_multiple(data, must_include = NULL, u, v, trace = F)}
\arguments{
  \item{data}{Data used for model selection.}
  \item{must_include}{The names of the variables that must be included in the model selection.}
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
