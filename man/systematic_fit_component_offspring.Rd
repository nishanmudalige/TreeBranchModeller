\name{systematic_fit_component_offspring}
\alias{systematic_fit_component_offspring}
\title{A function to select the best variables to add to the model for the number of offspring}
\description{A function to select the best variables to add to the model for the number of offspring. The function will start with a base model and add a variable from the input set of variables. Repeat process with remaining variables until AIC of new model is not less than AIC of previous model.}
\usage{systematic_fit_component_offspring(base, vars, data, trace = F)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base}{The baseline model.}
  \item{vars}{The candidate set of variable names.}
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
