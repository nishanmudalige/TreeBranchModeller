\name{WTobsn}
\alias{WTobsn}
\docType{data}
\title{A data set on Wonder Tree}
\description{A data set on Wonder Tree}
\usage{data("WTobsn")}
\format{
  A data frame with 189 observations on the following 27 variables.
  \describe{
    \item{\code{id1}}{Branch ID for level 1 branch}
    \item{\code{x1}}{The position of the trunk relative to the parent branch. This value is zero since the trunk is the refence and does not have any parents.}
    \item{\code{C1.1}}{The position of the trunk in the x-direction}
    \item{\code{C1.2}}{The position of the trunk in the y-direction}
    \item{\code{C1.3}}{The position of the trunk in the z-direction}
    \item{\code{L1}}{The relative length of the branch.}
    \item{\code{N1}}{The number of offspring branhes.}
    \item{\code{id2}}{Branch ID for level 2 branch}
    \item{\code{x2}}{The position of the level 2 branch relative to the trunk.}
    \item{\code{C2.1}}{The position of the level 2 branch in the x-direction realtive to the trunk}
    \item{\code{C2.2}}{The position of the level 2 branch in the y-direction realtive to the trunk}
    \item{\code{C2.3}}{The position of the level 2 branch in the z-direction realtive to the trunk}
    \item{\code{L2}}{The length of the level 2 branch relative to the trunk}
    \item{\code{N2}}{The number of offspring of the level 2 branch}
    \item{\code{id3}}{Branch ID for level 3 branch}
    \item{\code{x3}}{a numeric vector}
    \item{\code{C3.1}}{The position of the level 3 branch in the x-direction realtive to the level 2 branch from which it emanates}
    \item{\code{C3.2}}{The position of the level 3 branch in the y-direction realtive to the level 2 branch from which it emanates}
    \item{\code{C3.3}}{The position of the level 3 branch in the z-direction realtive to the level 2 branch from which it emanates}
    \item{\code{L3}}{The length of the level 3 branch relative to the level 2 branch from which it emanates}
    \item{\code{N3}}{The number of offspring of the level 3 branch}
    \item{\code{id4}}{Branch ID for level 4 branch}
    \item{\code{x4}}{a numeric vector}
    \item{\code{C4.1}}{The position of the level 4 branch in the x-direction realtive to the level 3 branch from which it emanates}
    \item{\code{C4.2}}{The position of the level 4 branch in the y-direction realtive to the level 3 branch from which it emanates}
    \item{\code{C4.3}}{The position of the level 4 branch in the z-direction realtive to the level 3 branch from which it emanates}
    \item{\code{L4}}{The length of the level 4 branch relative to the level 3 branch from which it emanates}
  }
}
\examples{
data(WTobsn)
## str(WTobsn)
}
\keyword{datasets}
