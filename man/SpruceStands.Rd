\name{SpruceStands}
\alias{SpruceStands}
\docType{data}
\title{
Stand-Level Data for Norway Spruce Stands
}
\description{
Data from four long-term growth and yield experiments in even-aged Norway Spruce Stands in Norway
}
\usage{data("SpruceStands")}
\format{
  A data frame with 4 observations on the following 21 variables.
  \describe{
    \item{\code{plot.id}}{a factor with levels \code{0807-1} \code{0929-14} \code{0968-7} \code{0986-1}}
    \item{\code{age}}{a numeric vector}
    \item{\code{tph}}{a numeric vector}
    \item{\code{bph}}{a numeric vector}
    \item{\code{hd}}{a numeric vector}
    \item{\code{si}}{a numeric vector}
    \item{\code{qmd}}{a numeric vector}
    \item{\code{vol}}{a numeric vector}
    \item{\code{hd_thin}}{a logical vector}
    \item{\code{thinid}}{a numeric vector}
    \item{\code{bpha}}{a logical vector}
    \item{\code{bphb}}{a logical vector}
    \item{\code{bphr}}{a logical vector}
    \item{\code{tage}}{a logical vector}
    \item{\code{vola}}{a logical vector}
    \item{\code{volb}}{a logical vector}
    \item{\code{volr}}{a logical vector}
    \item{\code{tpha}}{a logical vector}
    \item{\code{tphb}}{a logical vector}
    \item{\code{qmda}}{a logical vector}
    \item{\code{qmdb}}{a logical vector}
  }
}

\examples{
data(SpruceStands)
## maybe str(SpruceStands) ; plot(SpruceStands) ...
}
\keyword{datasets}
