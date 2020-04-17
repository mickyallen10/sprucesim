\name{perc_saw_volume}
\alias{perc_saw_volume}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percentage of Volume in Sawtimber
}
\description{
Predicts the percentage of standing volume in the sawtimber product class.
}
\usage{
perc_saw_volume(stand.df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stand.df}{
A data frame with the required stand values. See ??standsim for details.
}
}
\value{
Provides a values between 0 and 1 interpreted as the percentage of stand level volume in the sawtimber product class for unthinned stands.
}
\references{
Allen, M.G., C. Anton-Fernandez, and R. Astrup. 2020. Economical Impacts of Thinning in Managed Norway Spruce Forests in Norway. Scandinavian Journal of Forest Research (in review).
}
\author{
Micky G Allen II
}


