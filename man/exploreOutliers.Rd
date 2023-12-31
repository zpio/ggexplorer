% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploreOutliers.R
\name{exploreOutliers}
\alias{exploreOutliers}
\title{Explore Outliers}
\usage{
exploreOutliers(data)
}
\arguments{
\item{data}{A data frame or tibble}
}
\value{
A tibble
}
\description{
Explore the outliers of all numeric variables in data frame.
}
\examples{

library(dplyr)

iris \%>\%
   exploreOutliers()

iris \%>\%
   group_by(Species) \%>\%
   exploreOutliers()

}
