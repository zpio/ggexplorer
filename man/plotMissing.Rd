% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMissing.R
\name{plotMissing}
\alias{plotMissing}
\title{Explore Missing values}
\usage{
plotMissing(
  data,
  fill = "steelblue",
  width = 0.7,
  prop = TRUE,
  title = NULL,
  x_lab = NULL,
  y_lab = NULL,
  expand_mult_x = c(0, 0.15)
)
}
\arguments{
\item{data}{A data frame or tibble.}

\item{fill}{character. Color of the bars. Overrided if fill_var is specified.}

\item{width}{numeric. Width of the bars.}

\item{prop}{logical. Whether or not to display proportions instead of counts.}

\item{title}{character. Title for the plot.}

\item{x_lab}{character. X-axis label for the plot.}

\item{y_lab}{character. Y-axis label for the plot.}

\item{expand_mult_x}{character. A numeric vector of length 2 indicating the expansion multiplier for the x-axis.}
}
\value{
A static \code{ggplot2} plot
}
\description{
Explore Missing values
}
\examples{

plotMissing(iris)

}
