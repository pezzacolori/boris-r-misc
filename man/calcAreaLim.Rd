% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{calcAreaLim}
\alias{calcAreaLim}
\title{Area under a curve}
\usage{
calcAreaLim(x, y, xupper = NULL)
}
\arguments{
\item{x}{vector holding the x values}

\item{y}{vector holding the y values}

\item{xupper}{x value}
}
\value{
area under the curve
}
\description{
Calculates the trapezoid area (boxes+traingles) under the curve y=f(x)
up to a given x limit (xupper), when given
}
\details{
If xupper is not one of the x values, the corresponding y value is 
calculated using the approx function
}
\seealso{
\code{\link{calcArea}}
}
