\name{cor2df}
\alias{cor2df}
\title{Correlated variable pairs}
\usage{
  cor2df(cor.mat, threshold = 0.6)
}
\arguments{
  \item{cor.mat}{correlation matrix}

  \item{threshold}{correlation threshold}
}
\value{
  a dataframe holding the variable pairs with a correlation
  higher than the specified threshold
}
\description{
  This function returns a dataframe with the variable pairs
  above a given correlation threshold
}
\details{
  It is based on the \code{\link{cor}} function, but
  instead of a correlation matrix it returns a dataframe
  with the pairwise combinations above a threshold.
}
\seealso{
  \code{\link{cordf}}
}

