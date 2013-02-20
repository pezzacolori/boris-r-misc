\name{kfold.seq}
\alias{kfold.seq}
\title{Sequential k-fold partitioning}
\usage{
  kfold.seq(x, k = 5, by = NULL)
}
\arguments{
  \item{x}{a vector, matrix, data.frame, or Spatial object}

  \item{k}{number of groups}

  \item{by}{Optional argument. A vector or factor with
  sub-groups (e.g. species). Its length should be the same
  as the number of records in x}
}
\value{
  a vector with group assignments
}
\description{
  Modified version of the \code{\link{kfold}} function,
  that returns subsequent (not random) folds (consistent
  among runs)
}
\seealso{
  \code{\link{kfold}}
}

