\name{repairGaps}
\alias{repairGaps}
\title{Fill 1-value gaps in a vector}
\usage{
  repairGaps(x,
    method = c("linearize", "previous", "next"))
}
\arguments{
  \item{x}{numeric vector}

  \item{method}{how to fill in the gaps (default by
  linearization, otherwise by previous/next value
  duplication)}
}
\value{
  numeric vector with filled 1-value gaps
}
\description{
  Fill gaps of single values with linearization (mean of
  the adjacent values) or repetition of previous/next
  value.
}

