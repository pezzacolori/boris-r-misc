\name{accuracy.me.simple}
\alias{accuracy.me.simple}
\title{Accuracy function to evaluate a maxent model}
\usage{
  accuracy.me.simple(me, p, a, abundance = NULL)
}
\arguments{
  \item{me}{maxent model}

  \item{p}{presences}

  \item{a}{absences}

  \item{abundance}{abundances, should be passed in same
  order as presences and same length}
}
\value{
  a vector of named arguments (n=number data, np=numer of
  presences, )
}
\description{
  Evaluate according to multiple criteria a maxent model
  (\code{\link{maxent}})
}

