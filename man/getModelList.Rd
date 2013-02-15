\name{getModelList}
\alias{getModelList}
\title{Models from formulae}
\usage{
  getModelList(prefix, formulae, suffix,
    envir = parent.frame(1))
}
\arguments{
  \item{x}{a vector, matrix, data.frame, or Spatial object}

  \item{prefix}{string representation of the model prefix
  (e.g.  "glm(")}

  \item{formulae}{list of formulae as string expressions}

  \item{suffix}{string representation of the model suffix
  (e.g.  ", family="binomial")" )}

  \item{envir}{environment in which to evaluate the model
  expression}
}
\value{
  a list of models
}
\description{
  Build a list of models according to different formulae, a
  prefix and a suffix
}

