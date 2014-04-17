\name{bkr.for.tpr}
\alias{bkr.for.tpr}
\title{Background portion for a given true positive rate}
\usage{
bkr.for.tpr(d, tp.rate, depvar_name = "y", occurrence_colname = "presence")
}
\arguments{
  \item{d}{dataframe}

  \item{tp.rate}{true positive rate}

  \item{depvar_name}{name of the column holding the model
  output}

  \item{occurrence_colname}{name of the column holding
  presence [0/1]}
}
\value{
background proportion
}
\description{
Background portion for a given true positive rate
}

