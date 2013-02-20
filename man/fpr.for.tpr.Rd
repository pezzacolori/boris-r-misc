\name{fpr.for.tpr}
\alias{fpr.for.tpr}
\title{False positive rate for a given true positive rate}
\usage{
  fpr.for.tpr(d, tp.rate, depvar_name = "y",
    occurrence_colname = "presence")
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
  false positive rate
}
\description{
  False positive rate for a given true positive rate
}

