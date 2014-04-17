\name{me.predict}
\alias{me.predict}
\title{Predict new values of a maxent model}
\usage{
me.predict(m, data)
}
\arguments{
  \item{m}{either a maxent model or a character vector
  containing the lines of a maxent lambda file}

  \item{data}{data frame holding the data}
}
\value{
predictions
}
\description{
Calculates new values of a maxent model
(\code{\link{maxent}})
}

