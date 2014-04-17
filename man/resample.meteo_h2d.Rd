\name{resample.meteo_h2d}
\alias{resample.meteo_h2d}
\title{Resample a data.frame with meteorological data with hourly interval to a daily interval,
allowing the specification at which time to cut the day (e.g. can be summarized foro noon to noon).}
\usage{
resample.meteo_h2d(h, time_h = 24, timevar, varnames,
  aggregation = c("sample", "sum", "mean", "max", "min"), na.rm = F,
  add_suffix = F)
}
\arguments{
  \item{h}{a data.frame holding the hourly data}

  \item{time_h}{hour value at which to cut the hourly
  data.frame to build summaries (e.g. 12 for noon to noon).
  Defaults to 24.}

  \item{timevar}{name of the column holding the date-time
  information (in POSIX numeric format).}

  \item{varnames}{character vector holding the names of the
  columns holding the variables to be processed.}

  \item{aggregation}{character vector holding the types of
  aggregations to perform on the selected variables.}

  \item{na.rm}{boolean to specify if aggregation function
  should consider or skip NA's.}

  \item{add_suffix}{boolean to specify if the vairable
  names sohould be completed with the specification of the
  aggregation.}
}
\value{
a new data.frame with daily timestep and the selected
variables and aggregations .
}
\description{
Resample a data.frame with meteorological data with hourly
interval to a daily interval, allowing the specification at
which time to cut the day (e.g. can be summarized foro noon
to noon).
}
\seealso{
\code{\link{kfold}}
}

