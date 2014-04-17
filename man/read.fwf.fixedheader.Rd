\name{read.fwf.fixedheader}
\alias{read.fwf.fixedheader}
\title{Reads a fixed width formatted data with the header in the same format}
\usage{
read.fwf.fixedheader(file, widths, ...)
}
\arguments{
  \item{file}{name of the file.}

  \item{widths}{integer vector, giving the widths of the
  fixed-width fields (of one line).}

  \item{...}{further arguments to be passed to
  \code{\link{read.fwf}}.}
}
\value{
A data.frame as produced by \code{\link{read.fwf}} which is
called internally.
}
\description{
The base function \code{\link{read.fwf}} can read fixed
width formatted data, however when including an header,
this needs to have another format (e.g. tab-separated, as
specified by the sep argument). This function allows to
read data with the header specifically in the same fixed
width format as the data.
}

