\name{formulae}
\alias{formulae}
\title{Formulae from variable combinations}
\usage{
  formulae(dep, vars, nullmodelterm = "1", minsize = 1,
    maxsize = NULL)
}
\arguments{
  \item{dep}{name of the dependent variable}

  \item{vars}{character vector with the names of the
  independent variables}

  \item{nullmodelterm}{to specify in case of an always
  required fixed term (should not be included in the vars)}

  \item{minsize}{minimum size of the formula (number of
  independent variables)}

  \item{maxsize}{maximum size of the formula (number of
  independent variables). NULL means unrestricted.}
}
\value{
  character vector hold the strings of the generated
  formulae.
}
\description{
  Build the formulae (as strings) from variable names
}

