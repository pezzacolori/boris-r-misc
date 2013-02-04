\name{formulae2}
\alias{formulae2}
\title{Formulae from formula with all terms}
\usage{
  formulae2(formula, nullmodelterm = "1", minsize = 1,
    maxsize = NULL)
}
\arguments{
  \item{formula}{formula with all the terms}

  \item{nullmodelterm}{to specify in case of an always
  required fixed term (should not be included in the input
  formula)}

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
  Build the formulae (as strings) from a formula holding
  all the terms
}

