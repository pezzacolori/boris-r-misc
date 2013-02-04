\name{getNonNADataFromFormula}
\alias{getNonNADataFromFormula}
\title{Remove NA's}
\usage{
  getNonNADataFromFormula(data, formula)
}
\arguments{
  \item{data}{dataframe with the data}

  \item{formula}{formula with dependent and independent
  variables}
}
\value{
  A dataframe without NA's in the columns holded by
  independent variables of the formula
}
\description{
  Return the given dataframe without the rows where one of
  the independent variables (extracted from formula) are NA
}
\details{
  This is useful to link abundances to the model datasets,
  since the built models internally exclude those rows
}

