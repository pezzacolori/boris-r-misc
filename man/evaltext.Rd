\name{evaltext}
\alias{evaltext}
\title{Concatenate and evaluate string expressions in a specified environment}
\usage{
  evaltext(..., envir = parent.frame(),
    enclos = if (is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv(),
    sep = "")
}
\value{
  The result of evaluating the object: for an expression
  vector this is the result of evaluating the last element
}
\description{
  This function allows to write in a shorter form the
  evaluation of a vector of characters.
}
\examples{
evaltext("m = ",prefix,formulae[i], suffix)
}

