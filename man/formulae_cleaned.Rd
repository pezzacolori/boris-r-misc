% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{formulae_cleaned}
\alias{formulae_cleaned}
\title{Formulae from variable combinations without correlated variables}
\usage{
formulae_cleaned(
  formula,
  dep = NULL,
  vars = NULL,
  nullmodelterm = "1",
  minsize = 1,
  maxsize = NULL,
  data,
  threshold = 0.6,
  use = "everything",
  method = c("pearson", "kendall", "spearman")
)
}
\arguments{
\item{formula}{formula with all the terms (beyond optimal model)}

\item{dep}{name of the dependent variable. If the \code{formula} is specified, this argument is not considered.}

\item{vars}{character vector with the names of the independent variables (wihtout nullmodel term). If the \code{formula} is specified, this argument is not considered.}

\item{nullmodelterm}{to specify in case of an always required fixed term (should not be included in the vars)}

\item{minsize}{minimum size of the formula (number of independent variables)}

\item{maxsize}{maximum size of the formula (number of independent variables). NULL means unrestricted.}

\item{data}{dataframe holding the dataset with the column names corresponding to vars}

\item{threshold}{correlation threshold}

\item{use}{an optional character string giving a method for computing covariances in the 
presence of missing values. This must be (an abbreviation of) one of the strings 
"everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"}

\item{method}{a character string indicating which correlation coefficient 
is to be computed. One of "pearson" (default), "kendall", or "spearman", can be abbreviated}
}
\value{
list of \itemize{
\item formulae: character vector hold the strings of the generated formulae
\item vars: list of variables names combinations 
}
}
\description{
Build the formulae (as strings) from variable names
}
