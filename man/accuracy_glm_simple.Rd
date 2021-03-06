% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_utils.r
\name{accuracy_glm_simple}
\alias{accuracy_glm_simple}
\title{Accuracy function to evaluate a glm model}
\usage{
accuracy_glm_simple(m, p, a, abundance = NULL)
}
\arguments{
\item{m}{glm model}

\item{p}{presences}

\item{a}{absences}

\item{abundance}{abundances, should be passed in same order as presences and same length}
}
\value{
a vector of named arguments (n=number data, np=numer of presences, )
}
\description{
Evaluate according to multiple criteria a glm model
}
