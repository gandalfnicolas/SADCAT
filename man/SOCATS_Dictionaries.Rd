% Generated manually to match documentation in R/data.R
\docType{data}
\name{SOCATS_Dictionaries}
\alias{SOCATS_Dictionaries}
\title{Raw SOCATS social category dictionary}
\format{
A data frame with SOCATS category labels and raw dictionary terms. Key column groups:
\itemize{
  \item \code{word}: Raw social category term
  \item One binary indicator column per SOCATS category used by the matcher
  \item Category families spanning race/ethnicity, age, sexuality, gender, jobs, and geography
}
}
\usage{
SOCATS_Dictionaries
}
\description{
Source data used by \code{prepare_socats_dictionaries()} to build the
quanteda dictionary for SOCATS social category matching.
}
\keyword{datasets}
