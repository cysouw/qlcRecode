\name{qlcRecode-package}
\alias{qlcRecode-package}
\alias{qlcRecode}
\docType{package}
\title{
Functions for data recoding in Quantitative Language Comparison (QLC)
}
\description{
When using external data, there are often various tweaks that one would like to perform before using the data for further research. This package offers assistance for some common recoding problems occurring in Quantitative Language Comparison (QLC). Currently only the recoding of nominal data (\code{\link{recode}}) and the tokenization of strings with an orthography profile (\code{\link{tokenize}}) are implemented.
}
\details{
\tabular{ll}{
Package: \tab qlcRecode\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2014-06-20\cr
License: \tab GPL-3\cr
}
The \code{\link{recode}} function allows for an easy and transparent way to specify a recoding of an existing nominal dataset. The specification of the recoding-decisions is preferably saved in an easily accessible YAML-file. There are utility function for writing and reading such files included.

The \code{\link{tokenize}} function allows for an easy and transparent way to specify the tokenization of a string. The specification of the graphemes, replacements, and possibly extra rules are read from an orthography profile. There are utility functions for writing and reading orthography files included.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\keyword{ package }