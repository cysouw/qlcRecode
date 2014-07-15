\name{morfessor}
\alias{morfessor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Morfessor morphological analysis
}
\description{
Access Morfessor via command line
}
\usage{
morfessor(data)
}
\arguments{
  \item{data}{
vector of strings to be separated into morphemes
}
}
\details{
Location of bash executable linking to Morfessor is currently hardcoded. A temporary file with the data is made at \code{getwd()} and deleted afterwards.
}
\value{
Returns a vector of the same lengths as \code(data) with morphemes separated by spaces.
}
\references{
Morfessor: \url{http://www.cis.hut.fi/projects/morpho/morfessor2.shtml}
}
\author{
Michael Cysouw
}
\examples{
morfessor(c("test","testing","tested"))
}
