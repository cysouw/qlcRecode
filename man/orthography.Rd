\name{orthography}
\alias{orthography}
\alias{tokenize}
\alias{tokenise}
\alias{write.orthography.profile}
\alias{read.orthography.profile}
\title{
Tokenization of character strings based on an orthography profile
}
\description{
To process strings it is often very useful to tokenise them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations (`transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile.
}
\usage{
tokenize(strings, orthography.profile, 
  graphemes = "graphemes", replacements = NULL,
  sep = " ", boundary = "#", add.boundary = FALSE)

read.orthography.profile(file, graphemes = "graphemes", replacements = NULL)
write.orthography.profile(strings, file = NULL, sep = " ")
}
\arguments{
  \item{strings}{
  vector of strings to the tokenized.
  }
  \item{orthography.profile}{
  orthography profile specifying the graphemes for the tokenization, and possibly any replacements of the available graphemes. Can be a filename or an object as returned by \code{read.orthography.profile}.
  }
  \item{graphemes}{
  name (or number) of the column in the orthography profile listing the graphemes to be used for the tokenization
  }
  \item{replacements}{
  name (or number) of the column in the orthography profile listing the replacements. If NULL, then nothing is replaced.
  }
  \item{file}{
  filename for orthography profile to be read or written.
  }
  \item{sep}{
  separator to be inserted between graphemes. Defaults to space.
  }
  \item{boundary}{
  boundary symbol to be inserted into the tokenized strings. If the separator (see above) is present in the original string, this original separator is replaced by the boundary symbol. Defaults to hash (#).
  }
  \item{add.boundary}{
  logical: should a boundary symbol be added to the start and the end of all strings? Defaults to \code{FALSE}
  }
  \item{ignore}{
  elements to be ignored while preparing orthography profile. Can be any regular expression, but defaults to space.
  }
} 
\details{
To produce an orthography profile, consider using \code{write.orthography.profile} to produce a useful starting point. This function will take a vector of strings and produce a table with all possible graphemes and their frequency. Combining diacritics and spacing modifier letters are combined with their preceding characters to obtain a reasonable first guess at available graphemes. There is no attempt made to recognize real multigraphs (like 'sch' of 'aa'). Such multigraphs can be specified manually in the output file of this function.

The function \code{read.orthography.profile} can be used to read any saved orthography profile into R, though mostly this will be used internally by \code{tokenize}. An orthography profile currently consists of minimally a csv-table with a column of graphemes to be separated, typically using a \code{.prf} suffix. Further columns with replacements can be specified. When further rule-based changes are needed (for complex orthographic regularities), these can be specified as regex 'search, replace' strings in a separate file with the same name, but using a \code{.rules} suffix.

The central functionality is found in the \code{tokenize} function. This function will tokenize (and possibly replace) the strings into graphemes. Any rules specified in the \code{.rules} file will be applied first, and then the graphemes from the \code{.prf} table will be tokenized, starting from the largest graphemes (in unicode-code-point count using NFC normalisation). Any unmatched sequences in the data will be reported with a warning
}
\value{
Without warnings, \code{tokenize} will return a vector with the tokenized strings in NFC normalisation. When a warning occurs, a list of two elements is returned:
\item{parsed}{the vector with the parsed strings}
\item{warnings}{a table with all original strings and the unmatched parts}
}
\author{
Michael Cysouw
}
\examples{
# produce statistics
example <- "nana änngschä ach"
write.orthography.profile(example)

# make a better orthography profile
gr <- cbind(c("a","ä","n","ng","ch","sch"),c("a","e","n","N","x","sh"))
colnames(gr) <- c("graphemes","replace")
( op <- list(graphs = gr, rules = NULL) )

# tokenization
tokenize("nana änngschä ach", op, graphemes = "graphemes")
# with replacements and an error message
tokenize("Naná änngschä ach", op, graphemes = "graphemes", replace = "replace")
}
