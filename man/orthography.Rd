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
tokenize(strings, normalize = "NFC",
  orthography.profile = NULL, graphemes = NULL, replacements = NULL,
  sep = "\u00B7", traditional.output = FALSE, file = NULL) 

read.orthography.profile(file, graphemes = "graphemes", replacements = NULL)
write.orthography.profile(strings, sep = NULL, file = NULL, info = FALSE)
}
\arguments{
  \item{strings}{
  vector of strings to the tokenized.
  }
  \item{normalize}{
  which normalization to use, defaults to "NFC". Other option is "NFD". Any other input will result in no normalisation being performed.
  }
  \item{orthography.profile}{
  orthography profile specifying the graphemes for the tokenization, and possibly any replacements of the available graphemes. Can be a filename or an object as returned by \code{read.orthography.profile}. If NULL then the orthography profile will be created on the fly using the defaults of \code{write.orthography.profile}.
  }
  \item{graphemes}{
  name (or number) of the column in the orthography profile listing the graphemes to be used for the tokenization
  }
  \item{replacements}{
  name (or number) of the column in the orthography profile listing the replacements. If NULL, then nothing is replaced.
  }
  \item{file}{
  filename for results to be written (or orthography profile to be read).
  }
  \item{sep}{
  with the function \code{tokenize}, this is the separator to be inserted between graphemes. Defaults to U+00B7 ("MIDDLE DOT"). With the function \code{write.orthography.profile}, this is the separator to separate the strings. When NULL here (by default), unicode character definitions are used to split.
  }
  \item(traditional.output){
  logical, defaults to FALSE. When TRUE, then the tokenized strings are presented with spaces between graphemes and space-hash-space (\code{" # "}) instead of spaces in the original strings.
  }
  \item{info}{
  logical: should extra Unicode-info (codepoints and Unicode names) be added in the generation of an orthography profile? Defaults to \code{FALSE}.
  }
  }
} 
\details{
To produce an orthography profile, consider using \code{write.orthography.profile} to produce a useful starting point. This function will take a vector of strings and produce a table with all graphemes and their frequency. Combining diacritics and spacing modifier letters are combined with their preceding characters to obtain a reasonable first guess at available graphemes. There is no attempt made to recognize 'tailored' multigraphs (like 'sch' of 'aa'). Such multigraphs can be specified manually in the output file of this function.

The function \code{read.orthography.profile} can be used to read any saved orthography profile into R, though mostly this function will be used internally by \code{tokenize}. An orthography profile currently consists of minimally a csv-table with a column of graphemes to be separated, typically using a \code{.prf} suffix. Further columns with replacements can be specified. When further rule-based changes are needed (for complex orthographic regularities), these can be specified as regex 'search, replace' strings in a separate file with the same name, but using a \code{.rules} suffix.

The central functionality is found in the \code{tokenize} function. This function will tokenize (and possibly replace) the strings into graphemes. Any rules specified in the \code{.rules} file will be applied first, and then the graphemes from the \code{.prf} table will be tokenized, starting from the largest graphemes (in unicode-code-point count using NFC normalisation). Any unmatched sequences in the data will be reported with a warning
}
\value{
Without warnings, \code{tokenize} will return a vector with the tokenized strings in NFC normalisation. Note that any NFD (or other) encoding in the original data is turned into NFC. 

When a warning occurs, a list of two elements is returned:
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
