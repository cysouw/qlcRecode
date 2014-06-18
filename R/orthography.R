# Orthography Profiles
# An orthography profile is assumed to be a list of (1) a character matrix and (2) some regex-expression

# write.orthography.profile

# ========================
# read orthography profile
# ========================

read.orthography.profile <- function(file, graphemes = "Graphemes", replace = NULL) {

  # prepare naming of files
  if (substr(file, nchar(file)-2, nchar(file)) == "prf") {
    filename <- substr(file, 1, nchar(file)-4)
  } else {
    filename <- file
  }
  
  # prepare table
  tableFile <- paste(filename, ".prf", sep = "")
  op <- read.table(tableFile, sep = "\t", header = TRUE, colClasses = "character", quote = "", fill = TRUE)
  graphs <- op[, c(graphemes, replace), drop = FALSE]

  # prepare rules
  rulesFile <- paste(filename, ".rules", sep = "")
  if (!is.null(rulesFile)) {
    rules <- NULL #do something here
  } else {
    rules  <- NULL
  }
  
  # return orthography profile as a list of two
  return( list(graphs = graphs, rules = rules) )
}

# shortcut
read.op <- read.orthography.profile

# ================
# tokenize strings
# ================

tokenize <- function(strings, orthography.profile, 
                     graphemes = "Graphemes", replace = NULL,
                     sep = " ", boundary = "#", add.boundary = FALSE ) {

  # prepare strings, and normalise nfd, just to be sure
  strings <- stri_trans_nfd(strings)
  strings <- gsub(pattern = sep, replacement = boundary, x = strings)
  if (add.boundary) {
    strings <- paste(boundary, strings, boundary, sep = "")
  }
  
  # read orthography profile and normalise nfd, just to be sure
  if (is.character(orthography.profile)) {
    orthography.profile <- read.orthography.profile(orthography.profile, 
                                                    graphemes = graphemes, replace = replace)
  }
  graphs <- orthography.profile$graphs[,graphemes]
  graphs <- stri_trans_nfd(graphs)

  # order graphs to size
  graphs_parts <- strsplit(graphs, split = "")
  graphs_length <- sapply(graphs_parts, length)
  replacing <- order(graphs_length, decreasing = TRUE)
  
  # check for missing graphems in orthography profile
  check <- strings
  for (i in replacing) {
    check <- gsub(graphs[i],"",check)
  }
  leftover <- check != ""
  if (sum(leftover) > 0) {
    warning("There are characters in the data that are not in the orthography profile. 
    Check output$warnings for a table with all problematic strings.")
    problems <- cbind(strings[leftover],check[leftover])
    colnames(problems) <- c("original strings","unmatched parts")
    rownames(problems) <- which(leftover)
  }
 
  # take care of multigraphs  
  for (i in replacing) {
    # just take some high unicode range
    strings <- gsub(pattern = graphs[i], replacement = intToUtf8(1110000 + i), strings)
  }
  
  # parse strings
  parsed_strings <- strsplit(strings, split = "")
  # put back with separator
  parsed_strings <- sapply(parsed_strings, function(x){paste(x, collapse = sep)})
  rm(strings)

  # put back the multigraphs
  for (i in replacing) {
    parsed_strings <- gsub(pattern = intToUtf8(1110000 + i), replacement = graphs[i], parsed_strings)
  }
  
  # replace orthography
  if (!is.null(replace)) {
    replace <- orthography.profile$graphs[,replace]
    replace <- stri_trans_nfd(replace)
    for (i in replacing) {
      parsed_strings <- gsub(pattern = graphs[i], replacement = replace[i], parsed_strings)
    }
  }
  
  # return parsed strings, possibly with warnings of unmatch strings
  if (sum(leftover) == 0) {
    return(parsed_strings)
  } else {
    return( list(parsed = parsed_strings, warnings = problems) )
  }
}








