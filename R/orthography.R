# Orthography Profiles
# An orthography profile is assumed to be a list of (1) a character matrix and (2) some regex-expression as a list

# =================================================================
# write orthography profile with frequencies for further processing
# =================================================================

write.orthography.profile <- function(strings, file = NULL) {

  # add space
  strings <- gsub(""," ",strings)
  # remove space before combining and diacritics
  strings <- stri_replace_all_regex(strings, " (\\p{Lm}|\\p{DIACRITIC}|\\p{EXTENDER})", "$1")
  # remove empty spaces at start and end
  strings <- stri_trim_both(strings)
  # split everything by space
  strings <- unlist(strsplit(strings," "))
  # compute statistics
  summary <- table(strings)
  
  # return statistics as table, or write to file when "file" is specified
  if (is.null(file)) {
    return(summary)
  } else {
    export <- cbind(names(summary), names(summary), summary)
    colnames(export) <- c("Graphemes", "Replace", "Frequency")
    write.table(export, file = "~/Desktop/test.txt", quote = FALSE, sep = "\t", row.names = FALSE)
  }
}
  
  
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
  if (file.exists(rulesFile)) {
    rules <- scan(rulesFile, , what = "character", sep = "\n")
    rules <- rules[grep("^[^#]", rules)]
    rules <- strsplit(rules, split = ", ")
  } else {
    rules  <- NULL
  }
  
  # return orthography profile as a list of two
  return( list(graphs = graphs, rules = rules) )
}

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
  graph_order <- order(graphs_length, decreasing = TRUE)
  
  # check for missing graphems in orthography profile
  # and take care of multigraphs
  # just take some high unicode range and replace all graphemes with individual characters
  check <- strings
  for (i in graph_order) { 
    check <- gsub(graphs[i],"",check)
    strings <- gsub(pattern = graphs[i], replacement = intToUtf8(1110000 + i), strings, fixed = TRUE)    
  }
  
  # check for missing graphems in orthography profile and produce warning
  leftover <- check != ""
  if (sum(leftover) > 0) {
    warning("There are characters in the data that are not in the orthography profile. 
    Check output$warnings for a table with all problematic strings.")
    problems <- cbind(strings[leftover],check[leftover])
    colnames(problems) <- c("original strings","unmatched parts")
    rownames(problems) <- which(leftover)
  }
  
  # parse strings
  # and put them back with separator
  strings <- strsplit(strings, split = "")  
  strings <- sapply(strings, function(x){paste(x, collapse = sep)})
  
  # replace orthography if specified
  if (!is.null(replace)) {
    graphs <- orthography.profile$graphs[,replace]
    graphs <- stri_trans_nfd(graphs)
    graphs[graphs == "NULL"] <- ""
  }
  
  # put back the multigraphs-substitution characters
  for (i in graph_order) {
    strings <- gsub(pattern = intToUtf8(1110000 + i), replacement = graphs[i], strings, fixed = TRUE)
  }
  
  # remove superfluous spaces and boundary symbols
  strings <- gsub(pattern = paste("^", sep, sep = ""), replacement = "", strings)
  strings <- gsub(pattern = paste(sep, "$", sep = ""), replacement = "", strings)
  strings <- gsub(pattern = paste(sep, sep, "+", sep = ""), replacement = sep, strings)
  strings <- gsub(pattern = paste(boundary, "(", sep, boundary, ")+", sep = ""), replacement = boundary, strings)  
  
  # apply regexes when specified in the orthography profile
  # this does not yet work as expected
  if(!is.null(orthography.profile$rules)) {
    for (i in orthography.profile$rules) {
      strings <- gsub(pattern = i[1], replacement = i[2], strings)
    }
  }
  
  # return parsed strings, possibly with warnings of unmatch strings
  if (sum(leftover) == 0) {
    return(strings)
  } else {
    return( list(parsed = strings, warnings = problems) )
  }
}
