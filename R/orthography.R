# Orthography Profiles
# An orthography profile is assumed to be a list of (1) a character matrix and (2) some regex-expression as a list

# =================================================================
# write orthography profile with frequencies for further processing
# =================================================================

write.orthography.profile <- function(strings, file = NULL, ignore = " "
	, clustering = c("Lm", "DIACRITIC", "EXTENDER")) {

  # remove space (by default)
  strings <- gsub(ignore,"",strings)
  # add space
  strings <- gsub(""," ",strings)
  # remove space before combining and diacritics
  cluster_regex <- paste0(" (\\p{",paste(clustering, collapse = "}|\\p{"),"})")
  strings <- stri_replace_all_regex(strings, cluster_regex, "$1")
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
    colnames(export) <- c("graphemes", "replace", "frequency")
    write.table(export, file = file, quote = FALSE, sep = "\t", row.names = FALSE)
  }
}
  
# ========================
# read orthography profile
# ========================

read.orthography.profile <- function(file, graphemes = "graphemes", replacements = NULL) {

  # prepare naming of files
  if (substr(file, nchar(file)-2, nchar(file)) == "prf") {
    filename <- substr(file, 1, nchar(file)-4)
  } else {
    filename <- file
  }
  
  # prepare table
  graphemesFile <- paste(filename, ".prf", sep = "")
  if (file.exists(graphemesFile)) {
    op <- read.table(graphemesFile, sep = "\t", header = TRUE, 
                     colClasses = "character", quote = "", fill = TRUE)
    graphs <- op[, c(graphemes, replacements), drop = FALSE]
  } else {
    graphs = NULL
  }

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
                     graphemes = "graphemes", replacements = NULL,
                     sep = " ", boundary = "#", add.boundary = FALSE) {

  # prepare strings, and normalise nfc, just to be sure
  originals <- as.vector(strings)
  strings <- stri_trans_nfc(strings)
  strings <- gsub(pattern = sep, replacement = boundary, x = strings)
  if (add.boundary) {
    strings <- paste(boundary, strings, boundary, sep = "")
  }
  
  # read orthography profile
  if (is.character(orthography.profile)) {
    orthography.profile <- read.orthography.profile(
      orthography.profile, graphemes = graphemes, replacements = replacements)
  }
  
  # do grapheme-splitting and normalise nfc, just to be sur
  if(!is.null(orthography.profile$graphs)) {
    graphs <- orthography.profile$graphs[,graphemes]
    graphs <- stri_trans_nfc(graphs)
  
    # order graphs to size
    graphs_parts <- strsplit(graphs, split = "")
    graphs_length <- sapply(graphs_parts, length)
    graph_order <- order(graphs_length, decreasing = TRUE)
  
    # check for missing graphems in orthography profile
    # and take care of multigraphs
    # just take some high unicode range and replace all graphemes with individual characters
    check <- strings
    for (i in graph_order) { 
      check <- gsub(graphs[i],"",check, fixed = TRUE)
      strings <- gsub(pattern = graphs[i], replacement = intToUtf8(1110000 + i), 
                      strings, fixed = TRUE)    
    }
    
    # check for missing graphems in orthography profile and produce warning
    check <- gsub(boundary,"",check)
    check <- stri_replace_all_regex(check, "(\\p{DIACRITIC})", " $1")
    leftover <- check != ""
    if (sum(leftover) > 0) {
      warning("There are characters in the data that are not in the orthography profile. 
              Check $warnings for a table with all problematic strings.")
      problems <- cbind(originals[leftover],check[leftover])
      colnames(problems) <- c("original strings","unmatched parts")
      rownames(problems) <- which(leftover)
    }
    
    # parse strings
    # and put them back with separator
    strings <- strsplit(strings, split = "")  
    strings <- sapply(strings, function(x){paste(x, collapse = sep)})
    
    # replace orthography if specified
    if (!is.null(replacements)) {
      graphs <- orthography.profile$graphs[,replacements]
      graphs <- stri_trans_nfc(graphs)
      graphs[graphs == "NULL"] <- ""
    }
    
    # put back the multigraphs-substitution characters
    for (i in graph_order) {
      strings <- gsub(pattern = intToUtf8(1110000 + i), replacement = graphs[i], 
                      strings, fixed = TRUE)
    }
    
    # remove superfluous spaces and boundary symbols
    strings <- gsub(pattern = paste("^", sep, sep = ""), replacement = "", strings)
    strings <- gsub(pattern = paste(sep, "$", sep = ""), replacement = "", strings)
    strings <- gsub(pattern = paste(sep, sep, "+", sep = ""), replacement = sep, strings)
    strings <- gsub(paste(boundary, "(", sep, boundary, ")+", sep = ""), boundary, strings)  

  } else {
    # with no graphemes-specified, nothing is parsed
    # also no error message returned
    leftover <- 0
  }
    
  # apply regexes when specified in the orthography profile
  # this does not yet completely work as expected
  if(!is.null(orthography.profile$rules)) {
    for (i in orthography.profile$rules) {
      i <- stri_trans_nfc(i)
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

tokenise <- tokenize
