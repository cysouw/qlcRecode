# Orthography Profiles
# An orthography profile is assumed to be a list of (1) a character matrix and (2) some regex-expression as a list

# =================================================================
# write orthography profile with frequencies for further processing
# =================================================================

write.orthography.profile <- function(strings, replacements = TRUE, sep = NULL, file = NULL, info = TRUE) {

  # split using unicode definitions, except when 'sep' is specified, then split by sep
  if (is.null(sep)) {
    splitted <- stri_split_boundaries(strings, boundary = "character")
  } else {
    splitted <- strsplit(strings, sep)
  }
    
  # prepare result
  summary <- table(unlist(splitted))
  chars <- names(summary)
  
  # add a column for editing replacements when 'replacements = TRUE'
  if (replacements) {
    result <- cbind(chars, chars, summary)
    colnames(result) <- colnames(result) <- c("graphemes", "replacements", "frequency")
  } else {
    result <- cbind(chars, summary)
    colnames(result) <- colnames(result) <- c("graphemes", "frequency")   
  }
  rownames(result) <- NULL

  # add codepoints and Unicode names when info = TRUE
  if (info) {    
    codepoints <- sapply(chars, function(x) {
      paste(stri_trans_general(unlist(strsplit(x,"")), "Any-Hex/Unicode"), collapse = ", ")
    })
    names <- sapply(chars, function(x) {
      paste(stri_trans_general(unlist(strsplit(x,"")), "Any-Name"), collapse = ", ")
    })
    names <- gsub("\\N{", "", names, fixed= TRUE)
    names <- gsub("}", "", names, fixed = TRUE)
    result <- cbind(result, codepoints, names)
  }

  # return statistics as data frame, or write to file when "file" is specified
  if (is.null(file)) {
    return(as.data.frame(result))
  } else {
    write.table(result, file = file, quote = FALSE, sep = "\t", row.names = FALSE)
  }
}
  
# ========================
# read orthography profile
# ========================

read.orthography.profile <- function(file, 
                                     graphemes = "graphemes", 
                                     patterns = "patterns", 
                                     replacements = "replacements") {

  # prepare naming of files
  if (substr(file, nchar(file)-2, nchar(file)) == "prf") {
    filename <- substr(file, 1, nchar(file)-4)
  } else {
    filename <- file
  }
  
  # prepare table
  graphemesFile <- paste(filename, ".prf", sep = "")
  if (file.exists(graphemesFile)) {
    graphs <- read.table(graphemesFile, sep = "\t", header = TRUE, 
                     colClasses = "character", quote = "", fill = TRUE)
    graphs <- graphs[, c(graphemes, replacements), drop = FALSE]
  } else {
    graphs = NULL
  }

  # prepare rules
  rulesFile <- paste(filename, ".rules", sep = "")
  if (file.exists(rulesFile)) {
    rules <- read.table(rulesFile, sep = "\t", header = TRUE,
                        colClasses = "character", quote = "")
    rules <- rules[, c(patterns, replacements), drop = FALSE]
  } else {
    rules  <- NULL
  }
  
  # return orthography profile as a list of two
  return( list(graphs = graphs, rules = rules) )
}

# ================
# tokenize strings
# ================

tokenize <- function(strings, orthography.profile = NULL, 
                     graphemes = "graphemes", patterns = "patterns", replacements = NULL,
                     sep = "\u00B7", normalize = "NFC", 
                     traditional.output = TRUE, file = NULL) {

  # normalization
  if (normalize == "NFC" | normalize == "nfc") {
    transcode <- stri_trans_nfc
  } else if (normalize == "NFD" | normalize == "nfd") {
    transcode <- stri_trans_nfd
  } else {
    transcode <- identity
  }
  
  # prepare strings, and normalize NFC everything by default
  originals <- as.vector(strings)
  strings <- transcode(originals)
  
  # read orthography profile (or make new one)
  if (is.null(orthography.profile)) {
    # make new orthography profile   
    graphs  <- write.orthography.profile(strings)[, graphemes, drop = FALSE]
    profile <- list(graphs = graphs, rules = NULL)    
  } else if (is.character(orthography.profile)) {
    # read profile from file
    profile <- read.orthography.profile(orthography.profile, graphemes, patterns, replacements)
  } else {
    # in case orthography profile is an R object
    profile <- orthography.profile
  }
  
  # do grapheme-splitting
  if(!is.null(profile$graphs)) {
    # normalise characters in profile, just to be sure
    graphs <- transcode(profile$graphs[,graphemes])
  
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
      strings <- gsub(pattern = graphs[i]
                      , replacement = intToUtf8(1110000 + i)
                      , strings, fixed = TRUE)    
    }
    
    # check for missing graphems in orthography profile and produce warning
    check <- stri_replace_all_regex(check, "(\\p{DIACRITIC})", " $1")
    leftover <- check != ""
    if (sum(leftover) > 0) {
      warning("There are characters in the data that are not in the orthography profile. Check warnings for a table with all problematic strings.")
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
      graphs <- profile$graphs[,replacements]
      graphs <- transcode(graphs)
      graphs[graphs == "NULL"] <- ""
    }
    
    # put back the multigraphs-substitution characters
    for (i in graph_order) {
      strings <- gsub(pattern = intToUtf8(1110000 + i)
                      , replacement = graphs[i]
                      , strings, fixed = TRUE)
    }
    
    # remove superfluous spaces at start and end
    strings <- gsub(pattern = paste("^", sep, sep = ""), replacement = "", strings)
    strings <- gsub(pattern = paste(sep, "$", sep = ""), replacement = "", strings)

  } else {
    # with no graphemes-specified, nothing is parsed
    # also no error message returned
    leftover <- 0
  }

  # make traditional output when asked
  if (traditional.output){
    strings <- gsub(" ","#",strings)
    strings <- gsub(sep," ",strings)
    sep <- " # | "
  }

  # apply regexes when specified in the orthography profile
  # this does not yet completely work as expected
  if(!is.null(profile$rules) & !is.null(replacements)) {
    for (i in 1:nrow(profile$rules) ) {
      regex <- transcode(as.character(profile$rules[i,]))
      strings <- gsub(pattern = regex[1], replacement = regex[2], strings)
    }
  }
  
  # prepare results
  # first: combine orignal strings and tokenized strings in a dataframe
  tokenization <- as.data.frame(cbind(originals = originals, tokenized = strings))
  profile <- write.orthography.profile(strings, sep = sep, info = TRUE)
  
  # various options for output
  if (is.null(file)) {
    # output as R object (list)
       
    if (is.null(orthography.profile)) {
      # user didn't specify an orthography profile, so give one in return
      return(list(strings = tokenization, orthography.profile = profile, warnings =  NULL))

    } else {
      if (is.null(replacements)) {
      # with OP, but without replacements, an orthography profile is returned as well
        if (sum(leftover) == 0 ) {
         # no errors, return tokenization and OP
          return(list(strings = tokenization, orthography.profile = profile, warnings = NULL))
        } else { 
        # with errors, add table with errors
        return(list(strings = tokenization, orthography.profile = profile, warnings = problems))
        } 
      } else {
        # with replacements, no orthography profile can be returned in a sensible way
        if (sum(leftover) == 0 ) {
          # no errors, just return tokenization
          return(list(strings = tokenization, orthography.profile = NULL, warnings = NULL))
        } else {
          # with errors, add table with errors
          return(list(strings = tokenization, orthography.profile = NULL, warnings = problems))
        }
      }
    }

  } else {
    # output as file(s)
    
    # file with tokenization is always returned
    write.table(tokenization
                , file = paste(file, ".txt", sep = "")
                , quote = FALSE, sep = "\t", row.names = FALSE)
    
    if (is.null(replacements)) {
      # additionally write orthography profile
      write.orthography.profile(strings, sep = sep, info = TRUE
                                , file = paste(file, ".prf", sep=""))
    }
    
    if (sum(leftover) > 0 ) {
      # additionally write table with warnings
      write.table(problems
                  , file = paste(file, "_warnings.txt", sep = "")
                  , quote = FALSE, sep = "\t", row.names = FALSE)
    }
  }
}  
 

# allow alternative spelling
tokenise <- tokenize
