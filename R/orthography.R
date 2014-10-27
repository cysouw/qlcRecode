# Orthography Profiles
# An orthography profile is assumed to be a list of (1) a character matrix and (2) some regex-expression as a list

# =================================================================
# write orthography profile with frequencies for further processing
# =================================================================

write.orthography.profile <- function(strings, sep = NULL, file = NULL, info = FALSE) {

  if (is.null(sep)) {
    splitted <- stri_split_boundaries(strings, boundary = "character")
  } else {
    splitted <- strsplit(strings, sep)
  }
  summary <- table(unlist(splitted))
  
  # prepare result
  chars <- names(summary)
  result <- cbind(chars, chars, summary)
  colnames(result) <- colnames(result) <- c("graphemes", "replacements", "frequency")
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

tokenize <- function(strings, normalize = "NFC",
                     orthography.profile = NULL, graphemes = NULL, replacements = NULL,
                     sep = "\u00B7", traditional.output = FALSE, file = NULL) {

  # normalization
  if (normalize == "NFC" | normalize == "nfc") {
    transcode <- stri_trans_nfc
  } else if (normalize == "NFD" | normalize == "nfd") {
    transcode <- stri_trans_nfd
  } else {
    transcode <- identity
  }
  
  # prepare strings, and normalize NFC everything always
  originals <- as.vector(strings)
  strings <- transcode(originals)
  
  # read orthography profile (or make new one)
  if (is.null(orthography.profile)) {
    graphs  <- write.orthography.profile(strings)[, c("graphemes", "replacements"), drop = FALSE]
    profile <- list(graphs = graphs, rules = NULL)    
  } else if (is.character(orthography.profile)) {
    profile <- read.orthography.profile(orthography.profile
                                        , graphemes = graphemes
                                        , replacements = replacements
                                        )
  } else {
      # in case profile is an R object
      profile <- orthography.profile
  }
  
  # do grapheme-splitting
  if(!is.null(profile$graphs)) {
    # normalise, just to be sure
    graphs <- transcode(profile$graphs[,"graphemes"])
  
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
    check <- stri_replace_all_regex(check, "(\\p{DIACRITIC})", " $1")
    leftover <- check != ""
    if (sum(leftover) > 0) {
      warning("There are characters in the data that are not in the orthography profile. Check $warnings for a table with all problematic strings.")
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
      graphs <- stri_trans_nfc(graphs)
      graphs[graphs == "NULL"] <- ""
    }
    
    # put back the multigraphs-substitution characters
    for (i in graph_order) {
      strings <- gsub(pattern = intToUtf8(1110000 + i), replacement = graphs[i], 
                      strings, fixed = TRUE)
    }
    
    # remove superfluous spaces at start and end
    strings <- gsub(pattern = paste("^", sep, sep = ""), replacement = "", strings)
    strings <- gsub(pattern = paste(sep, "$", sep = ""), replacement = "", strings)

  } else {
    # with no graphemes-specified, nothing is parsed
    # also no error message returned
    leftover <- 0
  }
    
  # apply regexes when specified in the orthography profile
  # this does not yet completely work as expected
  if(!is.null(orthography.profile$rules)) {
    for (i in orthography.profile$rules) {
      i <- transcode(i)
      strings <- gsub(pattern = i[1], replacement = i[2], strings)
    }
  }

  # make traditional output when asked
  if (traditional.output){
    strings <- gsub(" ","#",strings)
    strings <- gsub(sep," ",strings)
    sep <- " # | "
  }

  # make 'empirical' orthography profile
  

  # results
  if (is.null(file)) {
    empirical.profile <- write.orthography.profile(strings, sep = sep)
    # return parsed strings, possibly with warnings of unmatch strings inside R
    if (sum(leftover) == 0) {
      return( list( strings = cbind(originals = originals, tokenized = strings)
                    , orthography.profile = empirical.profile
                    )
              )
    } else {     
      return( list( strings = cbind(originals = originals, tokenized = strings)
                    , warnings = problems
                    , orthography.profile = empirical.profile$graphs
                    )
              )
    }
  } else {
    write.table(
      cbind(originals = originals, tokenized = strings)
      , file = paste(file, ".txt", sep = "")
      , quote = FALSE, sep = "\t", row.names = FALSE
      )
    write.orthography.profile(strings, sep = sep, file = paste(file, ".prf", sep=""), info = TRUE)
    if (sum(leftover) != 0) {
      write.table(
        problems
        , file = paste(file, "_problems.txt", sep = "")
        , quote = FALSE, sep = "\t", row.names = FALSE
        )
    }
  }
}

tokenise <- tokenize
