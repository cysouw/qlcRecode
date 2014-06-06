# visual help function: expand values for combination of attributes

expandValues <- function(attributes, data) {
  combination <- expand.grid(
    sapply( attributes, function(x){ levels(data[,x]) }, simplify = FALSE )
  )
  combination <- apply(combination,1,function(x){paste(x, collapse = " + ")})
  names(combination) <- 1:length(combination)
  return(as.list(combination))
}

write.recoding.template <- function(attributes, data, file, yaml = TRUE) {

  # prepare the template for one attribute
  makeTemplate <- function(attribute, data) {
    if (length(attribute) > 1) {
      originalValues <- expandValues(attribute, data)
    } else {
      originalValues <- levels(data[,attribute])
    }
    return(list(
      attribute = NULL,
      values = list(NULL,NULL),
      link = NULL,
      recodingOf = attribute,
      originalValues = originalValues
      ))
  }
  
  # combine all templates
  attributes <- as.list(sapply(attributes,function(x){colnames(data)[x]}))
  result <- list(
    title = NULL,
    author = NULL,
    date = format(Sys.time(),"%Y-%m-%d"),
    original_data = NULL,
    recoding = sapply(attributes, function(x) { makeTemplate(x, data) }, simplify = FALSE)
    )
  
  # return the result, defaults to a yaml-file
  if (yaml) {
    if (is.null(file)) {
      stop("please specify file")
    }
    cat(as.yaml(result), file = file)
  } else {
    return(result)
  }
}

read.recoding <- function(recoding, file = NULL, data = NULL) {
  
  # recodings can be a file as input
  if (is.character(recoding)) {
    infile <- yaml.load_file(recoding)
    meta <- infile[-which(names(infile)=="recoding")]
    recoding <- infile$recoding  
  } else {
    if (!is.null(recoding$recoding)) {
      meta <- recoding[-which(names(recoding)=="recoding")]
      recoding <- recoding$recoding      
    } else {
      meta <- NULL
    }
  }
  
  # Allow for various shortcuts in the writing of recodings
  reallabels <- c("attribute", "values", "link", "recodingOf", "originalValues", "doNotRecode")
  for (i in 1:length(recoding)) {
    # write labels in full
    names(recoding[[i]]) <- reallabels[pmatch(names(recoding[[i]]),reallabels)]    
  
    if (is.null(recoding[[i]]$doNotRecode)) {
      # recodingOf is necessary
      if (is.null(recoding[[i]]$recodingOf)) {
        stop(paste("Specify **recodingOf** for recoding number",number,sep = " "))
      }
      # with no link, add doNotRecode
      if (is.null(recoding[[i]]$link)) { 
        recoding[[i]] <- list(doNotRecode = recoding[[i]]$recodingOf)
      } else {
        recoding[[i]]$link <- as.integer(recoding[[i]]$link)
        # make attribute and value names if necessary
        if (is.null(recoding[[i]]$attribute)) {
          recoding[[i]]$attribute <- paste("Att", i, sep = "")
        }
        if (is.null(recoding[[i]]$values)) {
          recoding[[i]]$values <- paste("val", 1:length(recoding[[i]]$link), sep = "")
        }
      }
    }
    # when data is specified, add names of original attributes and original values
    # this leads to nicer documentation of the recoding
    if (!is.null(data)) {
      if (is.numeric(recoding[[i]]$recodingOf)) {
        recoding[[i]]$recodingOf <- colnames(data)[recoding[[i]]$recodingOf]
      }
      if (length(recoding[[i]]$recodingOf) == 1) {  
        recoding[[i]]$originalValues <- levels(data[,recoding[[i]]$recodingOf])
      }
      if (length(recoding[[i]]$recodingOf) > 1) {
        recoding[[i]]$originalValues <- expandValues(recoding[[i]]$recodingOf, data)
      }
      if (is.numeric(recoding[[i]]$doNotRecode)) {
        recoding[[i]]$doNotRecode <- colnames(data)[recoding[[i]]$doNotRecode]
      }    
    }
    # put everything in the same order
    recoding[[i]] <- recoding[[i]][reallabels]
    recoding[[i]] <- recoding[[i]][na.omit(names(recoding[[i]]))]
  }
  
  # return result
  if (is.null(file)) {
    return(recoding)
  } else {
    # add metadata and write out as yaml
    if (!("date" %in% names(meta))) {
      meta <- c(list(date = format(Sys.time(),"%Y-%m-%d")), meta)
    }
    if (!("author" %in% names(meta))) {
      meta <- c(list(author = NULL), meta)
    }
    if (!("title" %in% names(meta))) {
      meta <- c(list(title = NULL), meta)
    }
    outfile <- c(meta, list(recoding = recoding))
    cat(as.yaml(outfile), file = file)
  }
}

recode <- function(data,recoding) {

  # expand the possible shortcuts in the formulation of a recoding
  recoding <- read.recoding(recoding)
  
  # recoding of a single new attribute
  makeAttribute <- function(recoding) {

    # when doNotRecode is specified, do not recode attributes
    if (!is.null(recoding$doNotRecode)) {
      newAttribute <- data[,recoding$doNotRecode, drop = FALSE]
    } else {
      
      # simple when it is based on a single old attribute
      if (length(recoding$recodingOf) == 1) {
      newAttribute <- data[,recoding$recodingOf, drop = FALSE]
      levels(newAttribute[,1]) <- recoding$values[recoding$link]
      colnames(newAttribute) <- recoding$attribute
      return(newAttribute)
      } else {
        
        # a bit more complex for combinations of attributes
        # this can probably be made more efficient!
        newAttribute <- data[,recoding$recodingOf, drop = FALSE]
        newAttribute <- apply(newAttribute,1,function(x){paste(x, collapse = " + ")})
        match <- expand.grid(
          sapply(recoding$recodingOf, function(x){ levels(data[,x]) }, simplify = FALSE )
          )
        match <- apply(match,1,function(x){paste(x, collapse = " + ")})
        newAttribute <- factor(newAttribute, levels = match)
        levels(newAttribute) <- recoding$values[recoding$link]
        newAttribute <- as.data.frame(newAttribute)
        colnames(newAttribute) <- recoding$attribute
        return(newAttribute)
      }
    }
  }
  
  # Make the recoding and return result
  result <- as.data.frame(sapply(recoding, makeAttribute,simplify=F))
  return(result)
}

