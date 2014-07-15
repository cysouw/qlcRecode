# access to morphessor via command line tool

morfessor <- function(data) {
  cat(data, file = "datatmp", sep = "\n")
  result <- system("/Applications/Morfessor-2.0.1/build/scripts-2.7/morfessor -t datatmp -T datatmp", intern = TRUE)
  file.remove("datatmp")
  return(result)
}

# using snowball via SnowballC
#
# library(SnowballC)
# getStemLanguages()
# wordStem(words, language="dutch")