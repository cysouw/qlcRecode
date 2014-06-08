# recoding WALS

library(qlcRecode)

# reading data
wals <- read.csv("../sources/language.csv",sep=",",row.names=1,header=T,na.strings="",comment.char="")
meta <- wals[,1:7]
data <- wals[,8:ncol(wals)]

# preparing recoding for phonological features only

phonFeat <- 1:19

write.recoding.template(
	attributes = phonFeat, 
	file = "1. WALStemplate.yml",
	data = data
	)

# nothing is said to be changed in the bare template
# so nothing is recoded

all.equal(data[,phonFeat], recode(data,"1. WALStemplate.yml")) # TRUE

# just taking the empty template and cleaning it

read.recoding(
	recoding = "1. WALStemplate.yml",
	file = "2. EmptyRecodingWALS.yml",
	data = data
	)

# looks are changed, but still nothing is recoded

all.equal(data[,phonFeat], recode(data,"2. EmptyRecodingWALS.yml")) # TRUE
	
# The template has now been filled out
# metadata is added (note that there are even more kinds of information now)
# normalise this manually work: this is also interesting to check consistency

read.recoding(
	recoding = "3. WALSrecoding.yml",
	file = "4. CleanWALSrecoding.yml",
	data = data
	)

# 19 original attributes are now 31	
walsRecoded <- recode(data,"4. CleanWALSrecoding.yml")
summary(walsRecoded)

# Purely binary characteristics

read.recoding(
	recoding = "5. WALSbinary.yml",
	file = "6. CleanWALSbinary.yml",
	data = data
	)

walsBinary <- recode(data,"6. CleanWALSbinary.yml")
summary(walsBinary)

# analyse with qlcMatrix
library(qlcMatrix)

att <- sim.att(walsBinary, method="chuprov")
plot(hclust(as.dist(-att),method="average"),cex=.7)
heatmap(as.matrix(att)^0.5, symm = TRUE)


