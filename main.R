## dataset source
# url = "http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html"

setwd('d:/Coursera/DataScience/Capstone/')
library(tm)
library(Stem)
library(RWeka)
library(markovchain)
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)

set.seed(125)
###----- download and read data -----###
setwd('D:/Coursera/Datascience/Capstone')
url = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
destfile = "Coursera-SwiftKey.zip"    
if (!file.exists(destfile)) {
    download.file(fileURL, destfile, method="auto")
    unzip("Coursera-SwiftKey.zip", overwrite = FALSE)}

con <- file("final/en_US/en_US.twitter.txt", "r")
enTwitter <- readLines(con, -1)
close(con)
con <- file("final/en_US/en_US.blogs.txt", "r")
enBlogs <- readLines(con, -1)
close(con)
con <- file("final/en_US/en_US.news.txt", "r")
enNews <- readLines(con, -1)
close(con)

###----- Testing and Training model -----###

textAll <- c(enTwitter, enBlogs, enNews)
samplePercent = 0.0005
textSample <- textAll[sample(c(1:length(textAll)), size = length(textAll)*samplePercent, replace = F)]
indexTrain <- sample(c(1:length(textSample)), size = length(textSample)*0.8, replace = F)
textTrain <- textSample[indexTrain]
textTest <- textSample[-indexTrain]

N_Gram <- c(1:4)
for (n in N_Gram)
{
    varName <- paste("text", n, 'Gram', sep = "")
    assign(varName, preprocessData(textTrain, n))

}
textMatrixAll <- list(text1Gram$ngramMatrx, text2Gram$ngramMatrx, text3Gram$ngramMatrx, text4Gram$ngramMatrx)
smoothGT <- lapply(textMatrixAll, GoodTuring)

## Back off model


















