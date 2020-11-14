#### test code of the tm package
## dataset source
# url = "http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html"

library(tm)
library(Stem)
library(RWeka)
library(quanteda)

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

preprocessData <- function(data, sampleSize)
{
    ## sample the dataset of each category
    dataSample = data[sample(c(1:length(data)), size = sampleSize, replace = F)]
    ## pre-process: remove excess blanks, lower all letters, remove non-alphabet text
    dataPostPro = stripWhitespace(
           tolower(
           gsub('[[:digit:]]+', '',
           gsub('[[:punct:]]','', unlist(strsplit(dataSample, '\\.+|\\!+|\\?+|\\,+'))))))
}

phraseList <- list(Twitter = enTwitter,Blogs = enBlogs,News = enNews)
sampleSize <-  200
sampleResult <- lapply(phraseList, function(data) preprocessData(data,sampleSize))

cat(sampleResult[[1]],file = "final/en_US/twitter_sample.txt",sep = "\n")
cat(sampleResult[[2]],file = "final/en_US/blogs_sample.txt",sep = "\n")
cat(sampleResult[[3]],file = "final/en_US/news_sample.txt",sep = "\n")

fileTwitter <- DirSource("final/en_US/", pattern = "twitter_sample.txt")
fileBlogs <- DirSource("final/en_US/", pattern = "blogs_sample.txt")
fileNews <- DirSource("final/en_US/", pattern = "news_sample.txt")
tCorpus <- VCorpus(fileTwitter, readerControl = list(language = "English"))
bCorpus <- VCorpus(fileBlogs, readerControl = list(language = "English"))
nCorpus <- VCorpus(fileNews, readerControl = list(language = "English"))

testng <- NGramTokenizer(tCorpus, Weka_control(min = 2, max = 2))

tdm.bigram <- TermDocumentMatrix(tCorpus, control = list(tokenize = testng))


getNgram <- function(corpus, ngram)
{
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
    
    tdm.ngramText <- switch (ngram,
                             '2' = TermDocumentMatrix(tCorpus, control = list(tokenize = BigramTokenizer)),
                             '3' = TermDocumentMatrix(tCorpus, control = list(tokenize = TrigramTokenizer)),
                             '4' = TermDocumentMatrix(tCorpus, control = list(tokenize = QuadgramTokenizer)),
                             stop("Only 2,3 and 4 are used for ngram model"))
    
    ngramMatrx <- as.matrix(tdm.ngramText)
}

##### findAssocs(crudeTDM, "oil", 0.85)

