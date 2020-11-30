# ## dataset source
# # url = "http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html"
# 
setwd('d:/Coursera/DataScience/Capstone/')
library(tm)
library(RWeka)
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)

source('D:/Coursera/DataScience/Capstone/PreProcessFun.R')
source('D:/Coursera/DataScience/Capstone/FunGoodTuring.R')
source('D:/Coursera/DataScience/Capstone/FunPredictiveWordBackOff.R')
source('D:/Coursera/DataScience/Capstone/funCalculateProbs.R')

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

###----- week2 assignemnt -----###
countLineWord <- function(data, sampleSize)
{
    ## sample the dataset of each category
    dataSample = data[sample(c(1:length(data)), size = sampleSize, replace = F)]
    ## split line by .?! and count
    line = grep('[[:alnum:]]', unlist(strsplit(dataSample, '\\.+|\\!+|\\?+')), value = T)
    lineCount = length(line)
    ## split words and count
    word <- unlist(sapply(dataSample, function(x)
        {unlist(strsplit(cleanText(x), '\\.+|\\!+|\\?+|\\,+|\\s+'))}
        ), use.names = F)
    word <- word[grepl('[a-z]+', word)]
    wordUnique <- data.frame(table(word))
    wordOrder <- sort(wordUnique$Freq, decreasing = T, index.return = T)
    wordCount <- mutate(data.frame( wordSort = wordUnique$word[wordOrder$ix], times = wordOrder$x), freq = times/length(word))
    result <- list("line" = line, "lineCount" = lineCount, "words" = word, "wordCount" = wordCount)
}

phraseList <- list(Twitter = enTwitter,Blogs = enBlogs,News = enNews)
sampleSize <-  10000
lineWordCount <- lapply(phraseList, function(x) countLineWord(x,sampleSize = sampleSize))

dataCount <- data.frame(lineCounts = c(lineWordCount$Twitter$lineCount,
                                       lineWordCount$Blogs$lineCount,
                                       lineWordCount$News$lineCount),
                        wordCounts = c(length(lineWordCount$Twitter$words),
                                       length(lineWordCount$Blogs$words),
                                       length(lineWordCount$News$words))) %>%
    mutate(media = c("twitter", "Blogs", "News"),
           lineCounts = lineCounts/sampleSize,
           wordCounts = wordCounts/sampleSize) %>%
    gather(key = type, value = counts, - media)

## figure of words and line counts
gCount <- ggplot(dataCount, aes(x = media, y = counts, fill = media)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = 'Averaged number of line and word of three medias') +
    facet_grid(type ~. , scales = "free")
gCount

## figure of most frequent words

twitterTopTen = data.frame(word = as.character(head(lineWordCount$Twitter$wordCount$word,10)), 
                           freq  = head(lineWordCount$Twitter$wordCount$freq,10))
blogTopTen = data.frame(word = as.character(head(lineWordCount$Blogs$wordCount$word,10)), 
                        freq  = head(lineWordCount$Blogs$wordCount$freq,10))
newsTopTen = data.frame(word = as.character(head(lineWordCount$News$wordCount$word,10)), 
                        freq  = head(lineWordCount$News$wordCount$freq,10))
dataTopTen <- list(twitter = twitterTopTen, blog = blogTopTen, news = newsTopTen)

gTopTen <- lapply(dataTopTen, function(dataMedia)
{g <- ggplot(dataMedia, aes(x = reorder(word, -freq), y = freq, fill = reorder(word, -freq))) +
    geom_bar(stat = "identity",show.legend = FALSE)  +
    scale_fill_grey() +
    labs(x = 'word', y = 'frequency') +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.text.x  = element_text(size = 14)) +
    ylim(c(0, max(dataTopTen$twitter$freq, dataTopTen$blog$freq, dataTopTen$news$freq)))
})

gTopTen <- list(
    gTopTen$twitter + labs(title = 'Top ten most frequently used words in Twitter'),
    gTopTen$blog + labs(title = 'Top ten most frequently used words in Blogs'),
    gTopTen$news + labs(title = 'Top ten most frequently used words in News'))

###----- Testing and Training model -----###

textAll <- c(enTwitter, enBlogs, enNews)
samplePercent = 0.1
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

save(dataTopTen, dataCount, textMatrixAll, smoothGT, file = 'allData.RData')
save(gTopTen, gCount, file = 'allFigures.RData')


















