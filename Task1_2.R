# library(tidyr)
# library(tm)
# library(stringr)
# library(ggplot2)
# library(dplyr)
# 
# set.seed(125)
# ###----- download and read data -----###
# setwd('D:/Coursera/Datascience/Capstone')
# url = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
# destfile = "Coursera-SwiftKey.zip"
# if (!file.exists(destfile)) {
#     download.file(fileURL, destfile, method="auto")
#     unzip("Coursera-SwiftKey.zip", overwrite = FALSE)}
# 
# con <- file("final/en_US/en_US.twitter.txt", "r")
# enTwitter <- readLines(con, -1)
# close(con)
# con <- file("final/en_US/en_US.blogs.txt", "r")
# enBlogs <- readLines(con, -1)
# close(con)
# con <- file("final/en_US/en_US.news.txt", "r")
# enNews <- readLines(con, -1)
# close(con)

###----- processing data Week1 -----###
# lenEn <- list(T = nchar(enTwitter), B = nchar(enBlogs), N = nchar(enNews))
# lenEnMax <- lapply(lenEn,max)
# time_love <- grep('love', enTwitter)
# time_hate <- grep('hate', enTwitter)
# ratioLoveHate <- length(time_love)/length(time_hate)
# phraseBiostats <- enTwitter[grep('biostats', enTwitter)]
# phraseKick <- grep('A computer once beat me at chess, but it was no match for me at kickboxing', enTwitter)

#####----- Week 2 -----#####


# sum entry and size
sumAll <- data.frame(Entry = c(length(enTwitter),length(enBlogs),length(enNews)),
                     Size = c(object.size(enTwitter),object.size(enBlogs),object.size(enNews))) %>%
    mutate(Size = round(Size/1e06,2))
row.names(sumAll) = c('Twitter', 'Blog', 'News')

countLineWords <- function(data, sampleSize)
{
    ## sample the dataset of each category
    dataSample = data[sample(c(1:length(data)), size = sampleSize, replace = F)]
    ## split line by .?! and count
    line = grep('[[:alnum:]]', unlist(strsplit(dataSample, '\\.+|\\!+|\\?+')), value = T)
    lineCount = length(line)
    # ## split words and count
    # # word <- tolower(unlist(sapply(dataSample, function(x) {str_extract_all(x,"[[:alnum:]]+")}), use.names = F))
    # word <- unlist(sapply(dataSample, function(x) 
    #     {unlist(strsplit(cleanText(x), '\\.+|\\!+|\\?+|\\,+|\\s+'))}
    #     ), use.names = F)
    # wordUnique <- data.frame(table(word))
    # wordOrder <- sort(wordUnique$Freq, decreasing = T, index.return = T)
    # wordCount <- mutate(data.frame( wordSort = wordUnique$word[wordOrder$ix], times = wordOrder$x), freq = times/length(word))
    # ## return result
    # result <- list("line" = line, "lineCount" = lineCount, "words" = word, "wordCount" = wordCount)
    result <- list("line" = line, "lineCount" = lineCount)
    return(result)
}

phraseList <- list(Twitter = enTwitter,Blogs = enBlogs,News = enNews)
sampleSize <-  10000
cleanResult <- lapply(phraseList, function(x) countLineWords(x,sampleSize = sampleSize))

dataCount <- data.frame(lineCounts = c(cleanResult$Twitter$lineCount,
                                       cleanResult$Blogs$lineCount,
                                       cleanResult$News$lineCount),
                        wordCounts = c(length(cleanResult$Twitter$words),
                                       length(cleanResult$Blogs$words),
                                       length(cleanResult$News$words))) %>%
    mutate(media = c("twitter", "Blogs", "News"),
           lineCounts = lineCounts/sampleSize,
           wordCounts = wordCounts/sampleSize) %>%
    gather(key = type, value = counts, - media)
##  
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
##

twitterTopTen = data.frame(word = head(cleanResult$Twitter$wordCount$word,10), freq  = head(cleanResult$Twitter$wordCount$freq,10))
blogTopTen = data.frame(word = head(cleanResult$Blogs$wordCount$word,10), freq  = head(cleanResult$Blogs$wordCount$freq,10))
newsTopTen = data.frame(word = head(cleanResult$News$wordCount$word,10), freq  = head(cleanResult$News$wordCount$freq,10))
dataTopTen <- list(twitter = twitterTopTen, blog = blogTopTen, news = newsTopTen)

gCount <- ggplot(dataCount, aes(x = media, y = counts, fill = media)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = 'Averaged number of line and word of three medias') +
    facet_grid(type ~. , scales = "free")
gCount

gTopTen <- lapply(dataTopTen, function(dataMedia)
{g <- ggplot(dataMedia, aes(x = reorder(word, -freq), y = freq, fill = reorder(word, -freq))) +
    geom_bar(stat = "identity",show.legend = FALSE)  +
    scale_fill_grey() +
    labs(x = 'word', y = 'frequency') +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x  = element_text(size = 14)) +
    ylim(c(0, max(dataTopTen$twitter$freq, dataTopTen$blog$freq, dataTopTen$news$freq)))
})

gTopTen$twitter + labs(title = 'Top ten most frequently used words in Twitter')
gTopTen$blog + labs(title = 'Top ten most frequently used words in Blogs')
gTopTen$news + labs(title = 'Top ten most frequently used words in News')






















