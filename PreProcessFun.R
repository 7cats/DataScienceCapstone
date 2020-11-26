getNgram <- function(corpus, N_Gram)
{
    UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
    
    ngramTextTDM <- switch (as.String(N_Gram),
                            '1' = TermDocumentMatrix(corpus, control = list(tokenize = UnigramTokenizer)),
                            '2' = TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)),
                            '3' = TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)),
                            '4' = TermDocumentMatrix(corpus, control = list(tokenize = QuadgramTokenizer)),
                            stop("number of gram are limited up to 4")
    )
    
    ngramMatrx <- as.matrix(ngramTextTDM)
    returnList <- list(ngramTextTDM, ngramMatrx)
}

getNgramTokenMatrix <- function(DF, N_Gram)
{
    gramStr <- row.names(DF)
    splitGram <- str_split(gramStr,' ')
    tokens <- data.frame(matrix(unlist(splitGram), nrow = length(splitGram), byrow = T)) %>%
              cbind(., counts = DF)
    
    colnames(tokens) <- switch(as.String(N_Gram),
                                '1' = c('g1', 'counts'),
                                '2' = c('g1', 'g2', 'counts'),
                                '3' = c('g1', 'g2', 'g3', 'counts'),
                                '4' = c('g1', 'g2', 'g3', 'g4', 'counts'))
    return(tokens)
}


preprocessData <- function(data, N_Gram)
{
    ## pre-process: remove excess blanks, lower all letters, remove non-alphabet text
    dataPostPro <-  stripWhitespace(
        tolower(
            # sub duplicated characters like happyyyyy -> happy
            gsub('([[:alpha:]])\\1{2,}', '\\1',
            # sub non-en characters
            gsub('[^\x01-\x7F]+', '',
            # 
            gsub('[^[:alpha:][:blank:]]','', 
            # delete meaningless hashtag in twitter
            gsub("\\#[[:alnum:]]+", '',
                 unlist(strsplit(data, '\\.+|\\!+|\\?+|\\,+'))))))))
    # remove blank element or element with only one word
    dataPostPro <- dataPostPro[grepl('[a-z]\\s[a-z]', dataPostPro)]
    
    ## transform to TDM format and Matrix format
    cat(dataPostPro, file = "final/en_US/sample.txt",sep = "\n")
    fileDirSource <- DirSource("final/en_US/", pattern = "sample.txt")
    textCorpus <- VCorpus(fileDirSource, readerControl = list(language = "English"))
    textTdmMatrix <- getNgram(textCorpus, N_Gram)
    tokenMatrix <- getNgramTokenMatrix(data.frame(textTdmMatrix[[2]]), N_Gram)
    
    ##
    returnList <- list(textPostPro = dataPostPro, 
                       textTDM = textTdmMatrix[[1]], 
                       ngramMatrx = tokenMatrix)
}