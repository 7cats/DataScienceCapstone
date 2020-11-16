getNgram <- function(corpus, N_Gram)
{
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
    
    ngramTextTDM <- switch (as.String(N_Gram),
                            '2' = TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)),
                            '3' = TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)),
                            '4' = TermDocumentMatrix(corpus, control = list(tokenize = QuadgramTokenizer)),
                            stop("Only 2,3 and 4 are used for ngram model")
    )
    
    ngramMatrx <- as.matrix(ngramTextTDM)
    returnList <- list(ngramTextTDM, ngramMatrx)
}

getNgramTokenMatrix <- function(DF, N_Gram)
{
    gramStr <- row.names(DF)
    splitGram <- str_split(gramStr,' ')
    tokens <- data.frame(matrix(unlist(splitGram), nrow =length(splitGram), byrow = T))
    
    colnames(tokens) <- switch (as.String(N_Gram),
                                '2' = c('g1', 'g2'),
                                '3' = c('g1', 'g2', 'g3'),
                                '4' = c('g1', 'g2', 'g3', 'g4'))
    
    tokens <- data.frame(tokens) %>%
        cbind(.,counts = DF) %>%
        group_by(g1)
}

preprocessData <- function(data, N_Gram)
{
    ## pre-process: remove excess blanks, lower all letters, remove non-alphabet text
    dataPostPro = stripWhitespace(
        tolower(
            gsub('[^\x01-\x7F]+', '',
                 gsub('[^[:alpha:][:blank:]]','', unlist(strsplit(data, '\\.+|\\!+|\\?+|\\,+'))))))
    
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