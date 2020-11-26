predictWordBackOff <- function(smoothGT, textMatrixAll, phrase)
{
    gramsRaw <- unlist(strsplit(phrase, '\\s'))
    ## giving message and error message
    if(length(gramsRaw) == 3)
    {
        message('Three grams are given to predict the next one. Using the quadgram model.')
        wordList <- getCount4Gram(gramsRaw, textMatrxAll[[4]])
    }
    else if(length(gramsRaw) == 2)
    {
        message('Three grams are given to predict the next one. Using the trigram model.')
        wordList <- getCount4Gram(gramsRaw, textMatrxAll[[3]])
    }
    else if(length(gramsRaw) == 1)
    {
        message('Three grams are given to predict the next one. Using the bigram model.')
        wordList <- getCount4Gram(gramsRaw, textMatrxAll[[2]])
    }
    else if(length(gramsRaw) == 0)
    {
        message('Three grams are given to predict the next one. Using the unigram model.')
        wordList <- getCount4Gram(gramsRaw, textMatrxAll[[1]])
    }
    else 
        warning('Too much grams are given. Only the last three will be used.')
    
    return(wordList)
    
}
getProb <- function(Count, smoothGT, N)
{
    i <- 1
    for(c in Count)
    {
        prob[i] <- smoothGT[[N]]$pStar[which(smoothGT[[N]] == c)]
        i <- i+1
    }
}

getCount4Gram <- function(gramsRaw, textMatrixAll, smoothGT)
{
    textMatrix <- textMatrixAll[[4]]
    existIndex <- which(gramsRaw[1] == textMatrix$g1 &
                        gramsRaw[2] == textMatrix$g2 &
                        gramsRaw[3] == textMatrix$g3)
    if(as.logical(wordList[1]) == TRUE)
    {
        wordList <- data.frame(counts = textMatrix$counts[existIndex],
                               index = existIndex,
                               word = textMatrix$g4[existIndex])
        wordList$probs <- getProb(countIndex$counts, smoothGT[[4]], 4)
        wordList <- wordList[order(probs, decreasing = T),]
    }
    else
    {
        WordList <- getCount3Gram(gramsRaw[-1],textMatrixAll[-4], smoothGT[-4])
        ## still need to times a probability
    }
}

getCount3Gram <- function(gramsRaw, textMatrix, smoothGT)
{
    existIndex <- which( gramsRaw[1] == textMatrix$g1 &
                         gramsRaw[2] == textMatrix$g2)
    if(as.logical(wordList[1]) == TRUE)
    {
        wordList <- data.frame(counts = textMatrix$counts[existIndex],
                               index = existIndex,
                               word = textMatrix$g4[existIndex])
        wordList$probs <- getProb(countIndex$counts, smoothGT[[3]], 3)
        wordList <- wordList[order(probs, decreasing = T),]
    }
    else
    {
        WordList <- getCount2Gram(gramsRaw[-1],textMatrixAll[-3], smoothGT[-3])
        ## still need to times a probability
    }
}

getCount2Gram <- function(gramsRaw, textMatrix, smoothGT)
{
    existIndex <- which( gramsRaw[1] == textMatrix$g1)
    if(as.logical(wordList[1]) == TRUE)
    {
        wordList <- data.frame(counts = textMatrix$counts[existIndex],
                               index = existIndex,
                               word = textMatrix$g4[existIndex])
        wordList$probs <- getProb(countIndex$counts, smoothGT[[2]], 2)
        wordList <- wordList[order(probs, decreasing = T),]
    }
    else
    {
        # return words with highest prob
        ## still need to times a probability
    }
}