predictWordBackOff <- function(smoothGT, textMatrixAll, phrase, returnNum = 10)
{
    gramsRaw <- unlist(strsplit(cleanText(phrase),'\\s'))
    ## giving message and error message
    
    if(length(gramsRaw) == 3)
    {
        message('Three grams are given to predict the next one. Using the quadgram model.')
        wordList <- getCount4Gram(gramsRaw, textMatrixAll, smoothGT, returnNum)
    }
    else if(length(gramsRaw) == 2)
    {
        message('Two gram are given to predict the next one. Using the trigram model.')
        wordList <- getCount3Gram(gramsRaw, textMatrixAll[-4], smoothGT[-4], returnNum)
    }
    else if(length(gramsRaw) == 1)
    {
        message('One grams is given to predict the next one. Using the bigram model.')
        wordList <- getCount2Gram(gramsRaw, textMatrixAll[1:2], smoothGT[1:2], returnNum)
    }
    else if(length(gramsRaw) == 0)
    {
        message('No gram are given to predict the next one. Simply give words with highest probability.')
        wordList <- getCount1Gram(gramsRaw, textMatrixAll[[1]],smoothGT[[1]], returnNum)
    }
    else 
    {
        warning('Too much grams are given. Only the last three will be used.')
        wordList <- getCount4Gram(gramsRaw[length(gramsRaw)-2:length(gramsRaw)], textMatrixAll, smoothGT, returnNum)
    }
    return(wordList)
    
}
getProbs <- function(Count, smoothGT)
{
    i <- 1
    probs <- NULL
    for(c in Count)
    {
        probs[i] <- smoothGT$pAve[which(smoothGT$counts == c)]
        i <- i+1
    }
    return(probs)
}

getCount4Gram <- function(gramsRaw, textMatrixAll, smoothGT, returnNum)
{
    textMatrix <- textMatrixAll[[4]]
    existIndex <- which(gramsRaw[1] == textMatrix$g1 &
                        gramsRaw[2] == textMatrix$g2 &
                        gramsRaw[3] == textMatrix$g3)
    if(length(existIndex) != 0)
    {
        wordList <- data.frame(counts = textMatrix$counts[existIndex],
                               index = existIndex,
                               word = textMatrix$g4[existIndex])
        wordList$prob <- getProbs(wordList$counts, smoothGT[[4]])
        wordList <- wordList[order(wordList$prob, decreasing = T),]
    }
    else
    {
        message("Could not find matching text in quadGram model, switch to triGram model")
        wordList <- getCount3Gram(gramsRaw[-1],textMatrixAll[-4], smoothGT[-4], returnNum)
    }
    return(head(wordList,returnNum))
}

getCount3Gram <- function(gramsRaw, textMatrixAll, smoothGT, returnNum)
{
    textMatrix <- textMatrixAll[[3]]
    existIndex <- which( gramsRaw[1] == textMatrix$g1 &
                         gramsRaw[2] == textMatrix$g2)
    if(length(existIndex) != 0)
    {
        wordList <- data.frame(counts = textMatrix$counts[existIndex],
                               index = existIndex,
                               word = textMatrix$g3[existIndex])
        wordList$prob <- getProbs(wordList$counts, smoothGT[[3]])
        wordList <- wordList[order(wordList$prob, decreasing = T),]
    }
    else
    {
        message("Could not find matching text in tridGram model, switch to biGram model")
        wordList <- getCount2Gram(gramsRaw[-1],textMatrixAll[-3], smoothGT[-3], returnNum)
    }
    return(head(wordList,returnNum))
}

getCount2Gram <- function(gramsRaw, textMatrixAll, smoothGT, returnNum)
{
    textMatrix <- textMatrixAll[[2]]
    existIndex <- which( gramsRaw[1] == textMatrix$g1)
    if(length(existIndex) != 0)
    {
        wordList <- data.frame(counts = textMatrix$counts[existIndex],
                               index = existIndex,
                               word = textMatrix$g2[existIndex])
        wordList$prob <- getProbs(wordList$counts, smoothGT[[2]])
        wordList <- wordList[order(wordList$prob, decreasing = T),]
        return(head(wordList,returnNum))
    }
    else
    {
        message("Could not find matching text in bidGram model, switch to UniGram model")
        wordList <- getCount1Gram(gramsRaw[-1],textMatrixAll[[1]], smoothGT[[1]], returnNum)
    }
    return(head(wordList,returnNum))
}

getCount1Gram <- function(gram, textMatrix, smoothGT1Gram, returnNum)
{
    smoothGT1Gram <- smoothGT1Gram[sort(smoothGT1Gram$pAve,decreasing = T, index.return = T)$ix,]
    # textMatrix <- textMatrix[sort(textMatrix$counts, decreasing = T, index.return = T)$ix,]
    # WordList <- data.frame(word = textMatrix$g1[1:returnNum],
    #                        counts = textMatrix$counts[1:returnNum])
    sumCount <- NULL
    for(i in 1:dim(smoothGT1Gram)[1])
    {
        sumCount[i] <- sum(smoothGT1Gram$Nc[1:i])
    }
    splitPoint <- which(returnNum < sumCount)[1]
    wordList <- data.frame() 
    for(n in 1:splitPoint)
    { 
        wordList <- rbind(wordList, 
                          matrix(rep(t(c(smoothGT1Gram$counts[n],smoothGT1Gram$pAve[n])),
                                       times = smoothGT1Gram$Nc[n]),
                                 nrow = smoothGT1Gram$Nc[n], byrow = T))
    # wordList <- data.frame(counts = smoothGT1Gram$counts[1:splitPoint],
    #                        prob = smoothGT1Gram$pAve[1:splitPoint])
    }
    colnames(wordList) <- c('count','prob')
    words <- NULL
    for(i in 1:dim(wordList)[1])
    {
        words <- c(words,textMatrix$g1[wordList$count[i] == textMatrix$counts])
    }
    wordList$word <- words
    return(wordList)
}