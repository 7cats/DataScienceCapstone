calculateProbs <- function(smoothGT, textMatrixAll, gramsRaw)
{
    grams <- unlist(strsplit(cleanText(gramsRaw),'\\s'))
    ## giving message and error message
    if(length(grams) == 4)
    {
        message('Four grams are given. Start with the quad-gram model to calculate the probability.')
        wordProb <- getProb4Gram(grams, textMatrixAll, smoothGT)
    }
    else if(length(grams) == 3)
    {
        message('Three grams are given. Start with the tri-gram model to calculate the probability.')
        wordProb <- getProb3Gram(grams, textMatrixAll, smoothGT)
    }
    else if(length(grams) == 2)
    {
        message('Two grams are given. Start with the bi-gram model to calculate the probability.')
        wordProb <- getProb2Gram(grams, textMatrixAll, smoothGT)
    }
    else if(length(grams) == 1)
    {
        message('Only one grams are given. Simply give the probability of the word.')
        wordProb <- getProb(textMatrixAll[[1]]$counts[which(grams[1] == textMatrixAll[[1]]$g1)], smoothGT[[1]])
    }
    else 
    {
        warning('Too much grams are given. Only the last four will be used.')
        wordProb <- getProb4Gram(grams[length(grams)-2:length(grams)], textMatrixAll, smoothGT)
    }
    return(wordProb)
}

getProb <- function(Count, smoothGT)
    prob <- smoothGT$pAve[which(smoothGT$counts == Count)]


getProb4Gram <- function(grams, textMatrixAll, smoothGT)
{
    textMatrix <- textMatrixAll[[4]]
    existIndex <- which(grams[1] == textMatrix$g1 &
                         grams[2] == textMatrix$g2 &
                         grams[3] == textMatrix$g3 &
                         grams[4] == textMatrix$g4)
    if(length(existIndex) != 0)
    {
        wordCount <- textMatrix$counts[existIndex]
        totalProb <- sum(predictWordBackOff(smoothGT, textMatrixAll, 
                                            paste(grams[1],grams[2],grams[3], sep = ' '))$prob)
        wordProb4Gram <- getProb(wordCount, smoothGT[[4]])/totalProb
    }
    else
    {
        message("Could not find matching text in quadGram model, switch to triGram model")
        WordProb <- getProb3Gram(grams[-1],textMatrixAll[[-4]], smoothGT[[-4]])
    }
}

getProb3Gram <- function(grams, textMatrixAll, smoothGT)
{
    textMatrix <- textMatrixAll[[3]]
    existIndex <- which(grams[1] == textMatrix$g1 &
                         grams[2] == textMatrix$g2 &
                         grams[3] == textMatrix$g3)
    if(length(existIndex) != 0)
    {
        wordCount <- textMatrix$counts[existIndex]
        totalProb <- sum(predictWordBackOff(smoothGT, textMatrixAll, 
                                            paste(grams[1],grams[2], sep = ' '))$prob)
        wordProb3Gram <- getProb(wordCount, smoothGT[[3]])/totalProb
    }
    else
    {
        message("Could not find matching text in tridGram model, switch to biGram model")
        WordProb <- getProb2Gram(grams[-1],textMatrixAll[[-3]], smoothGT[[-3]])
    }
}

getProb2Gram <- function(grams, textMatrixAll, smoothGT)
{
    textMatrix <- textMatrixAll[[2]]
    existIndex <- which(grams[1] == textMatrix$g1 &
                         grams[2] == textMatrix$g2)
    if(length(existIndex) != 0)
    {
        wordCount <- textMatrix$counts[existIndex]
        totalProb <- sum(predictWordBackOff(smoothGT, textMatrixAll, grams[1])$prob)
        wordProb2Gram <- getProb(wordCount, smoothGT[[2]])/totalProb
    }
    else
    {
        message("Could not find matching text in BidGram model, only return last word\'s probability")
        WordProb <- getProb(textMatrix$counts[which(grams[1] == textMatrixAll[[1]]$g1)], smoothGT[[1]])
    }
}
