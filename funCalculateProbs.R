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
        wordProb <- getProb(textMatrix$counts[which(grams[1] == textMatrixAll[[1]]$g1)])
    }
    else 
    {
        warning('Too much grams are given. Only the last four will be used.')
        wordProb <- getProb4Gram(gramsRaw[length(gramsRaw)-2:length(gramsRaw)], textMatrixAll, smoothGT)
    }
    return(wordProb)
    
}
getProb <- function(Count, smoothGT)
{
    prob <- smoothGT$pAve[which(smoothGT$counts == Count)]
}

getProb4Gram <- function(grams, textMatrixAll, smoothGT)
{
    textMatrix <- textMatrixAll[[4]]
    exist4Index <- which(grams[1] == textMatrix$g1 &
                         grams[2] == textMatrix$g2 &
                         grams[3] == textMatrix$g3 &
                         grams[4] == textMatrix$g4)
    if(length(existIndex) != 0)
    {
        wordCount <- textMatrix$counts[existIndex]
        wordProb4Gram <- getProb(wordCount, smoothGT[[4]])
    }
    else
    {
        message("Could not find matching text in quadGram model, switch to triGram model")
        WordProb <- getProb3Gram(gramsRaw[-1],textMatrixAll[-4], smoothGT[-4])
        # alpha <- getCount4Gram(grams, textMatrixAll, smoothGT)
        # existRestIndex <- which(grams[1] == textMatrix$g1 &
        #                         grams[2] == textMatrix$g2 &
        #                         grams[3] == textMatrix$g3)
        # if(length(existRestIndex > 1))
        # {
        #     existRestIndex <- existRestIndex[-which(exist4Index == existRestIndex)]
        #     restWordCount <- textMatrix$counts[exist3Index]
        #     conditionalProb <- 1 - sum(getProb(restWordCount, smoothGT[[4]]))
        #     wordProb3Gram <- getProb()
        # }
    }
    
}

getProb3Gram <- function(grams, textMatrixAll, smoothGT)
{
    textMatrix <- textMatrixAll[[3]]
    exist4Index <- which(grams[1] == textMatrix$g1 &
                         grams[2] == textMatrix$g2 &
                         grams[3] == textMatrix$g3)
    if(length(existIndex) != 0)
    {
        wordCount <- textMatrix$counts[existIndex]
        wordProb4Gram <- getProb(wordCount, smoothGT[[3]])
    }
    else
    {
        message("Could not find matching text in tridGram model, switch to biGram model")
        WordProb <- getProb2Gram(gramsRaw[-1],textMatrixAll[-3], smoothGT[-3])
    }
    
}

getProb2Gram <- function(grams, textMatrixAll, smoothGT)
{
    textMatrix <- textMatrixAll[[2]]
    exist4Index <- which(grams[1] == textMatrix$g1 &
                         grams[2] == textMatrix$g2)
    if(length(existIndex) != 0)
    {
        wordCount <- textMatrix$counts[existIndex]
        wordProb4Gram <- getProb(wordCount, smoothGT[[2]])
    }
    else
    {
        message("Could not find matching text in BidGram model, only return last word\'s probability")
        WordProb <- getProb(textMatrix$counts[which(grams[1] == textMatrixAll[[1]]$g1)])
    }
    
}
