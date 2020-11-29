## get an average tranform???
## read the paper again
getNcZest <- function(c, nc)
{
    d <- c(1, diff(c))
    dc <- c(0.5 * (d[-1] + d[ -length(d)]), d[length(d)])
    return(nc/dc)
}

getcStar <- function(c, coef)
{
    return(c * (1 + 1/c)^(1 + coef[2]))
}

GoodTuring <- function(textMatrix)
{
    textMatrix <- textMatrix[sort(textMatrix$counts, decreasing = T, index.return = T)$ix,]
    textMatrix <- group_by(textMatrix, counts)
    goodTuring <- data.frame(Nc = group_size(textMatrix),
                             C = summarise(textMatrix))
    
    Counts <- goodTuring$counts # c counts
    Nc <- goodTuring$Nc # Nc frequency 
    N <- sum(Counts*Nc)
    goodTuring <- mutate(goodTuring, p = counts*Nc/N)
    
    ## make averaging transform
    NcZest <- getNcZest(Counts,Nc)
    
    ## get Linear Good-Turing estimate
    fitLGT <- lsfit(log(Counts),log(NcZest))
    coef <- fitLGT$coef
    cStar <- getcStar(Counts,coef)
    cStarLGT <- cStar/Counts
    
    #get Turing estimate
    cTE <- Counts == c(Counts[-1]-1,0)
    cStarTE <- rep(0,length(Counts))
    cStarTE[cTE] <- (Counts[cTE]+1)/Counts[cTE]*c(Nc[-1],0)[cTE]/Nc[cTE]
    
    #make switch from Turing to LGT estimates
    turingSD <- rep(1,length(Counts)) # standard deviation
    for(i in 1:length(Counts))
        if(cTE[i])
            turingSD[i] <- (i+1)/Nc[i]*sqrt(Nc[i+1]*(1 + Nc[i+1]/Nc[i]))
    
    cStarcmbrel <- rep(0,length(Counts))
    useTuring <- TRUE
    
    for(r in 1:length(Counts))
    {
        if(!useTuring)
            cStarcmbrel[r] <- cStarLGT[r]
        else 
            if(abs(cStarLGT-cStarTE)[r]*r/turingSD[r] > 1.65)
                cStarcmbrel[r] <- cStarTE[r]
            else{
                useTuring <- FALSE
                cStarcmbrel[r] <- cStarLGT[r]}
    }
    
    ## renormalize the probabilities for observed objects
    sumProbRaw <- sum(cStarcmbrel*Counts*Nc/N)
    cStarcmbrel <- cStarcmbrel*(1 - Nc[1]/N)/sumProbRaw
    
    ## output
    goodTuring <- rbind(c(Nc = N, C = 0, p = 0), goodTuring)
    goodTuring <- goodTuring %>%
        mutate(cStar = c(Nc[2]/N, Counts*cStarcmbrel),
               pStar = cStar*Nc/N,
               NcZest = c(Nc[1],NcZest),
               pAve = cStar/N)
    return(goodTuring)
}
