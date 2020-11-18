predictWordsGroup <- function(dataNGram, g1 = NULL, g2 = NULL, g3 = NULL)
{
    gramList <- list(g1, g2, g3)
    N_Gram <- switch(as.String(sum(sapply(gramList,is.null))),
                     '2' = 2,
                     '1' = 3,
                     '0' = 4,
                     '3' = stop('Please give at least on input gram for prediction'))

        topFreqList <- switch (as.String(N_Gram),
        '2' = predict2Gram(dataNGram, g1),
        '3' = predict3Gram(dataNGram, g1, g2),
        '4' = predict4Gram(dataNGram, g1, g2, g3))
    return(topFreqList)
}

predict2Gram <- function(dataNGram, gram1)
{
    data2Gram <- dataNGram[[1]]
    data2Gram <- data2Gram[data2Gram$g1 == gram1,]
    data2Gram <- data2Gram[order(-data2Gram$counts),] %>%
        mutate(freq = counts/sum(counts))
    return(data2Gram[1:5,])
}

predict3Gram <- function(dataNGram, gram1, gram2)
{
    data3Gram <- data.frame(dataNGram[[2]])
    data3Gram <- data3Gram[data3Gram$g1 == gram1 & data3Gram$g2 == gram2,]
    if (dim(data3Gram)[1] == 0)
    {
        message('')
        return(predict2Gram(dataNGram, gram2))
    }
    else{
        data3Gram <- data3Gram[order(-data3Gram$counts),] %>%
        mutate(freq = counts/sum(counts))
        return(data3Gram[1:5,])}
}

predict4Gram <- function(dataNGram, gram1, gram2, gram3)
{
    data4Gram <- data.frame(dataNGram[[3]])
    data4Gram <- data4Gram[data4Gram$g1 == gram1 & data4Gram$g2 == gram2 & data4Gram$g3 == gram3,]
    if (dim(data4Gram)[1] == 0)
    {
        message('')
        return(predict3Gram(dataNGram, gram2, gram3))
    }
    else{
        data4Gram <- data4Gram[order(-data4Gram$counts),] %>%
            mutate(freq = counts/sum(counts))
        return(data4Gram[1:5,])}
}