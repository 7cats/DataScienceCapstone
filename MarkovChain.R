sequence <- c("a", "b", "a", "a", "a", "a", "b", "a", "b", "a", "b", "a", "a",
              "b", "b", "b", "a")
sequence2 <- c("c", "c", "d", "c", "d", "d", "c", "c", "c", "d", "c", "d", "c",
                             "c", "c", "c", "c")
sequenceMatr <- createSequenceMatrix(sequence, sanitize = FALSE)
sequenceMatr2 <- createSequenceMatrix(sequence2, sanitize = FALSE)
sequenceMatr2 + sequenceMatr

mcFitMLE <- markovchainFit(data = sequence)
mcFitBSP <- markovchainFit(data = sequence, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")
na.sequence <- c("a", NA, "a", "b")
# There will be only a (a,b) transition
na.sequenceMatr <- createSequenceMatrix(na.sequence, sanitize = FALSE)
mcFitMLE <- markovchainFit(data = na.sequence)
# data can be a list of character vectors
sequences <- list(x = c("a", "b", "a"), y = c("b", "a", "b", "a", "c"))
mcFitMap <- markovchainFit(sequences, method = "map")
mcFitMle <- markovchainFit(sequences, method = "mle")

