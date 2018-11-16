## The steps to create a subsample could be encapsulated as a function to avoid
## code duplication
CreateSubSample <- function(dataset, number.of.events, number.of.nonevents) {
    dataset$SBP <- as.numeric(dataset$SBP)
    dataset$PULSE <- as.numeric(dataset$PULSE)
    dataset$RR <- as.numeric(dataset$RR)
    dataset$GCSTOT <- as.numeric(dataset$GCSTOT)
    dataset.events <- subset(dataset, Event == 1)
    dataset.nonevents <- subset(dataset, Event == 0)
    sample.dataset.events <- dataset.events[sample(nrow(dataset.events), number.of.events),]
    sample.dataset.nonevents <- dataset.nonevents[sample(nrow(dataset.nonevents), number.of.nonevents),]
    subsample <- rbind(sample.dataset.events, sample.dataset.nonevents)
    return(subsample)
}