## The steps to create a subsample could be encapsulated as a function to avoid
## code duplication
CreateSubSample <- function(dataset, number.of.events, number.of.nonevents) {
    #Bootstrap dataset
    set.seed(89); datasetBootstrap <- dataset[sample(nrow(dataset), nrow(dataset), replace = TRUE),]
    
    #Pick events and nonevents
    dataset.events <- subset(datasetBootstrap, Event == 1)
    dataset.nonevents <- subset(datasetBootstrap, Event == 0)
    
    set.seed(89); sample.dataset.events <- dataset.events[sample(nrow(dataset.events), number.of.events),]
    set.seed(89); sample.dataset.nonevents <- dataset.nonevents[sample(nrow(dataset.nonevents), number.of.nonevents),]
    subsample <- rbind(sample.dataset.events, sample.dataset.nonevents)
    return(subsample)
}