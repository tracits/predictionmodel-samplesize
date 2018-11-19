GetDevelopmentModel <- function(datasetA, numberofevents, numberofdevelopmentnonevents) {
    #Get developing data (datasetA), get events and non-events, choose sample size and join together
    sample.dataset.A <- CreateSubSample(datasetA, numberofevents, numberofdevelopmentnonevents)
    #create model
    print("Creating model")
    modelM <- glm(Event ~ SBP + PULSE + RR + GCSTOT, data = sample.dataset.A, family = 'binomial')
    return(c(sample.dataset.A, modelM))
}