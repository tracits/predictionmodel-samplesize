GetValidateModel <- function(datasetC, numberofevents, numberofvalidationnonevents, sample.dataset.A, modelM) {
    #get validation data (DatasetC) and pick sample 
    sample.dataset.C <- CreateSubSample(datasetC, numberofevents, numberofvalidationnonevents)

    #compare results
    print("Predicting/validating model")
    resA <- predict(modelM, sample.dataset.C, type = 'response')
    resA
    resA <- predict(modelM, sample.dataset.A, type = 'response')
    resA

    confmatrixA <- table(Actual_value = sample.dataset.A$Event, Predicted_value = resA > 0.5)
    confmatrixA

    #modelMres <- (confmatrixA[[1, 1]] + confmatrixA[[2, 2]]) / sum(confmatrixA)
    #modelMres
    return(c(sample.dataset.C))
}