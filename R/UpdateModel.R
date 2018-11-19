GetUpdatingModel <- function(datasetB, numberofupdatingevents, numberofupdatingnonevents)  {
    #Get updating data (Dataset B), pick samples
    sample.dataset.B <- CreateSubSample(datasetB, numberofupdatingevents, numberofupdatingnonevents)

    #update model
    print("Updating model")
    sample.dataset.B$p <- predict(modelM, newdata = sample.dataset.B, type = 'response')
    modelUM <- glm(Event ~ p, data = sample.dataset.B)

    mMIntercept <- coef(modelM)["(Intercept)"]
    mMcoefSBP <- coef(modelM)["SBP"]
    mMcoefPULSE <- coef(modelM)["PULSE"]
    mMcoefRR <- coef(modelM)["RR"]
    mMcoefGCS <- coef(modelM)["GCS"]

    mUMIntercept <- coef(modelM)["(Intercept)"]
    mUMcoefP <- coef(modelM)["p"]

    #ln(p / (p - 1)) = mMIntercept + mUMIntercept + mUMcoefP * (mMcoefSBP * x1 + mMcoefPULSE * x2 + mMcoefRR * x3 + mMcoefGCS * x4)

    #compare results
    #resB <- predict(modelUM, sample.dataset.C, type = 'response')
    #resB
    #resB <- predict(modelUM, sample.dataset.B, type = 'response')
    #resB

    #confmatrixB <- table(Actual_value = sample.dataset.B$Event, Predicted_value = resB > 0.5)
    #confmatrixB

    #modelUMres <- (confmatrixB[[1, 1]] + confmatrixB[[2, 2]]) / sum(confmatrixB)
    return(c(sample.dataset.B, modelUM,mMIntercept,mMcoefSBP,mMcoefPULSE,mMcoefRR,mMcoefGCS,mUMIntercept,mUMcoef))
}