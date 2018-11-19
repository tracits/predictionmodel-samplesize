library('dbConnect')
#Initialize
source(".sshconfig.R")
source("R/MySQLFunctions.R")
source("R/CreateSubSample.R")
source("R/CompareModels.R")

RunStudy <- function() {
    ## Settings

    ## Note that it is the number of events in datasetB that should vary between 1
    ## and 1000, but that the outcome prevalence should be the same in datasetC and
    ## datasetB. So if the prevelance is 0.02, and the number of events is 1, then
    ## the number of patients in datasetB should be 50, because 1/0.02=50.

    ## The number of events in datasetA and C should always be 200, and the number
    ## of non-events should vary depending on the outcome prevalence
    numberofevents <- 200
    prevalenceinterval = c(0.02, 0.05, 0.10)

    #Get Datasets
    print("Get Datasets")
    datasetA <- ImportMangroveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangrove.table = '2012_summary')
    datasetB <- ImportMangroveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangrove.table = '2013_summary')
    datasetC <- ImportMangroveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangrove.table = '2014_summary')
    loopCount <- 0

    executionID <- format(Sys.time(), "%Y%m%d%H%M%OS")
    #Start loop number of updating events (1-1000) changes with each loop
    for (numberofupdatingevents in c(1:1000)) {
        #Test with only 200
        gc
        #Start loop confindence (0.02, 0.05, 0.10) changes with each loop. Note that the
        #outcome prevelance should always be the same in datasetB and datasetC

        for (prevalenceArr in data.frame(t(expand.grid(prevalenceinterval, prevalenceinterval)))) {
            loopCount <- loopCount + 1
            print(loopCount)
            developmentprevalence <- prevalenceArr[1]
            updatingvalidationprevalence <- prevalenceArr[2]

            ## Now define the number of non-events
            numberofdevelopmentnonevents <- ceiling((numberofevents / developmentprevalence) - numberofevents)
            numberofvalidationnonevents <- ceiling((numberofevents / updatingvalidationprevalence) - numberofevents)
            numberofupdatingnonevents <- ceiling((numberofupdatingevents / updatingvalidationprevalence) - numberofupdatingevents)

            #Get developing data (datasetA), get events and non-events, choose sample size and join together
            sample.dataset.A <- CreateSubSample(datasetA, numberofevents, numberofdevelopmentnonevents)
            #create model
            modelM <- glm(Event ~ SBP + PULSE + RR + GCSTOT, data = sample.dataset.A, family = 'binomial')

            #get validation data (DatasetC) and pick sample 
            sample.dataset.C <- CreateSubSample(datasetC, numberofevents, numberofvalidationnonevents)

            #compare results
            resA <- predict(modelM, sample.dataset.C, type = 'response')
            resA
            resA <- predict(modelM, sample.dataset.A, type = 'response')
            resA

            confmatrixA <- table(Actual_value = sample.dataset.A$Event, Predicted_value = resA > 0.5)
            confmatrixA

            #modelMres <- (confmatrixA[[1, 1]] + confmatrixA[[2, 2]]) / sum(confmatrixA)
            #modelMres

            #Get updating data (Dataset B), pick samples
            sample.dataset.B <- CreateSubSample(datasetB, numberofupdatingevents, numberofupdatingnonevents)

            #update model
            sample.dataset.B$p <- predict(modelM, newdata = sample.dataset.B, type = 'response')
            modelUM <- glm(Event ~ p, data = sample.dataset.B)

            modelMIntercept <- coef(modelM)["(Intercept)"]
            modelMSBP <- coef(modelM)["SBP"]
            modelMPULSE <- coef(modelM)["PULSE"]
            modelMRR <- coef(modelM)["RR"]
            modelMGCSTOT <- coef(modelM)["GCSTOT"]

            modelUMIntercept <- coef(modelUM)["(Intercept)"]
            modelUMP <- coef(modelUM)["p"]

            eVal <- exp(modelMIntercept + modelUMIntercept + modelUMP * (modelMSBP * 1 + modelMPULSE * 1 + modelMRR * 1 + modelMGCSTOT * 1))
            pVal = eVal / (eVal - 1)

            #compare results from both models
            #cm <- CompareModels('')

            StoreLoopData(executionID, loopCount, developmentprevalence, updatingvalidationprevalence, numberofdevelopmentnonevents, numberofvalidationnonevents, numberofupdatingnonevents, modelMIntercept, modelMSBP, modelMPULSE, modelMRR, modelMGCSTOT, modelUMIntercept, 0)
        }
    }
}
library(compiler)
cnoise <- cmpfun(RunStudy)
system.time(cnoise())