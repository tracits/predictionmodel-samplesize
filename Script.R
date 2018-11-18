library('dbConnect')

## Settings

## Note that it is the number of events in datasetB that should vary between 1
## and 1000, but that the outcome prevalence should be the same in datasetC and
## datasetB. So if the prevelance is 0.02, and the number of events is 1, then
## the number of patients in datasetB should be 50, because 1/0.02=50.

## The number of events in datasetA and C should always be 200, and the number
## of non-events should vary depending on the outcome prevalence
numberofevents <- 200
confidenceinterval = c(0.02, 0.05, 0.10)


#Initialize
source(".sshconfig.R")
source("R/MySQLFunctions.R")
source("R/CreateSubSample.R")

#Get Datasets
print("Get Datasets")
datasetA <- ImportMangrooveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangroove.table = '2012_summary')
datasetB <- ImportMangrooveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangroove.table = '2013_summary')
datasetC <- ImportMangrooveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangroove.table = '2014_summary')
loopCount <- 0

executionID <- format(Sys.time(), "%Y%m%d%H%M%OS")
#Start loop number of updating events (1-1000) changes with each loop
for (numberofupdatingevents in c(1:1000)) { #Test with only 200
    print(paste("Start loop numberofupdatingevents", numberofupdatingevents))
    #Start loop confindence (0.02, 0.05, 0.10) changes with each loop. Note that the
    #outcome prevelance should always be the same in datasetB and datasetC
    
    for (confindence in data.frame(t(expand.grid(confidenceinterval, confidenceinterval)))) {
        loopCount <- loopCount+1
        developmentprevalence <- confindence[1]
        updatingvalidationprevalence <- confindence[2]
        print(paste("Start loop confindence", developmentprevalence, updatingvalidationprevalence))

        ## Now define the number of non-events
        numberofdevelopmentnonevents <- ceiling((numberofevents / developmentprevalence) - numberofevents)
        numberofvalidationnonevents <- ceiling((numberofevents / updatingvalidationprevalence) - numberofevents)
        numberofupdatingnonevents <- ceiling((numberofupdatingevents / updatingvalidationprevalence) - numberofupdatingevents)

        #Get developing data (datasetA), get events and non-events, choose sample size and join together
        sample.dataset.A <- CreateSubSample(datasetA, numberofevents, numberofdevelopmentnonevents)

        #create model
        print("Creating model")
        modelM <- glm(Event ~ SBP + PULSE + RR + GCSTOT, data = sample.dataset.A, family = 'binomial')
        summary(modelM)

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


        #Get updating data (Dataset B), pick samples
        sample.dataset.B <- CreateSubSample(datasetB, numberofupdatingevents, numberofupdatingnonevents)

        #update model
        print("Updating model")
        modelUM <- update(modelM, Event ~ SBP + PULSE + RR + GCSTOT, data = sample.dataset.B, family = 'binomial')

        #compare results
        resB <- predict(modelUM, sample.dataset.C, type = 'response')
        resB
        resB <- predict(modelUM, sample.dataset.B, type = 'response')
        resB

        confmatrixB <- table(Actual_value = sample.dataset.B$Event, Predicted_value = resB > 0.5)
        confmatrixB

        #modelUMres <- (confmatrixB[[1, 1]] + confmatrixB[[2, 2]]) / sum(confmatrixB)

        #compare results from both models
        #modelMres - modelUMres
        print("Finished")
        print(loopCount)
        test <- sprintf("INSERT INTO `NTDB_adam`.`runtime_data` (`executionID`,`loopCount`,`developmentprevalence`,`updatingvalidationprevalence`,`numberofdevelopmentnonevents`,`numberofvalidationnonevents`,`numberofupdatingnonevents`,`modelMIntercept`,`modelMSBP`,`modelMPulse`,`mdelMRR`,`modelMGCSTOT`,`modelUMIntercept`,`comparisonResult`) VALUES (%s,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g);", executionID, loopCount, developmentprevalence, updatingvalidationprevalence, numberofdevelopmentnonevents, numberofvalidationnonevents, numberofupdatingnonevents, 0, 0, 0, 0, 0, 0, 0)
        print(test)
        StoreLoopData(executionID, loopCount, developmentprevalence, updatingvalidationprevalence, numberofdevelopmentnonevents, numberofvalidationnonevents, numberofupdatingnonevents, 0, 0, 0, 0, 0, 0, 0)
    }
}