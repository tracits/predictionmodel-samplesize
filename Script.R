library('dbConnect')

#Settings
numberofvalidation <- 50

#Initialize
RFolder <- "C:\\Users\\Administrator\\Source\\Repos\\predictionmodel-samplesize\\R\\" #Change to your local Repo
source(paste(sep = "", RFolder, "ImportMySQL.R"))

#Get Datasets
print("Get Datasets")
datasetA <- ImportMangrooveMySQL(mysql.Mangroove.table = '2012_summary')
datasetB <- ImportMangrooveMySQL(mysql.Mangroove.table = '2013_summary')
datasetC <- ImportMangrooveMySQL(mysql.Mangroove.table = '2014_summary')


#Start loop number of events (1-1000) changes with each loop
for (numberofevents in c(200)) { #Test with only 200
    print(paste("Start loop numberofevents",numberofevents))
    #Start loop confindence (0.02, 0.05, 0.10) changes with each loop
    for (confindence in c(0.02)) {
        print(paste( "Start loop confindence", confindence))
        numberofnonevents <- ceiling((numberofevents / confindence) - numberofevents)

        #Get developing data (datasetA), get events and non-events, choose sample size and join together
        datasetAevents <- subset(datasetA, Event == 1)
        datasetAnonevents <- subset(datasetA, Event == 0)

        sampledatasetAevents <- datasetAevents[sample(nrow(datasetAevents), numberofevents),]
        sampledatasetAnonevents <- datasetAnonevents[sample(nrow(datasetAnonevents), numberofnonevents),]
        sampledatasetA <- rbind(sampledatasetAevents, sampledatasetAnonevents)

        #create model
        print("Creating model")
        modelM <- glm(Event ~ SBP + PULSE + RR + GCSTOT, data = sampledatasetA, family = 'binomial')
        summary(modelM)

        #get validation data (DatasetC) and pick sample 
        sampledatasetC <- datasetC[sample(nrow(datasetC), numberofvalidation),]

        #compare results
        print("Predicting/validating model")
        resA <- predict(modelM, sampledatasetC, type = 'response')
        resA
        resA <- predict(modelM, sampledatasetA, type = 'response')
        resA

        confmatrixA <- table(Actual_value = sampledatasetA$Event, Predicted_value = resA > 0.5)
        confmatrixA

        modelMres <- (confmatrixA[[1, 1]] + confmatrixA[[2, 2]]) / sum(confmatrixA)
        modelMres


        #Get updating data (Dataset B), pick samples
        datasetBevents <- subset(datasetB, Event == 1)
        datasetBnonevents <- subset(datasetB, Event == 0)

        sampledatasetBevents <- datasetBevents[sample(nrow(datasetBevents), numberofevents),]
        sampledatasetBnonevents <- datasetBnonevents[sample(nrow(datasetBnonevents), numberofnonevents),]
        sampledatasetB <- rbind(sampledatasetBevents, sampledatasetBnonevents)

        #update model
        print("Updating model")
        modelUM <- update(modelM, Event ~ SBP + PULSE + RR + GCSTOT, data = sampledatasetB, family = 'binomial')

        #compare results
        resB <- predict(modelUM, sampledatasetC, type = 'response')
        resB
        resB <- predict(modelUM, sampledatasetB, type = 'response')
        resB

        confmatrixB <- table(Actual_value = sampledatasetB$Event, Predicted_value = resB > 0.5)
        confmatrixB

        modelUMres <- (confmatrixB[[1, 1]] + confmatrixB[[2, 2]]) / sum(confmatrixB)

        #compare results from both models
        modelMres - modelUMres
    }
}