library('dbConnect')

ImportMangrooveMySQL <- function(mysql.server.name = "127.0.0.1", mysql.server.port = 3307, mysql.database = "NTDB_adam", mysql.username = "x", mysql.password = "x", mysql.Mangroove.table = "2012_summary") {
    mydb <- dbConnect(MySQL(), user = mysql.username, password = mysql.password, dbname = mysql.database, host = mysql.server.name, port = mysql.server.port)
    ## Select all data from database
    study.data <- dbGetQuery(mydb, sprintf("select * from %s WHERE Event is not null", mysql.Mangroove.table))
    dbDisconnect(mydb) ## Disconnect from database
    return(study.data)
}

#Settings
numberofvalidation <- 50



#Start loop number of events (1-1000) changes with each loop
numberofevents <- 200

#Start loop confindence (0.02, 0.05, 0.10) changes with each loop
confindence <- 0.02

numberofnonevents <- ceiling((numberofevents / confindence) - confindence)

#Get developing data (datasetA), get events and non-events, choose sample size and join together
datasetA <- ImportMangrooveMySQL(mysql.Mangroove.table = '2012_summary')
datasetA$SBP <- as.factor(datasetA$SBP)
datasetA$PULSE <- as.factor(datasetA$PULSE)
datasetA$RR <- as.factor(datasetA$RR)
datasetAevents <- subset(datasetA, Event == 1)
datasetAnonevents <- subset(datasetA, Event == 0)

sampledatasetAevents <- datasetAevents[sample(nrow(datasetAevents), numberofevents),]
sampledatasetAnonevents <- datasetAnonevents[sample(nrow(datasetAnonevents), numberofnonevents),]
sampledatasetA <- rbind(sampledatasetAevents, sampledatasetAnonevents)

#create model
modelM <- glm(Event ~ SBP + PULSE + RR, data = sampledatasetA, family = 'binomial')
summary(modelM)

#get validation data (DatasetC) and pick sample 
datasetC <- ImportMangrooveMySQL(mysql.Mangroove.table = '2014_summary')
datasetC$SBP <- as.factor(datasetC$SBP)
datasetC$PULSE <- as.factor(datasetC$PULSE)
datasetC$RR <- as.factor(datasetC$RR)
sampledatasetC <- datasetC[sample(nrow(datasetC), numberofvalidation),]

#compare results
resA <- predict(modelM, sampledatasetC, type = 'response')
resA
resA <- predict(modelM, sampledatasetA, type = 'response')
resA

confmatrixA <- table(Actual_value=sampledatasetA$Event,Predicted_value=resA>0.5)
confmatrixA

modelMres <- (confmatrixA[[1,1]] + confmatrixA[[2,2]]) / sum(confmatrixA)
modelMres


#Get updating data (Dataset B), pick samples
datasetB <- ImportMangrooveMySQL(mysql.Mangroove.table = '2013_summary')
datasetB$SBP <- as.factor(datasetB$SBP)
datasetB$PULSE <- as.factor(datasetB$PULSE)
datasetB$RR <- as.factor(datasetB$RR)
datasetBevents <- subset(datasetB, Event == 1)
datasetBnonevents <- subset(datasetB, Event == 0)

sampledatasetBevents <- datasetBevents[sample(nrow(datasetBevents), numberofevents),]
sampledatasetBnonevents <- datasetBnonevents[sample(nrow(datasetBnonevents), numberofnonevents),]
sampledatasetB <- rbind(sampledatasetBevents, sampledatasetBnonevents)

#update model
modelUM <- update(modelM, Event ~ SBP + PULSE + RR, data = sampledatasetB, family = 'binomial')

#compare results
resB <- predict(modelUM, sampledatasetC, type = 'response')
resB
resB <- predict(modelUM, sampledatasetB, type = 'response')
resB

confmatrixB <- table(Actual_value = sampledatasetB$Event, Predicted_value = resB > 0.5)
confmatrixB

modelUMres <- (confmatrixB[[1, 1]] + confmatrixB[[2, 2]]) / sum(confmatrixB)

#compare results from both models
modelMres-modelUMres