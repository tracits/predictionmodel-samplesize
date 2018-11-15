library('dbConnect')
source("../.sshconfig.R")

#Change mysql username and password below
ImportMangrooveMySQL <- function(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangroove.table = "2012_summary") {
    mydb <- dbConnect(MySQL(), user = mysql.username, password = mysql.password, dbname = mysql.database, host = mysql.server.name, port = mysql.server.port)
    ## Select all data from database
    study.data <- dbGetQuery(mydb, sprintf("select * from %s WHERE Event is not null", mysql.Mangroove.table))
    dbDisconnect(mydb) ## Disconnect from database
    return(study.data)
}

## Settings

## Note that it is the number of events in datasetB that should vary between 1
## and 1000, but that the outcome prevalence should be the same in datasetC and
## datasetB. So if the prevelance is 0.02, and the number of events is 1, then
## the number of patients in datasetB should be 50, because 1/0.02=50.

#Start loop number of events (1-1000) changes with each loop
numberofupdatingevents <- 1

## The number of events in datasetA and C should always be 200, and the number
## of non-events should vary depending on the outcome prevalence
numberofevents <- 200

#Start loop confindence (0.02, 0.05, 0.10) changes with each loop. Note that the
#outcome prevelance should always be the same in datasetB and datasetC
developmentprevalence <- 0.02
updatingvalidationprevalence <- 0.02

## Now define the number of non-events
numberofdevelopmentnonevents <- ceiling((numberofevents / developmentprevalence) - numberofevents)
numberofvalidationnonevents <- ceiling((numberofevents / updatingvalidationprevalence) - numberofevents)
numberofupdatingnonevents <- ceiling((numberofupdatingevents / updatingvalidationprevalence) - numberofupdatingevents)

## The steps to create a subsample could be encapsulated as a function to avoid
## code duplication
CreateSubSample <- function(dataset, number.of.events, number.of.nonevents) {
    dataset$SBP <- as.numeric(dataset$SBP)
    dataset$PULSE <- as.numeric(dataset$PULSE)
    dataset$RR <- as.numeric(dataset$RR)
    dataset.events <- subset(dataset, Event == 1)
    dataset.nonevents <- subset(dataset, Event == 0)
    sample.dataset.events <- dataset.events[sample(nrow(dataset.events), number.of.events),]
    sample.dataset.nonevents <- dataset.nonevents[sample(nrow(dataset.nonevents), number.of.nonevents),]
    subsample <- rbind(sample.dataset.events, sample.dataset.nonevents)
    return(subsample)
}

#Get developing data (datasetA), get events and non-events, choose sample size and join together
datasetA <- ImportMangrooveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangroove.table = '2012_summary')
sample.dataset.A <- CreateSubSample(datasetA, numberofevents, numberofdevelopmentnonevents)

## datasetA$SBP <- as.numeric(datasetA$SBP)
## datasetA$PULSE <- as.numeric(datasetA$PULSE)
## datasetA$RR <- as.numeric(datasetA$RR)
## datasetAevents <- subset(datasetA, Event == 1)
## datasetAnonevents <- subset(datasetA, Event == 0)
## sampledatasetAevents <- datasetAevents[sample(nrow(datasetAevents), numberofevents),]
## sampledatasetAnonevents <- datasetAnonevents[sample(nrow(datasetAnonevents), numberofnonevents),]
## sampledatasetA <- rbind(sampledatasetAevents, sampledatasetAnonevents)

#create model
modelM <- glm(Event ~ SBP + PULSE + RR, data = sample.dataset.A, family = 'binomial')
summary(modelM)

#get validation data (DatasetC) and pick sample 
datasetC <- ImportMangrooveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangroove.table = '2014_summary')
sample.dataset.C <- CreateSubSample(datasetC, numberofevents, numberofvalidationnonevents)
                                    
## datasetC$SBP <- as.numeric(datasetC$SBP)
## datasetC$PULSE <- as.numeric(datasetC$PULSE)
## datasetC$RR <- as.numeric(datasetC$RR)
## datasetCevents <- subset(datasetC, Event == 1)
## datasetCnonevents <- subset(datasetC, Event == 0)
## sampledatasetCevents <- datasetCevents[sample(nrow(datasetCevents), numberofvalidationevents),]
## sampledatasetCnonevents <- datasetCnonevents[sample(nrow(datasetCnonevents), numberofvalidationnonevents),]
## sampledatasetC <- rbind(sampledatasetCevents, sampledatasetCnonevents)

#compare results
resA <- predict(modelM, sample.dataset.C, type = 'response')
resA
resA <- predict(modelM, sample.dataset.A, type = 'response')
resA

confmatrixA <- table(Actual_value=sample.dataset.A$Event,Predicted_value=resA>0.5)
confmatrixA

modelMres <- (confmatrixA[[1,1]] + confmatrixA[[2,2]]) / sum(confmatrixA)
modelMres

#Get updating data (Dataset B), pick samples
datasetB <- ImportMangrooveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangroove.table = '2013_summary')
sample.dataset.B <- CreateSubSample(datasetB, numberofupdatingevents, numberofupdatingnonevents)

## datasetB$SBP <- as.factor(datasetB$SBP)
## datasetB$PULSE <- as.factor(datasetB$PULSE)
## datasetB$RR <- as.factor(datasetB$RR)
## datasetBevents <- subset(datasetB, Event == 1)
## datasetBnonevents <- subset(datasetB, Event == 0)

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
