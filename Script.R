library('dbConnect')
#Initialize
setwd("/home/adam/Desktop/source/repos/predictionmodel-samplesize")
source(".sshconfig.R")
source("R/MySQLFunctions.R")
source("R/CreateSubSample.R")
source("R/CompareModels.R")
## Settings

## Note that it is the number of events in datasetB that should vary between 1
## and 1000, but that the outcome prevalence should be the same in datasetC and
## datasetB. So if the prevelance is 0.02, and the number of events is 1, then
## the number of patients in datasetB should be 50, because 1/0.02=50.

## The number of events in datasetA and C should always be 200, and the number
## of non-events should vary depending on the outcome prevalence
numberofevents <- 200
prevalenceinterval <- c(0.02, 0.05, 0.10)

#Get Datasets
print("Get Datasets")
datasetA <- ImportMangroveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangrove.table = '2012_summary')
datasetB <- ImportMangroveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangrove.table = '2013_summary')
datasetC <- ImportMangroveMySQL(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangrove.table = '2014_summary')

executionID <- format(Sys.time(), "%Y%m%d%H%M%OS")
allprevalences <- data.frame(t(expand.grid(prevalenceinterval, prevalenceinterval)))
RunStudy <- function(numberofupdatingevents,loopCount) {
    gc()
    #Start loop confindence (0.02, 0.05, 0.10) changes with each loop. Note that the
    #outcome prevelance should always be the same in datasetB and datasetC

    for (prevalenceArr in allprevalences) {
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

        #Get updating data (Dataset B), pick samples
        sample.dataset.B <- CreateSubSample(datasetB, numberofupdatingevents, numberofupdatingnonevents)

        # Update model
        sample.dataset.B$p <- predict(modelM, newdata = sample.dataset.B)
        modelUM <- glm(Event ~ p, data = sample.dataset.B)

        modelMIntercept <- coef(modelM)["(Intercept)"]
        modelMSBP <- coef(modelM)["SBP"]
        modelMPULSE <- coef(modelM)["PULSE"]
        modelMRR <- coef(modelM)["RR"]
        modelMGCSTOT <- coef(modelM)["GCSTOT"]

        modelUMIntercept <- coef(modelUM)["(Intercept)"]
        modelUMP <- coef(modelUM)["p"]

        # Use both M and UM to predict in validation sample
        sample.dataset.C$Mlp <- with(sample.dataset.C, modelMIntercept + modelMSBP * SBP + modelMPULSE * PULSE + modelMRR * RR + modelMGCSTOT * GCSTOT)
        # Convert to probability
        sample.dataset.C$Mp <- 1/(1 + exp(-sample.dataset.C$Mlp))
        
        # Repeat with UM
        sample.dataset.C$UMlp <- with(sample.dataset.C, modelMIntercept + modelUMIntercept + modelUMP * (modelMSBP * SBP + modelMPULSE * PULSE + modelMRR * RR + modelMGCSTOT * GCSTOT))
        sample.dataset.C$UMp <- 1/(1 + exp(-sample.dataset.C$UMlp))

        #compare results from both models
        #cm <- CompareModels(sample.dataset.C$UMp, sample.dataset.C$Up)

        StoreLoopData(executionID, loopCount, developmentprevalence, updatingvalidationprevalence, numberofdevelopmentnonevents, numberofvalidationnonevents, numberofupdatingnonevents, modelMIntercept, modelMSBP, modelMPULSE, modelMRR, modelMGCSTOT, modelUMIntercept, 0)
        print(loopCount)
    }
}

library(doParallel)
library(foreach)

myCluster <- makeCluster(detectCores(), type = "FORK") # why "FORK"?
registerDoParallel(myCluster)

init <- Sys.time()

print("Loop starting")
loopCount <- 0

#Start loop number of updating events (1-1000) changes with each loop
updatingevents <- c(1:1000)
repeattimesforconfidence <- 1 #should be 1000 times
r <-foreach(s=rep(updatingevents,repeattimesforconfidence), .combine=c) %dopar% {
  loopCount = loopCount + 1
  RunStudy(s,loopCount)
}

a <- Sys.time() - init
print(a)
stopCluster(myCluster)

