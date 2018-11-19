library('dbConnect')

## Settings

## Note that it is the number of events in datasetB that should vary between 1
## and 1000, but that the outcome prevalence should be the same in datasetC and
## datasetB. So if the prevelance is 0.02, and the number of events is 1, then
## the number of patients in datasetB should be 50, because 1/0.02=50.

## The number of events in datasetA and C should always be 200, and the number
## of non-events should vary depending on the outcome prevalence
numberofevents <- 200
prevalenceinterval = c(0.02, 0.05, 0.10)


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
for (numberofupdatingevents in c(1:1)) { #Test with only 200
    print(paste("Start loop numberofupdatingevents", numberofupdatingevents))
    #Start loop confindence (0.02, 0.05, 0.10) changes with each loop. Note that the
    #outcome prevelance should always be the same in datasetB and datasetC
    
    for (prevalenceArr in data.frame(t(expand.grid(prevalenceinterval, prevalenceinterval)))) {
        loopCount <- loopCount+1
        developmentprevalence <- prevalenceArr[1]
        updatingvalidationprevalence <- prevalenceArr[2]
        print(paste("Start loop prevalence", developmentprevalence, updatingvalidationprevalence))

        ## Now define the number of non-events
        numberofdevelopmentnonevents <- ceiling((numberofevents / developmentprevalence) - numberofevents)
        numberofvalidationnonevents <- ceiling((numberofevents / updatingvalidationprevalence) - numberofevents)
        numberofupdatingnonevents <- ceiling((numberofupdatingevents / updatingvalidationprevalence) - numberofupdatingevents)

        gdm <- GetDevelopmentModel(datasetA, numberofevents, numberofdevelopmentnonevents)
        sample.dataset.A <- gdm[1]
        modelM <- gdm[2]

        gvm <- GetValidateModel(datasetC, numberofevents, numberofvalidationnonevents, sample.dataset.A, modelM)
        sample.dataset.A <- gvm[1]

        gum <- GetUpdatingModel(datasetB, numberofupdatingevents, numberofupdatingnonevents)
        sample.dataset.B <- gum[1];
        modelUM <- gum[2];
        mMIntercept <- gum[3];
        mMcoefSBP <- gum[4];
        mMcoefPULSE <- gum[5];
        mMcoefRR <- gum[6];
        mMcoefGCS <- gum[7];
        mUMIntercept <- gum[8];
        mUMcoef <- gum[9];

        #compare results from both models
        cm <- CompareModels('')

        print(loopCount)
        StoreLoopData(executionID, loopCount, developmentprevalence, updatingvalidationprevalence, numberofdevelopmentnonevents, numberofvalidationnonevents, numberofupdatingnonevents, mMIntercept, mMcoefSBP, mMcoefPULSE, mMcoefRR, mMcoefGCS, mUMIntercept, mUMcoefP)
        print("Finished")
    }
}