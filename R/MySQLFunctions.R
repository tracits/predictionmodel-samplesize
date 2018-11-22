ImportMangroveMySQL <- function(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangrove.table = "2012_summary") {
    mydb <- dbConnect(MySQL(), user = mysql.username, password = mysql.password, dbname = mysql.database, host = mysql.server.name, port = mysql.server.port)
    ## Select all data from database
    study.data <- dbGetQuery(mydb, sprintf("select * from %s", mysql.Mangrove.table))
    dbDisconnect(mydb) ## Disconnect from database
    return(study.data)
}

StoreLoopData <- function(executionID, loopCount, developmentprevalence, updatingvalidationprevalence, numberofdevelopmentnonevents, numberofvalidationnonevents, numberofupdatingnonevents, modelMIntercept, modelMSBP, modelMPULSE, modelMRR, modelMGCSTOT, modelUMIntercept, comparisonResult) {
    mydb <- dbConnect(MySQL(), user = mysql.username, password = mysql.password, dbname = mysql.database, host = mysql.server.name, port = mysql.server.port)
    ## Select all data from database
    dbGetQuery(mydb, sprintf("INSERT INTO `NTDB_adam`.`runtime_data` (`executionID`,`loopCount`,`developmentprevalence`,`updatingvalidationprevalence`,`numberofdevelopmentnonevents`,`numberofvalidationnonevents`,`numberofupdatingnonevents`,`modelMIntercept`,`modelMSBP`,`modelMPULSE`,`modelMRR`,`modelMGCSTOT`,`modelUMIntercept`,`comparisonResult`) VALUES (%s,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g);", executionID, loopCount, developmentprevalence, updatingvalidationprevalence, numberofdevelopmentnonevents, numberofvalidationnonevents, numberofupdatingnonevents, modelMIntercept, modelMSBP, modelMPULSE, modelMRR, modelMGCSTOT, modelUMIntercept, comparisonResult))
    dbDisconnect(mydb) ## Disconnect from database
}