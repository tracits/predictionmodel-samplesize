ImportMangrooveMySQL <- function(mysql.server.name, mysql.server.port, mysql.database, mysql.username, mysql.password, mysql.Mangroove.table = "2012_summary") {
    mydb <- dbConnect(MySQL(), user = mysql.username, password = mysql.password, dbname = mysql.database, host = mysql.server.name, port = mysql.server.port)
    ## Select all data from database
    study.data <- dbGetQuery(mydb, sprintf("select * from %s", mysql.Mangroove.table))
    dbDisconnect(mydb) ## Disconnect from database
    return(study.data)
}