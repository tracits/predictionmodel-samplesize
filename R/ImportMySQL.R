#Change mysql username and password below
ImportMangrooveMySQL <- function(mysql.server.name = "127.0.0.1", mysql.server.port = 3307, mysql.database = "NTDB_adam", mysql.username = "x", mysql.password = "x", mysql.Mangroove.table = "2012_summary") {
    mydb <- dbConnect(MySQL(), user = mysql.username, password = mysql.password, dbname = mysql.database, host = mysql.server.name, port = mysql.server.port)
    ## Select all data from database
    study.data <- dbGetQuery(mydb, sprintf("select * from %s", mysql.Mangroove.table))
    dbDisconnect(mydb) ## Disconnect from database
    return(study.data)
}
