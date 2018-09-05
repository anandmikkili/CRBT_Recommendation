getDBConnection <- function() {
  dbProps <- read.properties("DB.properties")
  drv <-
    JDBC(dbProps$MYSQL_DRIVER,
         dbProps$CONNECTOR_JAR_PATH,
         identifier.quote = "`")
  con <-
    dbConnect(
      drv,
      paste(
        "jdbc:mysql://",
        dbProps$HOST,
        ":",
        dbProps$PORT,
        "/",
        dbProps$DATABASE_NAME,
        sep = ""
      ),
      dbProps$USERNAME,
      dbProps$PASSWORD
    )
  connection_list <- list(con, dbProps)
  return(connection_list)
}

getToneCategories <- function() {
  connection <- getDBConnection()[[1]]
  dbProps <- getDBConnection()[[2]]
  toneDetails <- dbGetQuery(connection, dbProps$CAT_QUERY)
  dbDisconnect(connection)
  return(toneDetails)
}

getToneDescription <- function() {
  connection <- getDBConnection()[[1]]
  dbProps <- getDBConnection()[[2]]
  toneDescriptions <- dbGetQuery(connection, dbProps$QUERY)
  dbDisconnect(connection)
  return(toneDescriptions)
}

getMsisdn <- function() {
  connection <- getDBConnection()[[1]]
  dbProps <- getDBConnection()[[2]]
  msisdns <- dbGetQuery(connection, dbProps$QUERY2)
  dbDisconnect(connection)
  return(msisdns)
}

formatQuery <-
  function(subs_no, preddate) {
    print(preddate)
    connection <- getDBConnection()[[1]]
    dbProps <- getDBConnection()[[2]]
    query <- dbProps$QUERY3
    query <- gsub("SUBS_NO", subs_no, query)
    query <- gsub("PREDICT_DATE", preddate, query)
    msisdns <- dbGetQuery(connection, query)
    dbDisconnect(connection)
    return(msisdns)
}
