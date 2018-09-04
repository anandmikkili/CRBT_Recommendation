source("PackageLoad.R")
source("Utils.R")
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
toneDetails <- dbGetQuery(con, dbProps$QUERY)

toneDetails <- toneDetails %>% arrange(CATEGORY_ID)
categories <- getData(fileToReco)
interestedCategories <- categories[[1]]
interestedCategories <-
  interestedCategories %>% arrange(CATEGORY_ID)
msisdnToneDetails <-
  left_join(interestedCategories, toneDetails, by = "CATEGORY_ID")
purchasedCategories <- categories[[2]]
msisdnToneDetails <-
  msisdnToneDetails[!(
    msisdnToneDetails$MSISDN %in% purchasedCategories$MSISDN &
      msisdnToneDetails$CATEGORY_ID %in% purchasedCategories$CATEGORY_ID &
      msisdnToneDetails$TONE_ID %in% purchasedCategories$TONE_ID
  ),]
msisdnGroupList <- getMsisdnGroupList(msisdnToneDetails)
topRecoList <- NULL
for (i in 1:length(msisdnGroupList)) {
  topRecoList[[i]] <- getTopRecos(msisdnGroupList[[i]])
}
finalFrame <- do.call("rbind", topRecoList)
