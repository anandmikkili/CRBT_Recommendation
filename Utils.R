'
Script    : Utils
Created   : August 31, 2018
'
getData <- function(file) {
  interested <- file[, c(1, 2)]
  purchased <- file[, c(1, 3)]
  interested <-
    separate_rows(interested, names(interested)[2], sep = ":")
  names(interested)[2] <- "CATEGORY_ID"
  second1 <-
    separate_rows(purchased, names(purchased)[2], sep = ":")
  purchased <-
    second1 %>% separate(names(purchased)[2], c("CATEGORY_ID", "TONE_ID")) %>% drop_na()
  data <- list(interested, purchased)
  return(data)
}

getTopRecos <- function(msisdnGroup) {
  msisdnNRJFY <-
    msisdnGroup %>% filter(NAME == "NewReleases" |
                             NAME == "JUST FOR YOU") %>% group_by(NAME) %>% top_n(n = -20, wt = RANK)
  msisdnTR <-
    msisdnGroup %>% filter(NAME == "Trending") %>% group_by(NAME) %>% top_n(n = -10, wt = RANK)
  if (nrow(msisdnNRJFY) == 0 & nrow(msisdnTR) == 0) {
    print("Top recommendations are Empty..!!")
    return(NULL)
  } else {
    finalRecos <- rbind(msisdnNRJFY, msisdnTR)
    return(finalRecos)
  }
}

getMsisdnGroupList <- function(msisdnToneDetails) {
  msisdnGroupList <- NULL
  msisdnList <- unique(msisdnToneDetails$MSISDN)
  for (i in 1:length(msisdnList)) {
    msisdnFrame <- msisdnToneDetails %>% filter(MSISDN == msisdnList[i])
    categoryIDs <- unique(msisdnFrame$CATEGORY_ID)
    msisdnGroupFrame <- data.frame()
    for (j in 1:length(categoryIDs)) {
      categoryFrame <-
        msisdnFrame %>% filter(CATEGORY_ID == categoryIDs[j]) %>% arrange(NAME, RANK)
      msisdnGroupFrame <- rbind(msisdnGroupFrame, categoryFrame)
    }
    msisdnGroupList[[i]] <- msisdnGroupFrame
  }
  return(msisdnGroupList)
}

getRecos <- function(fileToReco) {
  toneDetails <- getToneDescription()
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
    ), ]
  msisdnGroupList <- getMsisdnGroupList(msisdnToneDetails)
  topRecoList <- NULL
  for (i in 1:length(msisdnGroupList)) {
    topRecoList[[i]] <- getTopRecos(msisdnGroupList[[i]])
  }
  finalFrame <- do.call("rbind", topRecoList)
  return(finalFrame)
}

getSunburstCategories <- function() {
  categories <- getToneCategories()
  categories <-
    categories %>%  mutate_each(funs(replace(., . == '', "Unknown")))
  categoryGroup <-
    categories %>% group_by(NAME, PARENT_ID, CATAGORY_ID) %>% summarise(Count =
                                                                          n())
  categoryGroup <- as.data.frame(categoryGroup)
  finalData <-
    within(categoryGroup, index2 <-
             paste(NAME, PARENT_ID, sep = "-"))
  finalData <-
    within(finalData, index3 <-
             paste(index2, CATAGORY_ID, sep = "-"))
  finalData <- finalData[, c(6, 4)]
  return(finalData)
}

getRecoCategories <- function(fileToReco) {
  recoDetails <- getRecos(fileToReco)
  names(recoDetails) <-
    c(
      "Subscriber Number",
      "Category ID",
      "Parent ID",
      "Tone ID",
      "Tone Name",
      "Download Count",
      "Like Count",
      "Rank",
      "Name"
    )
  recoDetails <-
    recoDetails %>%  mutate_each(funs(replace(., . == '', "Unknown")))
  finalReco <-
    recoDetails %>% group_by(Name) %>% summarise(Count = n())
  finalReco <- as.data.frame(finalReco)
  return(finalReco)
}
