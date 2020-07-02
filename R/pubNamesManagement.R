source("R/citsManagementFunctions.R")
library(readxl)
refsList <- list.files("results")
refsList <- refsList[!refsList %in% c("queriesInfo.WOKnScopus_wg2_ch05_2020-06-24.xlsx", "queriesInfo.WOKnScopus_wg2_ch05_2020-06-24.rds", "Readme.txt", "tryError.txt", "dois")]
scopusPubNames <- data.table(publicationName = character(), doi = character(), title = character(), queryName = character())
wokPubNames <- data.table(publicationName = character(), doi = character(), title = character(), queryName = character())
for (i in refsList) {
  print(i)
  queryName <- strsplit(i, "_")[[1]][1]
  temp.scopus <- as.data.table(read_excel(paste0("results/", i), 
                                        sheet = "SCOPUS complete"))
  temp.scopus <-  temp.scopus [, c("publicationName", "doi", "title")]
  
  temp.scopus$queryName <- queryName
  temp.wok <- as.data.table(read_excel(paste0("results/", i), 
                                        sheet = "WOK unique"))
  temp.wok <-  temp.wok [, c("publicationName", "doi", "title")]
  
  temp.wok$queryName <- queryName
  
  scopusPubNames <- rbind(scopusPubNames, temp.scopus)
  wokPubNames <- rbind(wokPubNames, temp.wok)
}

totRefsScopus <- nrow(scopusPubNames)
totRefsWOK <- nrow(wokPubNames)

scopusPubNames_unique <- as.data.table(scopusPubNames$publicationName, sorted=TRUE)
setnames(scopusPubNames_unique, old = "V1", new = "publicationName")
scopusPubNames_unique <- setorder(scopusPubNames_unique, publicationName)
scopusPubNames_unique <- unique(scopusPubNames_unique)

wokPubNames_unique <- as.data.table(wokPubNames$publicationName, sorted=TRUE)
setnames(wokPubNames_unique, old = "V1", new = "publicationName")
wokPubNames_unique <- setorder(wokPubNames_unique, publicationName)
wokPubNames_unique <- unique(wokPubNames_unique)

scopusPubNames_unique$keep <- "y"
wokPubNames_unique$keep <- "y"

write.csv(scopusPubNames_unique, "scopusPubNames_unique.csv")
write.csv(wokPubNames_unique, "wokPubNames_unique.csv")

#count the number of times a title occurs in a list of titles
duplicateTitles_wok <- wokPubNames %>% count(title)
duplicateTitles_scopus <- scopusPubNames %>% count(title)