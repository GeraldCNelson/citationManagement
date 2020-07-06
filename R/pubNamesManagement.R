source("R/citsManagementFunctions.R")
library(readxl)
refsList <- list.files("results")
removeFromRefsList <- refsList[grep("queriesIn", refsList, fixed = TRUE)]
refsList <- refsList[!refsList %in% c("Readme.txt", "tryError.txt", "dois")]
refsList <- refsList[!refsList %in% removeFromRefsList]
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

queryCt <- length(refsList)
totRefsScopus <- nrow(scopusPubNames)
totRefsWOK <- nrow(wokPubNames)
uniqueDOIsScopus <- unique(scopusPubNames[, c("doi", "title")])
ct_uniqueDOIsScopus <- nrow(uniqueDOIsScopus)
uniqueDOIsSWOK <- unique(wokPubNames[, c("doi", "title")])
ct_uniqueDOIsWOK <- nrow(uniqueDOIsSWOK)

scopusPubNames_unique <- as.data.table(scopusPubNames$publicationName, sorted=TRUE)
setnames(scopusPubNames_unique, old = "V1", new = "publicationName")
scopusPubNames_unique <- setorder(scopusPubNames_unique, publicationName)
scopusPubNames_unique <- unique(scopusPubNames_unique)
ct_uniquePubs_Scopus <- nrow(scopusPubNames_unique)

wokPubNames_unique <- as.data.table(wokPubNames$publicationName, sorted=TRUE)
setnames(wokPubNames_unique, old = "V1", new = "publicationName")
wokPubNames_unique <- setorder(wokPubNames_unique, publicationName)
wokPubNames_unique <- unique(wokPubNames_unique)
ct_uniquePubs_Wok <- nrow(wokPubNames_unique)

scopusPubNames_unique$keep <- "y"
wokPubNames_unique$keep <- "y"

write.csv(scopusPubNames_unique, "scopusPubNames_unique.csv")
write.csv(wokPubNames_unique, "wokPubNames_unique.csv")

#count the number of times a title occurs in a list of titles
duplicateTitles_wok <- wokPubNames %>% count(title)
duplicateTitles_scopus <- scopusPubNames %>% count(title)

#count the number of times a publication occurs in a list of publications
duplicatePubs_wok <- wokPubNames %>% count(publicationName)
duplicatePubs_scopus <- scopusPubNames %>% count(publicationName)

lowUsePubs_wok <- duplicatePubs_wok[n == 1,]
lowUsePubs_scopus <- duplicatePubs_scopus[n == 1,]

lowUsePubsRefs_wok <- wokPubNames[publicationName %in% lowUsePubs_wok$publicationName,]
lowUsePubsRefs_scopus <- scopusPubNames[publicationName %in% lowUsePubs_scopus$publicationName,]

write.csv(lowUsePubsRefs_wok, "results/oneRefPubs_wok.csv")
write.csv(lowUsePubsRefs_scopus, "results/oneRefPubs_scopus.csv")
