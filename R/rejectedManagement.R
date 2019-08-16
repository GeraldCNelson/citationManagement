#Copyright (C) 2019 Gerald C. Nelson, except where noted.

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.
source("R/citsManagementFunctions.R")
rejectedPath.in <- "data/rejected/rejected_in/"
rejectedPath.out <- "data/rejected/rejected_out/"

rejectfilesToProcess <- list.files(rejectedPath.in)
rejectfilesOut <- list.files(rejectedPath.out)
rejectfilesToProcess <- grep("xlsx", rejectfilesToProcess, value = TRUE)
rejectfilesOld <- grep("xlsx", rejectfilesToProcess, value = TRUE)


for (i in rejectfilesToProcess) {
  fileComponents <- strsplit(i, split = "_")
  queryName <- fileComponents[[1]][[1]]
  queryDate <- fileComponents[[1]][[2]]
  rejectAuthor <- gsub(".xlsx", "", fileComponents[[1]][[3]])
  dt <- paste0(rejectedPath.in, i)
  dt.scopus <- as.data.table(read_excel(dt, sheet = "SCOPUS complete"))
  dt.wok <- as.data.table(read_excel(dt, sheet = "WOK unique"))
  dt_reject_scopus <- dt.scopus[keepRef %in% c("N", "n"),]
  dt_reject_wok <- dt.wok[keepRef %in% c("N", "n"),]
  dt_keep_scopus <- dt.scopus[!keepRef %in% c("N", "n"),]
  dt_keep_wok <- dt.wok[!keepRef %in% c("N", "n"),]
  
  outFileName <- paste0(queryName, queryDate, rejectAuthor, )
  assign()
}

chapter <- "wg2_ch05"
queries <- as.data.table(read_excel(paste0("data-raw/queries_", chapter, ".xlsx"), sheet = "Queries"))
outputName <- "pestsCrops"
reviewer <- queries[outFileName %in% outputName, reviewer]
reviewer <- "RBK"
reviewer <- "DD"
nameCombo <- paste(outputName, reviewer, sep = "_")
rejectedPath <- "data/rejected/"
emptyPathNname <- paste0(rejectedPath, nameCombo)
fileList <- list.files(rejectedPath)
# get just the names of the files that end in xlsx
dt <- as.data.table(read_excel("data/rejected/pestsCrops_2019-07-31-rbk.xlsx", 
                               sheet = "SCOPUS complete"))
dt <- dt[keepRef %in% c("N", "n"),]
dt_doi <- dt$doi
dt_doiEmpty <- dt[is.na(doi),]
dt_doi <- dt_doi[!is.na(dt_doi)]
write.xlsx(dt_doiEmpty, file = paste0(emptyPathNname, ".xlsx"))
write.csv(dt_doi, file = paste0(emptyPathNname, ".csv"))

h <- new_handle()
handle_setheaders(h, "accept" = "application/x-bibtex")

# for (i in 1:length(dt_doi)) {
#   url <- paste0("https://doi.org/", dt_doi[i])
#   print(paste0("url: ", url))
#   curl_download(url, destfile = "curltest.bib", handle = h, mode = "a")
# }

# create url list for all the dois in the reject dataset
urls <- character()
for (i in 1:length(dt_doi)) {
  urls <- c(urls, paste0("https://doi.org/", dt_doi[i]))
}

outputFileName <- paste0(rejectedPath, outputName, "_", reviewer, "_", chapter, ".bib")
remove(eval((parse(text = outputFileName)))) # to start with an empty outputFileName
y <- sapply(urls, createBibtexfile, outputFileName = outputFileName)
z <- as.data.table(unlist(y))
t <- cbind(dt_doi, z)
t <- t[V1 %in% "Rejected",][, V1 := NULL]
IntroString <- "doi.org was not able to access information for the following DOIs."
t <- rbind(IntroString, t, use.names = FALSE)
toutfile <- paste0(rejectedPath, outputName, "_", reviewer, "_", chapter, "doi.org.rejects.csv")
write.csv(t, file = toutfile)