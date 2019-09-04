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

# keep only xlsx files
rejectfilesToProcess <- grep("xlsx", rejectfilesToProcess, value = TRUE)

# get the list of xlsx files that already exist in the out folder
rejectfilesOld <- grep("xlsx", rejectfilesOut, value = TRUE)

# first open up previous list of rejected and accepted files for this query or create it
if ("dt_reject_list.csv" %in% rejectfilesOut) {
  dt_reject_list <- as.data.table(read_csv("dt_reject_list.csv"))
}else{
  dt_reject_list <- data.table(queryName = character(), rejectTitle = character(), rejectDoi = character(),
                               rejectAuthor = character(), rejectDate = character(), query_scopus = character(), query_wok = character())
}

for (i in rejectfilesToProcess) {
  fileComponents <- strsplit(i, split = "_")
  queryName <- fileComponents[[1]][[1]]
  queryDate <- fileComponents[[1]][[2]]
  rejectAuthor <- gsub(".xlsx", "", fileComponents[[1]][[3]])
  rejectedfileName_scopus <- paste0(queryName, "_rejected_master_scopus.xlsx")
  rejectedfileName_wok <- paste0(queryName, "_rejected_master_wok.xlsx")
  
  
  # now read in the information for this query for scopus and wok sheets
  dt <- paste0(rejectedPath.in, i)
  dt.scopus <- as.data.table(read_excel(dt, sheet = "SCOPUS complete"))
  dt.wok <- as.data.table(read_excel(dt, sheet = "WOK unique"))
  dt.metadata <- as.data.table(read_excel(dt, sheet = "Metadata"))
  
  # extract references that have been rejected and get unique identifier, currently the title.
  dt_reject_scopus <- dt.scopus[keepRef %in% c("N", "n"),]
  query_scopus <- dt.metadata[3, searchString]
  query_wok <- dt.metadata[4, searchString]
  reject.title.scopus <- dt_reject_scopus$title
  reject.doi.scopus <- dt_reject_scopus$doi
  dt_reject_wok <- dt.wok[keepRef %in% c("N", "n"),]
  reject.title.wok <- dt_reject_wok$title
  reject.doi.wok <- dt_reject_wok$doi
  dt <- data.table(
    rejectTitle = c(reject.title.scopus, reject.title.wok),
    rejectDoi = c(reject.doi.scopus, reject.doi.wok), query_scopus = query_scopus, query_wok = query_scopus)
    dt[, rejectAuthor := rejectAuthor][, rejectDate := queryDate][
      , queryName := queryName]
  dt_reject_list <- rbind(dt_reject_list, dt)
  
  # get references to keep for scopus and wok
  dt_keep_scopus <- dt.scopus[!keepRef %in% c("N", "n"),]
  dt_keep_wok <- dt.wok[!keepRef %in% c("N", "n"),]
}

dt_reject_list <- unique(dt_reject_list)
outFileName <- paste0(rejectedPath.out, "rejectList_master", ".xlsx")
# write_csv(dt_reject_list, outFileName)  

wbGeneral <- openxlsx::createWorkbook()
longName <- outFileName
outName <- strtrim(outFileName, c(31))

# first add the needed worksheets
openxlsx::addWorksheet(wb = wbGeneral, sheetName = "Rejected Refs")
openxlsx::writeDataTable(
  wbGeneral,
  dt_reject_list,  sheet = "Rejected Refs", startRow = 1, startCol = 1, rowNames = FALSE,
  colNames = TRUE, withFilter = TRUE)
openxlsx::setColWidths(
  wbGeneral, sheet = "Rejected Refs", cols = 1:5 , widths = c(20, 70, 30, 15, 15))
openxlsx::saveWorkbook(wbGeneral, outFileName, overwrite = TRUE)


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