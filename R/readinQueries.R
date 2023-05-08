#Copyright (C) 2019-2020 Gerald C. Nelson, except where noted.

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details at http://www.gnu.org/licenses/.
# scopus search terms at https://dev.elsevier.com/sc_search_views.html

source("R/citsManagementFunctions.R")
library(crayon)
chapter <- "wg2_ch05"
#chapter <- "wg2_ch16"
# year range
yearCoverage.scopus <- "PUBYEAR > 2020"
yearCoverage.wok <- "= 2021-2023"
queries <- as.data.table(read_excel(paste0("data-raw/queries_wg2_ch05.xlsx"), sheet = "baseQueries"))

if (!nrow(queries) == max(queries$queryNumber)) {
  stop("Query numbers probably need to be updated.")
}

#get info for all the queries in queries from wok database
queryInfo <- getDBinfo(queries, yearCoverage.wok, yearCoverage.scopus,CCSearchString = CCSearchString)

inDT <- queryInfo
outName <- paste0("queriesInfo.WOKnScopus_", chapter) 
desc = "SCOPUS and Web of Knowledge Query, QueryId, and number of references for each query in queries.xlsx"
cleanup(inDT = inDT, outName = outName, destDir = "results", writeFiles = "xlsx", numVal = "0", wrapCols = c(1,4, 9:10))

maxQueries <- 5000
# assemble queries
#keep only queries that have less than maxQueries SCOPUS references
queries.small <- queryInfo[nrResults.scopus < maxQueries,]
#queriestoProcessList <- paste0("1:", nrow(queries.small))
queryNum <- 1

for (i in 1:nrow(queries.small)) {
  queryNum <- eval(parse(text = queries.small[,.SD[i]]$queryNumber))     
  prepareOutput(queryNum = queryNum, queries = queries.small, rejectList_master, climateMitigation)
}

# get the urls for each of the dois -----
outFile.bib <- paste0("results/bibout", "_", queryInfo$fileName, "_", Sys.Date(), ".bib")
h <- new_handle()
handle_setheaders(h, "accept" = "application/x-bibtex")

# for (i in 161:length(dois.combined)) {
#      print(i)
#   url <- paste0("https://doi.org/", dois.combined[i])
#   if (!grepl("NULL", url, fixed = TRUE)) {
#     try(curl_download(url, destfile = outFile.bib, handle = h, mode = "a"), outFile = "results/tryError.txt") # use this to keep going even if there is an error
#     #curl_download(url, destfile = outFile.bib, handle = h, mode = "a")
#   }
# }

## create a vector of urls
#urls <- (character())
# for (i in 161:176) {
#   url <- paste0("https://doi.org/", dois.combined[i])
#   urls <- c(url, urls)
# }
# error_filename = paste0("results/doiError", "_", queryInfo$fileName, "_", Sys.Date(), ".txt")
# outFile.bib <- paste0("results/bibout", "_", queryInfo$fileName, "_", Sys.Date(), ".bib")
# #for (i in 161:length(dois.combined)) {
# for (i in 161:176) {
#   print(paste0("working on doi ", dois.combined[i]))
# url <- paste0("https://doi.org/", dois.combined[i])
# tryCatch({
#   curl_download(url, destfile = outFile.bib, handle = h, mode = "a")
# }, error = function(e) {
#   # if there's an error, save the result and increment the counter
#   error_message <- paste0("Error at URL ", url, ": ", conditionMessage(e), "\n")
#   print(error_message)
#   write_file(error_message, error_filename, append = TRUE)
# })
# }

doiToBibtex(doiList = dois.combined, filename = outFile.bib)
