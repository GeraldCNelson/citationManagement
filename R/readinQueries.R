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

# get Scopus api key
get_api_key(api_key = Sys.getenv('Elsevier_API'))
if (!have_api_key()) stop("Missing api key")

# get WOS api key
wosliteKey <- Sys.getenv("wosliteKey")

# year range
yearCoverage.scopus <- "> 2013"
yearCoverage.wok <- "= 2014-2019"

#get info for all the queries in queries from wok database
queryInfo <- getDBinfo(queries, yearCoverage.wok, yearCoverage.scopus,CCSearchString = CCSearchString)
inDT <- queryInfo
outName <- "queriesInfo.WOKnScopus"
desc = "SCOPUS and Web of Knowledge Query, QueryId, and number of references for each query in queries.xlsx"
cleanup(inDT = inDT, outName = outName, destDir = "results", writeFiles = "xlsx", numVal = "0", wrapCols = c(1,4, 9:10))

# assemble queries
#keep only queries that have more than zero and less than 5000 SCOPUS references
queries.small <- queryInfo[nrResults.scopus < 5000 & !nrResults.scopus == 0,]
queriestoProcessList <- paste0("1:", nrow(queries.small))
queryNum <- 2

for (i in 1:nrow(queries.small)) {
  queryNum <- eval(parse(text = queries.small[,.SD[i]]$queryNumber))
prepareOutput(queryNum = queryNum, queries = queries.small)
}
  
# for (i in 1:queryCount) {
#   print(paste0("working on query ", i, " of ", queryCount))
#   queryRowNumber <- queries.small[i, queryNumber]
#   rawQuery <- queries[queryNumber %in% queryRowNumber,query]
#   outFileName <- queries[queryNumber %in% queryRowNumber, outFileName]
#   # rawQuery.scopus <- gsub('" ', '} ', rawQuery)
#   # rawQuery.scopus <- gsub('"', '{ ', rawQuery.scopus)
#   query.scopus <- constructQuery.scopus(rawQuery, yearCoverage.scopus) # need to use queries here to get at the raw query string
#   query.wok <- constructQuery.WOK(rawQuery, yearCoverage.wok) # need to use queries here to get at the raw query string
#   QueryID.wok <- queries.small[queryNumber %in% queryRowNumber, QueryID.wok] # for WOK
#   nrResults.wok <- queries.small[queryNumber %in% queryRowNumber, nrResults.wok] # for WOK
#   # queryResults.wok <- readinWOK(query = query.wok)
#   queryResults.wok <- readinWOKWithQueryID(query = query.wok, QueryID = QueryID.wok, nrResults = nrResults.wok)
#   
#   queryResults.scopus <- readinSCOPUS(query = query.scopus)
#   
#   # process if both results have content
#   if (!nrow(queryResults.scopus) == 0 & (!nrow(queryResults.wok) == 0)) {
#     doi.wok <- sort(queryResults.wok$doi)
#     doi.scopus <- sort(queryResults.scopus$doi)
#     
#     doi.common <- doi.wok[doi.wok %in% doi.scopus]
#     doi.unique.wok <- doi.wok[!doi.wok %in% doi.scopus]
#     doi.unique.scopus <- doi.scopus[!doi.scopus %in% doi.wok]
#     queryResults.wok.unique <- queryResults.wok[!doi %in% doi.common,]
#     
#     # do the same thing with eIssns
#     eissn.wok <- sort(queryResults.wok$eIssn)
#     eissn.scopus <- sort(queryResults.scopus$eIssn)
#     
#     eissn.common <- eissn.wok[eissn.wok %in% eissn.scopus]
#     eissn.unique.wok <- eissn.wok[!eissn.wok %in% eissn.scopus]
#     eissn.unique.scopus <- eissn.scopus[!eissn.scopus %in% eissn.wok]
#     queryResults.wok.unique <- queryResults.wok.unique[!eIssn %in% eissn.common,]
#     
#     prepareSpreadsheet(sectionName = queries.small[i, sectionName], queryResults.scopus, query.scopus, queryResults.wok = queryResults.wok.unique, query.wok, outFileName)
#   }
#   if (nrow(queryResults.scopus) == 0 ){
#     print(paste0("SCOPUS results for query ", i, ", query" , query.scopus, " are empty"))
#   }
#   if (nrow(queryResults.wok) == 0 ){
#     print(paste0("WOK results for query ", i, ", query" , query.qok, " are empty"))
#   }
# }

idois.combined <- unique(c(doi.wok, doi.scopus))
doi2bib(dois.combined, file = paste("results/", outFileName, "_", Sys.Date(),".bib"), quiet = TRUE)

# doi2bib(doi.common, file = paste("results/", outFileName, "_scopus.bib"), quiet = TRUE)
# doi2bib(doi.wok, file = paste("results/", outFileName, "_wok.bib"), quiet = TRUE)


