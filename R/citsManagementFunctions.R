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
library(readxl)
library(data.table)
library(openxlsx)
library(readr)
library(stringr)
library(rscopus)
library(httr)
library(jsonlite)
library(stringr)
library(doi2bib) #install with install.packages("remotes"); remotes::install_github("wkmor1/doi2bib")

# get list of queries
queries <- as.data.table(read_excel("data-raw/queries_wg2_ch05.xlsx", sheet = "Queries"))
climateChangeSearchTerms <- as.data.table(read_excel("data-raw/queries_wg2_ch05.xlsx", sheet = "CCSearchString"))
CCSearchString <- climateChangeSearchTerms[searchStringName %in% "searchStrings.climateChange", searchString]

# get country names
regions_lookup <- read_excel("data-raw/regions lookup June 15 2018.xlsx")
searchStrings.countries <- regions_lookup$country_name.ISO
searchStrings.regionsByIncome <- regions_lookup$region_code.WB.income
# other search strings
searchStringsList <- as.data.table(read_excel("data-raw/queries_wg2_ch05.xlsx", sheet = "searchTerms"))
countrylist <- list("searchStrings.countries", as.list(searchStrings.countries))
regionsByIncomeList <- list("searchStrings.regionsByIncome", as.list(searchStrings.regionsByIncome))
searchStringsList <- rbind(list("searchStrings.countries", list(searchStrings.countries)), searchStringsList)
# next line commented out because no need to have a column for searchStrings.regionsByIncome. It will be empty.
# searchStringsList <- rbind(searchStringsList, list("searchStrings.regionsByIncome", list(searchStrings.regionsByIncome)))
searchStrings <- searchStringsList$searchStringName
searchStrings.names <- gsub("searchStrings.", "", searchStrings)

#' Title cleanupRefFiles - remove old versions and save rds and xlsx or csv versions of the file
#' @param inDT - name of the data table or frame to be written out
#' @param outName - short name of the file to be written out
#' @param destDir - directory where the cleanup takes place
#' @param writeFiles - format to use for writing output in addition to RDS
#' @desc brief description of the contents of the file
cleanupRefFiles <- function(metadata, inDT.scopus, inDT.wok, outName, destDir, writeFiles) {
  # sourceFile <- get("sourceFile", envir = .GlobalEnv)
  if (missing(writeFiles)) {writeFiles = "xlsx"}
  
  removeOldVersions(outName, destDir)
  sprintf("\nWriting the rds for %s to %s ", outName, destDir)
  # print(proc.time())
  # next line removes any key left in the inDT data table; this may be an issue if a df is used
  # data.table::setkey(outName, NULL)
  outFile <- paste(destDir, "/", outName, "_", Sys.Date(), ".RDS", sep = "")
  #saveRDS(inDT, file = outFile)
  
  # # update files documentation -----
  # Note: fileDocumentation.csv is currently not being used.
  # fileDoc <- data.table::as.data.table(read.csv(paste(fileloc("rawData"), "fileDocumentation.csv", sep = "/"),
  #     header = TRUE, colClasses = c("character","character","character")))
  # fileDoc <- fileDoc[!fileShortName == outName]
  # fileDocUpdate <- as.list(c(outName, outFile, paste0(names(inDT), collapse = ", ")))
  # fileDoc <- rbind(fileDoc, fileDocUpdate)
  # write.csv(fileDoc, paste(fileloc("mData"), "fileDocumentation.csv", sep = "/"), row.names = FALSE)
  #
  # #print(proc.time())
  # if (nrow(inDT.scopus) > 75000) {
  #   sprintf("\nThe number of rows in the SCOPUS data set, %s, is greater than 50,000. Not writing xlsx or csv", nrow(inDT.scopus))
  #   writeFiles <- writeFiles[!writeFiles %in% c("xlsx")]
  # }
  # 
  # if (nrow(inDT.wok) > 75000) {
  #   sprintf("\nThe number of rows in the WOK data set, %s, is greater than 50,000. Not writing xlsx or csv", nrow(inDT.wok))
  #   writeFiles <- writeFiles[!writeFiles %in% c("xlsx")]
  # }
  # if ("csv"  %in% writeFiles) {
  #   sprintf("\nWriting the csv for %s to %s ", outName, destDir)
  #   write.csv(inDT,file = paste(destDir, "/", outName, "_", Sys.Date(), ".csv", sep = ""), row.names = FALSE)
  # }
  if ("xlsx"  %in% writeFiles) {
    #    cat("\nwriting the xlsx for ", outName, " to ", dir, sep = ""))
    numStyle <- openxlsx::createStyle(numFmt = "0.000")
    wrapStyle <- createStyle( wrapText = TRUE, valign = "top")
    
    wbGeneral <- openxlsx::createWorkbook()
    longName <- outName
    outName <- strtrim(outName, c(31))
    
    # first add the needed worksheets
    openxlsx::addWorksheet(wb = wbGeneral, sheetName = "Metadata")
    if (nrow(inDT.scopus) > 0) openxlsx::addWorksheet(wb = wbGeneral, sheetName = "SCOPUS complete")
    openxlsx::addWorksheet(wb = wbGeneral, sheetName = "WOK unique")
    openxlsx::addWorksheet(wb = wbGeneral, sheetName = "Notes")
    
    Notes <- data.table(Notes = character())
    notesTextIntro <- list("This worksheet provides more details on the process of creating this workbook")
    notesTextLicense <- list("Accessing either the SCOPUS or Clarivate databases requires either a key or an IP address associated with a institution buying access to the database. This version is based on access at the University of Illinois, Urbana-Champaign. Email nelson.gerald.c@gmail.com for directions on how to modify the code for your institution.")
    notesTextWorkbookNames <- list("The 'SCOPUS complete' workbook contains all the information from the SCOPUS query. The 'WOK unique' workbook has only records that are not contained in the SCOPUS complete workbook. The UIUC WOK license doesn't including downloading abstracts so the subsetting columns are not available." )
    notesTextSubsetting <- list("The 'SCOPUS complete' workbook contains several subsetting columns. They are listed in the Metadata workbook beneath the row with 'search string names' in the first column.")
    notesTextgithub <- list("The code and data for this project are open source and available at 'https://github.com/GeraldCNelson/citationManagement'. ")
    Notes <- rbindlist(list(Notes, notesTextIntro, notesTextLicense, notesTextWorkbookNames, notesTextSubsetting, notesTextgithub))
    
    #add data
    openxlsx::writeDataTable(
      wbGeneral,
      metadata,  sheet = "Metadata", startRow = 1, startCol = 1, rowNames = FALSE,
      colNames = TRUE, withFilter = TRUE)
    if (nrow(inDT.scopus) > 0) {
      openxlsx::writeDataTable(
        wbGeneral,
        inDT.scopus,  sheet = "SCOPUS complete", startRow = 1, startCol = 1, rowNames = FALSE,
        colNames = TRUE, withFilter = TRUE)
    }
    openxlsx::writeDataTable(
      wbGeneral,
      inDT.wok,  sheet = "WOK unique", startRow = 1, startCol = 1, rowNames = FALSE,
      colNames = TRUE, withFilter = TRUE)
    openxlsx::writeDataTable(
      wbGeneral,
      Notes,  sheet = "Notes", startRow = 1, startCol = 1, rowNames = FALSE,
      colNames = TRUE, withFilter = TRUE)
    
    # set column widths here
    # column numbers in WoS/SCOPUS files for title, publicationName, and abstract 
    colNums.scopus <- c(match("title",names(inDT.scopus)), match("publicationName",names(inDT.scopus)), match("abstract",names(inDT.scopus)))
    colNums.wok <- c(match("title", names(inDT.wok)), match("publicationName",names(inDT.wok)))
    openxlsx::setColWidths(
      wbGeneral, sheet = "Metadata", cols = 1:2 , widths = c(20,70))
    if (nrow(inDT.scopus) > 0) {
      openxlsx::setColWidths(
        wbGeneral, sheet = "SCOPUS complete", cols = 1:ncol(inDT.scopus), widths = "10" )
      openxlsx::setColWidths(
        wbGeneral, sheet = "SCOPUS complete", cols = colNums.scopus, widths = c(40,30,70))
    }
    openxlsx::setColWidths(
      wbGeneral, sheet = "WOK unique", cols = 1:ncol(inDT.wok), widths = "10" )
    openxlsx::setColWidths(
      wbGeneral, sheet = "WOK unique", cols = colNums.wok, widths = c(40,30))
    openxlsx::setColWidths(
      wbGeneral, sheet = "Notes", cols = 1, widths = "80" )
    
    openxlsx::addStyle(
      wbGeneral, sheet = "Metadata", style = wrapStyle, rows = 1:nrow(metadata) + 1, cols = 1:2, 
      gridExpand = TRUE )
    if (nrow(inDT.scopus) > 0) {
    openxlsx::addStyle(
      wbGeneral, sheet = "SCOPUS complete", style = numStyle, rows = 1:nrow(inDT.scopus) + 1, cols = 2:ncol(inDT.scopus), 
      gridExpand = TRUE )
    openxlsx::addStyle(
      wbGeneral, sheet = "SCOPUS complete", style = wrapStyle, rows = 1:nrow(inDT.scopus) + 1, cols = 1:ncol(inDT.scopus), #
      gridExpand = TRUE )
    }
    openxlsx::addStyle(
      wbGeneral, sheet = "WOK unique", style = numStyle, rows = 1:nrow(inDT.wok) + 1, cols = 2:ncol(inDT.wok), 
      gridExpand = TRUE )
    openxlsx::addStyle(
      wbGeneral, sheet = "WOK unique", style = wrapStyle, rows = 1:nrow(inDT.wok) + 1, cols = 1:ncol(inDT.wok), #
      gridExpand = TRUE )
    openxlsx::addStyle(
      wbGeneral, sheet = "Notes", style = wrapStyle, rows = 1:nrow(Notes), cols = 1:2, #
      gridExpand = TRUE )
    
    xcelOutFileName = paste(destDir, "/", longName, "_", Sys.Date(), ".xlsx", sep = "") # added longName functionality June 20, 2018
    openxlsx::saveWorkbook(wbGeneral, xcelOutFileName, overwrite = TRUE)
    #   cat("\nDone writing the xlsx for ", outName, sep = "")
    #  print(proc.time())
  }
}

#' Title removeOldVersions - removes old version of a rawData file
#' @param fileShortName - short name of the file to be removed
#' @param dir - directory of the file to be removed
#' @export
removeOldVersions <- function(fileShortName,dir) {
  removeFcn <- function(regExp) {
    oldVersionList <-
      grep(regExp,
           list.files(dir),
           value = TRUE,
           perl = TRUE)
    if (length(oldVersionList) > 0) {
      #      print(oldVersionList)
      file.remove(paste(dir, oldVersionList, sep = "/"))
    }
  }
  # remove .rawData versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*rawData$)", sep = "")
  removeFcn(regExp)
  # remove .rds versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*rds$)", sep = "")
  removeFcn(regExp)
  # remove .xlsx versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*xlsx$)", sep = "")
  removeFcn(regExp)
  # remove .csv versions
  regExp <- paste("(?=^", fileShortName, ")(?=.*csv$)", sep = "")
  removeFcn(regExp)
}

# readin WOK function
readinWOK <- function(query) {
  firstRecord <- 1
  nrResults <- -1
  queryID = 1
  count <- 100
  notFinished = TRUE
  url <- 'https://api.clarivate.com/api/woslite/'
  # queryResults.complete <- data.table(UT=character(), Keyword.Keywords=character(), Title.Title=character(),
  #                            Doctype.Doctype=character(), Author.Authors=character(), Author.BookGroupAuthors=character(),
  #                            Author.BookAuthors=character(), Source.Pages=character(), Source.Issue=character(),
  #                            Source.SourceTitle=character(), Source.Volume=character(), Source.Published.BiblioDate=character(),
  #                            Source.Published.BiblioYear=character(), Source.BookSeriesTitle=character(), Source.SupplementNumber=character(),
  #                            Source.SpecialIssue=character(), Other.Identifier.Eissn=character(), Other.Identifier.Doi=character(),
  #                            Other.Identifier.Issn=character(), Other.ResearcherID.Disclaimer=character(), Other.Identifier.Ids=character(),
  #                            Other.Contributor.ResearcherID.Names=character(), Other.Contributor.ResearcherID.ResearcherIDs=character(),
  #                            Other.Identifier.Eisbn=character(), Other.Identifier.article_no=character(), Other.Identifier.Isbn=character(),
  #                            Other.Identifier.Parent_Book_Doi=character())
  queryResults <- c(Title.Title=character(), Author.Authors=character(), Author.BookAuthors=character(), Source.SourceTitle=character(),  Source.Pages=character(), 
                    Source.Volume=character(), Source.Issue=character(), Source.Published.BiblioDate=character(), Source.Published.BiblioYear=character(), 
                    Other.Identifier.Doi=character(),Other.Identifier.Isbn=character(), Doctype.Doctype=character(), Keyword.Keywords=character())
  keepListCol <- c("Title.Title", "Author.Authors", "Author.BookAuthors", "Source.SourceTitle",  "Source.Pages", 
                   "Source.Volume", "Source.Issue", "Source.Published.BiblioDate", "Source.Published.BiblioYear", 
                   "Other.Identifier.Doi","Other.Identifier.Eissn", "Doctype.Doctype", "Keyword.Keywords")
  
  newNames <- c("title","authors","bookAuthors", "publicationName", "pageRange", 
                "volume","issue", "date", "year", 
                "doi", "eIssn", "document_type", "keywords")
  
  # do some testing first and get the ID for this query
  {response <- httr::GET(url, httr::add_headers(accept = 'application/json', `X-APIKey` = wosliteKey),
                         query = list(databaseId = 'WOK', usrQuery = query, count = 1, firstRecord = 1))
    stop_for_status(response, task = "bad http status")
    suppressMessages(jsonResp <- content(response, as =  "text")) # suppress messages to get rid of warning about defaulting to UTF-8
    j <- fromJSON(jsonResp)
    QueryID  <- j$QueryResult$QueryID
    nrResults <- j$QueryResult$RecordsFound
    print(paste(nrResults, 'results from WOK query ID ', QueryID))
    #    print(paste0("QueryID: ", QueryID))
    if (nrResults > 5000) stop("Number of WOK records greater than 5000")
    # if successful, load the initial download into queryResults
    # jData <- as.data.table(flatten(j$Data))
    # jData[, setdiff(names(jData), keepListCol) := NULL]
    # jData[, ] <- lapply(jData[, ], as.character)
    # queryResults <- rbind(queryResults, jData, fill = TRUE)
    url = paste0('https://api.clarivate.com/api/woslite/query/', QueryID, "/") # get data from the specific query
  }
  
  step = 0
  while (notFinished) {
    step = step + 1
    print(step)
    if (nrResults < (firstRecord + count) & !nrResults == -1) {
      count = nrResults - firstRecord
    }
    response <- httr::GET(url, httr::add_headers(accept = 'application/json', `X-APIKey` = wosliteKey),
                          query = list(databaseId = 'WOK', usrQuery = query, count = count, firstRecord = firstRecord))
    suppressMessages(jsonResp <- content(response, as =  "text")) # suppress messages to get rid of warning about defaulting to UTF-8
    j <- fromJSON(jsonResp)
    QueryID  <- j$QueryResult$QueryID
    # print(paste0("QueryID: ", QueryID))
    # print(paste0("firstRecord: ", firstRecord))
    # print(paste0("nrResults: ", nrResults))
    #    print(str(j$Data, max.level = 2))
    jData <- as.data.table(flatten(j$Data))
    jData[, setdiff(names(jData), keepListCol) := NULL]
    jData[, ] <- lapply(jData[, ], as.character)
    
    queryResults <- rbind(queryResults, jData, fill = TRUE)
    if (nrResults <= firstRecord + count) {
      notFinished = FALSE
      print('Done with WOK')
    }else{
      #      queryIdMode = TRUE
      firstRecord = firstRecord + count
    }
  }
  
  setnames(queryResults, old = keepListCol, new = newNames, skip_absent = TRUE)
  return(queryResults)
}

# readin WOK function using a queryID
readinWOKWithQueryID <- function(query, QueryID, nrResults) {
  firstRecord <- 1
  # nrResults <- -1
  #  queryID = 1
  count <- 100
  notFinished = TRUE
  url <- paste0('https://api.clarivate.com/api/woslite/query/', QueryID)
  # queryResults.complete <- data.table(UT=character(), Keyword.Keywords=character(), Title.Title=character(),
  #                            Doctype.Doctype=character(), Author.Authors=character(), Author.BookGroupAuthors=character(),
  #                            Author.BookAuthors=character(), Source.Pages=character(), Source.Issue=character(),
  #                            Source.SourceTitle=character(), Source.Volume=character(), Source.Published.BiblioDate=character(),
  #                            Source.Published.BiblioYear=character(), Source.BookSeriesTitle=character(), Source.SupplementNumber=character(),
  #                            Source.SpecialIssue=character(), Other.Identifier.Eissn=character(), Other.Identifier.Doi=character(),
  #                            Other.Identifier.Issn=character(), Other.ResearcherID.Disclaimer=character(), Other.Identifier.Ids=character(),
  #                            Other.Contributor.ResearcherID.Names=character(), Other.Contributor.ResearcherID.ResearcherIDs=character(),
  #                            Other.Identifier.Eisbn=character(), Other.Identifier.article_no=character(), Other.Identifier.Isbn=character(),
  #                            Other.Identifier.Parent_Book_Doi=character())
  queryResults <- c(Title.Title = character(), Author.Authors = character(), Author.BookAuthors = character(), Source.SourceTitle = character(),  Source.Pages = character(), 
                    Source.Volume = character(), Source.Issue = character(), Source.Published.BiblioDate = character(), Source.Published.BiblioYear = character(), 
                    Other.Identifier.Doi = character(),Other.Identifier.Isbn = character(), Doctype.Doctype = character(), Keyword.Keywords = character())
  keepListCol <- c("Title.Title", "Author.Authors", "Author.BookAuthors", "Source.SourceTitle",  "Source.Pages", 
                   "Source.Volume", "Source.Issue", "Source.Published.BiblioDate", "Source.Published.BiblioYear", 
                   "Other.Identifier.Doi","Other.Identifier.Eissn", "Other.Identifier.Isbn", "Doctype.Doctype", "Keyword.Keywords")
  
  newNames <- c("title","authors","bookAuthors", "publicationName", "pageRange", 
                "volume","issue", "date", "year", 
                "doi", "eIssn", "isbn", "document_type", "keywords")
  
  while (notFinished) {
    if (nrResults < (firstRecord + count) & !nrResults == -1) {
      count = nrResults - firstRecord
    }
    response <- httr::GET(url, httr::add_headers(accept = 'application/json', `X-APIKey` = wosliteKey),
                          query = list(databaseId = 'WOK', usrQuery = query, count = count, firstRecord = firstRecord))
    Sys.sleep(0.4)
    suppressMessages(jsonResp <- content(response, as =  "text")) # suppress messages to get rid of warning about defaulting to UTF-8
    j <- fromJSON(jsonResp)
    # QueryID  <- j$QueryResult$QueryID
    # print(paste0("QueryID: ", QueryID))
    # print(paste0("firstRecord: ", firstRecord))
    # print(paste0("nrResults: ", nrResults))
    
    if (!is.data.frame(j$Data)) { # a loop to skip over set of 100 records that has an internal server error
      print(paste("j$Data is not a data frame. Nearest record is ", firstRecord))
    } else {
      jData <- as.data.table(flatten(j$Data))
      jData[, setdiff(names(jData), keepListCol) := NULL]
      jData[, ] <- lapply(jData[, ], as.character)
      
      queryResults <- rbind(queryResults, jData, fill = TRUE)
    }
    if (nrResults <= firstRecord + count) {
      notFinished = FALSE
      print('Done with WOK')
    }else{
      #      queryIdMode = TRUE
      firstRecord = firstRecord + count
    }
  }
  
  setnames(queryResults, old = keepListCol, new = newNames, skip_absent = TRUE)
  queryResults[, eIssn := gsub("-", "", eIssn)]
  queryResults[, keepRef := "y"]
  setcolorder(queryResults, "keepRef")
  return(queryResults)
}

constructQuery.WOK <- function(rawQuery, yearCoverage.wok, CCSearchString) {
  query.wok <- sprintf('PY %s AND TS=((%s) AND ((%s)))', yearCoverage.wok, rawQuery, CCSearchString)
  query.wok <- gsub('{','"', query.wok, perl = TRUE)
  query.wok <- gsub('}','"', query.wok, perl = TRUE)
  return(query.wok)
}

constructQuery.scopus <- function(rawQuery, yearCoverage.scopus, CCSearchString) {
  query.scopus <- sprintf('PUBYEAR %s AND TITLE-ABS-KEY((%s) AND ((%s)))', yearCoverage.scopus, rawQuery,  CCSearchString)
  return(query.scopus)
}

getDBinfo <- function(queries, yearCoverage.wok, yearCoverage.scopus,  CCSearchString = CCSearchString) {
  queryInfo <- data.table(sectionName = character(),fileName = character(), queryNumber = character(), rawQuery = character(), 
                          QueryID.wok = numeric(),QueryID.scopus = numeric(), nrResults.wok = numeric(), nrResults.scopus = numeric(), query.wok = character(), query.scopus = character())
  url.wok <- 'https://api.clarivate.com/api/woslite/'
  workingText <- "Working on query: "
  for (i in 1:nrow(queries)) {
    rawQuery <- queries[i, query]
    query.wok <- constructQuery.WOK(rawQuery, yearCoverage.wok = yearCoverage.wok, CCSearchString = CCSearchString)
    query.scopus <- constructQuery.scopus(rawQuery, yearCoverage.scopus = yearCoverage.scopus, CCSearchString = CCSearchString)
    print(paste( workingText, query.wok))
    
    #    print(query.wok)
    response.wok <- httr::GET(url.wok, httr::add_headers(accept = 'application/json', `X-APIKey` = wosliteKey),
                              query = list(databaseId = 'WOK', usrQuery = query.wok, count = 1, firstRecord = 1))
    # stop_for_status(response, task = "bad http status")
    suppressMessages(jsonResp <- content(response.wok, as =  "text")) # suppress messages to get rid of warning about defaulting to UTF-8
    j <- fromJSON(jsonResp)
    if (!is.null(j$code)) print(paste0("This query is malformed: ", query.wok))
    QueryID.wok  <- j$QueryResult$QueryID
    # print(paste("QueryID: ", QueryID))
    # print(paste("nrResults: ", nrResults))
    # if (is.null(QueryID))   print(paste0(i, "th row has QueryID null."))
    nrResults.wok <- j$QueryResult$RecordsFound
    print(paste0("query.scopus: ", query.scopus))
    response.scopus = scopus_search(query = query.scopus, max_count = 1, count = 1,  start = 1, verbose = FALSE, view = c( "COMPLETE"))
    nrResults.scopus <- response.scopus$total_results
    QueryID.scopus <- NA
    
    newRow <- list(queries$sectionName[i], queries$outFileName[i], queries$queryNumber[i], rawQuery, QueryID.wok, QueryID.scopus, nrResults.wok, nrResults.scopus, query.wok, query.scopus)
    queryInfo <- rbind(queryInfo, newRow)
    Sys.sleep(0.4)
  }
  print("Done!")
  return(queryInfo)
}

getScopusinfo <- function(queries, yearCoverage.scopus) {
  queryInfo <- data.table(query = character(), QueryID = numeric(), nrResults = (numeric()))
  workingText <- "Working ."
  for (i in 1:nrow(queries)) {
    print(workingText)
    rawQuery <- queries[i, query]
    query <- constructQuery.scopus(rawQuery, yearCoverage.scopus = yearCoverage.scopus, CCSearchString = CCSearchString)
    
    response = scopus_search(query = query, max_count = 1, count = 1,  start = 1, verbose = FALSE, view = c( "COMPLETE"))
    nrResults <- response$total_results
    QueryID <- "unknown"
    
    newRow <- list(queries$outFileName[i], queries$queryNumber[i], query.wok, QueryID, nrResults)
    #    print(paste(query.wok, "; ", QueryID, ";" , nrResults))
    #    print(newRow)
    queryInfo <- rbind(queryInfo, newRow)
    Sys.sleep(0.4)
    workingText <- paste0(workingText, ".")
  }
  print("Done!")
  return(queryInfo)
}

readinSCOPUS <- function(query) {
  keepListCol.content <- c("dc:title","dc:creator","prism:publicationName", # removed "prism:url","dc:identifier","eid",
                           "prism:eIssn","prism:volume","prism:issueIdentifier","prism:coverDate", "prism:pageRange",
                           "prism:doi","dc:description","citedby-count","prism:aggregationType",
                           "subtypeDescription", "authkeywords")
  keepListCol.content.newNames <- c("title","firstAuthor","publicationName", # removed "url","identifier","eid",
                                    "eIssn","volume","issue","date", "pageRange",
                                    "doi","abstract","citations","pubType",
                                    "document_type", "author_keywords")
  
  keepListCol.author <- c("@seq", "authname", "entry_number")
  
  searchCols <- c("title", "abstract", "author_keywords", "document_type") # what variables in the reference list should be searched for
  # create blank queryResuls in case there are 0 results of a search
  queryResults <- data.table(NULL)
  # next few lines used to get totalresults
  response = scopus_search(query = query, max_count = 1, count = 1,  start = 1, verbose = FALSE, view = c( "COMPLETE"))
  nrResults <- response$total_results
  if (nrResults > 5000) stop("Number of SCOPUS records greater than 5000")
  if (nrResults == 0) {
    print("No references for this query")
  }else{
    response = scopus_search(query = query, max_count = nrResults, count = 25,  start = 0, verbose = FALSE, view = c("COMPLETE"))
    df <- gen_entries_to_df(response$entries)
    dt.content <- as.data.table(df$df)
    dt.content[, setdiff(names(dt.content), keepListCol.content) := NULL]
    # cat(sort(names(dt.content)), "\n\n")
    # print("Next")
    # cat(sort(keepListCol.content), "\n")
    # if (!all(str_detect(sort(names(dt.content)),sort(keepListCol.content)))) stop("missing column names")
    setnames(dt.content, old = keepListCol.content, new = keepListCol.content.newNames, skip_absent = TRUE)
    dt.authorInfo <- as.data.table(df$author)
    dt.authorInfo[, setdiff(names(dt.authorInfo), keepListCol.author) := NULL]
    
    authsList <- data.table(authrs = character())
    for (i in unique(dt.authorInfo$entry_number)) {
      temp <-dt.authorInfo[entry_number == i, authname]
      temp <- as.list(paste(temp, collapse = ", "))
      authsList <- rbind(authsList, temp)
    }
    queryResults <- unique(dt.content)
    temp.author <- unique(dt.authorInfo)
    
    for (i in searchStrings.names) {
      queryResults[, (i) := "None"]
    }
    
    # new columns with no search strings. Used for manual entry of information
    colsWithNoSearchStrings <- c("warmingDegrees (C)", "prod_system",  "resid_damage", "comments")
    # default values
    for (i in colsWithNoSearchStrings) {
      queryResults[, (i) := "None"]
    }
    queryResults[, keepRef := "y"]
    setcolorder(queryResults, "keepRef")
    queryResults[, notPeerRev := "No"]
    
    setkey(queryResults)
    
    for (i in 1:length(searchStrings)) {
      searchSt <- eval(parse(text = searchStringsList[,.SD[i]]$searchString)) # get list of search terms for the ith search list
      for (j in searchSt) {
        # grepl and startsWith return a logical vector
        if (grepl("*", j, fixed = TRUE)) {
          jnew <-  gsub("*", "", j)
          #         jnew <- paste0("\\<", jnew)
          i1 <- queryResults[, Reduce("|", lapply(.SD, function(x) startsWith(x, jnew))), .SDcols = searchCols]
          #         print(paste("jnew ",  jnew))
        } else {
          i1 <- queryResults[, Reduce("|", lapply(.SD, function(x) grepl(j, x))), .SDcols = searchCols]
          #          print(paste("j ",  j))
        }
        
        queryResults[i1, (searchStrings.names[i]) := paste(get(searchStrings.names[i]),j, sep = ", ")]
      }
    }
    
    for (i in searchStrings.names) {
      queryResults[, (i) := gsub(paste0("None, "), "", get(i))]
    }
    queryResults[, (searchStrings.names) := (lapply(.SD, function(x) {
      sapply(x, function(y) paste(sort(trimws(strsplit(y, ',')[[1]])), collapse = ','))
    })), .SDcols = searchStrings.names]
    
    for (i in searchStrings.names) {
      queryResults[, (i) := gsub(paste0("No, "), "", get(i))]
    }
    for (i in searchStrings.names) {
      queryResults[, (i) := gsub(paste0(i, ", "), "", get(i))]
    }
    queryResults[, author_keywords := str_replace_all(author_keywords, "\\|, ", "")]
    queryResults[, author_keywords := str_replace_all(author_keywords, "\\|", ",")]
  }
  print('Done with SCOPUS')
  return(queryResults)
}

prepareSpreadsheet <- function(sectionName, queryResults.scopus, query.scopus, queryResults.wok, query.wok, outName) {
  # construct metadata variable
  DT <- data.table(
    searchStringName = character(),
    searchString = character()
  )
  
  #      variable_name = character(),
  #   variable_value = character()
  # )
  
  metadata.querystring.scopus <- list("SCOPUS query", query.scopus)
  metadata.querystring.wok <- list("WOK query", query.wok)
  metadata.datestring <- list("date queried", as.character(Sys.Date()))
  metadata.datestring <- list("Report section", sectionName)
  metadata.recordCount.scopus <- list("SCOPUS reference count", nrow(queryResults.scopus))
  metadata.recordCount.wok <- list("WOK only reference count", nrow(queryResults.wok))
  metadata.searchStringLabel <- list("search string names", "search string content, SCOPUS only")
  metadata.notes <- list("See Notes worksheet for more details.", "")
  
  DT <- rbind(DT, metadata.notes) # put here to put the recommendation to read the Notes at the top of the metadata.
  DT <- rbind(DT, metadata.querystring.scopus)
  DT <- rbind(DT, metadata.querystring.wok)
  DT <- rbind(DT, metadata.datestring)
  DT <- rbind(DT, metadata.recordCount.scopus)
  DT <- rbind(DT, metadata.recordCount.wok)
  DT <- rbind(DT, metadata.searchStringLabel)
  
  
  for (i in 1:length(searchStrings)) {
    #   newRow <- list(searchStrings.names[i], paste(get(searchStrings[i]), collapse = ", "))
    newRow <- as.list(searchStringsList[,.SD[i]])
    DT <- rbind(DT, newRow)
  }
  inDT.scopus <- queryResults.scopus
  inDT.wok <- queryResults.wok
  metadata <- DT
  cleanupRefFiles(metadata, inDT.scopus = inDT.scopus, inDT.wok = inDT.wok, outName = outName, destDir = "results", writeFiles = "xlsx")
}

#' Title cleanup - remove old versions and save rds and xlsx or csv versions of the file
#' @param inDT name of the data table or frame to be written out
#' @param outName short name of the file to be written out
#' @param destDir directory where the cleanup takes place
#' @param writeFiles format to use for writing output in addition to RDS
#' @description brief description of the contents of the file
cleanup <- function(inDT, outName, destDir, writeFiles, desc, numVal, wrapCols) {
  #    srcFile <- get("sourceFile", envir = .GlobalEnv)
  if (missing(writeFiles)) {writeFiles = "xlsx"}
  if (missing(destDir)) {destDir = fileloc("mData")}
  if (missing(numVal)) numVal = "0.000"
  wrapColsOn = 1
  if (missing(wrapCols)) wrapColsOn = 0
  colNames <- paste(colnames(inDT), collapse = ", ")
  # outInfo <- list(outName, srcFile, destDir, desc, colNames)
  # metadataDT <<- rbind(metadataDT, outInfo)
  #  cat("\n", "Outfilename: ", outName, " Destination: ", Destination," Script: ", srcFile," Desc: ", desc," Col. names: ", colNames, "\n")
  #convert to a standard order
  oldOrder <- names(inDT)
  startOrder <- c("scenario","year")
  if (all(startOrder %in% oldOrder)) {
    remainder <- oldOrder[!oldOrder %in% startOrder]
    data.table::setcolorder(inDT,c(startOrder,remainder))
    data.table::setorderv(inDT,c(startOrder,remainder))
  }
  
  removeOldVersions(outName, destDir)
  sprintf("\nWriting the rds for %s to %s ", outName, destDir)
  # print(proc.time())
  # next line removes any key left in the inDT data table; this may be an issue if a df is used
  data.table::setkey(inDT, NULL)
  outFile <- paste(destDir, "/", outName, "_", Sys.Date(), ".rds", sep = "")
  saveRDS(inDT, file = outFile)
  
  # # update files documentation -----
  # Note: fileDocumentation.csv is currently not being used.
  # fileDoc <- data.table::as.data.table(read.csv(paste(fileloc("rawData"), "fileDocumentation.csv", sep = "/"),
  #     header = TRUE, colClasses = c("character","character","character")))
  # fileDoc <- fileDoc[!fileShortName == outName]
  # fileDocUpdate <- as.list(c(outName, outFile, paste0(names(inDT), collapse = ", ")))
  # fileDoc <- rbind(fileDoc, fileDocUpdate)
  # write.csv(fileDoc, paste(fileloc("mData"), "fileDocumentation.csv", sep = "/"), row.names = FALSE)
  #
  # #print(proc.time())
  if (missing(writeFiles)) {writeFiles = "xlsx"}
  if (nrow(inDT) > 75000) {
    sprintf("\nThe number of rows in the data, %s, is greater than 50,000. Not writing xlsx or csv", nrow(inDT))
    writeFiles <- writeFiles[!writeFiles %in% c("xlsx")]
  }
  if ("csv"  %in% writeFiles) {
    sprintf("\nWriting the csv for %s to %s ", outName, destDir)
    write.csv(inDT,file = paste(destDir, "/", outName, "_", Sys.Date(), ".csv", sep = ""), row.names = FALSE)
  }
  if ("xlsx"  %in% writeFiles) {
    #    cat("\nwriting the xlsx for ", outName, " to ", dir, sep = ""))
    wbGeneral <- openxlsx::createWorkbook()
    longName <- outName
    outName <- strtrim(outName, c(31))
    openxlsx::addWorksheet(wb = wbGeneral, sheetName = outName)
    
    openxlsx::writeDataTable(
      wbGeneral,
      inDT,  sheet = outName, startRow = 1, startCol = 1, rowNames = FALSE,
      colNames = TRUE, withFilter = TRUE)
    
    openxlsx::setColWidths(
      wbGeneral, sheet = outName, cols = 1:ncol(inDT), widths = 12)
    
    if (!wrapColsOn == 0) {
      openxlsx::setColWidths(
        wbGeneral, sheet = outName, cols = wrapCols, widths = 60 )
    }
    
    numStyle <- openxlsx::createStyle(numFmt = numVal, valign = "top")
    wrapStyle <- createStyle( wrapText = TRUE, valign = "top")
    
    openxlsx::addStyle(
      wbGeneral, sheet = outName, style = numStyle, rows = 1:nrow(inDT) + 1, cols = 2:ncol(inDT),
      gridExpand = TRUE )
    
    openxlsx::addStyle(
      wbGeneral, sheet = outName, style = wrapStyle, rows = 1:nrow(inDT) + 1, cols = wrapCols,
      gridExpand = TRUE )
    
    openxlsx::addStyle(
      wbGeneral, sheet = outName, style = wrapStyle, rows = 1:nrow(inDT) + 1, cols = 1,
      gridExpand = TRUE )
    
    xcelOutFileName = paste(destDir, "/", longName, "_", Sys.Date(), ".xlsx", sep = "") # added longName functionality June 20, 2018
    openxlsx::saveWorkbook(wbGeneral, xcelOutFileName, overwrite = TRUE)
    #   cat("\nDone writing the xlsx for ", outName, sep = "")
    #  print(proc.time())
  }
}

prepareOutput <- function(queryNum, queries) {
  print(paste0("working on query ", queryNum))
  rawQuery <- queries[queryNumber %in% queryNum, rawQuery]
  outFileName <- queries[queryNumber %in% queryNum, fileName]
  query.wok <- constructQuery.WOK(rawQuery, yearCoverage.wok, CCSearchString) 
  QueryID.wok <- queries[queryNumber %in% queryNum, QueryID.wok] # for WOK
  nrResults.wok <- queries[queryNumber %in% queryNum, nrResults.wok] # for WOK
  queryResults.wok <- readinWOKWithQueryID(query = query.wok, QueryID = QueryID.wok, nrResults = nrResults.wok)
  
  nrResults.scopus <- queries[queryNumber %in% queryNum, nrResults.scopus] # for SCOPUS
  if (nrResults.scopus > 5000) {
    print(paste0("Number of results for scopus greater than 5000 for query ", queryNum, ". Skipping it."))
  } 
  if (nrResults.scopus == 0) {
    print(paste0("No results for scopus for query ", queryNum, ". Skipping it."))
    queryResults.scopus <- data.table(NULL)
  }
  
  if ((!nrResults.scopus > 5000) & (!nrResults.scopus == 0)) {
    query.scopus <- constructQuery.scopus(rawQuery, yearCoverage.scopus, CCSearchString) 
    queryResults.scopus <- readinSCOPUS(query = query.scopus)
  }
  # process if both results have content
  if (!nrow(queryResults.scopus) == 0 & (!nrow(queryResults.wok) == 0)) {
    doi.wok <- sort(queryResults.wok$doi)
    doi.scopus <- sort(queryResults.scopus$doi)
    
    doi.common <- doi.wok[doi.wok %in% doi.scopus]
    doi.unique.wok <- doi.wok[!doi.wok %in% doi.scopus]
    doi.unique.scopus <- doi.scopus[!doi.scopus %in% doi.wok]
    queryResults.wok.unique <- queryResults.wok[!doi %in% doi.common,]
    
    # do the same thing with eIssns
    eissn.wok <- sort(queryResults.wok$eIssn)
    eissn.scopus <- sort(queryResults.scopus$eIssn)
    
    eissn.common <- eissn.wok[eissn.wok %in% eissn.scopus]
    eissn.unique.wok <- eissn.wok[!eissn.wok %in% eissn.scopus]
    eissn.unique.scopus <- eissn.scopus[!eissn.scopus %in% eissn.wok]
    queryResults.wok.unique <- queryResults.wok.unique[!eIssn %in% eissn.common,]
    
    prepareSpreadsheet(sectionName = queries[queryNumber == queryNum, sectionName], queryResults.scopus, query.scopus, queryResults.wok = queryResults.wok.unique, query.wok, outFileName)
  }
  if ((nrow(queryResults.scopus) == 0) & (!nrow(queryResults.wok) == 0)) {
    queryResults.wok.unique <-   queryResults.wok
    prepareSpreadsheet(sectionName = queries[queryNumber == queryNum, sectionName], queryResults.scopus, query.scopus, queryResults.wok = queryResults.wok.unique, query.wok, outFileName)
  }
  # these if statements should always be FALSE
  # if (nrow(queryResults.scopus) == 0 ) {
  #   print(paste0("SCOPUS results for query ", queryNum, ", query: " , query.scopus, " are empty"))
  # }
  # if (nrow(queryResults.wok) == 0 ) {
  #   print(paste0("WOK results for query ", queryNum, ", query: " , query.wok, " are empty"))
  # }
}


