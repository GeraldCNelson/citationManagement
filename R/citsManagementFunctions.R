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

#This file is used in conjunction with readinQueries.R and is sourced from there.

library(readxl)
library(data.table)
library(openxlsx)
library(rscopus)
library(httr)
library(jsonlite)
library(stringr)
library(curl)
library(textclean)
library(tidyverse) # includes ggplot2, purrr, dplyr, tibble, tidyr, stringr, readr, forcats
library(readr)

#set up some the access codes -----
#queries.org <- queries
#set Scopus institution token in .Renviron
#run this in R - usethis::edit_r_environ()
# then type insttoken="xxxxxxxxxxxxxx") into the newly opened window
# get Scopus api key
#get_api_key(api_key = Sys.getenv('Elsevier_API'))
Scopus_api_key <- Sys.getenv('Elsevier_API')
#if (!have_api_key()) stop("Missing api key")

# get SCOPUS institutional key
insttoken <- Sys.getenv("insttoken")
if (!exists("insttoken")) stop("insttoken missing")
# get WOS api key
wosliteKey <- Sys.getenv("wosliteKey")

# get list of queries
climateChangeSearchTerms <- as.data.table(read_excel("data-raw/queries_wg2_ch05.xlsx", sheet = "CCSearchStrings"))

# get list of rejected references
rejectList_master <- as.data.table(read.xlsx("data/rejected/rejected_out/rejectList_master.xlsx"))
# get IPCC country and region names
RegionLookupWG2 <- as.data.table(read_excel("data-raw/Regional_chapters_Countries_WGII.xlsx", skip = 1))
regionColsToKeep <- c("WGII_Region", "Sub-region Name", "Country or Area")
regionColsToDelete <- names(RegionLookupWG2)[!names(RegionLookupWG2) %in% regionColsToKeep]
RegionLookupWG2[, (regionColsToDelete) := NULL]
countryLongNameAdditions <- as.data.table(read_excel("data-raw/countryLongNameAdditions.xlsx"))
RegionLookupWG2 <- rbind(RegionLookupWG2, countryLongNameAdditions)
setnames(RegionLookupWG2, old = c("WGII_Region", "Sub-region Name", "Country or Area"), new = c("WGII_Region", "WGII_Subregion", "countryName"))

# construct the climate change search string ----
climateChangeSource <- climateChangeSearchTerms[searchStringName %in% "searchStrings.climateChangeSource", searchString]
climateImpacts <- climateChangeSearchTerms[searchStringName %in% "searchStrings.climateImpacts", searchString]
climateMitigation <- climateChangeSearchTerms[searchStringName %in% "searchStrings.mitigation", searchString]

CCSearchString <- climateChangeSource

# get country names
regions_lookup <- as.data.table(read_excel("data-raw/regions lookup June 15 2018.xlsx"))
colsToKeep <- c("ISO_code",  "country_name.ISO")
colsToDelete <- names(regions_lookup)[!names(regions_lookup) %in% colsToKeep]
regions_lookup[, (colsToDelete) := NULL]

shortNames.countries <- c("Syria", "Ivory Coast", "Laos", "Libya", "South Korea", "North Korea", "Russia", "Vietnam")
countriesToRemove <- c("Anguilla", "Åland Islands", "Albania", "Andorra", "Netherlands Antilles", "Northern Mariana Islands", "Bonaire, Sint Eustatius and Saba", "Saint Barthélemy", "Bouvet Island", 
                       "Cocos (Keeling) Islands", "Christmas Island", "Guernsey", "Gibraltar", "Heard Island and McDonald Islands", "Isle of Man", "British Indian Ocean Territory",
                       "Mayotte", "Norfolk Island", "South Georgia and The South Sandwich Islands", "Saint Helena, Ascension and Tristan da Cunha", "Svalbard and Jan Mayen", 
                       "San Marino", "Saint Pierre and Miquelon", "Macao", "Saint Martin (French part)",  "Sint Maarten (Dutch part)", "Saint Vincent and The Grenadines", "Wallis and Futuna", "Tokelau")
abstractStringToDelete <- read_lines("data-raw/countryStringsToDelete.txt") # used to eliminate from the .temp version of the abstract column words like anomalies that include mali in them.
abstractStringToDelete <- paste( abstractStringToDelete, collapse = "|")
searchStrings.countries <- regions_lookup$country_name.ISO
searchStrings.countries <- searchStrings.countries[!searchStrings.countries %in% countriesToRemove]
searchStrings.countries <- c(searchStrings.countries, shortNames.countries)
searchStrings.regionsByIncome <- regions_lookup$region_code.WB.income
searchStrings.USStates <-  state.name #built into r
# next three lines needed so West Virgina string is removed before Virginia in the process of converting state names to United States
virginias <- c("West Virginia", "Virginia")
searchStrings.USStates <- searchStrings.USStates[!searchStrings.USStates %in% virginias]
searchStrings.USStates <- c(searchStrings.USStates, virginias)
searchStrings.countries <- c(searchStrings.countries, searchStrings.USStates)
# other search strings
searchStringsList <- as.data.table(read_excel("data-raw/queries_wg2_ch05.xlsx", sheet = "filterTerms"))
countrylist <- list("searchStrings.countries", as.list(searchStrings.countries))
regionsByIncomeList <- list("searchStrings.regionsByIncome", as.list(searchStrings.regionsByIncome))
# remove the countries search so it can be done separately to deal with the 'global' problem
searchStringsList <- rbind(list("searchStrings.countries", list(searchStrings.countries)), searchStringsList)

#' Title cleanupRefFiles - remove old versions and save rds and xlsx or csv versions of the file
#' @param inDT - name of the data table or frame to be written out
#' @param outName - short name of the file to be written outs
#' @param destDir - directory where the cleanup takes place
#' @param writeFiles - format to use for writing output in addition to RDS
#' @desc brief description of the contents of the file
cleanupRefFiles <- function(metadata, inDT.scopus, inDT.wok, outName, destDir, writeFiles) {
  # sourceFile <- get("sourceFile", envir = .GlobalEnv)
  if (missing(writeFiles)) {writeFiles = "xlsx"}
  
  removeOldVersions(outName, destDir)
  sprintf("\nWriting the rds for %s to %s ", outName, destDir)
  outFile <- paste(destDir, "/", outName, "_", Sys.Date(), ".RDS", sep = "")
  
  if ("xlsx"  %in% writeFiles) {
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
    colNums.scopus <- c(match("title",names(inDT.scopus)), match("publicationName",names(inDT.scopus)), match("description",names(inDT.scopus)))
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
      #      writeLines(oldVersionList)
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
  query <- gsub("\\\\", "", query)
  firstRecord <- 1
  
  queryID = 1
  count <- 100
  notFinished = TRUE
  url <- 'https://api.clarivate.com/api/woslite/'
  queryResults <- c(Title.Title = character(), Author.Authors = character(), Author.BookAuthors = character(), Source.SourceTitle = character(),  Source.Pages = character(), 
                    Source.Volume = character(), Source.Issue = character(), Source.Published.BiblioDate = character(), Source.Published.BiblioYear = character(), 
                    Other.Identifier.Doi = character(),Other.Identifier.Isbn = character(), Doctype.Doctype = character(), Keyword.Keywords = character())
  keepListCol <- c("Title.Title", "Author.Authors", "Author.BookAuthors", "Source.SourceTitle",  "Source.Pages", 
                   "Source.Volume", "Source.Issue", "Source.Published.BiblioDate", "Source.Published.BiblioYear", 
                   "Other.Identifier.Doi","Other.Identifier.Eissn", "Doctype.Doctype", "Keyword.Keywords")
  
  newNames <- c("title","authors","bookAuthors", "publicationName", "pageRange", 
                "volume","issue", "date", "year", 
                "doi", "eIssn",  "keywords")
  
  {
    response <- httr::GET(url, httr::add_headers(accept = 'application/json', `X-APIKey` = wosliteKey), query = list(databaseId = 'WOK', usrQuery = query, count = 1, firstRecord = 1))
    stop_for_status(response, task = "bad http status")
    suppressMessages(jsonResp <- content(response, as =  "text")) # suppress messages to get rid of warning about defaulting to UTF-8
    j <- fromJSON(jsonResp)
    QueryID  <- j$QueryResult$QueryID
    nrResults <- j$QueryResult$RecordsFound
    writeLines(paste(nrResults, 'results from WOK query ID ', QueryID))
    #    writeLines(paste0("QueryID: ", QueryID))
    if (nrResults > maxQueries) stop(paste0("Number of WOK records greater than maximum queries allowed - ", maxQueries))
    # if successful, load the initial download into queryResults
    jData <- as.data.table(flatten(j$Data))
    jData[, setdiff(names(jData), keepListCol) := NULL]
    jData[, ] <- lapply(jData[, ], as.character)
    queryResults <- rbind(queryResults, jData, fill = TRUE)
    url = paste0('https://api.clarivate.com/api/woslite/query/', QueryID, "/") # get data from the specific query
  }
  step = 0
  while (notFinished) {
    step = step + 1
    writeLines(step)
    if (nrResults < (firstRecord + count) & !nrResults == -1) {
      count = nrResults - firstRecord
    }
    response <- httr::GET(url, httr::add_headers(accept = 'application/json', `X-APIKey` = wosliteKey), query = list(databaseId = 'WOK', usrQuery = query, count = count, firstRecord = firstRecord))
    suppressMessages(jsonResp <- content(response, as =  "text")) # suppress messages to get rid of warning about defaulting to UTF-8
    j <- fromJSON(jsonResp)
    QueryID  <- j$QueryResult$QueryID
    jData <- as.data.table(flatten(j$Data))
    jData[, setdiff(names(jData), keepListCol) := NULL]
    jData[, ] <- lapply(jData[, ], as.character)
    
    queryResults <- rbind(queryResults, jData, fill = TRUE)
    if (nrResults <= firstRecord + count) {
      notFinished = FALSE
      writeLines('Done getting WOK results.\n\n')
    }else{
      firstRecord = firstRecord + count
    }
  }
  
  setnames(queryResults, old = keepListCol, new = newNames, skip_absent = TRUE)
  return(queryResults)
}

# readin WOK function using a queryID
readinWOKWithQueryID <- function(query, QueryID, nrResults) {
  #  browser()
  query <- gsub("\\\\", "", query)
  firstRecord <- 1
  # nrResults <- -1
  #  queryID = 1
  count <- 100
  notFinished = TRUE
  url <- paste0('https://api.clarivate.com/api/woslite/query/', QueryID)
  queryResults <- c(Title.Title = character(), Author.Authors = character(), Author.BookAuthors = character(), Source.SourceTitle = character(),  Source.Pages = character(), Source.Volume = character(), Source.Issue = character(), Source.Published.BiblioDate = character(), Source.Published.BiblioYear = character(), Other.Identifier.Doi = character(),Other.Identifier.Isbn = character(), Doctype.Doctype = character(), Keyword.Keywords = character())
  keepListCol.long <- c("Title.Title", "Author.Authors", "Author.BookAuthors", "Source.SourceTitle",  "Source.Pages", "Source.Volume", "Source.Issue", "Source.Published.BiblioDate", "Source.Published.BiblioYear", "Other.Identifier.Doi","Other.Identifier.Eissn", "Other.Identifier.Isbn", "Doctype.Doctype", "Keyword.Keywords")
  keepListCol.short <- c("Title", "Authors", "BookAuthors", "SourceTitle", "Pages", "Volume", "Issue", "Published.BiblioDate", "Published.BiblioYear", "Identifier.Doi","Identifier.Eissn", "Identifier.Isbn", "Doctype", "Keywords")
  
  newNames <- c("title","authors","bookAuthors", "publicationName", "pageRange", 
                "volume","issue", "date", "year", 
                "doi", "eIssn", "isbn", "doctype", "keywords")
  while (notFinished) {
    if (nrResults < (firstRecord + count) & !nrResults == -1) {
      count = nrResults - firstRecord
    }
    response <- httr::GET(url, httr::add_headers(accept = 'application/json', `X-APIKey` = wosliteKey), query = list(databaseId = 'WOK', usrQuery = query, count = count, firstRecord = firstRecord))
    Sys.sleep(0.4)
    suppressMessages(jsonResp <- content(response, as = "text")) # suppress messages to get rid of warning about defaulting to UTF-8
    j <- fromJSON(jsonResp)
    
    if (!is.data.frame(j$Data)) { # loop to skip over 100 records that has an internal server error
      writeLines(paste("j$Data is not a data frame. Nearest record is ", firstRecord))
    } else {
      j[["Data"]][["UT"]] <- NULL
      jData <- as.data.table(flatten(j$Data))
      if ("Title.Title"  %in%  names(jData)) {keepListCol <- keepListCol.long} else {keepListCol <- keepListCol.short}
      setnames(jData, old = keepListCol, new = newNames, skip_absent = TRUE)
      setnames(jData, old = "BookAuthors", new = "bookAuthors", skip_absent = TRUE)
      setnames(jData, old = "Issue", new = "issue", skip_absent = TRUE)
      setnames(jData, old = "Published.BiblioDate", new = "date", skip_absent = TRUE)
      if (!"bookAuthors" %in% names(jData)) jData[, bookAuthors := "NULL"] # to deal with jData files that don't have any books
      if (!"isbn" %in% names(jData)) jData[, isbn := "NULL"] # to deal with jData files that don't have any books
      if (!"issue" %in% names(jData)) jData[, issue := "None"]
      if (!"date" %in% names(jData)) jData[, date := "None"]
      
      jData <- jData[, (newNames), with = FALSE]
      jData[, ] <- lapply(jData[, ], as.character)
      queryResults <- rbind(queryResults, jData, fill = TRUE)
    }
    if (nrResults <= firstRecord + count) {
      notFinished = FALSE
      writeLines('Done with WOK')
    }else{
      #      queryIdMode = TRUE
      firstRecord = firstRecord + count
    }
  }
  
  if (!"eIssn" %in% names(queryResults)) queryResults[, eIssn := NA] # added July 26 because of a search that didn't return an eIssn column
  queryResults[, eIssn := gsub("-", "", eIssn)]
  queryResults[, keepRef := "y"]
  setcolorder(queryResults, "keepRef")
  return(queryResults)
}

constructQuery.WOK <- function(rawQuery, yearCoverage.wok, CCSearchString, climateMitigation) {
  CCSearchString <- gsub('{', '"', CCSearchString, fixed = TRUE) # get rid of the braces that scopus seems to want 
  CCSearchString <- gsub('}', '"', CCSearchString, fixed = TRUE)
  query.wok <- sprintf('PY %s AND TS=((%s) AND ((%s)) NOT ((%s)))', yearCoverage.wok, rawQuery, CCSearchString, climateMitigation)
  query.wok <- gsub('{','"', query.wok, perl = TRUE)
  query.wok <- gsub('}','"', query.wok, perl = TRUE)
  query.wok <- replace_curly_quote(query.wok)
  return(query.wok)
}

constructQuery.scopus <- function(rawQuery, yearCoverage.scopus, CCSearchString, climateMitigation) {
  query.scopus <- sprintf('%s AND (TITLE-ABS-KEY(%s) OR AUTHKEY(%s)) AND (TITLE-ABS-KEY(%s) OR AUTHKEY(%s))', yearCoverage.scopus, rawQuery, rawQuery, CCSearchString, CCSearchString)
  query.scopus <- replace_curly_quote(query.scopus)
  writeLines(query.scopus)
  query.scopus <- gsub("\\\\", "", query.scopus)
  return(query.scopus)
}

getDBinfo <- function(queries, yearCoverage.wok, yearCoverage.scopus,  CCSearchString = CCSearchString) {
  queryInfo <- data.table(sectionName = character(),fileName = character(), queryNumber = character(), rawQuery = character(), QueryID.wok = numeric(),QueryID.scopus = numeric(), nrResults.wok = numeric(), nrResults.scopus = numeric(), query.wok = character(), query.scopus = character())
  url.wok <- 'https://api.clarivate.com/api/woslite/'
  workingText <- "Working on query"
  for (i in 1:(nrow(queries))) {
    rawQuery <- queries[i, query]
    
    #working on WOK
    query.wok <- constructQuery.WOK(rawQuery, yearCoverage.wok = yearCoverage.wok, CCSearchString = CCSearchString, climateMitigation)
    query.wok <- gsub("AND NOT", "NOT", query.wok)
    writeLines(paste( workingText, queries$queryNumber[i], ": output name:", queries$outFileName[i]))
    writeLines(paste0("query.wok: ", query.wok))
    response.wok <- httr::GET(url.wok, httr::add_headers(accept = 'application/json', `X-APIKey` = wosliteKey), query = list(databaseId = 'WOK', usrQuery = query.wok, count = 1, firstRecord = 1))
    suppressMessages(jsonResp <- content(response.wok, as =  "text")) # suppress messages to get rid of warning about defaulting to UTF-8
    j <- fromJSON(jsonResp)
    if (!is.null(j$code)) {
      print(paste0("j$code ", j$code))
      writeLines(paste0("This query is malformed: ", query.wok))
    }
    QueryID.wok  <- j$QueryResult$QueryID
    nrResults.wok <- j$QueryResult$RecordsFound
    
    #SCOPUS   
    query.scopus <- constructQuery.scopus(rawQuery, yearCoverage.scopus = yearCoverage.scopus, CCSearchString = CCSearchString, climateMitigation)
    writeLines(paste0("query.scopus: ", query.scopus))
    response.scopus = scopus_search(query = query.scopus, api_key = Scopus_api_key, insttoken = insttoken, max_count = 1, count = 1,  start = 1, verbose = FALSE, view = c("COMPLETE"))
    nrResults.scopus <- response.scopus$total_results
    QueryID.scopus <- NA 
    
    newRow <- list(queries$sectionName[i], queries$outFileName[i], queries$queryNumber[i], rawQuery, QueryID.wok, QueryID.scopus, nrResults.wok, nrResults.scopus, query.wok, query.scopus)
    queryInfo <- rbind(queryInfo, newRow)
    Sys.sleep(0.4)
  }
  writeLines("Done!")
  return(queryInfo)
}

getScopusinfo <- function(queries, yearCoverage.scopus) {
  queryInfo <- data.table(query = character(), QueryID = numeric(), nrResults = (numeric()))
  workingText <- "Working ."
  for (i in 1:nrow(queries)) {
    writeLines(workingText)
    rawQuery <- queries[i, query]
    query <- constructQuery.scopus(rawQuery, yearCoverage.scopus = yearCoverage.scopus, CCSearchString = CCSearchString)
    
    response = scopus_search(query = query, max_count = 1, count = 1,  start = 1, verbose = FALSE, view = c( "COMPLETE"))
    nrResults <- response$total_results
    QueryID <- "unknown"
    
    newRow <- list(queries$outFileName[i], queries$queryNumber[i], query.wok, QueryID, nrResults)
    queryInfo <- rbind(queryInfo, newRow)
    Sys.sleep(0.4)
    workingText <- paste0(workingText, ".")
  }
  writeLines("Done getting Scopus info!")
  return(queryInfo)
}

readinSCOPUS <- function(query, rawQuery, searchStringsList) {
  #  browser()
  keepListCol.content <- 
    c("dc:title", "prism:publicationName", "prism:aggregationType", "prism:isbn", "prism:issn", "prism:volume", "prism:issueIdentifier", "prism:pageRange", "prism:coverDate", "prism:doi", "dc:creator", "citedby-count", "author", "dc:description", "authkeywords")
  keepListCol.content.newNames <- 
    c("title", "publicationName",	"pubType", "isbn",	"issn",	"volume",	"issue", "pageRange",	"date", "doi", "firstAuthor", "citations", "allAuthors", "description", "author_keywords")
  keepListCol.author <- c("@seq", "authname", "entry_number")
  
  # rawQuery added here to add a new column to the spreadsheet for its terms
  rawQueryTerms <- rawQuerySearchTerms(rawQuery)
  rawQuery.name <- "searchStrings.baseQuery"
  rawQueryList <- list(rawQuery.name, rawQueryTerms)
  
  # this is where the rawQuery is put into the filter column order
  searchStringsList <- rbindlist(list(searchStringsList[1:2, ], rawQueryList, searchStringsList[3:nrow(searchStringsList), ]))
  
  searchStrings <- searchStringsList$searchStringName
  searchStrings <- replace_curly_quote(searchStrings)
  searchStrings.names <- gsub("searchStrings.", "", searchStrings)
  
  # what variables in the reference list should be searched for
  searchCols <- c("title", "description", "author_keywords") # removed , "document_type" Aug 1, 2019
  # create blank queryResults in case there are 0 results of a searchs
  queryResults <- data.table(NULL)
  # next few lines used to get totalresults
  query <- gsub("\\\\", "", query)
  response = scopus_search(query = query, api_key = Scopus_api_key, insttoken = insttoken, max_count = 1, count = 1,  start = 1, verbose = FALSE, view = c( "COMPLETE"))
  nrResults <- response$total_results
  if (nrResults > maxQueries) stop(paste0("Number of SCOPUS records greater than maximum queries allowed - ", maxQueries))
  if (nrResults == 0) {
    writeLines("No references for this query")
  }else{
    cat(green(query)) # display scopus query in green
    print(paste0("\nScopus query: ", query))
    response = scopus_search(query = query, api_key = Scopus_api_key, insttoken = insttoken, max_count = nrResults, count = 25,  start = 0, verbose = FALSE, view = c("COMPLETE"))
    df <- gen_entries_to_df(response$entries)
    dt.content <- as.data.table(df$df)
    dt.content[, setdiff(names(dt.content), keepListCol.content) := NULL]
    setnames(dt.content, old = keepListCol.content, new = keepListCol.content.newNames, skip_absent = TRUE)
    dt.authorInfo <- as.data.table(dt.content$firstAuthor)
    queryResults <- unique(dt.content)
    temp.author <- unique(dt.authorInfo$V1) # a kludge because Scopus changed their output
    
    for (i in searchStrings.names) {
      queryResults[, (i) := "None"] # add None to the cell and then update below where there is actually an entry
    }
    # new columns with no search strings. Used for manual entry of information
    colsWithNoSearchStrings <- c("comments")
    # default values
    for (i in colsWithNoSearchStrings) {
      queryResults[, (i) := "None"]
    }
    queryResults[, keepRef := "y"]
    # queryResults[, IPCCregions := "None"]
    # queryResults[, IPCCsubregions := "None"]
    setcolorder(queryResults, "keepRef")
    queryResults[, notPeerRev := "No"]
    setkey(queryResults)
    
    # remove non-peer reviewed reference types
    nonPeerReviewed.scopus <- c("Conference Proceeding", "Letter", " Correction", "Editorial", "Editorial Material", "Note", "Trade Journal", "Conference Paper", "Proceedings Paper", "Conference Review", "Erratum", "Short Survey", "Business Article", NA)
    queryResults[, abstract.temp := description]
    queryResults[, abstract.temp.global := description]
    write_rds(queryResults, "queryResultsBefore.RDS")
    require(stringr)
    
    # from https://stackoverflow.com/questions/56083274/removing-multiple-words-from-a-string-using-a-vector-instead-of-regexp-in-r
    queryResults[, abstract.temp := trimws(gsub(abstractStringToDelete, "\\1", abstract.temp, ignore.case = TRUE))]
    searchColsTemp <- c("title", "abstract.temp", "author_keywords")
    
    #queryResults
    
    # remove searchStrings.region to deal with the 'global' problem separately
    searchStrings <- searchStrings[!searchStrings %in% "searchStrings.regions"]
    searchStringsList <- searchStringsList[!searchStringName %in% "searchStrings.regions",]
    searchStrings.names <-  searchStrings.names[!searchStrings.names %in% "regions"]
    for (i in 1:length(searchStrings)) {
      searchSt <- eval(parse(text = searchStringsList[,.SD[i]]$searchString)) # get list of search terms for the ith search list
      searchSt <- replace_curly_quote(searchSt)
      for (j in searchSt) {
        #      print(j)
        i1 <- queryResults[, Reduce("|", lapply(.SD, function(x) grepl(j, x, ignore.case = TRUE, perl = FALSE))), .SDcols = searchColsTemp]
        queryResults[i1, (searchStrings.names[i]) := paste(get(searchStrings.names[i]), j , sep = ", ")]
      }
    }
    
    globalStringToDelete <- c("global warming", "global climate", "global climate model","global population", "global famine", "global drought", "global average", "global land", "global water", "global circulation", "global SOC", "global climate circulation models", "global circulation", "global food", "global heating", "global south", "IGI global", "global eco-systems", "global terrestrial", "global cropland", "global land", "global validation", "global challenges", "global incidence", "global cotton", "global meta-analysis", "global perspectives", "global socio-economic", "global cereal", "global network", "global connectivity", "global dynamic", "global event", "genomic/global", "global transcriptomic", "global dependency", "global view")
    globalStringToDelete <- paste( globalStringToDelete, collapse = "|")
    queryResults[, abstract.temp.global := trimws(gsub(globalStringToDelete, "\\1", abstract.temp.global, ignore.case = TRUE))]
    
    searchColsGlobal <- c("title", "abstract.temp.global", "author_keywords")
    
    iglobal <- queryResults[, Reduce("|", lapply(.SD, function(x) grepl("global", x, ignore.case = TRUE, perl = FALSE))), .SDcols = searchColsGlobal]
    queryResults[iglobal, regions := paste("global" , sep = ", ")]
    queryResults[, c("abstract.temp", "abstract.temp.global") := NULL]
    
    for (i in searchStrings.names) {
      queryResults[, (i) := gsub(paste0("None, "), "", get(i))]
    }
    queryResults[, (searchStrings.names) := (lapply(.SD, function(x) {
      sapply(x, function(y) paste(sort(trimws(strsplit(y, ',')[[1]])), collapse = ', '))
    })), .SDcols = searchStrings.names]
    
    for (i in searchStrings.names) {
      queryResults[, (i) := gsub(paste0("No, "), "", get(i))]
    }
    for (i in searchStrings.names) {
      queryResults[, (i) := gsub(paste0(i, ", "), "", get(i))]
    }
    queryResults[, author_keywords := str_replace_all(author_keywords, " \\|, ", " ")]
    queryResults[, author_keywords := str_replace_all(author_keywords, " \\|", ", ")]
  }
  
  for (countryName in state.name) {
    queryResults[, countries := gsub(countryName, "United States", countries)]
    queryResults[, countries := gsub("United States, United States", "United States", countries)]
    queryResults[, countries := gsub("United States, United States, United States", "United States", countries)]
  }
  
  # do IPCCregions -----
  queryResult.countries <- queryResults[, c("countries")]
  queryResult.countries[, row := seq_len(.N)] # adds a new column called row that has the row number in each cell
  # next line adds single region to the IPCCregions column and adds rows for references that have multiple region names
  queryResult.countries <- splitstackshape::cSplit(queryResult.countries, 'countries', ',', 
                                                   direction = 'long')[RegionLookupWG2, IPCCregions := WGII_Region, on = c(countries = "countryName")]
  
  queryResult.countries <- queryResult.countries[, lapply(.SD, toString), row][,row:=NULL]
  queryResult.countries <- queryResult.countries[IPCCregions == "NA",  IPCCregions := "None"]
  queryResults[, countries := NULL]
  queryResults <- cbind(queryResults, queryResult.countries)
  
  # do IPCCsubregions -----
  queryResult.countries <- queryResults[, c("countries")]
  queryResult.countries[, row := seq_len(.N)] # adds a new column called row that has the row number in each cell
  # next line adds single region to the IPCCregions column and adds rows for references that have multiple region names
  #  queryResults.temp <- copy(queryResults)
  queryResult.countries <- splitstackshape::cSplit(queryResult.countries, 'countries', ',', 
                                                   direction = 'long')[RegionLookupWG2, IPCCsubregions := WGII_Subregion, on = c(countries = "countryName")]
  
  queryResult.countries <- queryResult.countries[, lapply(.SD, toString), row][,row:=NULL]
  queryResult.countries <- queryResult.countries[IPCCsubregions == "NA",  IPCCsubregions := "None"]
  queryResults[, countries := NULL]
  queryResults <- cbind(queryResults, queryResult.countries)
  
  # set scopus column name order here to make sure all queries have the same column order
  # column names that should be in all scopus refs
  scopusColNames <- c("title", "firstAuthor", "publicationName", "eIssn", "volume", "issue", "date", "doi", "description", "citations", "pubType",  "pageRange", "author_keywords")
  filterColumns <- names(queryResults)[!names(queryResults) %in% c("keepRef", "comments", scopusColNames)]
  queryColOrder <- c("keepRef", "title", "firstAuthor", "publicationName", "volume", "date", "doi", "description", "citations", "pubType",  "author_keywords", "eIssn", "issue", "pageRange", "countries", "regions", "IPCCregions", "IPCCsubregions", "baseQuery", "RCP", "SSP", "CCsource", "CCimpact", 
                     "CCadapt", "CCobserved", "CCvulnerability", "animals", "cereals", "fruits", "vegetables", "rootsNtubers", "stimulants", "fish", "foodSec", "foodSafety", 
                     "notPeerRev", "timePeriod", "econ", "gender", "indigenous", "ethnicity", "adaptStrat", "climZones", "crpSystems", "nutrients", "fibre", "pestsPlants", 
                     "pestsAnimals", "cropModels", "engLevel", "adapType", "adapProfile", "adapStage", "adapBeneficiary", "adapLimits", "adapConstraints", "extentCC", "biotech", "breadbaskets", "comments")
  if (!"issue" %in% names(queryResults)) queryResults[, issue := "None"]
  if (!"date" %in% names(queryResults)) queryResults[, date := "None"]
  if (!"eIssn" %in% names(queryResults)) queryResults[, eIssn := "None"]
  
  setcolorder(queryResults, neworder = queryColOrder)
  write_rds(queryResults, "queryResultsAfter.RDS")
  writeLines('Done with SCOPUS\n')
  return(queryResults)
}

prepareSpreadsheet <- function(sectionName, rawQuery, queryResults.scopus, query.scopus, queryResults.wok, query.wok, outName, searchStringsList) {
  # construct metadata variable
  DT <- data.table(searchStringName = character(),  searchString = character())
  metadata.querystring.scopus <- list("SCOPUS query", query.scopus)
  metadata.querystring.wok <- list("WOK query", query.wok)
  metadata.datestring <- list("date queried", as.character(Sys.Date()))
  metadata.datestring <- list("Report section", sectionName)
  metadata.recordCount.scopus <- list("SCOPUS reference count", nrow(queryResults.scopus))
  metadata.recordCount.wok <- list("WOK only reference count", nrow(queryResults.wok))
  rawQueryTerms <- rawQuerySearchTerms(rawQuery)
  metadata.baseQuery <- list("Base search terms", rawQueryTerms)
  
  metadata.searchStringLabel <- list("search string names", "search string content, SCOPUS only")
  metadata.notes <- list("See Notes worksheet for more details.", "")
  
  DT <- rbind(DT, metadata.notes) # put here to put the recommendation to read the Notes at the top of the metadata.
  DT <- rbind(DT, metadata.baseQuery) # put here to put the recommendation to read the Notes at the top of the metadata.
  DT <- rbind(DT, metadata.querystring.scopus)
  DT <- rbind(DT, metadata.querystring.wok)
  DT <- rbind(DT, metadata.datestring)
  DT <- rbind(DT, metadata.recordCount.scopus)
  DT <- rbind(DT, metadata.recordCount.wok)
  DT <- rbind(DT, metadata.searchStringLabel)
  
  searchStrings <- searchStringsList$searchStringName
  searchStrings.names <- gsub("searchStrings.", "", searchStrings)
  
  for (i in 1:length(searchStrings)) {
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
  if (missing(writeFiles)) {writeFiles = "xlsx"}
  if (missing(destDir)) {destDir = fileloc("mData")}
  if (missing(numVal)) numVal = "0.000"
  wrapColsOn = 1
  if (missing(wrapCols)) wrapColsOn = 0
  colNames <- paste(colnames(inDT), collapse = ", ")
  #convert to a COMPLETE order
  oldOrder <- names(inDT)
  startOrder <- c("scenario","year")
  if (all(startOrder %in% oldOrder)) {
    remainder <- oldOrder[!oldOrder %in% startOrder]
    data.table::setcolorder(inDT,c(startOrder,remainder))
    data.table::setorderv(inDT,c(startOrder,remainder))
  }
  
  removeOldVersions(outName, destDir)
  sprintf("\nWriting the rds for %s to %s ", outName, destDir)
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
    
    xcelOutFileName = paste(destDir, "/", longName, "_", Sys.Date(), ".xlsx", sep = "")
    openxlsx::saveWorkbook(wbGeneral, xcelOutFileName, overwrite = TRUE)
  }
}

prepareOutput <- function(queryNum, queries, rejectList_master, climateMitigation) {
  rawQuery <- queries[queryNumber %in% queryNum, rawQuery]
  rawQuery <- gsub("\\\\", "", rawQuery)
  outFileName <- queries[queryNumber %in% queryNum, fileName]
  writeLines(paste0("Working on query ", queryNum, " filename: ", outFileName))
  query.wok <- constructQuery.WOK(rawQuery, yearCoverage.wok, CCSearchString, climateMitigation) 
  query.wok <- gsub("AND NOT", "NOT", query.wok)
  writeLines(query.wok)
  QueryID.wok <- queries[queryNumber %in% queryNum, QueryID.wok] # for WOK
  nrResults.wok <- queries[queryNumber %in% queryNum, nrResults.wok] # for WOK
  queryResults.wok <- readinWOKWithQueryID(query = query.wok, QueryID = QueryID.wok, nrResults = nrResults.wok)
  writeLines(paste0("queryResults.wok count: ", nrow(queryResults.wok)))
  
  nrResults.scopus <- queries[queryNumber %in% queryNum, nrResults.scopus] # for SCOPUS
  if (nrResults.scopus > maxQueries) {
    writeLines(paste0("Number of results for scopus greater than ", maxQueries, " for query ", queryNum, ". Skipping it."))
  } 
  if (nrResults.scopus == 0) {
    writeLines(paste0("No results for scopus for query ", queryNum, ". Skipping it."))
    queryResults.scopus <- data.table(NULL)
  }
  
  if ((!nrResults.scopus > maxQueries) & (!nrResults.scopus == 0)) {
    query.scopus <- constructQuery.scopus(rawQuery, yearCoverage.scopus, CCSearchString, climateMitigation)
    queryResults.scopus <- readinSCOPUS(query = query.scopus, rawQuery, searchStringsList)
    temp <- rejectList_master[queryName %in% outFileName,]
    queryResults.scopus <- queryResults.scopus[!title %in% temp$rejectTitle,]
  }
  
  # process if both results have content
  doi.wok <- sort(queryResults.wok$doi)
  if (!nrow(queryResults.scopus) == 0 & !nrow(queryResults.wok) == 0 & !is.null(doi.wok)) {
    writeLines(paste0("doi.wok count: ", length(doi.wok))) # doi.wok is a character variable
    doi.scopus <- sort(queryResults.scopus$doi)
    writeLines(paste0("doi.scopus count: ", length(doi.scopus))) # doi.wok is a character variable
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
    temp <- rejectList_master[queryName %in% outFileName,]
    queryResults.wok.unique <- queryResults.wok.unique[!title %in% temp$rejectTitle,]
    dois.combined <- unique(c(doi.unique.scopus, doi.unique.wok))
    dois.combined <<- dois.combined[!is.null(dois.combined)]
    writeLines(paste("csv out: ", paste0("results/dois/combinedDOIs_/", outFileName, "_", Sys.Date(),".csv")))
    write_csv(as.data.table(dois.combined), paste0("results/dois/combinedDOIs_", outFileName, "_", Sys.Date(),".csv"))
    
    prepareSpreadsheet(sectionName = queries[queryNumber == queryNum, sectionName], rawQuery, queryResults.scopus, query.scopus, queryResults.wok = queryResults.wok.unique, query.wok, outFileName, searchStringsList)
  }
  if ((nrow(queryResults.scopus) == 0) & (!nrow(queryResults.wok) == 0)) {
    queryResults.wok.unique <-   queryResults.wok
    nonPeerReviewed.wok <- c("Conference Proceeding", "Letter", " Correction", "Editorial", "Editorial Material", "Note", "Trade Journal", "Conference Paper", "Conference Review", "Erratum", "Short Survey", "Business Article", NA)
    queryResults.wok.unique[!subtype %in% nonPeerReviewed.wok, ]
    prepareSpreadsheet(sectionName = queries[queryNumber == queryNum, sectionName], queryResults.scopus, query.scopus, queryResults.wok = queryResults.wok.unique, query.wok, outFileName, searchStringsList)
  }
}

# function to return raw query search terms
# rawQuery added here to add a new column to the spreadsheet for its terms
rawQuerySearchTerms <- function(rawQuery) {
  rawQuery <- gsub('  ', ' ', rawQuery, fixed = TRUE)
  rawQuery <- gsub(' AND ', ', ', rawQuery, fixed = TRUE)
  rawQuery <- gsub(' OR ', ', ', rawQuery, fixed = TRUE)
  rawQuery <- gsub(' or ', ', ', rawQuery, fixed = TRUE) # added because somewhere an OR is being converted to or and I don't want to figure out where.
  rawQuery <- gsub('(', '', rawQuery, fixed = TRUE)
  rawQuery <- gsub(')', '', rawQuery, fixed = TRUE)
  rawQuery <- as.character(strsplit(rawQuery, split = ", ", fixed = TRUE)[[1]])
  rawQuery <- as.list(rawQuery)
  # rawQuery <- gsub('{', '', rawQuery, fixed = TRUE)
  # rawQuery <- gsub('}', '', rawQuery, fixed = TRUE)
  rawQuery <- paste0(rawQuery, '"')
  rawQuery <- paste0('"', rawQuery)
  rawQuery <- paste0(rawQuery, collapse = ", ")
  rawQuery <- paste0("c(", rawQuery)
  rawQuery <- paste0(rawQuery, ")")
  rawQuery <- gsub('\"\"', '\"', rawQuery)
  rawQuery.name <- "searchStrings.baseQuery"
  rawQueryList <- list(rawQuery.name, rawQuery)
  return(rawQuery)
}

createBibtexEntry <- function(doiIn) {
  # code originally from https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      #      message("This is the 'try' part")
      h <- new_handle()
      handle_setheaders(h, "accept" = "application/x-bibtex")
      urlIn <- paste0("https://doi.org/", doiIn)
      temp <- paste0("curl(url = '", urlIn,"', handle = h)")
      
      con <- eval(parse(text = temp))
      bibtemp <- readLines(con, warn = FALSE)
      close(con)
      return(bibtemp)
      # readLines(con=url, warn=FALSE) 
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error = function(cond) {
      # message(paste("URL does not seem to exist:", url))
      # message("Here's the original error message:")
      # message(cond)
      # Choose a return value in case of error
      return("Rejected")
    }
    # warning = function(cond) {
    #   message(paste("URL caused a warning:", url))
    #   message("Here's the original warning message:")
    #   message(cond)
    #   # Choose a return value in case of warning
    #   return(NULL)
    # },
    # finally = {
    #   #   # NOTE:
    #   #   # Here goes everything that should be executed at the end,
    #   #   # regardless of success or error.
    #   #   # If you want more than one expression to be executed, then you 
    #   #   # need to wrap them in curly brackets ({...}); otherwise you could
    #   #   # just have written 'finally=<expression>' 
    #   #   message(paste("Processed URL:", url))
    #   close(con)
    # }
  )    
  return(out)
}

doiToBibtex <- function(doiList, filename) {
  #store all the bibtex entries to a file
  require(curl)
  file.create(filename)
  for (j in 1:length(doiList)) {
    print(paste0("working on DOI ", doiList[j], ", citation ", j, " of ",length(doiList) ))
    tempDOI <- doiList[j]
    bibtemp <- createBibtexEntry(tempDOI)
    write(bibtemp, file = filename, append = TRUE)
  }
}

# source of the DOI to Bibtex code is https://rdrr.io/github/wkmor1/doi2bib/src/R/doi2bib.r
#' Convert DOI to Bibtex
#'
#' Convert a digital object identifier (DOI) string into a bibtex entry using
#' the webservice \url{http://www.doi2bib.org}.
#'
#' @param ... One or more DOIs, as \code{character} strings. If arguments are
#'   named, names will replace default citekeys.
#' @param file an optional \code{character} string. If used, the bibtex
#'   references are sent to \code{file} rather than being returned.
#' @param append \code{logical}. Append results to file?
#' @param quiet \code{logical}. By default, bibtex references are printed to the
#'   console. By setting \code{quiet} to \code{TRUE}, this behaviour will be
#'   prevented.
#'
#' @return a \code{list}, returned invisibly, of bibtex references as
#'   \code{character} strings, as well as writing to file if \code{file} is
#'   specified.
#'
#' @importFrom httr accept content GET
#' @importFrom methods setGeneric setMethod signature
#' @importFrom xml2 xml_text xml_find_all
#'
#' @examples
#' doi2bib(Margules2000 = "10.1038/35012251")
#' doi2bib(Margules2000 = "10.1038/35012251",
#'         Myers2000    = "10.1038/35002501",
#'         Moilanen     = "978-0199547777")
#' @export

setGeneric(
  "doi2bib",
  function(..., file, append = TRUE, quiet = FALSE) {
    COMPLETEGeneric("doi2bib")
  },
  signature = signature("...")
)

replace_citekeys <-
  function(refs, nms) {
    setNames(
      mapply(
        function(ref, nm) {
          ifelse(
            nchar(nm) < 1,
            ref,
            sub("([^\\{]+\\{)[^,]+", paste0("\\1", nm), ref)
          )
        },
        refs,
        nms,
        SIMPLIFY = FALSE
      ),
      nms
    )
  }

refs_to_file <-
  function(refs, file, append) {
    cat(paste(refs, collapse = "\n"), file = file, append = append)
  }

get_doi <-
  function(doi) {
    content(
      GET(
        url    = "http://www.doi2bib.org/",
        config = accept("application/x-bibtex"),
        path   = "doi2bib",
        query  = list(id = doi)
      ),
      as = "text",
      encoding = "UTF-8"
    )
  }


