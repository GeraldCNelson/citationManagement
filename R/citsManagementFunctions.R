library(readxl)
library(data.table)
library(openxlsx)
library(readr)
#' Title cleanup - remove old versions and save rds and xlsx or csv versions of the file
#' @param inDT - name of the data table or frame to be written out
#' @param outName - short name of the file to be written out
#' @param destDir - directory where the cleanup takes place
#' @param writeFiles - format to use for writing output in addition to RDS
#' @desc brief description of the contents of the file
cleanup <- function(inDT, outName, destDir, writeFiles) {
 # sourceFile <- get("sourceFile", envir = .GlobalEnv)
  if (missing(writeFiles)) {writeFiles = "xlsx"}
 # if (missing(destDir)) {destDir = fileloc("mData")}
  
#  colNames <- paste(colnames(inDT), collapse = ", ")
#  outInfo <- list(outName, sourceFile, destDir, desc, colNames)
#  metadataDT <<- rbind(metadataDT, outInfo)
  #  cat("\n", "Outfilename: ", outName, " Destination: ", Destination," Script: ", sourceFile," Desc: ", desc," Col. names: ", colNames, "\n")
  #convert to a standard order
 # oldOrder <- names(inDT)
  # startOrder <- c("scenario",keyVariable("region"),"year")
  # if (all(startOrder %in% oldOrder)) {
  #   remainder <- oldOrder[!oldOrder %in% startOrder]
  #   data.table::setcolorder(inDT,c(startOrder,remainder))
  #   data.table::setorderv(inDT,c(startOrder,remainder))
  # }
  
  removeOldVersions(outName, destDir)
  sprintf("\nWriting the rds for %s to %s ", outName, destDir)
  # print(proc.time())
  # next line removes any key left in the inDT data table; this may be an issue if a df is used
  data.table::setkey(inDT, NULL)
  outFile <- paste(destDir, "/", outName, "_", Sys.Date(), ".RDS", sep = "")
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
    numStyle <- openxlsx::createStyle(numFmt = "0.000")
    wrapStyle <- createStyle( wrapText = TRUE, valign = "top")
    
    wbGeneral <- openxlsx::createWorkbook()
    longName <- outName
    outName <- strtrim(outName, c(31))
    openxlsx::addWorksheet(wb = wbGeneral, sheetName = outName)
    
    openxlsx::writeDataTable(
      wbGeneral,
      inDT,  sheet = outName, startRow = 1, startCol = 1, rowNames = FALSE,
      colNames = TRUE, withFilter = TRUE)
    
    openxlsx::setColWidths(
      wbGeneral, sheet = outName, cols = 1:ncol(inDT), widths = "10" )
    # column numbers in WoS file for title, publicationName, and abstract 
    colNums <- c(match("title",names(inDT)), match("publicationName",names(inDT)), match("abstract",names(inDT)))
    openxlsx::setColWidths(
      wbGeneral, sheet = outName, cols = colNums, widths = c(40,30,70))
    
    openxlsx::addStyle(
      wbGeneral, sheet = outName, style = numStyle, rows = 1:nrow(inDT) + 1, cols = 2:ncol(inDT), # +1 added Mar 24, 2017
      gridExpand = TRUE )
    openxlsx::addStyle(
      wbGeneral, sheet = outName, style = wrapStyle, rows = 1:nrow(inDT) + 1, cols = 1:ncol(inDT), # +1 added Mar 24, 2017
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