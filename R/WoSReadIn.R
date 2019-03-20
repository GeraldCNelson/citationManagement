source("R/citsManagementFunctions.R")

# get input data file name and location
infileName <- "data-raw/cc_livestock_03_19_2019.txt"
# outputFileContent is combined with "WoS to create the output file names
outFileContent <- "livestock"

# get country names
regions_lookup <- read_excel("data-raw/regions lookup June 15 2018.xlsx")
searchStrings.countries <- regions_lookup$country_name.ISO

# other search strings
searchCols <- c("title", "abstract", "author.keywords") # what variables in the reference list should be searched for
searchStrings.RCP <- c("RCP",  "RCP4.5", "RCP8.5", "CMIP", "SRES") # entries in the added RCP column
searchStrings.SSP <- c("SSP",  "SSP1", "SSP2", "SSP3", "SSP4","SSP5") # entries in the added SSP column
searchStrings.regions <- c("Latin America", "Central America", "Caribbean", 
                           "Europe", "Northern Europe", "Western Europe", "Southern Europe", "Eastern Europe", "Western Asia", "Middle East",
                           "Asia", "South Asia", "East Asia", "Central Asia", "Australia", "New Zealand",
                           "Southeast Asia") # entries in the added region column

# read in Wos mac tab delimited file
# get WoS variable names
WoS_Column_abbreviations_and_names <- as.data.table(read_excel("data-raw/WoS Column abbreviations and names.xlsx", 
                                                               col_names = TRUE))

#referenceList.wos <- as.data.table(read.delim(infileName, stringsAsFactors=FALSE))

referenceList.wos <- as.data.table(read_delim(infileName, delim = "\t", escape_double = FALSE, trim_ws = TRUE))
names.referenceList.wos <- names(referenceList.wos)
temp <- WoS_Column_abbreviations_and_names[abbrev %in% names.referenceList.wos, ]
setnames(referenceList.wos, old = temp$abbrev, new = temp$name_adj)
referenceList.wos <- referenceList.wos[!pubType %in% c("PT", "P", "\032") ] # PT is a consequence of pasting text files together manually
referenceList.wos[, pageRange := paste(page_start, "-", page_end)]
referenceList.wos[, c("page_start", "page_end") := NULL]
keepListCol.content.newNames <- c("title","firstAuthor","publicationName","document_type", # removed "url","identifier","eid",
                                  "eIssn","volume","issue","date", "year", "pageRange",
                                  "doi","abstract","citations","pubType",
                                  "refKind", "author.keywords","language")
referenceList.wos[, setdiff(names(referenceList.wos), c(keepListCol.content.newNames)) := NULL]
# fix publication type names
referenceList.wos <- referenceList.wos[pubType %in% "J", pubType := "journal"][pubType %in% "B", pubType := "book"][pubType %in% "S", pubType := "series"]

referenceList.wos[, RCP := "None"]
referenceList.wos[, SSP := "None"]
referenceList.wos[, region := "None"]
referenceList.wos[, country := "None"]
referenceList.wos[, keepRef := "No"]

setkey(referenceList.wos)
for (i in searchStrings.RCP) {
  i1 <- referenceList.wos[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  referenceList.wos[i1, RCP := paste(RCP,i, sep = ", ")]
}

for (i in searchStrings.SSP) {
  i1 <- referenceList.wos[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  referenceList.wos[i1, SSP := paste(SSP,i, sep = ", ")]
}

for (i in searchStrings.regions) {
  i1 <- referenceList.wos[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  referenceList.wos[i1, region := paste(region,i, sep = ", ")]
}

for (i in searchStrings.countries) {
  i1 <- referenceList.wos[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  referenceList.wos[i1, country := paste(country,i, sep = ", ")]
}
# remove first "none, "
referenceList.wos[, RCP := gsub("None, ", "", RCP)]
referenceList.wos[, SSP := gsub("None, ", "", SSP)]
referenceList.wos[, region := gsub("None, ", "", region)]
referenceList.wos[, country := gsub("None, ", "", country)]

inDT <- referenceList.wos
outName <- paste("WOS", outFileContent, sep = "_")
cleanup(inDT, outName, destDir = "results", writeFiles = "xlsx")
