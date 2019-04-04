library("rscopus")
source("R/citsManagementFunctions.R")

#get_api_key(api_key = Sys.getenv('Elsevier_API'))
 if (!have_api_key()) stop("Missing api key")

#query <- 'TITLE-ABS-KEY("climate change" AND food*)  AND PUBYEAR > 2013'
query <- 'TITLE-ABS-KEY("climate change" AND food or food+*secur* or food+access or food+afford* or food+insecure* or
                      food+insufficien* or food+choice* or food+choice* or food+choice* or food+choice* or food+choice* or
food+availabili* or food+intake* or food+utilization* or food+stability* or food+quality* or food+poverty*) AND  PUBYEAR > 2013'

query <- 'TITLE-ABS-KEY(land AND ocean AND climate AND change) AND (conflict OR adaptation OR use OR mitigation) AND PUBYEAR > 2013'
query <- 'TITLE-ABS-KEY("climate change" AND livestock*)  AND PUBYEAR > 2013'
query <- 'TITLE-ABS-KEY("climate change" AND "food value chain")  AND PUBYEAR > 2013'
query <- 'TITLE-ABS-KEY("climate change" AND food+system*)  AND PUBYEAR > 2013'
query <- 'TITLE-ABS-KEY("climate change" AND financialization AND agriculture)  AND PUBYEAR > 2013'
query <- 'TITLE-ABS-KEY("climate change" AND storage+loss* AND (crop OR livestock or fish))  AND PUBYEAR > 2013'
query <- 'TITLE-ABS-KEY("climate change" AND genet*) AND (crop OR livestock or fish)  AND PUBYEAR > 2013'
query <- 'TITLE-ABS-KEY("climate change" AND impact*) AND (crop OR livestock or fish)  AND PUBYEAR > 2013'
query <- 'TITLE-ABS-KEY(climate+change AND impact* AND frui* OR tomato* OR strawberr* OR blueberr* OR raspberr* OR grap*)  AND PUBYEAR > 2013'
outFileContent <- "food"
outFileContent <- "livestock"
outFileContent <- "valueChain"
outFileContent <- "foodSystem"
outFileContent <- "oceans"
outFileContent <- "financialization"
outFileContent <- "storageLoss"
outFileContent <- "geneticEng"
outFileContent <- "impacts"
outFileContent <- "fruitImpact"


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

# next few lines used to get totalresults
res = scopus_search(query = query, max_count = 1, count = 1,  start = 5, verbose = TRUE, view = c( "COMPLETE"))
total_results <- res$total_results
if (total_results > 5000) stop("Record number greater than 5000")

res = scopus_search(query = query, max_count = total_results, count = 25,  start = 0, verbose = FALSE, view = c( "COMPLETE"))
df <- gen_entries_to_df(res$entries)
dt.content <- as.data.table(df$df)
dt.content[, setdiff(names(dt.content), keepListCol.content) := NULL]
setnames(dt.content, old = keepListCol.content, new = keepListCol.content.newNames)
dt.authorInfo <- as.data.table(df$author)
dt.authorInfo[, setdiff(names(dt.authorInfo), keepListCol.author) := NULL]

authsList <- data.table(authrs = character())
for (i in unique(dt.authorInfo$entry_number)) {
  temp <-dt.authorInfo[entry_number == i, authname]
  temp <- as.list(paste(temp, collapse = ", "))
  authsList <- rbind(authsList, temp)
}
temp.content <- unique(dt.content)
temp.author <- unique(dt.authorInfo)

for (i in searchStrings.names) {
  temp.content[, (i) := "None"]
}

# new columns with no search strings
colsWithNoSearchStrings <- c("warmingDegrees (C)", "prod_system", "study_type", "study_period", "methodology", 
                             "adapt_strat", "resid_damage", "comments")
for (i in colsWithNoSearchStrings) {
  temp.content[, (i) := "None"]
}
temp.content[, keepRef := "No"]
temp.content[, notPeerRev := "No"]

setkey(temp.content)

for (i in 1:length(searchStrings)) {
  searchSt <- eval( parse(text = searchStrings[i]))
  for (j in searchSt) {
    i1 <- temp.content[, Reduce("|", lapply(.SD, function(x) grepl(j, x))), .SDcols = searchCols]
    temp.content[i1, (searchStrings.names[i]) := paste(get(searchStrings.names[i]),j, sep = ", ")]
  }
}

for (i in searchStrings.names){
  temp.content[, (i) := gsub(paste0("None, "), "", get(i))]
}
for (i in searchStrings.names){
  temp.content[, (i) := gsub(paste0("No, "), "", get(i))]
}
for (i in searchStrings.names){
  temp.content[, (i) := gsub(paste0(i, ", "), "", get(i))]
}
temp.content[, author_keywords := str_replace_all(author_keywords, "\\|, ", "")]
temp.content[, author_keywords := str_replace_all(author_keywords, "\\|", ",")]

# construct metadata variable
DT <- data.table(
  variable_name = character(),
  variable_value = character()
)

metadata.querystring <- list("query", query)
metadata.datestring <- list("date queried", as.character(Sys.Date()))
metadata.recordCount <- list("reference count", total_results)
metadata.searchStringLabel <- list("search string names", "search string content")

DT <- rbind(DT, metadata.querystring)
DT <- rbind(DT, metadata.datestring)
DT <- rbind(DT, metadata.recordCount)
DT <- rbind(DT, metadata.searchStringLabel)

for (i in 1:length(searchStrings)) {
  newRow <- list(searchStrings.names[i], paste(get(searchStrings[i]), collapse = ", "))
  DT <- rbind(DT, newRow)
}

inDT <- temp.content
outName <- paste("Scopus", outFileContent, sep = "_")
cleanup(inDT, outName, destDir = "results", writeFiles = "xlsx")
