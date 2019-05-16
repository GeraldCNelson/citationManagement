source("R/citsManagementFunctions.R")

# get Scopus api key
get_api_key(api_key = Sys.getenv('Elsevier_API'))
if (!have_api_key()) stop("Missing api key")

# get WOS api key
wosliteKey <- Sys.getenv("wosliteKey")

# get list of queries
queries <- read_excel("data-raw/queries.xlsx")

# year range
yearCoverage.scopus <- "> 2013"
yearCoverage.wok <- "= 2014-2019"

# assemble queries
queryRowNumber <- 16
rawQuery <- queries[queryRowNumber,2]
outFileName <- queries[queryRowNumber,1]
# rawQuery.scopus <- gsub('" ', '} ', rawQuery)
# rawQuery.scopus <- gsub('"', '{ ', rawQuery.scopus)
query.scopus <- sprintf('TITLE-ABS-KEY({climate change} AND %s) AND PUBYEAR %s', rawQuery, yearCoverage.scopus)
query.wok <- sprintf('TS=("climate change" AND %s) AND PY %s', rawQuery, yearCoverage.wok)
query.wok <- gsub('"',"'", query.wok )
queryResults.wok <- readinWOK(query = query.wok)
queryResults.wok[, eIssn := gsub("-", "", eIssn)]

queryResults.scopus <- readinSCOPUS(query = query.scopus)

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
  
  prepareSpreadsheet(queryResults.scopus, query.scopus, queryResults.wok = queryResults.wok.unique, query.wok, outName)
}

dois.combined <- unique(c(doi.wok, doi.scopus))
doi2bib(dois.combined, file = paste("results/", outFileName, "_", Sys.Date(),".bib"), quiet = TRUE)

# doi2bib(doi.common, file = paste("results/", outFileName, "_scopus.bib"), quiet = TRUE)
# doi2bib(doi.wok, file = paste("results/", outFileName, "_wok.bib"), quiet = TRUE)
