library(readr)
library(data.table)
library(handlr) # need to load into rstudio with 
library(bibtex)
library(RefManageR)

# from Scopus, easy search version
keepListCol.sco <-  c("Authors", "Title", "Year", "Source.title", "Volume", "Issue", "Page.start", "Page.end",
                      "Cited.by", "DOI", "Abstract", "Author.Keywords", "Index.Keywords", "Language.of.Original.Document",
                      "Document.Type", "Source")
# from WoS, maybe
keepListCol.wos <-  c("Author", "Title", "Publication.Year", "Publication.Title", "Volume", "Issue", "Pages",
                      "DOI", "Abstract.Note", "Language", "Item.Type", "Source")

# search strings
searchCols <- c("title", "abstract", "author.keywords") # what variables in the reference list should be searched for
searchStrings.RCP <- c("RCP",  "RCP4.5", "RCP8.5") # entries in the added RCP column
searchStrings.SSP <- c("SSP",  "SSP1", "SSP2", "SSP3", "SSP4","SSP5") # entries in the added SSP column
searchStrings.regions <- c("Latin America", "Central America", "Caribbean", 
                           "Europe", "Northern Europe", "Western Europe", "Southern Europe", "Eastern Europe", "Western Asia", "Middle East",
                           "Asia", "South Asia", "East Asia", "Central Asia", "Australia", "New Zealand",
                           "Southeast Asia") # entries in the added region column
searchStrings.countries <- c("Afghanistan", "Åland Islands", "Albania", "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", "Antarctica", "Antigua and Barbuda", 
                             "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", 
                             "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia, Plurinational State of", "Bonaire, Sint Eustatius and Saba", "Bosnia and Herzegovina", 
                             "Botswana", "Bouvet Island", "Brazil", "British Indian Ocean Territory", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi", 
                             "Cambodia", "Cameroon", "Canada", "Cape Verde", "Cayman Islands", "Central African Republic", "Chad", "Chile", "China", "Christmas Island", 
                             "Cocos (Keeling) Islands", "Colombia", "Comoros", "Congo", "Congo, The Democratic Republic of the", "Cook Islands", "Costa Rica", 
                             "Côte d'Ivoire", "Ivory Coast", "Croatia", "Cuba", "Curaçao", "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica", 
                             "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Falkland Islands", "Malvinas", 
                             "Faroe Islands", "Fiji", "Finland", "France", "French Guiana", "French Polynesia", "French Southern Territories", "Gabon", "Gambia", 
                             "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guernsey", 
                             "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard Island and McDonald Islands", "Holy See (Vatican City State)", 
                             "Honduras", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran, Islamic Republic of", "Iraq", "Ireland", "
                             Isle of Man", "Israel", "Italy", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea", 
                             "Korea, Democratic People's Republic of", "Korea, Republic of", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", 
                             "Latvia", "Lebanon", "Lesotho", "Liberia", "Libyan Arab Jamahiriya", "Liechtenstein", "Lithuania", "Luxembourg", "Macao", 
                             "Macedonia, The former Yugoslav Republic of", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", 
                             "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia", "Micronesia, Federated States of", 
                             "Moldova, Republic of", "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", 
                             "Nepal", "Netherlands", "Netherlands Antilles", "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfolk Island", 
                             "Northern Mariana Islands", "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", 
                             "Philippines", "Pitcairn", "Poland", "Portugal", "Puerto Rico", "Qatar", "Réunion", "Romania", "Russia", "Russian Federation", 
                             "Rwanda", "Saint Barthélemy", "Saint Helena, Ascension and Tristan da Cunha", "Saint Kitts and Nevis", "Saint Lucia", "
                             Saint Martin (French part)", "Saint Pierre and Miquelon", "Saint Vincent and The Grenadines", "Samoa", "San Marino", 
                             "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", 
                             "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Georgia and The South Sandwich Islands", 
                             "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Svalbard and Jan Mayen", "Swaziland", "Sweden", "Switzerland", 
                             "Syrian Arab Republic", "Taiwan, Province of China", "Tajikistan", "Tanzania, United Republic of", "Thailand", "Timor-Leste", 
                             "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", 
                             "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "United States Minor Outlying Islands", "Uruguay", 
                             "Uzbekistan", "Vanuatu", "Venezuela, Bolivarian Republic of", "Viet Nam", "Virgin Islands, British", "Virgin Islands, U.S.", 
                             "Wallis and Futuna", "Western Sahara", "Yemen", "Zambia", "Zimbabwe")

# put the names of the files to search for here
scopusFile <- "cc_fs_2000.csv" # "consumption_14_19.csv" #"livestock.csv"
  
x <- HandlrClient$new(x = z)
x$as_df() # empty data.frame
x$read()
x$as_df() # data.frame with citation data

# one citation
z <- system.file('extdata/exporttest.bib', package = "handlr")
test <- bibtex_reader("data-raw/thornton_savedrecs 4 terms.txt")

x <- handlr::HandlrClient$new(x = 'data-raw/Thornton4Phrasesexportlist.ris')
x$read("ris")


handl_to_df(x)

res2 <- bibtex_reader(x = z)
handl_to_df(res2)
x <- handlr::HandlrClient$new(x = 'data-raw/foodProcessing.bib')
x$read("bibtex")
res2 <- bibtex_reader(x = 'data-raw/consumption_14_19.bib')
handl_to_df(res2)

outFileName <- paste0("results/",  "cc_fs", ".csv")

# This function does two things. First it adds the new columns (currentl RCP, SSP, region, and country), puts in default values of None and then searches through
cleanCitations <- function(scopusFile = scopusFile, WoSFile, outFileName, keepListCol.sco, keepListCol.wos) {
  # From Scopus
  if (!missing(scopusFile)) {
    
    scopusFileName <- paste0("data-raw/", scopusFile)
    referenceList.sco <- as.data.table(read.csv(scopusFileName, stringsAsFactors=FALSE, na.strings = ""))
    referenceList.sco[, Source := "Scopus"]
    setnames(referenceList.sco, old = names(referenceList.sco), new = make.names(names(referenceList.sco)))
    referenceList.sco <- referenceList.sco[, keepListCol.sco, with = FALSE]
    setnames(referenceList.sco, old = keepListCol.sco, 
             new = c("authors", "title", "year", "source.title", "volume", "issue", "page.start", "page.end",
                     "citations", "doi", "abstract", "author.keywords", "index.keywords", "language",
                     "document.type", "source"))
    doi.referenceList.sco <- sort(paste0(scopusFile, "$doi"))
  }
  
  # from Web of Science
  if (!missing(WoSFile)) {
    WoSFileName <- paste0("data-raw/", WoSFile)
    referenceList.wos <- as.data.table(read.csv("data-raw/wos big cc ls 2014-2019.csv", stringsAsFactors=FALSE, na.strings = ""))
    referenceList.wos[, Source := "WoS"]
    referenceList.wos <- referenceList.wos[, keepListCol.wos, with = FALSE]
    setnames(referenceList.wos, old = keepListCol.wos,
             new = c("authors", "title", "year", "source.title", "volume", "issue", "pages", "doi", 
                     "abstract", "language", "document.type", "source"))
    referenceList.wos[, c("page.start", "page.end") := tstrsplit(pages, "-", fixed=TRUE)]
    referenceList.wos[, pages := NULL][, page.start := as.numeric(page.start)][, page.end := as.numeric(page.end)]
    doi.referenceList.wos <- sort(paste0(WoSFile, "$doi"))
  }
  
  # manage DOIs. remove all the references in the WoS file that are also in the Scopus file. Because Scopus download has more information. 
  
  if (!missing(scopusFile & !missing(WoSfile))) { 
    doi.scoOnly <- doi.referenceList.sco[!doi.referenceList.wos %in% doi.referenceList.sco]
    
    # remove documents from the WoS source with identical dois in both sources.
    referenceList.wos <- referenceList.wos[!doi %in% doi.scoOnly,]
    referenceList <- merge(referenceList.sco, referenceList.wos)
  }
  # done with DOIs
  
  # manage for ISBNs for duplicates, consider doing later
  
  
  referenceList[, RCP := "None"]
  referenceList[, SSP := "None"]
  referenceList[, region := "None"]
  referenceList[, country := "None"]
  
  setkey(livestock)
  for (i in searchStrings.RCP) {
    i1 <- livestock[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
    livestock[i1, RCP := i]
  }
  
  for (i in searchStrings.SSP) {
    i1 <- livestock[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
    livestock[i1, SSP := i]
  }
  
  for (i in searchStrings.regions) {
    i1 <- livestock[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
    livestock[i1, region := i]
  }
  
  for (i in searchStrings.countries) {
    i1 <- livestock[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
    livestock[i1, country := i]
  }
}

# note: | means "or". 
gs1 <- "(?=.*Sustainable)|(?=.*SDG)"
gs2 <- "(?=.*Feed)|(?=.*pasture)|(?=.*forage)|(?=.*grass)"
gs3 <- "(?=.*Water)"
gs4 <- "(?=.*biodiversity)"
gs5 <- "(?=.*Disease)|(?=.*vector)|(?=.*pathog)|(?=.*zoono)"

searchTerms <- c(gs1, gs2, gs3, gs4, gs5)
for (i in searchTerms) {
  dt <- copy(livestock)
  grepString <- i
  
  # i1 <- dt[, lapply(.SD, function(x) grepl(eval(i), x)), .SDcols = searchCols]
  # livestock[i1, RCP := i]
  
  temp <- dt[grep(grepString, c(Abstract, Author.Keywords), perl = TRUE, ignore.case = TRUE),]
  write.csv(temp, file = paste0("results/"i, ".csv"))
}

i1 <- dt[, lapply(.SD, function(x) grepl("(?=.*Sustainable)|(?=.*SDG)", x)), .SDcols = searchCols]

# now do consumption
consumption14 <- as.data.table(read.csv("data-raw/consumption14.csv", stringsAsFactors=FALSE))
consumption16_15 <- as.data.table(read.csv("data-raw/consumption16_15.csv", stringsAsFactors=FALSE))
consumption17 <- as.data.table(read.csv("data-raw/consumption17.csv", stringsAsFactors=FALSE))
consumption19_18 <- as.data.table(read.csv("data-raw/consumption19_18.csv", stringsAsFactors=FALSE))
consumptionlist <- list(consumption14, consumption16_15, consumption17, consumption19_18)
consumption <- rbindlist(consumptionlist)
setnames(consumption, old = names(consumption), new = make.names(names(consumption)))

consumption[, RCP := "None"]
consumption[, SSP := "None"]
consumption[, region := "None"]
consumption[, country := "None"]

for (i in searchStrings.RCP) {
  i1 <- consumption[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  consumption[i1, RCP := i]
}

for (i in searchStrings.SSP) {
  i1 <- consumption[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  consumption[i1, SSP := i]
}

for (i in searchStrings.regions) {
  i1 <- consumption[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  consumption[i1, region := i]
}

for (i in searchStrings.countries) {
  i1 <- consumption[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  consumption[i1, country := i]
}

xxxxx

Heat stress AND (feed intake OR utilisation OR reproduct* OR health OR mortality)

Biodiversity

Landscape OR system

Culture* OR identi* OR gender* OR ethic*
  
  Sustainable development goal OR SDG

Observ* AND (Vulnerab* OR risk)

Mixed OR agropas* OR agrosilv* OR aquacu* OR agroforest* OR “artisa* fish*”
