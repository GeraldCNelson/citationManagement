library(readr)
library(data.table)
library("DBI")
library("odbc")
library("rscopus")

con <- DBI::dbConnect(odbc::odbc(), "scopus", timeout = 10)
con <- dbConnect(odbc::odbc(), "cc_fs", timeout = 10)
temp <- dbReadTable(con, "climate_change_consumption")
temp <- dbReadTable(con, "cc_fs_scopus")
dbListFields(con, "climate_change_consumption")

# consumption search terms web of science
# food suppl* [tiab] OR food securit* [tiab] OR food insecurit* [tiab] OR food access* [tiab] OR food afford* [tiab] OR food insecure* [tiab] OR 
# food sufficien* [tiab] OR food insufficien* [tiab] OR food choice* [tiab] OR food availabili* [tiab] OR food intake [tiab]  OR food utilization* [tiab] OR 
# food stability [tiab] OR food quality* [tiab] OR food poverty* [tiab]

# https://rdrr.io/cran/rscopus/man/scopus_search.html

get_api_key()
have_api_key()

scopusQuery <- "climate+change AND  PUBYEAR > 2013 AND food*"
scopusQuery <- 'TITLE-ABS-KEY("climate change" AND livestock*)  AND PUBYEAR > 2013'
scopusQuery <- 'TITLE-ABS-KEY("climate change" or "climate warming" or "global change" AND livestock*)  AND PUBYEAR > 2013'

scopusQuery <- 'TITLE-ABS-KEY("climate change" AND food* AND waste) AND PUBYEAR > 2013'
scopusQuery <- 'TITLE-ABS-KEY("climate change" AND trade*) AND PUBYEAR > 2013'
scopusQuery <- 'TITLE-ABS-KEY("climate change" AND "food processing") AND PUBYEAR > 2013'
scopusQuery <- 'TITLE-ABS-KEY("climate change" AND "food *security") AND PUBYEAR > 2013'

keepListCol.content <- c("dc:title","dc:creator","prism:publicationName", # removed "prism:url","dc:identifier","eid",
                         "prism:eIssn","prism:volume","prism:issueIdentifier","prism:coverDate", "prism:pageRange",
                         "prism:doi","dc:description","citedby-count","prism:aggregationType",
                         "subtypeDescription", "authkeywords")
keepListCol.content.newNames <- c("title","firstAuthor","publicationName", # removed "url","identifier","eid",
                         "eIssn","volume","issue","date", "pageRange",
                         "doi","abstract","citations","pubType",
                         "refKind", "authorKeyWords")

keepListCol.author <- c("authorid", "authname", "surname")

res = scopus_search(query = scopusQuery, max_count = 1, count = 1,  start = 5, verbose = TRUE, view = c( "COMPLETE"))
df <- gen_entries_to_df(res$entries)
content <- as.data.table(df$df)
content[, setdiff(names(content), c(keepListCol.content)) := NULL]

authorInfo <- as.data.table(df$author)
authorInfo[, setdiff(names(authorInfo), c(keepListCol.author)) := NULL]

total_results <- res$total_results

#initialize 
temp.content <- data.table(1)[,`:=`((keepListCol.content),NA)][,V1:=NULL][.0]
temp.content[, names(temp.content) := lapply(.SD, as.character), .SDcols=names(temp.content)]
temp.author <- authorInfo[0]

#for (i in 1:50) {
#for (i in 1:ceiling(total_results/24)) {
  
  res = scopus_search(query = scopusQuery, max_count = total_results, count = 25,  start = 0, verbose = FALSE, view = c( "COMPLETE"))
  df <- gen_entries_to_df(res$entries)
  dt.content <- as.data.table(df$df)
  dt.content[, setdiff(names(dt.content), keepListCol.content) := NULL]
  dt.authorInfo <- as.data.table(df$author)
  dt.authorInfo[, setdiff(names(dt.authorInfo), keepListCol.author) := NULL]
  #print(i)
   # print(length(dt.content))
   # print(length(temp.content))
   # temp.content <- rbind(temp.content, dt.content)
   # temp.author <- rbind(temp.author, dt.authorInfo)
   # 
#}
temp.content <- unique(dt.content)
temp.author <- unique(dt.authorInfo)
newNames <- gsub("prism:", "", names(temp.content))
setnames(temp.content, old = names(temp.content), new = newNames)

newNames <- gsub("dc:", "", names(temp.content))
setnames(temp.content, old = names(temp.content), new = newNames)

setnames(temp.content, old = c("description", "authkeywords"), new = c("abstract", "authorKeyWords"))

# now do some more sorting
# search strings
searchCols <- c("title", "abstract", "authorKeyWords") # what variables in the reference list should be searched for
searchStrings.RCP <- c("RCP",  "RCP4.5", "RCP8.5", "CMIP", "SRES") # entries in the added RCP column
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

temp.content[, RCP := "None"]
temp.content[, SSP := "None"]
temp.content[, region := "None"]
temp.content[, country := "None"]
temp.content[, keepRef := "No"]

setkey(temp.content)
for (i in searchStrings.RCP) {
  i1 <- temp.content[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  temp.content[i1, RCP := i]
}

for (i in searchStrings.SSP) {
  i1 <- temp.content[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  temp.content[i1, SSP := i]
}

for (i in searchStrings.regions) {
  i1 <- temp.content[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  temp.content[i1, region := i]
}

for (i in searchStrings.countries) {
  i1 <- temp.content[, Reduce("|", lapply(.SD, function(x) grepl(i, x))), .SDcols = searchCols]
  temp.content[i1, country := i]
}

saveRDS(temp.content, file = "results/foodSecurityscopusQuery.RDS")
saveRDS(temp.author, file = "results/foodSecurityauthorListForscopusQuery.RDS")
#temp <- readRDS(temp, file = "results/scopusQuery.RDS")
write.csv(temp.content, file = "results/foodSecurityscopusQuery.csv")

scopusQuery <- '"climate change" AND  PUBYEAR > 2014 AND food or food+*secur* or food+access or food+afford* or food+insecure* or
                      food+insufficien* or food+choice* or food+choice* or food+choice* or food+choice* or food+choice* or
food+availabili* or food+intake* or food+utilization* or food+stability* or food+quality* or food+poverty*'
