# Read in citations unique DOI files and produce .bib output
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
library(data.table)

#'
#' Convert a digital object identifier (DOI) string into a bibtex entry using
#' the webservice \url{http://www.doi2bib.org}.
#' 
testURLs <- c("10.3390/su11030714",  "10.1007/s13351-015-4083-1", "10.1007/s40003-014-0103-0", "10.1002/2017EF000687", "10.3389/fpls.2018.00224",  "10.3390/su1101928", "10.1002/2017EF000687" , "10.3389/fpls.2018.00224", "10.1016/j.agee.2019.04.001", "10.3934/AGRFOOD.2018.4.406", "10.3390/cli7110125", "10.1007/s00484-018-1623-2", "10.4081/ija.2017.897", "10.3969/j.issn.1002-6819.2015.02.002", "10.1175/JAMC-D-16-0258.1")
outFile.bib <- paste0("test", "_", Sys.Date(),".bib")

latexCharLookup <- as.data.table(readxl::read_excel("data-raw/latexCodeLookup.xlsx"))

doiToBibtex <- function(doiList, filename) {
  #store all the bibtex entries to a file
  require(curl)
  file.create(filename)
  #  urls.combined <- as.list(paste0("https://doi.org/", doilist$dois.combined))
  badDOIs <<- character(0)
  for (j in 1:length(doiList)) {

    tempDOI <- doiList[j]
    print(tempDOI)
    #    print(paste0("doi num :", j, ", doi: ", tempDOI))
    bibtemp <- createBibtexEntry(tempDOI)
    bibtemp <- gsub("{\\textquotesingle}", "'", bibtemp, fixed = TRUE)
    # replace latex code
    for (i in 1:nrow(latexCharLookup)) {
      gsubIn <- paste('\\', latexCharLookup[i, latexCode], sep ="")
      gsubOut <- latexCharLookup[i, symbol]
      print(paste0("gsubIn: ", gsubIn, " gsubOut: ", gsubOut))
      bibtemp <- gsub(gsubIn, gsubOut, bibtemp, fixed = TRUE)
    }
    print(bibtemp)
    write(bibtemp, file = filename, append = TRUE) # this appends the bibtex entry to the file filename
  }
}

createBibtexEntry <- function(doiIn) {
  # code originally from https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  out <- tryCatch(
    {
      h <- new_handle()
      handle_setheaders(h, "accept" = "application/x-bibtex")
      urlIn <- paste0("https://doi.org/", doiIn)
      temp <- paste0("curl(url = '", urlIn,"', handle = h)")
      
      con <- eval(parse(text = temp))
      bibtemp <- readLines(con, warn = FALSE)
      close(con)
      return(bibtemp)
    },
    error = function(e){
      message(paste0("An error occurred with DOI", doiIn, "\n"), e)
      badDOIs <<- c(badDOIs, doiIn)
    }
  )    
  return(out)
}

doiToBibtex(fsdois, outFile.bib)

bibout <- gsub("{\textquotesingle}", "'", bibout)
# replace latex code
for (i in 1:length(latexCharLookup)) {
  gsubIn <- paste0("{", latexCharLookup[i, latexCode], "}")
  gsubOut <- latexCharLookup[i, symbol]
bibout <- gsub(gsubIn, gsubOut, bibout, perl = TRUE)
}

write.csv(badDOIs, "badDOIs.csv")

# new code to try
pacman::p_load(curl,readr, tidyverse) # load required packages

# dois from food safety text
fsdois <- c("10.1016/j.funbio.2019.03.00", "10.1016/S2542-5196(19)30094-4", "10.1016/j.prevetmed.2016.11.01", "10.1371/journal.pone.0153237", "10.1007/s12237-018-0424-5", 
            "10.2903/j.efsa.2015.398", "10.2903/j.efsa.2017.469", "10.3109/1040841X.2014.972335", "10.1053/j.gastro.2017.04.005", "10.1016/j.scitotenv.2014.01.080", 
            "10.1017/S0029665118002896", "10.1017/S175173111700324X", "10.1002/jsfa.9768", "10.1111/evo.12833", "10.1371/journal.pone.0218956", 
            "10.1016/j.cub.2018.03.054", "10.1016/j.foodres.2017.03.020", "10.1007/s40003-015-0152-z", "10.1016/j.jspr.2019.101532", 
            "10.1038/s41467-019-10442-3", "10.3390/su10103673", "10.1126/sciadv.1501452", "10.1093/ajae/aaz027", "10.1371/journal.pone.0235597", 
            "10.1016/j.jspr.2019.101532", "10.4060/ca8185en", "10.1016/j.foodres.2010.02.001", "10.1016/S2542-5196(19)30094-4")

urls <- paste0("https://doi.org/", fsdois)

h <- new_handle()
walk(urls, ~ {
  curl(., handle = h) %>%
    readLines(warn = TRUE) %>%
    write(file = "test.bib", append = TRUE)
})

bib_sect5.11 <- read_delim("test_2020-10-18.bib", delim = "\n") # this will add break lines to your bib file you created
write.csv(bib_sect5.11, "bib_sect5.11.bib", row.names = FALSE)

# thornton refs
thorntonRefs <- read.delim("~/Documents/workspace/ISIMIPData/references/thorntonRefs.txt", header=FALSE)
dois <- gsub(".*https://doi.org/", "", thorntonRefs$V1)
dois_clean <- gsub("10.1186/s13021-017-0079-8.", "10.1186/s13021-017-0079-8", dois)
dois_clean <- gsub("10.1016/j.scitotenv.2020.141779.", "10.1016/j.scitotenv.2020.141779", dois_clean)
dois_clean <- gsub("HLPE. 2017. Nutrition and food systems. A report by the High Level Panel of Experts on Food Security and Nutrition of the Committee on World Food Security, Rome. http://www.fao.org/fileadmin/user_upload/hlpe/hlpe_documents/HLPE_Reports/HLPE-Report-12_EN.pdf", "", dois_clean)
dois_clean <- gsub("HLPE. 2017. Nutrition and food systems. A report by the High Level Panel of Experts on Food Security and Nutrition of the Committee on World Food Security, Rome. http://www.fao.org/fileadmin/user_upload/hlpe/hlpe_documents/HLPE_Reports/HLPE-Report-12_EN.pdf", "", dois_clean)
dois_clean <- gsub("Dalberg Advisors. (2019). The Digitalisation of African Agriculture Report 2018 - 2019, Proud Press, The Netherlands. https://www.cta.int/en/digitalisation-agriculture-africa", "", dois_clean)
dois_clean <- gsub("FAO (Food and Agricultural Organization), 2019. FAOstat food balance sheets. Available at: http://www.fao.org/faostat/en/#home [accessed 31 July 2020].", "", dois_clean)
# delete bad rows
dois_clean <- dois_clean[-c(11,13,21)]
doiToBibtex(dois_clean, "references/thorntonRefs")

#perennials refs -----
#perennialsRefs <- c("10.1016/j.fcr.2011.07.001","10.1007/s10584-017-1951-y","10.1007/s00484-017-1443-9","10.17660/eJHS.2019/84.3.2  ","10.1038/s41438-019-0141-7","10.1371/journal.pone.0006166","10.1016/j.resconrec.2020.105379","10.1038/s41598-020-64806-7","10.17660/ActaHortic.2017.1180.40","10.1016/j.jclepro.2019.01.237","10.3390/antiox9060478","10.1094/PDIS-04-20-0812-FE","10.17660/ActaHortic.2020.1268.37","10.1016/j.scienta.2017.01.006","10.17660%2fActaHortic.2017.1160.30","10.1007/s00484-018-1649-5","10.4141/P05-032","10.1016/j.agrformet.2011.03.010","10.1016/j.compag.2019.105067","10.1111/gcb.12130","10.1080/07352689.2012.696453","10.1126/science.1204531","10.1007/s00484-010-0352-y","10.1371/journal.pone.0006166","10.1007/s00484-013-0715-2","10.1016/S0304-4238(00)00173-4")

perennialsRefs <- as.vector(read.csv("perennialsDOIs.txt")$X1) # note $X1
perennialsRefs2 <- as.vector(read.csv("data/DOIsAidanAlan.txt")$X1)
doiToBibtex(perennialsRefs, "references/perennialsRefs.bib")
doiToBibtex(perennialsRefs2, "references/perennialsRefs2.bib")
# additional aquaculture refs
library(readxl)
addAquaRefs <- read_excel("~/Documents/Projects New/IPCCAR6/references/Additional Aquaculture supplementary references.xlsx")
addAquaRefs_dois <- addAquaRefs$DOI
doiToBibtex(addAquaRefs_dois, "references/addAquaRefs")


