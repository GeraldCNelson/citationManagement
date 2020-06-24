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


#'
#' Convert a digital object identifier (DOI) string into a bibtex entry using
#' the webservice \url{http://www.doi2bib.org}.
#' 
testURLs <- c("10.3390/su11030714",  "10.1007/s13351-015-4083-1", "10.1007/s40003-014-0103-0", "10.1002/2017EF000687", "10.3389/fpls.2018.00224",  "10.3390/su1101928", "10.1002/2017EF000687" , "10.3389/fpls.2018.00224", "10.1016/j.agee.2019.04.001", "10.3934/AGRFOOD.2018.4.406", "10.3390/cli7110125", "10.1007/s00484-018-1623-2", "10.4081/ija.2017.897", "10.3969/j.issn.1002-6819.2015.02.002", "10.1175/JAMC-D-16-0258.1")#, "10.1007/s40995-019-00781-7", "10.1016/j.wace.2019.100240", "10.1371/journal.pone.0185690", "10.1002/pa.2040",  "10.17582/journal.sja/2019/35.1.284.292", "10.1016/j.ejrs.2016.12.007", "10.13930/j.cnki.cjea.180745", "10.5846/stxb201404100697", "10.1007/s10584-015-1579-8", "10.11975/j.issn.1002-6819.2015.18.021", "10.1007/s12665-016-6030-6", "10.1016/j.fcr.2016.02.008", "10.1088/1748-9326/aac4b1", "10.1007/s00704-016-1895-6", "10.1016/j.agrformet.2018.07.029", "10.1016/j.agrformet.2019.107778", "10.15835/nbha44210533", "10.1038/s41467-020-15076-4", "10.1038/ncomms12608", "10.1007/s10113-013-0455-1", "10.1371/journal.pone.0217148", "10.1201/b17684", "10.1016/j.scitotenv.2014.08.035", "10.17582/journal.sja/2019/35.3.880.889", "10.1017/S0021859616000149", "10.1007/s00704-014-1343-4", "10.1007/s00484-017-1336-y", "10.1007/978-4-431-54774-7_3", "10.1038/nplants.2014.26", "10.1016/j.agsy.2018.01.010",  "10.1016/j.ecolind.2017.12.014", "10.1111/gcb.14381", "10.1016/j.agwat.2018.10.030", "10.1016/j.scitotenv.2019.01.204", "10.1007/s11707-015-0527-2", "10.1016/j.eja.2013.09.020", "10.1111/cjag.12149", "10.1088/1748-9326/10/8/084003", "10.1002/joc.5818", "10.1007/s00704-014-1179-y", "10.1016/j.scitotenv.2014.12.004", "10.1016/j.ecolind.2016.12.013", "10.1016/j.fcr.2018.12.005", "10.1038/s41467-018-06525-2", "10.5846/stxb201306041314", "10.1016/j.agee.2015.11.010", "10.1016/j.jeem.2016.12.001", "10.1016/j.agee.2014.03.006", "10.13577/j.jnd.2014.0625", "10.1073/pnas.1415181112", "10.1007/s00484-015-1002-1", "10.3864/j.issn.0578-1752.2017.13.007", "10.3354/cr01307", "10.18697/ajfand.76.15685", "10.1111/gcb.12660", "10.3390/su9060970", "10.1002/joc.5015", "10.1038/s41598-019-49167-0", "10.3390/w11020343", "10.3390/su9010041", "10.1016/j.agee.2020.106888", "10.1093/jxb/erv163", "10.1016/j.agrformet.2018.11.004", "10.1175/JAMC-D-18-0174.1", "10.1007/s10584-016-1878-8", "10.1007/s10584-020-02666-w", "10.1016/j.eja.2015.08.003", "10.1175/WCAS-D-19-0026.1", "10.1111/geb.12120", "10.1007/s10584-014-1128-x", "10.13292/j.1000-4890.201703.009", "10.1016/j.agrformet.2015.10.004", "10.1038/ncomms4712",  "10.1017/S0021859614001154", "10.5073/JFK.2016.09.02", "10.1016/j.landusepol.2018.04.018", "10.1007/s10113-013-0418-6", "10.1016/j.atmosenv.2017.09.002", "10.1007/s10584-015-1350-1", "10.1016/j.agrformet.2014.09.011", "10.1016/j.eja.2015.03.006", "10.1002/joc.5492", "10.1038/srep30571", "10.1109/JSTARS.2014.2325898", "10.1088/1748-9326/10/2/024012", "10.1016/j.agrformet.2017.03.019", "10.1007/s10584-019-02622-3", "10.1016/j.still.2019.01.012", "10.1071/CP18117", "10.3390/rs11080981", "10.1016/j.fcr.2017.09.014", "10.1111/gcb.13719", "10.1071/CP19155", "10.1002/2017GL073606", "10.1016/j.scitotenv.2019.135250", "10.1007/s10661-017-6256-0", "10.1016/j.jhydrol.2015.04.071", "10.1071/CP19294", "10.17582/journal.sja/2019/35.1.36.42", "10.1016/j.agrformet.2018.09.019", "10.13930/j.cnki.cjea.180983", "10.1016/j.agrformet.2014.01.013", "10.1016/j.agrformet.2016.06.007", "10.3969/j.issn.1002-6819.2014.07.015", "10.3969/j.issn.1002-6819.2015.06.021", "10.1007/s13351-017-6137-z", "10.11821/dlxb201504006", "10.13292 / j.1000-4890.201903.025", "10.1016/j.jaridenv.2020.104164", "10.13930/j.cnki.cjea.170843", "10.1007/s00477-016-1278-7", "10.1016/j.agrformet.2019.05.019", "10.1007/s11027-013-9531-6", "10.1111/jac.12115", "10.1080/03650340.2018.1520979", "10.5846/stxb201505070945", "10.3354/cr01322",
# "10.1073/pnas.1701762114", "10.1111/agec.12429", "10.1016/j.agsy.2014.11.001", "10.1073/pnas.1409606112", "10.1038/s41559-018-0793-y", "10.15389/agrobiology.2015.2.137eng", "10.3390/cli4010013", "10.1016/j.scitotenv.2020.137893" ,"10.1007/s40009-016-0485-6", "10.1002/2016EF000525", "10.1007/s10584-015-1497-9", "10.1007/s10113-015-0920-0", "10.1007/s10584-015-1363-9", "10.1007/s12571-014-0370-4", "10.1016/j.envpol.2019.07.114", "10.1016/j.agsy.2017.12.009", "10.1088/1748-9326/ab5ebb",
# "10.1016/j.agrformet.2017.02.033", "10.1002/joc.3704")

outFile.bib <- paste0("test", "_", Sys.Date(),".bib")
latexCharLookup <- as.data.table(read_excel("data-raw/latexCodeLookup.xlsx"))

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

doiToBibtex(testURLs, outFile.bib)

bibout <- 

bibout <- gsub("{\textquotesingle}", "'", bibout)
# replace latex code
for (i in 1:length(latexCharLookup)) {
  gsubIn <- paste0("{", latexCharLookup[i, latexCode], "}")
  gsubOut <- latexCharLookup[i, symbol]
bibout <- gsub(gsubIn, gsubOut, bibout, perl = TRUE)
}


write.csv(badDOIs, "badDOIs.csv")