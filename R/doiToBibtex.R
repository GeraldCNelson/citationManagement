# Read in citatations unique DOI files and produce .bib output
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
library(doParallel) #Foreach Parallel Adaptor 
library(foreach) #Provides foreach looping construct

useCores <- parallel::detectCores() - 3
libList <- c("curl")
varList <- "doisIn"
cl <- clusterSetup(varList, libList, useCores)

# get rid of old .bib files
# unlink("results/dois/*.bib")
doisIn <- list.files("results/dois")
x <- foreach(i = doisIn) %dopar% {
# for (i in doisIn) {
  print(paste0("Working on : ", i, " pid: ", Sys.getpid(), " systime: ", Sys.time()))
  
  fileshortname <- strsplit(i, split = "_")[[1]][2]
  datestamp <- gsub(".csv", "", strsplit(i, split = "_")[[1]][3])
  outFile <- paste0("results/dois/", fileshortname,"_", datestamp, ".bib")
  doiList <- read.csv(paste0("results/dois/", i),  stringsAsFactors = FALSE)
  doiToBibtex(doiList = doiList, filename = outFile)
}
stopCluster(cl)

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

#' setGeneric(
#'   "doi2bib",
#'   function(..., file, append = TRUE, quiet = FALSE) {
#'     standardGeneric("doi2bib")
#'   },
#'   signature = signature("...")
#' )
#' 
#' replace_citekeys <-
#'   function(refs, nms) {
#'     setNames(
#'       mapply(
#'         function(ref, nm) {
#'           ifelse(
#'             nchar(nm) < 1,
#'             ref,
#'             sub("([^\\{]+\\{)[^,]+", paste0("\\1", nm), ref)
#'           )
#'         },
#'         refs,
#'         nms,
#'         SIMPLIFY = FALSE
#'       ),
#'       nms
#'     )
#'   }
#' 
#' refs_to_file <-
#'   function(refs, file, append) {
#'     cat(paste(refs, collapse = "\n"), file = file, append = append)
#'   }
#' 
#' get_doi <-
#'   function(doi) {
#'     content(
#'       GET(
#'         url    = "http://www.doi2bib.org/",
#'         config = accept("application/x-bibtex"),
#'         path   = "doi2bib",
#'         query  = list(id = doi)
#'       ),
#'       as = "text",
#'       encoding = "UTF-8"
#'     )
#'   }
#' 
#' get_isbn <-
#'   function(isbn)
#'     xml_text(
#'       xml_find_all(
#'         content(
#'           GET(
#'             url    = "http://lead.to/",
#'             path   = "amazon/en",
#'             query  = list(key = isbn, op = "bt")
#'           ),
#'           as =  "parsed",
#'           encoding = "UTF-8"
#'         ),
#'         "//div[contains(@class,'lef3em')]"
#'       )
#'     )
#' 
#' get_identifier <- function(id) {
#'   if (grepl("/", id, fixed = TRUE)) get_doi(id)
#'   else get_isbn(id)
#' }
#' 
#' #'@describeIn doi2bib Convert DOI to bibtex
#' setMethod(
#'   "doi2bib",
#'   "character",
#'   function(..., file, append, quiet) {
#'     
#'     stopifnot(missing(file) || is.character(file))
#'     stopifnot(is.logical(quiet))
#'     stopifnot(has_connection())
#'     
#'     dois <- c(...)
#'     
#'     refs <- lapply(dois, get_identifier)
#'     
#'     nms <- names(dois)
#'     
#'     if (!is.null(nms)) {
#'       refs <- replace_citekeys(refs, nms)
#'     }
#'     
#'     if (!quiet) message(paste(refs, collapse = "\n"))
#'     
#'     if (!missing(file)) {
#'       refs_to_file(refs, file, append)
#'     }
#'     
#'     invisible(refs)
#'     
#'   }
#' )
#' 
