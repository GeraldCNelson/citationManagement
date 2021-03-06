Citation download and management Readme
================

The goal of this project is to implement a process to prepare references
for assessment in a literature review or other literature assessment
activity. Two companies provide data on references – Clarivate Analytics
(<https://clarivate.com>) owns and/or manages the various databases
under the World of Knowledge name (WOS, BIOABS, BCI, CABI, CCC, DRCI,
DIIDW, FSTA, KJD, MEDLINE, RSCI, SCIELO, ZOOREC) and Elsevier owns the
SCOPUS database (<https://www.scopus.com>). Academic and other
institutions license access to these data bases at varying levels of
detail.

Each allows the user to construct queries that include search terms
(climate change, crop, etc) and constrain the searches in a variety of
ways (AND, OR, PY = 2014-2019). The queries are similar enough that one
basic query structure can be used, with modification, for both
databases. Both allow the user to construct a general query and then use
a second or third query on results from the general query.

While there is significant overlap between the two, there are also
significant numbers of references that only show up in one or the other.

The basic workflow is to read in a list of queries from a spreadsheet
(queries.wxls) in the data-raw directory, access the WOK data base to
determine how many records are available, limit the download process to
those queries with less than 2,000 records (currently), create a SCOPUS
query, download results from both WOK and SCOPUS, delete references in
the WOK data that are also in SCOPUS based on a comparison of DOI and
EISSN values), produce a spreadsheet that has metadata for the process
and the reference data with some added subsetting columns, and create a
.bib file from the references with DOIs.

Access to either database requires either an API key, a request from an
institutional IP address or both. Please email
<nelson.gerald.c@gmail.com> for a discussion of how to set this up for
the access your institution has.
