#' API WDS
#'
#' Information retrieval from WDS webservices
#'
#' Retrieve information from WDS data API in csv format.
#'
#' @return The \code{wdsAPI} function creates an URL with with the specified parameters, retrieves the zip file, extracts a csv file that is read into R and returned as data frame. \code{wdsDFtoXTS} converts the returned data frame to an xts object.
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords SOAP
#' @name wdsAPI
#' @export
#' @examples
#' \dontrun{
#' data.df <- wdsAPI(dataset = "2810027")
#'
#' ## convert to xts object, e.g. for use in \code{dygraphs} package
#' data.xts <- wdsDFtoXTS(data = data.df)
#' }

wdsAPI <- function(
    ## dataset=stop("'dataset' must be provided")
    dataset=stop("'dataset' must be provided"),
    query=FALSE,
    curl=NULL,
    ...
) {

    headerFields =
        c(Accept = "text/xml",
          'Content-Type' = "text/xml; charset=iso-8859-1",
          SOAPAction = "fullTableDownloadCSV"
          )

    body <- paste0(
        '<?xml version="1.0" encoding="iso-8859-1"?>',
        '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ws="http://ws.cansimws.statcan.gc.ca">',
        '<soapenv:Header/>',
        '<soapenv:Body>',
        '<ws:fullTableDownloadCSV>',
        '<!--Zero or more repetitions:-->',
        '<csvFileDownloadRequest>',
        ## '<arrayID>2810027</arrayID>',
        ## '<arrayID>', arrayID, '</arrayID>',
        '<arrayID>', dataset, '</arrayID>',
        '<!--Optional:-->',
        '<lang>ENG</lang>',
        '</csvFileDownloadRequest>',
        '</ws:fullTableDownloadCSV>',
        '</soapenv:Body>',
        '</soapenv:Envelope>'
    )

    if (is.null(curl)) curl <- RCurl::getCurlHandle()

    ## record stream
    stream = RCurl::basicTextGatherer()
    RCurl::curlPerform(url = "http://www8.statcan.gc.ca/CANSIMWebService/cansimWSService",
                httpheader = headerFields,
                postfields = body,
                writefunction = stream$update, # write stream
                curl = curl
                )
    tt <- stream$value()

    tt <- XML::xmlToList(tt)
    download.uri <- tt[["Body"]][["fullTableDownloadCSVResponse"]][["FileLocation"]][["url"]]

  if (query==TRUE) return(download.uri)
  ## download.uri = "http://data.statistics.gov.uk/ons/datasets/csv/CSV_QS208EW_2011WARDH_NAT_WD_REL_1.A.A.zip"

  tempfile <- tempfile(fileext = ".zip")
  ## include check: file already downloaded?
  ## download.file(url = download.uri, destfile = tempfile, ...)
  content <- RCurl::getBinaryURL(download.uri, curl = curl)
  writeBin(content, con = tempfile)

  tempdir <- tempdir()
  namefile <- unzip(zipfile = tempfile, list = TRUE)
  namefile <- namefile[["Name"]]
  csv.file <- namefile[tools::file_ext(namefile)=="csv"]

  unzip(zipfile = tempfile, exdir = tempdir)

  data <- read.csv(file.path(tempdir, csv.file), header = TRUE)
  for (file in list.files(tempdir)) unlink(file.path(tempdir, file))

  ## data.csv <- data
  ## data <- data.csv
  names(data) <- tolower(names(data))

  return(data)
}


#' @rdname wdsAPI
#' @param data a data frame created with \code{wdsAPI}
#' @export
wdsDFtoXTS <- function(
    data = stop("'data' must be provided")
) {

    names(data) <- sub("ref_date", "date", names(data))

    data.xts <-
        data %>%
            tidyr::unite(col = combine, -date, -value) %>%
                tidyr::spread(key = combine, value = value)

    data.xts$date <- as.Date(sapply(data.xts$date, wdsChangeDates))
    rownames(data.xts) <- data.xts$date

    data.xts <-
        data.xts %>%
        dplyr::select(-date) %>%
            xts::as.xts(dateFormat = "Date")

    return(data.xts)
}

#' @rdname wdsAPI
#' @param str a character string with WDS dates, e.g. \code{"2001"}

wdsChangeDates <- function(str) {

  ## str <- "2000"
  ## todo: add other frequencies
  ## if (stringr::str_detect(str, "K")) {
  ##   str <- stringr::str_replace(str, "K", "-")
  ##   str <- as.character(unname(zoo::as.Date(zoo::as.yearqtr(str))))
  ## } else if (stringr::str_detect(str, "M")) {
  ##   str <- stringr::str_replace(str, "M", "-")
  ##   str <- paste0(str, "-01")
  ## } else {
    str <- paste0(str, "-01-01")
  ## }

  return(str)
}
