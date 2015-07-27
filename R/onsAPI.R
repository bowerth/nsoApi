#' API Destatis Genesis
#'
#' Information retrieval from Destatis Genesis webservices
#'
#' Retrieve information from Destatis Genesis \code{quader} in linearised XML format.
#' The retrieval function requires a premium login https://www-genesis.destatis.de/genesis/online
#'
#' @param method
#' @param context
#' @param api.key
#' @param curl optional, \code{CURL} handle created with \code{RCurl::getCurlHandle()}.
#'
#' @return The main function creates an URL with with the specified parameters, retrieves the zip file, extracts a csv file that is read into R and returned as data frame. Additional functions convert the returned data frame to an xts objects.
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords API XML
#' @export
#' @examples
#' onsAPI(method = "contexts", api.key = api.key)
#' onsAPI(method = "collections", context = "Economic", api.key = api.key)

onsAPI <- function(
    method = "collections",
    context = "Economic",
    api.key = stop("'api.key' must be provided")
) {

    base.url <- "http://data.ons.gov.uk/ons/api/data"

    if (method == "contexts") {
        query <- "contexts.json?apikey="
    } else {
        query <- paste0(method, '.json?context=', context, '&apikey=')
    }

    req.uri <- file.path(base.url, paste0(query, api.key))

    tt <- RCurl::getURL(req.uri)
    list <- jsonlite::fromJSON(tt)

    if (list$ons$node$name=="Contexts") {
        return(list$ons$contextList$statisticalContext)
    } else if (list$ons$node$name=="Collections") {
        return(list$ons$collectionList$collection[c("id", "names", "description")])
    }

}


#' @rdname onsAPI
#' @param api.param a list with parameters used to construct the query, see examples.
#' @param dataset the dataset to download.
#' @param query logical, return the query containing the download link.
#' @export
#' @examples
#' ## Within a group there is a series for each combination of dimension items in the segment. In a time series dataset there will be several observations within a series, but in the case of Census data there is only one time (Census Day 2011).
#' api.param <- list(context = "Census",
#'                   geog = "2011WARDH",
#'                   totals = "false",
#'                   apikey = "xPuqnMzZ01")
#' data.QS208EW <- onsAPI(api.param = api.param, dataset = "QS208EW")
#' data.QS104EW <- onsAPI(api.param = api.param, dataset = "QS104EW")

onsCsvData <- function(
  api.param = stop("'api.param' must be provided"),
  dataset = stop("'dataset' must be provided"),
  query = FALSE,
  curl = NULL,
  ...) {

  ## ... additional parameters passed to download.file(), e.g method = "auto"

  api.param.char <- paste(names(api.param), unlist(api.param), sep = "=")
  api.param.str <- paste(api.param.char, collapse = "&")

  req.uri <- "http://data.ons.gov.uk/ons/api/data/dataset"
  req.uri <- file.path(req.uri, dataset, "dwn.csv")
  req.uri <- paste0(req.uri, "?", api.param.str)

  if (is.null(curl)) curl <- RCurl::getCurlHandle()

  if (query==TRUE) return(download.uri)

  tt <- RCurl::getURL(req.uri, curl = curl)

  dl.list <- XML::xmlToList(tt)

  download.uri <- dl.list$documents$document$href
  download.uri <- sub("/slice", "", download.uri)
  download.uri <- sub("_EN", "", download.uri)

  tempfile <- tempfile(fileext = ".zip")
  download.file(url = download.uri, destfile = tempfile, ...)

  tempdir <- tempdir()
  namefile <- unzip(zipfile = tempfile, list = TRUE)
  namefile <- namefile[["Name"]]
  csv.file <- namefile[tools::file_ext(namefile)=="csv"]

  unzip(zipfile = tempfile, exdir = tempdir)

  data <- read.csv(file.path(tempdir, csv.file), header = TRUE, skip = 7)

  ## data.csv <- data
  data <- data.csv
  names(data) <- gsub("[.]+", "_", tolower(names(data)))
  drop.col <- c("geographic_area")
  data <- subset(data, select = names(data)[!names(data)%in%drop.col])
  ## names(data)

  for (file in list.files(tempdir)) unlink(file.path(tempdir, file))

  return(data)
}

#' @rdname onsAPI
#' @param data a data frame created with \code{onsAPI}
#' @export
onsDFtoXTS <- function(
    data = stop("'data' must be provided")
) {

    data <- subset(data, select = names(data)[!names(data)%in%c("K", "QUALITAET", "GESPERRT", "WERT-VERFAELSCHT")])

    names(data) <- sub("ZI-WERT", "ZEIT", names(data))
    pivot.formula <- formula(paste("ZEIT ~", gsub(", ", " + ", toString(names(data)[!names(data)%in%c("ZEIT", "WERT")]))))
    data.d <- reshape2::dcast(data, pivot.formula, value.var = "WERT")

    rownames(data.d) <- paste0(as.character(data.d$ZEIT), '-01-01')
    ## data.d <- data.d[, -1]
    data.d <- subset(data.d, select = names(data.d)[names(data.d)!="ZEIT"])

    data.xts <- xts::as.xts(data.d, dateFormate = "Date")

    return(data.xts)
}

