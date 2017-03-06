#' API Bureau of Economic Analysis
#'
#' Connect to BEA datasets
#'
#' These functions allow connecting to BEA time series in JSON format with a valid user ID obtained from http://www.bea.gov/API/signup/index.cfm
#'
#' @param api.param a list of parameters specific to the API
#' @param curl optional, \code{CURL} handle created with \code{RCurl::getCurlHandle()}
#' @param query the API query
#' @param raw return unparsed API response
#'
#' @return The function creates an URL with with the specified
#' parameters, retrieves the JSON string and transforms to R list
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords JSON
#' @export
#' @examples
#' \dontrun{
#' curl <- RCurl::getCurlHandle()
#' api.param <- list(USERID = "api.key",
#'                   METHOD = "GETDATASETLIST",
#'                   RESULTFORMAT = "JSON")
#' List <- beaAPI(api.param = api.param, curl = curl)
#' df <- beaJSONtoDF(List = List, third = 1)
#' }


beaAPI <- function(api.param = stop("'api.param' must be specified"),
                   curl = NULL,
                   query = FALSE,
                   raw = FALSE) {
    ##
    api.url <- "https://www.bea.gov/api/data/?"
    ##
    req.uri <- api.url
    for (i in seq(along = api.param)) {
        if (length(api.param[[i]]) > 0) {
            req.uri <- paste0(req.uri, '&', names(api.param[i]), '=', api.param[[i]])
        }
    }
    req.uri <- paste0(req.uri, "&")
    if (query) return(req.uri)
    ##
    if (is.null(curl)) curl <- RCurl::getCurlHandle()

  ## tt <- RCurl::getURL(req.uri, curl = curl)

  httr_get <- httr::GET(url = req.uri,
                        config = c(add_headers(Connection = "keep-alive"),
                                   accept_json()))
  tt <- httr::content(res, "text")

    if (raw) return(tt)
    ## if (tolower(api.param[["METHOD"]])=="getdata") {
    ##     if (tolower(api.param[["DATASETNAME"]])=="gdpbyindustry") {
    ##         require(stringr)
    ##         tt <- str_replace(tt, "[}][]][}][}][}]", "}]]}}}") # replace ] with ]] in }]}}}
    ##     }
    ## }
    tt <- stringr::str_replace(tt, '\"Industrial Buildings\"', 'Industrial Buildings')
    tt <- stringr::str_replace(tt, '\"Industrial Structures,\"', 'Industrial Structures,')
    ## filecon <- file(file.path(dlpath, "bea.json"))
    ## writeLines(text = tt, con = filecon)
    ## close(filecon)

    ## result.list <- rjson::fromJSON(tt)
    result.list <- jsonlite::fromJSON(tt)

    return(result.list)
}


## #' @rdname beaAPI
## #' @param List
## #' @param third
## #' @export
## beaJSONtoDF <- function(List=stop("'List' must be specified"),
##                          third = 1) {

##     temp <- lapply(List[[1]][[2]][[third]], function(x) data.frame(x))
##     df <- NULL
##     for (i in seq(along = temp)) {
##         df <- rbind(df, temp[[i]])
##     }
##     return(df)
## }


#' @rdname beaAPI
#' @param data a data frame created with \code{beaJSONtoDF}
#' @export
beaDFtoXTS <- function(
    data = stop("'data' must be provided")
) {

    distinct.var <- names(data)
    ## if ("TimePeriod"%in%names(data)) setnames(data, "TimePeriod", "Date") # NIPA
    if ("TimePeriod"%in%names(data)) names(data) <- sub("TimePeriod", "Date", names(data)) # NIPA, FixedAssets
  if ("Year"%in%names(data)) {
    ## names(data) <- sub("Year", "Date", names(data)) # GDPbyIndustry
    ## ## now containing "Year" and "Quarter"
    data[["Date"]] <- ifelse(data[["Frequency"]]=="Q", paste0(data[["Year"]], "Q", as.numeric(as.roman(data[["Quarter"]]))),
                             data[["Year"]])    
  }
    distinct.var <- distinct.var[!distinct.var%in%c("Date", "Year", "Quarter", "IndustrYDescription", "DataValue", "NoteRef")]
    ## data.plots <- data
    distinct.col <- data[, colnames(data)%in%distinct.var]
    distinct.col2 <- data.frame(apply(distinct.col, 2, function(x) as.character(x)), stringsAsFactors = FALSE)
    data$variable <-  apply(distinct.col2, 1, function(x) gsub(", ", ".", toString(x)))
    data$DataValue <- as.numeric(as.character(data$DataValue))

    data.d <- reshape2::dcast(data, Date ~ variable, value.var = "DataValue")

    ## data.d$Date <- as.numeric(as.character(data.d$Date))

    ## data.d$Date <- paste0(data.d$Date, '-01-01')
    data.d$Date <- as.Date(sapply(data.d$Date, nsoApi:::beaChangeDates))
    ## unique(data.d$Date)

    rownames(data.d) <- data.d$Date
    ## data.d <- data.d[, colnames(data.d)!="Date"]
    ## need data frame
    data.d <- subset(data.d, select = names(data.d)[names(data.d)!="Date"])

    data.xts <- xts::as.xts(data.d, dateFormat = 'Date')
    ## return(data.d)
    return(data.xts)

}


#' @rdname beaAPI
#' @param str a character string with BEA dates, e.g. \code{"2000"}, \code{"2000Q1"}, \code{"2000M01"}
beaChangeDates <- function(str) {

  ## str <- "2000Q2"
  if (stringr::str_detect(str, "Q")) {
    str <- stringr::str_replace(str, "Q", "-")
    str <- as.character(unname(zoo::as.Date(zoo::as.yearqtr(str)))) # same
  } else if (stringr::str_detect(str, "M")) {
    str <- stringr::str_replace(str, "M", "-")
    str <- paste0(str, "-01")
  } else { # year
    ## if (nchar(str)==7) str <- paste0(str, "-01")
    str <- paste0(str, "-01-01")
  }
  return(str)
}
