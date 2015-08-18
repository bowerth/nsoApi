#' Query Statistik Austria open.data
#'
#' Retrieve information from Statistik Austria open.data
#'
#' Retrieve information from Statistik Austria open.data using CSV format.
#'
#' @param DSD a dataset identifier.
#' @param query logical to return OData http url only.
#' @param curl optional, \code{CURL} handle created with \code{RCurl::getCurlHandle()}
#'
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords JSON
#' @seealso \code{http://http://data.statistik.gv.at/web}
#' @export
#' @examples
#' dataset <- "OGD_vgr001_VGRJahresR_1"
#' statatAPI(dataset = dataset)

statatAPI <- function(dataset=stop("'DSD' must be provided"),
                      query=FALSE,
                      curl=NULL)
{

    base_url <- "http://data.statistik.gv.at"
    req_url <- file.path(base_url, "ogd", paste0("json?dataset=", dataset))

    if (query==TRUE) return(req_url)

    if (is.null(curl)) curl <- RCurl::getCurlHandle()

    tt <- RCurl::getURL(req_url, curl = curl)
    data_list <- jsonlite::fromJSON(tt)

    nameurl <- data_list$resources$url
    nameurl <- nameurl[1] # only data
    ## nameurl <- nameurl[c(1:2)] # only data and header

    ## url <- nameurl[1]
    data_all <- NULL
    for (url in nameurl) {
        tempfile <- tempfile(fileext = ".csv")
        content <- RCurl::getBinaryURL(url = url, curl = curl)
        writeBin(content, con = tempfile)
        data <- read.csv(tempfile, sep = ";") # will replace "-" with "."
        data_all <- c(data_all, list(data))
    }
    names(data_all) <- sub(".csv", "", basename(nameurl))
    names(data_all) <- sub(paste0(names(data_all)[1], "_"), "", names(data_all))

    ## data_all$HEADER
    ## str(data_all)

    data <- data_all[[1]]
    names(data) <- gsub("[.]", "_", names(data)) # minus not detected

    ## h(data)
    ## dplyr::mutate(value = as.numeric(sub(",", ".", value))) %>%
    ## replace comma with period in values

    names(data)[1] <- "date"
    classification_cols <- names(data)[substr(names(data), 1, 1)=="C"]
    gather_cols <- names(data)[!names(data)%in%c("date", classification_cols)]

    data <-
    data %>%
        tidyr::gather_(key_col = "variable", value_col = "value", gather_cols = gather_cols) %>%
            dplyr::mutate(value = as.numeric(sub(",", ".", value)))

    return(data)

}

#' @rdname statatAPI
#' @param data a data frame created with \code{get_pxweb_data}
#' @export
statatDFtoXTS <- function(
    data = stop("'data' must be provided")
) {

    ## names(data) <- tolower(names(data))

    data.xts <-
        data %>%
            ## tidyr::gather_(key_col = "variable", value_col = "value", gather_cols = gather_cols) %>%
            ##     dplyr::mutate(value = as.numeric(sub(",", ".", value))) %>%
                    tidyr::unite(col = combine, -date, -value) %>%
                        tidyr::spread(key = combine, value = value)

    data.xts$date <- as.Date(sapply(data.xts$date, statatChangeDates))
    rownames(data.xts) <- data.xts$date

    data.xts <-
        data.xts %>%
        dplyr::select(-date) %>%
            xts::as.xts(dateFormat = "Date")

    dygraphs::dygraph(data.xts)

    return(data.xts)
}

#' @rdname statatAPI
#' @param str a character string with Statistik Austria open.data dates, e.g. \code{"A10-1976"}, \code{"197311"}, \code{"VPIZR-201101" }
statatChangeDates <- function(str) {

    ## str <- "A10-1976"
    ## str <- "197311"
    ## str <- "VPIZR-201101"

    if (stringr::str_detect(str, "-")) {
        str <- stringr::str_extract(str, "-.+")
        str <- sub("-", "", str)
    }

    if (nchar(str)==4) str <- paste0(str, "-01-01")
    if (nchar(str)==6) str <- paste(substr(str, 1, 4), substr(str, 5, 6), "01", sep = "-")

  return(str)
}
