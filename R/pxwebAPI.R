#' API PX-Web
#'
#' Information retrieval from PX-Web webservices
#'
#' Retrieve information from PX-Web data APIs.
#'
#' @return Convert data frame retrieved by \code{get_pxweb_data} into xts object.
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords PC-Axis
#' @name pxwebAPI
#' @export
#' @examples
#' dims_list <- list(SNI2007 = c('*'),
#'                   Transaktionspost = c('*'),
#'                   ContentsCode = c('*'),
#'                   Tid = c('*'))
#' req.uri <- "http://api.scb.se/OV0104/v1/doris/en/ssd"
#' data.df <- pxweb::get_pxweb_data(url = req.uri, dims = dims_list, clean = TRUE)
#'
#' ## convert to xts object, e.g. for use in \code{dygraphs} package
#' data.xts <- pxwebDFtoXTS(data = data.df)

#' @rdname pxwebAPI
#' @param data a data frame created with \code{get_pxweb_data}
pxwebDFtoXTS <- function(
    data = stop("'data' must be provided")
) {

    names(data) <- sub("year", "date", names(data))
    names(data) <- sub("quarter", "date", names(data))
    names(data) <- sub("month", "date", names(data))

    data.xts <-
        data %>%
            tidyr::unite(col = combine, -date, -values) %>%
                tidyr::spread(key = combine, value = values)

    data.xts$date <- as.Date(sapply(data.xts$date, pxwebChangeDates))
    rownames(data.xts) <- data.xts$date

    data.xts <-
        data.xts %>%
        dplyr::select(-date) %>%
            xts::as.xts(dateFormate = "Date")

    return(data.xts)
}

#' @rdname pxwebAPI
#' @param str a character string with PXWEB dates, e.g. \code{"2000"}, \code{"2000K1"}, \code{"2000M01" }
pxwebChangeDates <- function(str) {

  ## str <- "2000K2"
  if (stringr::str_detect(str, "K")) {
    str <- stringr::str_replace(str, "K", "-")
    str <- as.character(unname(zoo::as.Date(zoo::as.yearqtr(str))))
  } else if (stringr::str_detect(str, "M")) {
    str <- stringr::str_replace(str, "M", "-")
    str <- paste0(str, "-01")
  } else {
    str <- paste0(str, "-01-01")
  }

  return(str)
}
