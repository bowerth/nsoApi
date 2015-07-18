#' Query CBS OData API
#'
#' Retrieve information from CBS Open Data API
#'
#' Retrieve information from CBS Netherlands Statline Open Data API using JSON format.
#'
#' @param api an API address
#' @param DSD a datastructure definition identified by the triplet \code{[collection; country; indicator]}.
#' @param scheme an API scheme. Available scheme "data".
#' @param filter for scheme "data": a named list of filters passed to the API. The position of list items corresponds to the API filter dimensions. Each list item is either empty (no filter on dimension) or a character vector containing dimension members to be included in the results. Dimension members can be obtained from \code{scheme="codelist"} and a codelist item, e.g. "CL_ECO_ISIC4".
#' @param query logical to return OData http url only.
#' @param append append string to the dimension url.
#' @param curl optional, \code{CURL} handle created with \code{RCurl::getCurlHandle()}
#'
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords OData JSON
#' @seealso \code{https://github.com/object/Simple.OData.Client}
#' @import jsonlite RCurl reshape2
#' @export
#' @examples
#' api <- "http://opendata.cbs.nl/ODataApi/OData/"
#' DSD <- "82572ENG" # Input-Output: "83068ENG"
#'
#' cbsODataAPI(api=api, DSD=DSD, scheme = NULL)
#' str(cbsODataAPI(api=api, DSD=DSD, scheme="SectorBranchesSIC2008", query=FALSE))
#' str(cbsODataAPI(api=api, DSD=DSD, scheme="TypedDataSet"))

cbsODataAPI <- function(api=stop("'api' must be provided"),
                      DSD=stop("'DSD' must be provided"),
                      scheme=NULL,
                      filter=NULL,
                      append=NULL,
                      query=FALSE,
                        curl=NULL)
{

  if (!is.null(scheme) > 0) {
    if (scheme == "getmember") {
      scheme <- "GetMember?DatasetCode="
    } else if (scheme == "getdimension") {
      scheme <- "GetDimension?DatasetCode="
    } else if (scheme == "getdata") {
      scheme <- file.path("ODataFeed", "OData")
    }
  }

  theurl <- file.path(api, DSD)
  if (!is.null(scheme)) theurl <- file.path(theurl, scheme)
  theurl <- gsub("//", "/", theurl)
  theurl <- sub("http:/", "http://", theurl)

  if (query==TRUE) return(theurl)

  if (is.null(curl)) curl <- RCurl::getCurlHandle()

  tt <- RCurl::getURL(theurl, .mapUnicode = FALSE, curl = curl)

  data.list2 <- jsonlite::fromJSON(txt = tt)
  return(data.list2$value)

}


#' @rdname cbsODataAPI
#' @param data a data frame created with \link{\code{cbsODataAPI}}
#' @export
cbsOdataDFtoXTS <- function(
    data = stop("'data' must be provided")
    ) {


    ## data <- read.csv(file.path(dlpath, "CBS_82572ENG.csv"))
    ## find positon of "Periods" in column names vectors
    varcol.periods <- match("Periods", names(data))
    gather.cols <- names(data)[(varcol.periods + 1):length(data)]
    id.cols <- names(data)[!names(data)%in%c("ID", "Periods", gather.cols)]

    ## data <-
    ##     data %>% dplyr::filter(SectorBranchesSIC2008 %in% c("300025"))

    data.xts <-
        data %>%
            dplyr::select(-ID) %>%
                tidyr::gather_(key_col = "TRANSACT", value_col = "VALUE", gather_cols = gather.cols) %>%
                    tidyr::unite_(col = "COMBINE", from = c(id.cols, "TRANSACT"), sep = "_") %>%
                        tidyr::spread(COMBINE, VALUE) # %>% head()

    rownames(data.xts) <- sub("JJ00", "-01-01", data.xts$Periods)
    data.xts <- data.xts[, !colnames(data.xts)%in%c("Periods")]

    data.xts <- as.xts(data.xts, dateFormat = "Date")

    return(data.xts)

}
