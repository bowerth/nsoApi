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
#' @export
#' @examples
#' \dontrun{
#' api <- "https://opendata.cbs.nl/ODataApi/OData/"
#' DSD <- "82572ENG" # Input-Output: "83068ENG"
#'
#' cbsODataAPI(api=api, DSD=DSD, scheme = NULL)
#' str(cbsODataAPI(api=api, DSD=DSD, scheme="SectorBranchesSIC2008", query=FALSE))
#' str(cbsODataAPI(api=api, DSD=DSD, scheme="TypedDataSet"))
#' }

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
  theurl <- sub("https:/", "https://", theurl)

  ## filter <- list(Sex = c(3000, 1100))
  if (length(filter) > 0) {

      ## substring(Sex,0,4) eq'3000'
      ## filter_str <- paste0('substring(', names(filter)[1], ',0,', nchar(filter[[1]]), ') eq \'', filter[[1]], '\'', collapse = ' or ')
      filter_str <- paste0('substring(', names(filter)[1], ',0,', nchar(filter[[1]]), ')%20eq%20%27', filter[[1]], '%27', collapse = ' or ')
      filter_str <- paste0('?$filter=', filter_str)
      theurl <- paste0(theurl, filter_str)

  }

  if (query==TRUE) return(theurl)

  if (is.null(curl)) curl <- RCurl::getCurlHandle()

  ## theurl <- "https://opendata.cbs.nl/ODataApi/OData/82579ENG/UntypedDataSet?$filter=substring(Gender,0,4)%20eq%20%271100%27"
  tt <- RCurl::getURL(theurl, .mapUnicode = FALSE, curl = curl)

  data.list2 <- jsonlite::fromJSON(txt = tt)
  return(data.list2$value) # returns a data frame

}


#' @rdname cbsODataAPI
#' @param xml.list a character string returned from
#' @export
cbsOdataDFgather <- function(
    data = stop("'data' must be provided")
) {

    ## list.files(path = dlpath)
    ## data <- read.csv(file.path(dlpath, "CBS_83068ENG.csv"))
    ## h(data)
    ## data <- read.csv(file.path(dlpath, "CBS_82572ENG.csv"))
    ## find positon of "Periods" in column names vectors
    varcol.periods <- match("Periods", names(data))
    gather.cols <- names(data)[(varcol.periods + 1):length(data)]
    id.cols <- names(data)[!names(data)%in%c("ID", "Periods", gather.cols)]

    ## data <-
    ##     data %>% dplyr::filter(SectorBranchesSIC2008 %in% c("300025"))

    data.m <-
        data %>%
            dplyr::select(-ID) %>%
                tidyr::gather_(key_col = "TOPIC", value_col = "VALUE", gather_cols = gather.cols) ## %>%
                    ## tidyr::unite_(col = "COMBINE", from = c(id.cols, "TOPIC"), sep = "_") %>%
                    ##     tidyr::spread(COMBINE, VALUE) # %>% head()

    data.m[["TOPIC"]] <- as.character(data.m[["TOPIC"]])

    ## h(data.m)
    return(data.m)
}

#' @rdname cbsODataAPI
#' @param data a data frame created with \code{cbsODataAPI}
#' @export
cbsOdataDFtoXTS <- function(
    data = stop("'data' must be provided")
    ) {

    names(data) <- tolower(names(data))
    ## data <- read.csv(file.path(dlpath, "CBS_82572ENG.csv"))
    ## data <- data.m
    ## ## data <- read.csv(file.path(dlpath, "CBS_82572ENG.csv"))
    ## ## find positon of "Periods" in column names vectors
    varcol.periods <- match("periods", names(data))
    ## gather.cols <- names(data)[(varcol.periods + 1):length(data)]
    ## TOPIC and VALU defined in cbsOdataDFgather

    ## id.cols <- names(data)[1:varcol.periods]
    ## id.cols <- id.cols[!id.cols%in%c("ID", "Periods")]
    id.cols <- names(data)
    id.cols <- id.cols[!id.cols%in%c("id", "periods", "value")]

    ## data <-
    ##     data %>% dplyr::filter(SectorBranchesSIC2008 %in% c("300025"))

    data.xts <-
        data %>%
            ## dplyr::select(-ID) %>%
            ##     tidyr::gather_(key_col = "TOPIC", value_col = "VALUE", gather_cols = gather.cols) %>%
                    ## tidyr::unite_(col = "COMBINE", from = c(id.cols, "TOPIC"), sep = "_") %>%
                    tidyr::unite_(col = "combine", from = c(id.cols), sep = "_") %>%
                        tidyr::spread(combine, value) # %>% head()

    ## rownames(data.xts) <- sub("JJ00", "-01-01", data.xts$Periods)
    rownames(data.xts) <- sapply(data.xts$periods, cbsODataChangeDates)

    ## data.xts <- data.xts[, !colnames(data.xts)%in%c("Periods")]
    data.xts <- subset(data.xts, select = names(data.xts)[!names(data.xts)%in%c("Periods")])

    data.xts <- xts::as.xts(data.xts, dateFormat = "Date")

    return(data.xts)

}

#' @rdname cbsODataAPI
#' @param url location of table list XML document.
#' @param fields character vector to extract content properties of entries.
#' @export
cbsODataTables <- function(
    url = "https://opendata.cbs.nl/ODataCatalog/Tables",
    fields = c("Identifier", "Title", "Frequency", "Period")
    ) {

    tt <- RCurl::getURL(url)
    list <- XML::xmlToList(tt)

    entries.idx <- seq(along = names(list))[names(list)=="entry"]
    entries <- list[entries.idx]

    fields <- c("Identifier", "Title", "Frequency", "Period")

    data <- NULL
    for (var in fields) {
        temp <- sapply(entries, function (x) ifelse(is.null(x$content$properties[[var]]), NA, x$content$properties[[var]]))
        temp <- unname(unlist(temp))
        temp <- iconv(temp, "latin1", "ASCII", sub="")
        data <- cbind(data, temp)
    }
    data.df <- as.data.frame(data)
    names(data.df) <- fields

    return(data.df)

}

#' @rdname cbsODataAPI
#' @param str a character string with CBS OData dates, e.g. \code{"1995JJ00"}
cbsODataChangeDates <- function(str) {

    str <- sub("JJ00", "-01-01", str)
    return(str)

}
