
#' API Bureau of Economic Analysis
#'
#' Connect to BEA datasets
#'
#' These functions allow connecting to BEA time series in JSON format
#' with a valid user ID obtained from
#' \link{http://www.bea.gov/API/signup/index.cfm}
#'
#' @param api.param
#' @param curl
#' @param query
#' @param raw
#'
#' @return The function creates an URL with with the specified
#' parameters, retrieves the JSON string and transforms to R list
#' @examples
#' api.param <- list(USERID = api.key,
#'                   METHOD = "GETDATASETLIST",
#'                   RESULTFORMAT = "JSON")
#' List <- beaAPI(api.param = api.param, curl = curl)
#' df <- beaJSONtoDF(List = List, third = 4)

## req.uri <- "http://www.bea.gov/api/data/?&DATASETNAME=FIXEDASSETS&FREQUENCY=A&METHOD=GETDATA&RESULTFORMAT=JSON&USERID=7023E825-15FF-488D-B8D9-D70E6F67D439&TABLEID=91&YEAR=2008,2009&a"

beaAPI <- function(api.param = stop("'api.param' must be specified"),
                   curl = getCurlHandle(),
                   query = FALSE,
                   raw = FALSE) {
    ##
    api.url <- "http://www.bea.gov/api/data/?"
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
    tt <- getURL(req.uri, curl = curl)
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
    result.list <- fromJSON(tt)
    ##
    return(result.list)
}

#' @rdname beaAPI
#' @param List
#' @param third
beaJSONtoDF <- function(List=stop("'List' must be specified"),
                         third = 1) {
    temp <- lapply(List[[1]][[2]][[third]], function(x) data.frame(x))
    df <- NULL
    for (i in seq(along = temp)) {
        df <- rbind(df, temp[[i]])
    }
    return(df)
}
