#' API SSB
#'
#' Information retrieval from Statistics Norway API
#'
#' Retrieve information from Statistics Norway API in \code{JSON-Stat} format.
#'
#' @return A data frame converted from JSON-Stat format
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords JSON-Stat
#' @name ssbAPI
#' @export
#' @examples
#' \dontrun{
#' req.uri <- "http://data.ssb.no/api/v0/dataset/44631.json?lang=en"
#' data.df <- rjstat::fromJSONstat(x = req.uri)
#'
#' ## convert to xts object, e.g. for use in \code{dygraphs} package
#' data.xts <- pxwebDFtoXTS(data = data.df)
#' }

ssbAPI <- function(dataset,
                   curl=NULL) {
  
  base_url <- "http://data.ssb.no/api/v0/dataset"

  append <- ".json?lang=en"

  req.uri <- file.path(base_url, paste0(dataset, append))

  if (is.null(curl)) curl <- getCurlHandle()
  
  tt <- RCurl::getURL(req.uri, curl = curl)
  
  data.df <- rjstat::fromJSONstat(tt)

  data.df <- data.df[[1]]

  names(data.df) <- sub("value", "values", names(data.df)) # harmonize with PX-Web
  
  return(data.df)
  
}
