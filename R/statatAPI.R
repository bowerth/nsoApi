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
#' \dontrun{
#' dataset <- "OGD_vgr001_VGRJahresR_1"
#' statatAPI(dataset = dataset)
#' }

statatAPI <- function(dataset=stop("'dataset' must be provided"),
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

  data$date <- as.Date(sapply(data$date, statatChangeDates))

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

  ## data.xts$date <- as.Date(sapply(data.xts$date, statatChangeDates))
  rownames(data.xts) <- data.xts$date

  data.xts <-
    data.xts %>%
    dplyr::select(-date) %>%
    xts::as.xts(dateFormat = "Date")

  ## dygraphs::dygraph(data.xts)

  return(data.xts)
}

#' @rdname statatAPI
#' @param url location of table list XML document.
#' @export
statatTables <- function(
                         baseurl="http://data.statistik.gv.at/web/catalog.jsp"
                         ) {

  dataset_title <-
    baseurl %>%
    RCurl::getURL() %>%
    XML::htmlParse(baseurl) %>%
    XML::readHTMLTable() %>%
    lapply(function(x) x[["Title"]]) %>%
    unlist %>%
    stringr::str_replace(pattern = "\\n.+", replacement = "")

  ## latin1 encoded - containing umlaute!!
  dataset_id <-
    baseurl %>%
    RCurl::getURL() %>%
    stringr::str_match_all("meta.jsp[?]dataset=.+[\"\"]") %>% # only dataset ID
    unlist() %>%
    stringr::str_replace(pattern = "meta.jsp[?]dataset=", replacement = "") %>%
    stringr::str_replace(pattern = "\"", replacement = "")
  data.df <- data.frame(ID = dataset_id, Title = dataset_title)
  return(data.df)
}

#' @rdname statatAPI
#' @param url location of table list XML document.
#' @param dimension dimension to be retrieved.
#' @export
#' @examples
#' \dontrun{
#' dataset <- "OGD_f1531neu_Aussenhandel_1"
#' dimension <- "C_UBL1531_0"
#' statatDimension(dataset = dataset, dimension = dimension)
#' }
statatDimension <- function(
                         dataset=stop("'dataset' must be provided"),
                         dimension=stop("'dimension' must be provided")
                         ) {

  ## http://data.statistik.gv.at/data/OGD_f1531neu_Aussenhandel_1_C-UBL1531-0.csv
  baseurl <- "http://data.statistik.gv.at/data"
  dimstr <- gsub("_", "-", dimension)
  csv_url <- file.path(baseurl, paste0(dataset, "_", dimstr, ".csv"))
  data_df <- read.table(csv_url, sep = ";", header = TRUE, quote="\"")
  return(data_df)

  ## baseurl = "http://data.statistik.gv.at/ogd/json?dataset="
  ## dimurl = "https://www.data.gv.at/katalog/api/3/action/package_show?id="
  ## resourceurl <- "https://www.data.gv.at/katalog/en/dataset/stat_aussenhandelsdaten-jahrlich/resource"

  ## url_md <- paste0(baseurl, dataset)

  ## md_list <-
  ##   url_md %>%
  ##   RCurl::getURL() %>%
  ##   jsonlite::fromJSON()

  ## url_dim <- paste0(dimurl, md_list$extras$metadata_identifier)

  ## dim_list <-
  ##   url_dim %>%
  ##   RCurl::getURL() %>%
  ##   jsonlite::fromJSON()

  ## dimension <- "OGD_f1531neu_Aussenhandel_1_C-UBL1531-0"

  ## resource_id <-
  ##   dim_list$result$resources %>%
  ##   filter(name == dimension) %>%
  ##   .$id

  ## url_resource <- file.path(resourceurl, resource_id)

  ## url_resource %>%
  ##   RCurl::getURL() %>%
  ##   ## read.csv()
  ##   XML::htmlParse(baseurl) %>%
  ##   XML::readHTMLTable()
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
