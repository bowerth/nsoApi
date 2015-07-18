#' API Destatis Genesis
#'
#' Information retrieval from Destatis Genesis webservices
#'
#' Retrieve information from Destatis Genesis \code{quader} in linearised XML format.
#' The retrieval function requires a premium \link{login https://www-genesis.destatis.de/genesis/online}
#'
#' @param api.param a list with parameters used to construct the query, see example
#' @param service the webservice to use, e.g. \code{ExportService} or \code{RechercheService}
#' @param curl optional, \code{CURL} handle created with \code{RCurl::getCurlHandle()}
#' @param query logical, return the https query
#'
#' @return The main function creates an URL with with the specified
#' parameters, retrieves the XML string and transforms into an R list.
#' Additional functions convert the returned list to data frame and xts objects.
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords API XML
#' @import dplyr RCurl reshape2 stringr tidyr XML xts
#' @export
#' @examples
#' api.param.datenexport <- list(
#'     method = "DatenExport",
#'     kennung = "KENNUNG",
#'     passwort = "PASSWORT",
#'     namen = "81000BJ002",
#'     bereich = "oeffentlich",
#'     format = "csv",
#'     werte = "true",
#'     metadaten = "false",
#'     zusatz = "false",
#'     startjahr = "",
#'     endjahr = "",
#'     zeitscheiben = "",
#'     regionalschluessel = "",
#'     sachmerkmal = "",
#'     sachschluessel = "",
#'     stand = "01.01.2001",
#'     sprache = "de"
#' )
#'
#' curl <- RCurl::getCurlHandle()
#' ## RCurl::curlSetOpt(.opts = list(proxy = ""), curl = curl)
#'
#' xml.list.datenexport <- genesisAPI(api.param = api.param.datenexport,
#'                                    service = "ExportService",
#'                                    curl = curl)
#'
#' ## convert to data frame
#' data.df <- genesisXMLtoDF(xml.list = xml.list.datenexport)
#'
#' ## convert to xts object, e.g. for use in \code{dygraphs} package
#' data.xts <- genesisDFtoXTS(data = data.df)

genesisAPI <- function(
    api.param = stop("'api.param' must be specified"),
    service = stop("'service' must be specified"),
    curl = NULL,
    query = FALSE
) {

    api.url <- "https://www-genesis.destatis.de/genesisWS/services"

    api.param.char <- paste(names(api.param), unlist(api.param), sep = "=")
    api.param.str <- gsub(", ", "&", toString(api.param.char))

    req.uri <- paste0(api.url, "/", service, "?", api.param.str)

    if (query) return(req.uri)
    if (is.null(curl)) curl <- RCurl::getCurlHandle()

    tt <- RCurl::getURL(req.uri, curl = curl,
                        .opts = list(ssl.verifypeer = FALSE))

    file.xml <- tempfile()
    filecon <- file(file.xml)
    writeLines(text = tt, con = filecon)
    close(filecon)

    ## XML::xmlParse
    result.xml <- XML::xmlParse(file = file.xml)
    result.list <- XML::xmlToList(result.xml)

    return(result.list)
}


#' @rdname genesisAPI
#' @param xml.list a character string returned from
#' @export
genesisXMLtoDF <- function(
    xml.list = stop("'xml.list' must be provided")
) {

    if (names(xml.list[[1]]) == "DatenExportResponse") {
        data.str <- xml.list[["Body"]][["DatenExportResponse"]][["DatenExportReturn"]][["quader"]][["quader"]][["quaderDaten"]]
    } else {
        stop("format not specified\n")
    }

    data.char <- strsplit(data.str, "\n")[[1]]
    namedim.begin <- match("K;DQA;NAME;RHF-BSR;RHF-ACHSE", data.char)
    nametime.begin <- match("K;DQZ;NAME;ZI-RHF-BSR;ZI-RHF-ACHSE", data.char)
    nameunit.begin <- match("K;DQI;NAME;ME-NAME;DST;TYP;NKM-STELLEN", data.char)
    header.substr <- "K;QEI;"
    header.begin <- match(header.substr, substr(data.char, 1, nchar(header.substr)))

    namedim.raw <- data.char[c((namedim.begin + 1):(nametime.begin - 1))]
    namedim <- sapply(strsplit(namedim.raw, ";"), "[[", 2)

    ## nametime.raw <- data.char[(nametime.begin + 1)]
    ## nametime <- sapply(strsplit(nametime.raw, ";"), "[[", 2)

    nameunit.raw <- data.char[c((nameunit.begin + 1):(header.begin - 1))]
    nameunit <- sapply(strsplit(nameunit.raw, ";"), "[[", 2)

    header.raw <- data.char[header.begin]
    header <- strsplit(header.raw, ";")[[1]]
    header <- header[!header%in%"QEI"]
    for (dim in namedim) {
        header[match("FACH-SCHL", header)] <- dim
    }

    data.raw <- data.char[c((header.begin + 1):length(data.char))]

    ## adjust header for additional value fields according to nameunit
    if (length(header) < length(strsplit(data.raw[1], split = ";")[[1]])) {
        header <- c(header, tail(header, 4))
    }

    for (unit in nameunit) {
        header[match("WERT", header)] <- unit
    }

    data.raw.df <- data.frame(data = data.raw)

    data.df <- tidyr::separate(data = data.raw.df, col = data, into = header, sep = ";")

    data.df <- data.df[, !colnames(data.df)%in%c("K", "QUALITAET", "GESPERRT", "WERT-VERFAELSCHT")]
    data.df.m <- reshape2::melt(data.df, id.vars = c(namedim, "ZI-WERT"), variable.name = "UNIT", value.name = "WERT")

    return(data.df.m)
}

#' @rdname genesisAPI
#' @param data a data frame created with \link{\code{genesisXMLtoDF}}
#' @export
genesisDFtoXTS <- function(
    data = stop("'data' must be provided")
) {

    data <- subset(data, select = names(data)[!names(data)%in%c("K", "QUALITAET", "GESPERRT", "WERT-VERFAELSCHT")])

    names(data) <- sub("ZI-WERT", "ZEIT", names(data))
    pivot.formula <- formula(paste("ZEIT ~", gsub(", ", " + ", toString(names(data)[!names(data)%in%c("ZEIT", "WERT")]))))
    data.d <- reshape2::dcast(data, pivot.formula, value.var = "WERT")

    rownames(data.d) <- paste0(as.character(data.d$ZEIT), '-01-01')
    data.d <- data.d[, -1]

    data.xts <- xts::as.xts(data.d, dateFormate = "Date")

    return(data.xts)
}
