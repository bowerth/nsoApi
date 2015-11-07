#' API Destatis Genesis
#'
#' Information retrieval from Destatis Genesis webservices
#'
#' Retrieve information from Destatis Genesis \code{quader} in linearised XML format.
#' The retrieval function requires a premium login https://www-genesis.destatis.de/genesis/online
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
#'
#' ## extract metadata
#' api.param.datenexport.meta <- 
#'  api.param.datenexport
#'
#' api.param.datenexport.meta[["werte"]] <- "false"
#' api.param.datenexport.meta[["zusatz"]] <- "true"
#' 
#' xml.list.datenexport.meta <- genesisAPI(api.param = api.param.datenexport.meta,
#'                                         service = "ExportService",
#'                                         curl = ui.apiGENESIS.curl
#'                                         )
#' 
#' meta.df <- genesisXMLtoDF(xml.list = xml.list.datenexport.meta,
#'                           meta = TRUE)

genesisAPI <- function(
    api.param = stop("'api.param' must be specified"),
    service = stop("'service' must be specified"),
    curl = NULL,
    query = FALSE
) {

    api.url <- "https://www-genesis.destatis.de/genesisWS/services"

    api.param.char <- paste(names(api.param), unlist(api.param), sep = "=")
    ## api.param.str <- gsub(", ", "&", toString(api.param.char))
    api.param.str <- paste(api.param.char, collapse = "&")

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
#' @param s metadata string to convert to data frame
#' @export
metaClean <- function(s) {
    s <- s[substring(s, 1, 2) == "D;"]
    X <- strsplit(s, split = ";")
    res <- data.frame(code = sapply(X, "[[", 2),
                      label = sapply(X, "[[", 3))
    return(res)
  }

#' @rdname genesisAPI
#' @param xml.list a character string returned from
#' @export
genesisXMLtoDF <- function(
  xml.list = stop("'xml.list' must be provided"),
  meta = FALSE
) {

  ## xml.list <- xml.list.datenexport

  if (names(xml.list[[1]]) == "DatenExportResponse") {
    data.str <- xml.list[["Body"]][["DatenExportResponse"]][["DatenExportReturn"]][["quader"]][["quader"]][["quaderDaten"]]
  } else {
    stop("format not specified\n")
  }

  if (meta==TRUE) {
    data.char <- strsplit(data.str, "\n")[[1]]
    ## namedim.begin <- match("K;ERH-D;GUELT-AB;GUELT-BIS;PZT", data.char)
      ## namemeasure.begin <- match("K;ME;ME-NAME;KTX;TYP;GRUND-ME;UMR-ME;POTENZ;FAKTOR;TRANS-FLAG-2;ME-NAME-2;KTX-2;NOTIZEN;DEF;DEF-2", data.char)
      dimensionlabel.begin <- match("K;MM;NAME;KTX;MM-TYP;BESTAND;SPR-TMP;OBER-BGR-JN;GUELT-AB;GUELT-BIS;GLIED-TYP;STD-SORT;VBD-SCHL-NR;GENESIS-VBD;REGIOSTAT;EU-VBD;DST;SUMMIERBAR;TRANS-FLAG-2;KTX-2;LTX;NOTIZEN;DEF;LTX-2;DEF-2", data.char)
      dimensionmember.begin <- match("K;KMA;FACH-SCHL;KTX;KLASS-SCHL;VBD-SCHL-NR;GENESIS-VBD;REGIOSTAT;EU-VBD;TRANS-FLAG-2;KTX-2;LTX;DEF;NOTIZEN;LTX-2;DEF-2", data.char)
      dimensionmember.end <- match("K;KMAZ;FACH-SCHL;NAME;GUELT-AB;GUELT-BIS;POS-NR;SPR-TMP", data.char)

      dimensionlabel.raw <- data.char[c((dimensionlabel.begin + 1):(dimensionmember.begin - 1))]

      dimensionmember.raw <- data.char[c((dimensionmember.begin + 1):(dimensionmember.end - 1))]

    dimensionlabel <- metaClean(s = dimensionlabel.raw)
    dimensionmember <- metaClean(dimensionmember.raw)
    
    result_list <- list(
      label = dimensionlabel,
      member = dimensionmember)
    
      return(result_list)
    }
    
    ## str(xml.list)
    ## substr(data.str, 1, 100)
    ## substr(data.str, 1, 500)
    ## tempfile <- tempfile(fileext = ".txt")
    ## filecon <- file(tempfile)
    ## writeLines(text = data.str, con = filecon)
    ## close(con = filecon)
    ## system(paste("sublime_text", tempfile), wait = FALSE)

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
        nrep.header.tail <- length(nameunit) - 1 # number of additional units in data
        header <- c(header, rep(tail(header, 4), nrep.header.tail))
    }

    for (unit in nameunit) {
        header[match("WERT", header)] <- unit
    }

    data.raw.df <- data.frame(data = data.raw)

    data.df <- tidyr::separate(data = data.raw.df, col = data, into = header, sep = ";")

    data.df <- data.df[, !colnames(data.df)%in%c("K", "QUALITAET", "GESPERRT", "WERT-VERFAELSCHT")]
    data.df.m <- reshape2::melt(data.df, id.vars = c(namedim, "ZI-WERT"), variable.name = "UNIT", value.name = "WERT")

    data.df.m[["UNIT"]] <- as.character(data.df.m[["UNIT"]])
    data.df.m[["ZI-WERT"]] <- as.numeric(data.df.m[["ZI-WERT"]])
    data.df.m[["WERT"]] <- as.numeric(data.df.m[["WERT"]])

    return(data.df.m)
}

#' @rdname genesisAPI
#' @param data a data frame created with \code{genesisXMLtoDF}
#' @export
genesisDFtoXTS <- function(
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

#' @rdname genesisAPI
#' @param namefilter numeric vector to filter dataset codes.
#' @param fields character vector to select fields from entries.
#' @param kennung user ID.
#' @param passwort user password.
#' @param curl handle created with `RCurl::getCurlHandle`.
#' @export
genesisTables <- function(
    namefilter = c(11:14, 2, 3, 41:49, 5:9),
    fields = c("code", "beschriftungstext"),
    kennung = stop("'kennung' must be provided"),
    passwort = stop("'passwort' must be provided"),
    curl = NULL
) {

    entries.all <- NULL
    for (f in namefilter) {

        api.param.katalog <- list(
            method = "DatenKatalog",
            kennung = kennung,
            passwort = passwort,
            ## filter = "", # "81*"
            filter = paste0(as.character(f), '*'),
            bereich = "Alle",
            listenLaenge = "500", # "500" max
            sprache = "de")

        if (is.null(curl)) curl <- RCurl::getCurlHandle()

        xml.list <- nsoApi::genesisAPI(api.param = api.param.katalog,
                                       service = "RechercheService",
                                       curl = curl
                                       )

        entries <- xml.list[["Body"]][["DatenKatalogResponse"]][["DatenKatalogReturn"]][["objektKatalogEintraege"]]

        entries.all <- c(entries.all, entries)
        cat(paste0('"', f, '*" filter: ', length(entries), ' entries \n'))
    }


    mat <- NULL
    for (field in fields) {
        temp <- sapply(entries.all, function (x) ifelse(field%in%names(x), x[[field]], NA))
        temp <- unname(unlist(temp))
        temp <- iconv(temp, "latin1", "ASCII", sub="")
        mat <- cbind(mat, temp)
    }
    df <- as.data.frame(mat)
    names(df) <- fields

    return(df)
}
