% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genesisAPI.R
\name{genesisAPI}
\alias{genesisAPI}
\alias{metaClean}
\alias{genesisXMLtoDF}
\alias{genesisDFtoXTS}
\alias{genesisTables}
\title{API Destatis Genesis}
\usage{
genesisAPI(api.param = stop("'api.param' must be specified"),
  service = stop("'service' must be specified"), curl = NULL,
  query = FALSE)

metaClean(s)

genesisXMLtoDF(xml.list = stop("'xml.list' must be provided"), meta = FALSE)

genesisDFtoXTS(data = stop("'data' must be provided"))

genesisTables(namefilter = c(11:14, 2, 3, 41:49, 5:9), fields = c("code",
  "beschriftungstext"), kennung = stop("'kennung' must be provided"),
  passwort = stop("'passwort' must be provided"), curl = NULL)
}
\arguments{
\item{api.param}{a list with parameters used to construct the query, see example}

\item{service}{the webservice to use, e.g. \code{ExportService} or \code{RechercheService}}

\item{curl}{optional, \code{CURL} handle created with \code{RCurl::getCurlHandle()}}

\item{query}{logical, return the https query}

\item{s}{metadata string to convert to data frame}

\item{xml.list}{a character string returned from}

\item{data}{a data frame created with \code{genesisXMLtoDF}}

\item{namefilter}{numeric vector to filter dataset codes.}

\item{fields}{character vector to select fields from entries.}

\item{kennung}{user ID.}

\item{passwort}{user password.}

\item{curl}{handle created with `RCurl::getCurlHandle`.}
}
\value{
The main function creates an URL with with the specified
parameters, retrieves the XML string and transforms into an R list.
Additional functions convert the returned list to data frame and xts objects.
}
\description{
Information retrieval from Destatis Genesis webservices
}
\details{
Retrieve information from Destatis Genesis \code{quader} in linearised XML format.
The retrieval function requires a premium login https://www-genesis.destatis.de/genesis/online
}
\examples{
\dontrun{
api.param.datenexport <- list(
    method = "DatenExport",
    kennung = "KENNUNG",
    passwort = "PASSWORT",
    namen = "81000BJ002",
    bereich = "oeffentlich",
    format = "csv",
    werte = "true",
    metadaten = "false",
    zusatz = "false",
    startjahr = "",
    endjahr = "",
    zeitscheiben = "",
    regionalschluessel = "",
    sachmerkmal = "",
    sachschluessel = "",
    stand = "01.01.2001",
    sprache = "de"
)

curl <- RCurl::getCurlHandle()
## RCurl::curlSetOpt(.opts = list(proxy = ""), curl = curl)

xml.list.datenexport <- genesisAPI(api.param = api.param.datenexport,
                                   service = "ExportService",
                                   curl = curl)

## convert to data frame
data.df <- genesisXMLtoDF(xml.list = xml.list.datenexport)

## convert to xts object, e.g. for use in \\code{dygraphs} package
data.xts <- genesisDFtoXTS(data = data.df)

## extract metadata
api.param.datenexport.meta <- 
 api.param.datenexport

api.param.datenexport.meta[["werte"]] <- "false"
api.param.datenexport.meta[["zusatz"]] <- "true"

xml.list.datenexport.meta <- genesisAPI(api.param = api.param.datenexport.meta,
                                        service = "ExportService",
                                        curl = ui.apiGENESIS.curl
                                        )

meta.df <- genesisXMLtoDF(xml.list = xml.list.datenexport.meta,
                          meta = TRUE)
}
}
\author{
Bo Werth <bo.werth@gmail.com>
}
\keyword{API}
\keyword{XML}
