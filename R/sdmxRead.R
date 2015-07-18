#' Query Data OECD.Stat
#'
#' Query data from OECD.Stat using SDMX
#'
#' Helper function to efficiently query data from OECD.Stat Extracts using SDMX-JSON API.
#'
#' @param api an API address
#' @param scheme an API scheme. Available schemes \code{data}, \code{codelist}.
#' @param DSD a datastructure definition identified by the triplet \code{[collection; country; indicator]}.
#' @param filter for scheme "data": a named list of filters passed to the API. The position of list items corresponds to the API filter dimensions. Each list item is either empty (no filter on dimension) or a character vector containing dimension members to be included in the results. Dimension members can be obtained from \code{scheme="codelist"} and a codelist item, e.g. "CL_ECO_ISIC4".
#' @param query logical to return SDMX http url only.
#' @param append append string to the dimension url.
#'
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords OECD.Stat, SDMX
#' @seealso \code{\link{https://data.oecd.org/api/sdmx-json-documentation/}}, \code{\link{http://cran.r-project.org/web/packages/jsonlite/index.html}}
#' @import RCurl
#' @export
#' @examples
#' ## list dimension members
#' test.codelist <- sdmxRead(DSD = "BTDIXE_I4", scheme = "codelist")
#' ## retrieve data
#' filter.list <- list(COU = c("ESP", "DEU"),
#'                     FLW = c("IMPO"),
#'                     PAR = c("WOR"),
#'                     EUC = c("TOTAL", "INT"),
#'                     IND = c("DTOTAL", "D01T03"),
#'                     VAL = c("VALUE"))
#' url.append <- paste0("/all?", paste("json-lang=en", "detail=Full", "dimensionAtObservation=AllDimensions", "startPeriod=1990", "endPeriod=2000", sep = "&"))
#' test.data <- sdmxRead(DSD = "BTDIXE_I4", filter = filter.list)

sdmxRead <- function(api="http://stats.oecd.org/SDMX-JSON",
                     scheme="data",
                     DSD="BTDIXE_I4",
                     filter=list(COU = c("ESP", "DEU"),
                         FLW = c("IMPO"),
                         PAR = c("WOR"),
                         EUC = c("TOTAL", "INT"),
                         IND = c("DTOTAL", "D01T03"),
                         VAL = c("VALUE")),
                     append=paste0("/all?", paste("json-lang=en", "detail=Full", "dimensionAtObservation=AllDimensions", "startPeriod=2000", "endPeriod=2010", sep = "&")),
                     query=FALSE)
{
    require(RCurl)
    require(jsonlite)
    if (scheme=="codelist") {
        ## http://stats.oecd.org/SDMX-JSON/metadata/SNA_TABLE4/all
        ## DSD <- "SNA_TABLE4"
        url.scheme <- "metadata"
        theurl <- paste(api, url.scheme, DSD, sep = '/')
        theurl <- paste0(theurl, append)

        if (query==TRUE) return(theurl)
        ## fetch values from URL
        tt <- getURL(theurl)
        codelist <- fromJSON(txt = tt)

        ## class(codelist[[2]]$dimensions$observation)
        codelist <- codelist[[2]]$dimensions$observation
        code.all <- codelist$values
        names(code.all) <- codelist$id
        return(code.all)
    }
    if (scheme=="data") {
        filter.string <- sapply(filter, FUN='toString')
        filter.string <- gsub(", ", "+", filter.string)
        filter.string <- toString(filter.string)
        filter.string <- gsub(", ", ".", filter.string)
        theurl <- paste(api, scheme, DSD, filter.string, sep = '/')
        theurl <- paste0(theurl, append)
        if (query==TRUE) return(theurl)
        ## fetch values from URL
        tt <- getURL(theurl)
        data.list2 <- fromJSON(txt = tt)
        data.names <- names(data.list2$dataSets$observations)
        ## convert list to data frame
        X <- strsplit(data.names, split = ":")
        data.df <- data.frame(t(data.frame(X)))
        row.names(data.df) <- NULL
        ## get metadata
        ## names(data.list2$structure$dimensions) # dataSet, series, observation
        ## data.list2$structure$dimensions$observation
        names(data.df) <- data.list2$structure$dimensions$observation$id
        ## convert to numeric
        cols = seq(along = data.df)
        data.df[,cols] = apply(data.df[,cols], 2, function(x) as.numeric(x))
        data.df <- data.df + 1 # to match with factor levels
        ## View(data.df)
        ## add values and id values for dimension members
        data.df1 <- data.df
        data.df1$value <- sapply(data.list2$dataSets$observations, '[[', 1)[1,]
        ## i <- 7
        for (i in seq(along = data.df))
        {
            conv.df <- cbind.data.frame(as.numeric(as.factor(data.list2$structure$dimensions$observation$values[[i]]$id)),
                                        data.list2$structure$dimensions$observation$values[[i]]$id)
            conv.df <- cbind.data.frame(seq(along = data.list2$structure$dimensions$observation$values[[i]]$id),
                                        data.list2$structure$dimensions$observation$values[[i]]$id)
            names(conv.df) <- c(names(data.df)[i], tolower(names(data.df)[i]))
            data.df1 <- merge(data.df1, conv.df)
        }
        names(data.df1) <- sub("time_period", "year", names(data.df1))
        ## View(data.df1)
        is.lower <- "[a-z]"
        result <- grepl(pattern = is.lower, x = names(data.df1))
        data.df1 <- subset(data.df1, select = sort(names(data.df1)[result]))
        return(data.df1)
    }
}
