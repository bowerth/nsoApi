#' Query Data OECD.Stat
#'
#' Query data from OECD.Stat using SDMX
#'
#' Helper function to efficiently query data from OECD.Stat Extracts using
#' SDMX-JSON API.
#'
#' @param api an API address
#' @param scheme an API scheme. Available schemes \code{data}, \code{codelist}.
#' @param DSD a datastructure definition identified by the triplet
#'   \code{[collection; country; indicator]}.
#' @param filter for scheme "data": a named list of filters passed to the API.
#'   The position of list items corresponds to the API filter dimensions. Each
#'   list item is either empty (no filter on dimension) or a character vector
#'   containing dimension members to be included in the results. Dimension
#'   members can be obtained from \code{scheme="codelist"} and a codelist item,
#'   e.g. "CL_ECO_ISIC4".
#' @param query logical to return SDMX http url only.
#' @param append append string to the dimension url.
#'
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords OECD.Stat, SDMX
#' @seealso https://data.oecd.org/api/sdmx-json-documentation
#' @export
#' @examples
#' \dontrun{
#' ## list dimension members
#' test.codelist <- sdmxRead(DSD = "BTDIXE_I4", scheme = "codelist")
#' ## retrieve data
#' filter.list <- list(COU = c("ESP", "DEU"),
#'                     FLW = c("IMPO"),
#'                     PAR = c("WOR"),
#'                     EUC = c("TOTAL", "INT"),
#'                     IND = c("DTOTAL", "D01T03"),
#'                     VAL = c("VALUE"))
#' url.append <- paste0("/all?",
#'                      paste("json-lang=en", "detail=Full",
#'                            "dimensionAtObservation=AllDimensions",
#'                            "startPeriod=1990", "endPeriod=2000",
#'                            sep = "&"))
#' test.data <- sdmxRead(DSD = "BTDIXE_I4", filter = filter.list)
#' }

sdmxRead <- function(api="https://stats.oecd.org/SDMX-JSON", # changed from http
                     scheme="data",
                     DSD="BTDIXE_I4",
                     filter=list(COU = c("ESP", "DEU"),
                         FLW = c("IMPO"),
                         PAR = c("WOR"),
                         EUC = c("TOTAL", "INT"),
                         IND = c("DTOTAL", "D01T03"),
                         VAL = c("VALUE")),
                     append=paste0("/all?", paste("json-lang=en", "detail=Full", "dimensionAtObservation=AllDimensions",
                                                  ## "startPeriod=2000", "endPeriod=2010",
                                                  sep = "&")),
                     query=FALSE)
{
    require(RCurl)
    require(jsonlite)
    if (scheme=="codelist") {
        ## https://stats.oecd.org/SDMX-JSON/metadata/FISH_INLAND/all?json-lang=en&amp;detail=Full&amp;dimensionAtObservation=AllDimensions&amp;startPeriod=2000&amp;endPeriod=2010
        ## http://stats.oecd.org/SDMX-JSON/metadata/SNA_TABLE4/all
        ## DSD <- "SNA_TABLE4"
        url.scheme <- "metadata"
        theurl <- paste(api, url.scheme, DSD, sep = '/')
        theurl <- paste0(theurl, append)
        ## theurl <- "https://stats.oecd.org/SDMX-JSON/metadata/FISH_INLAND/all?json-lang=en"

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

#' splitSdmxRead
#'
#' \code{splitSdmxRead}: create query for part of dataset and write to disk
#'
#' splitSdmxRead(loc, split_dim, codelist, destdir, start, end)
#'
#' @param id member of \code{split_dim}
#' @param split_dim dimension to be split
#' @param codelist created by \code{sdmxRead} using \code{{codelist}} scheme
#' @param destdir local directory to export results
#' @param start integer value query start year
#' @param end integer value query end year
#' @return exporting one json file per dimension member of \code{{split_dim}}
#' @rdname sdmxRead
#' @examples
#' library(doMC)
#' registerDoMC(detectCores(logical = TRUE) - 1)
#' flow <- "TEC7_REV4"
#' src_id <- tolower(flow)
#' codelist <- nsoApi::sdmxRead(DSD = flow, scheme = "codelist")
#' split_dim <- "REPORTER"
#' namecou <- codelist[[split_dim]][["id"]]
#' foreach(cou = namecou) %dopar% splitSdmxRead(id = cou, codelist = codelist, destdir = file.path(origdir, src_id))
#' @export
splitSdmxRead <- function(id, split_dim, codelist, destdir, start, end) {
  filter_list <- as.list(rep("", length(codelist)-1))
  names(filter_list) <- names(codelist)[-length(codelist)]
  filter_list[[split_dim]] <- id

  url_append <-
    paste0("/all?",
           paste("json-lang=en",
                 "detail=Full",
                 "dimensionAtObservation=AllDimensions",
                 paste0("startPeriod=", as.character(start)),
                 paste0("endPeriod=", as.character(end)),
                 sep = "&"))
  test_query <- sdmxRead(DSD = flow, filter = filter_list, append = url_append, query = TRUE)
  tt <- RCurl::getURL(test_query)

  ## write to disk
  jsonfile <- file.path(destdir, paste0(id, ".json"))
  filecon <- file(jsonfile)
  writeLines(text = tt, con = filecon)
  close(filecon)
}


#' exportCodes
#'
#' \code{exportCodes}: write codelist to disk
#'
#' exportCodes(codelist, destdir, prefix)
#'
#' @param prefix character string appended to export file
#' @return a tab-separated codelist file for each dimension
#' @rdname sdmxRead
#' @examples
#' codelist <- nsoApi::sdmxRead(DSD = "TEC7_REV4", scheme = "codelist")
#' exportCodes(codelist = codelist, destdir = ".", prefix = paste0("_"))
#' @export
exportCodes <- function(codelist, destdir, prefix) {
  for (dim in names(codelist)) {
    dat <- codelist[[dim]]
    fname <- paste0(prefix, dim, ".tsv")
    fpath <- file.path(destdir, fname)
    write.table(dat, file = fpath, sep = "\t", row.names = FALSE)
  }
  return(fpath)
}


#' @rdname sdmxRead
#' @export
loadJSON <- function(loc) {
  jsonfile <- file.path(origdir, src_id, paste0(loc, ".json"))
  filecon <- file(jsonfile)
  tt <- readLines(con = filecon)
  close(filecon)
  sdmx_list <-
    tt %>%
    rjson::fromJSON()                   # simplify=TRUE
  return(sdmx_list)
}


#' @rdname sdmxRead
#' @export
transformData <- function(x, elem = "dataSets", extractNames = FALSE) {
  dat <- x[[elem]]
  if (extractNames) {
    ## listnames <- names(dat[["dataSets"]][[1]][["observations"]])
    listnames <- names(dat[[1]][["observations"]])
    return(listnames)
  }
  xrow <- dat[[1]][["observations"]]
  xrow_na <-
  lapply(xrow,
         function(y) sapply(y, function(x) ifelse(is.null(x), NA, x))
         )
  res <- do.call("rbind", xrow_na)
  ## res <- list(
  ##   values = do.call("rbind", xrow_na),
  ##   names = names(xrow)
  ##   ## names = names2df(names(xrow))
  ## )
  return(res)
}
## dat_df <- transformData(x = listelem, elem = "dataSets") %>% as.data.frame()
## str(dat_df)

#' @rdname sdmxRead
#' @export
factorLevels <- function(x, elem = "structure") {
  dat <- x[[elem]]
  ids <- sapply(dat[["dimensions"]][["observation"]], function(x) x[["id"]])
  res <-
    lapply(
      ## dat[["dimensions"]][["observation"]][["values"]],
      dat[["dimensions"]][["observation"]],
      ## function(x) x[["id"]]
      function(x) sapply(x[["values"]], function(y) y[["id"]])
    )
  names(res) <- ids
  return(res)
}


#' @rdname sdmxRead
#' @export
names2df <- function(names, factor_levels) {
  names_split <- stringr::str_split(names, pattern = ":")
  names_df <-
    ## do.call("rbind", names_split)
    do.call("rbind", names_split)
  ## add 1 to match with factor levels
  ## return(names_df)
  names_df_num <-
    apply(names_df, 2, function(x) as.numeric(x) + 1) %>%
     as.data.frame()
  names(names_df_num) <- names(factor_levels)
  names_df_factor <- names_df_num %>% assignFactors(factor_levels)
  ## return(names_df_num)
  return(names_df_factor)
}
## factor_levels <- sdmx_list %>% factorLevels()
## sdmx_df <-
##   sdmx_trans %>% rownames() %>% names2df(factor_levels = factor_levels) %>% head()


#' @rdname sdmxRead
#' @export
assignFactors <- function(data, factor_levels) {
  data_copy <- data
  for (var in names(data_copy)) {
    var_list <- as.list(seq(along = factor_levels[[var]]))
    names(var_list) <- factor_levels[[var]]
    data_copy[[var]] <- factor(data_copy[[var]])
    levels(data_copy[[var]]) <- var_list
  }
  return(data_copy)
}
## sdmx_df %>% assignFactors(factor_levels = factor_levels)


#' @rdname sdmxRead
#' @export
processFile <- function(loc) {
  cat("processing", loc, "...\n")
  sdmx_list <- loadJSON(loc = loc)
  factor_levels <- factorLevels(sdmx_list)
  sdmx_trans <- transformData(sdmx_list)
  names_df <- sdmx_trans %>% rownames() %>% names2df(factor_levels = factor_levels)
  sdmx_df <- cbind.data.frame(names_df, data.frame(VALUE = sdmx_trans[, 1]))
  rownames(sdmx_df) <- NULL
  ## sdmx_df %>% head()
  return(sdmx_df)
}
