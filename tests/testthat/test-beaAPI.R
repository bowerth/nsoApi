require(nsoApi, quietly = TRUE) # load the nsoAPI package
require(testthat) # load the testthat package
require(RCurl)
context("beaAPI") # create a unit test context for the given script file

curl <- getCurlHandle()
## ## enter proxy if required
## curlSetOpt(.opts = list(proxy = proxy), curl = curl)

test_that("beaAPI GETPARAMETERVALUES TABLEID",{

  file.apiKey.enc <- system.file("apiKey.R.gpg", package = "nsoApi")
  nsoApi::nsoApiGPG(file = file.apiKey.enc,
                    gpg = NULL,
                    passphrase = Sys.getenv("NSOAPIGPG"),
                    keep = FALSE
                    )

  ## expect_equal(length(apiKey), 3)
  ## expect_equal(apiKey[["BEA"]][["userid"]], "7023E825-15FF-488D-B8D9-D70E6F67D439")

  datasetlist <- c("GDPbyIndustry", "NIPA", "FixedAssets")
  tablelist.all <- NULL
  ## dataset <- datasetlist[1]
  for (dataset in datasetlist) {
    api.param <- list(
      DATASETNAME = dataset,
      METHOD = "GETPARAMETERVALUES",
      RESULTFORMAT = "JSON",
      USERID = apiKey$BEA$userid,
      PARAMETERNAME = "TABLEID"
    )
    tablelist <- beaAPI(api.param = api.param, curl = curl)
    tablelist <- tablelist$BEAAPI$Results$ParamValue
    if (length(tablelist) > 0) {
      tablelist <- data.frame(ID = tablelist[, 1], Label = tablelist[, 2], Dataset = dataset, stringsAsFactors = FALSE)
      tablelist$ID <- as.numeric(tablelist$ID)
      tablelist <- tablelist[order(tablelist$ID),]
    }
    tablelist.all <- rbind(tablelist.all, tablelist)
  }

  expect_equal(names(tablelist.all), c("ID", "Label", "Dataset"))

})

test_that("beaAPI GETDATA GDP", {

  conv.var.gdp <- rbind.data.frame(
    c("1", "VALU")
    ## , # Value Added by Industry
    ## c("8", "VKOT") # Chain-Type Quantity Indexes for Value Added by Industry
    ## ,
    ## c("15", "PROD"), # Gross Output by Industry
    ## c("16", "PKOT"), # Chain-Type Quantity Indexes for Gross Output by Industry
    ## c("20", "INTI"), # Intermediate Inputs by Industry
    ## c("21", "IKOT") # Chain-Type Quantity Indexes for Intermediate Inputs by Industry
    )
  names(conv.var.gdp) <- c("tableid", "var")

  ## need to specify more than one
  tableid <- gsub(", ", ",", toString(as.character(conv.var.gdp$tableid)))

  ## if (download==TRUE) {

    df.gdp.all <- NULL
    ## tableid <- "1,15"

    api.param <- list(
      DATASETNAME = "GDPBYINDUSTRY",
      ## FREQUENCY = "A",
      FREQUENCY = "Q",
      INDUSTRY = "11,21",
      ## INDUSTRY = "ALL",
      METHOD = "GETDATA",
      RESULTFORMAT = "JSON",
      USERID = apiKey$BEA$userid,
      ## TABLEID = "ALL"
      TABLEID = tableid, # "1,8,15,16,20,21",
      ## YEAR = gsub(" ", "", toString(c(1997:2007)))
      ## YEAR = "ALL"
      YEAR="2008,2009"
    )

    List <- beaAPI(api.param = api.param, curl = curl)
    df.gdp.all <- List$BEAAPI$Results$Data

    expect_equal(tolower(names(df.gdp.all)), c("tableid", "frequency", "year", "quarter", "industry", "industrydescription", "datavalue", "noteref"))
    expect_gt(nrow(df.gdp.all), 0) # at least one row in result

    xts.gdp.all <- beaDFtoXTS(data = df.gdp.all)
    expect_gt(nrow(xts.gdp.all), 0)

})

test_that("beaAPI GETDATA NIPA", {

  conv.var.nipa <- rbind.data.frame(
    c(185, "LABR")
   ## ,
   ##   c(189, "WAGE")
   ## ,
   ##  c(193, "EMPE"),
   ##  c(197, "FTEE"),
   ##  c(209, "FTEN")
  )
  names(conv.var.nipa) <- c("tableid", "var")

  df.nipa.all <- NULL
  ## tableid <- conv.var.nipa$tableid[1]
    for (tableid in conv.var.nipa$tableid) {
      ## retrieve data: NIPA
      api.param <- list(
        DATASETNAME = "NIPA",
        FREQUENCY = "A",
        METHOD = "GETDATA",
        RESULTFORMAT = "JSON",
        USERID = apiKey$BEA$userid,
        TABLEID = tableid,
        YEAR = "X" # restrict time period to 1970 onwards
      )
      ## print(tableid)

      List <- beaAPI(api.param = api.param, curl = curl)
      ## df.nipa.table <- beaJSONtoDF(List=List, third = 4)
      df.nipa.table <- List$BEAAPI$Results$Data
      ## h(df.nipa.table)
      df.nipa.all <- rbind(df.nipa.all, df.nipa.table)
    }

    expect_equal(tolower(names(df.nipa.all)), c("tableid", "seriescode", "linenumber", "linedescription", "timeperiod", "cl_unit", "unit_mult", "datavalue", "noteref"))
    expect_gt(nrow(df.nipa.all), 0)

})

test_that("GETDATA FIXEDASSETS", {

  conv.var.fa <- rbind.data.frame(
    c(126, "CAPN") # Table 3.1ESI. Current-Cost Net Stock of Private Fixed Assets by Industry (A)
    ## ,
    ## c(128, "CNOT") # Table 3.2ESI. Chain-Type Quantity Indexes for Net Stock of Private Fixed Assets by Industry (A)
    ## ,
    ## c(138, "GFCF"), # Table 3.7ESI. Investment in Private Fixed Assets by Industry (A)
    ## c(57, "GKOT"), # Table 3.8E. Chain-Type Quantity Indexes for Investment in Private Equipment by Industry (A)
    ## ## ## not used
    ## ## c(98, "GFCF"), # Table 5.7. Investment in Residential Fixed Assets by Type of Owner, Legal Form of Organization, and Tenure Group (A)
    ## ## c(99, "GKOT"), # Table 5.8. Chain-Type Quantity Indexes for Investment in Residential Fixed Assets by Type of Owner, Legal Form of Organization, and Tenure Group (A)
    ## c(35, "CAPN"), # Table 7.1A. Current-Cost Net Stock of Government Fixed Assets, 1925-1996 (A)
    ## c(30, "CAPN"), # Table 7.1B. Current-Cost Net Stock of Government Fixed Assets (A)
    ## c(36, "CNOT"), # Table 7.2A. Chain-Type Quantity Indexes for Net Stock of Government Fixed Assets, 1925-1996 (A)
    ## c(31, "CNOT"), # Table 7.2B. Chain-Type Quantity Indexes for Net Stock of Government Fixed Assets (A)
    ## c(100, "GFCF"), # Table 7.5A. Investment in Government Fixed Assets, 1901-1996 (A)
    ## c(101, "GFCF") # Table 7.5B. Investment in Government Fixed Assets (A)
    ## ,
    ## c(102, "GKOT"), # Table 7.6A. Chain-Type Quantity Indexes for Investment in Government Fixed Assets, 1901-1996 (A)
    ## c(103, "GKOT") # Table 7.6B. Chain-Type Quantity Indexes for Investment in Government Fixed Assets (A)
    )
names(conv.var.fa) <- c("tableid", "var")
## remove data after 1996 from historical tables
fa.hist <- c(35, 36, 100, 102)
conv.var.fa <- conv.var.fa[!conv.var.fa$tableid%in%fa.hist,]

df.fa.all <- NULL
## tableid <- conv.var.fa$tableid[1]
    for (tableid in conv.var.fa$tableid) {
        api.param <- list(
            DATASETNAME = "FIXEDASSETS",
            FREQUENCY = "A",
            METHOD = "GETDATA",
            RESULTFORMAT = "JSON",
            USERID = apiKey$BEA$userid,
            TABLEID = tableid,
            ## restrict time period to 1970 onwards
            YEAR = "X"
            ## YEAR = gsub(" ", "", toString(c(1970:2013)))
        )
        ## beaAPI(api.param = api.param, curl = curl, query=TRUE)
        ## print(tableid)
        List <- beaAPI(api.param = api.param, curl = curl)
        ## df.fa.table <- beaJSONtoDF(List=List, third = 4)
        df.fa.table <- List$BEAAPI$Results$Data
        df.fa.all <- rbind(df.fa.all, df.fa.table)
    }

    expect_equal(tolower(names(df.fa.all)), c("tableid", "seriescode", "linenumber", "linedescription", "timeperiod", "cl_unit", "unit_mult", "datavalue", "noteref"))
    expect_gt(nrow(df.fa.all), 0)

  })
