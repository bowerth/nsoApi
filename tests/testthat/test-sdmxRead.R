library(nsoApi)
library(magrittr)
library(testthat)
context("sdmxRead")

test_that("check if correct url is produced", {

  api <- "http://stats.oecd.org/SDMX-JSON"
  scheme <- "data"
  DSD <- "BTDIXE_I4"
  filter_list <- list(COU = c("ESP", "DEU"),
                 FLW = c("IMPO"),
                 PAR = c("WOR"),
                 EUC = c("TOTAL", "INT"),
                 IND = c("DTOTAL", "D01T03"),
                 VAL = c("VALUE"))
  append <- paste0("/all?", paste("json-lang=en", "detail=Full", "dimensionAtObservation=AllDimensions", "startPeriod=2000", "endPeriod=2010", sep = "&"))
  query=TRUE

  sdmxRead(api, scheme, DSD, filter_list, append, query) %>%
    expect_equal("http://stats.oecd.org/SDMX-JSON/data/BTDIXE_I4/ESP+DEU.IMPO.WOR.TOTAL+INT.DTOTAL+D01T03.VALUE/all?json-lang=en&detail=Full&dimensionAtObservation=AllDimensions&startPeriod=2000&endPeriod=2010")

})

test_that("check if data can be downloaded", {

  api <- "http://stats.oecd.org/SDMX-JSON"
  scheme <- "data"
  DSD <- "STANI4_2016"
  filter_list <- list(
    LOCATION = c("FRA", "DEU"),
    VAR = c("VALU", "PROD"),
    IND = c("D01T99", "D01T03")
  )
  append <- paste0("/all?", paste("json-lang=en", "detail=Full", "dimensionAtObservation=AllDimensions", "startPeriod=2000", "endPeriod=2010", sep = "&"))
  query=FALSE

  test_data <- sdmxRead(api, scheme, DSD, filter_list, append, query)

  ## names(test_data)

  expect_test_data_var <- factor(c("PROD", "VALU"), levels = c("PROD", "VALU"))
  unique(test_data$var) %>%
    expect_equal(expect_test_data_var)

})
