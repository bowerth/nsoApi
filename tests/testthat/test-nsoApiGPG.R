require(nsoApi, quietly = TRUE) # load the nsoAPI package
require(testthat) # load the testthat package
context("nsoApiGPG") # create a unit test context for the given script file

#unit test 1
test_that("Test1",{
  
  ## load key file
  file.apiKey.enc <- system.file("apiKeyExample.R.gpg", package = "nsoApi")
  file.apiKey.dec <- sub(".gpg", "", file.apiKey.enc)
  if (file.exists(file.apiKey.dec)) {
    source(file.apiKey.dec)
  } else {
    nsoApi::nsoApiGPG(file = file.apiKey.enc,
                      gpg = NULL,
                      passphrase = "nsoapi",
                      ## keep = FALSE # do not create file, only load temporarily
                      keep = TRUE
                      )
  }
  ## if (exists("apiKey")) apiKey
  expect_equal(length(apiKey), 3)
  expect_equal(apiKey[["BEA"]][["userid"]], "29UG8ZKT-JN9R5N2Q")
  ## unlink(file.apiKey.dec)
  ## rm(list = ls())
  ## rm("apiKey")

})

