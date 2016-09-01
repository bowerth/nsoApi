[![Travis-CI Build Status](https://travis-ci.org/bowerth/nsoApi.svg?branch=master)](https://travis-ci.org/bowerth/nsoApi)

# API Keys

Use gpg encrypted key file

```
file.apiKey.enc <- system.file("apiKey.R.gpg", package = "nsoApi")
nsoApiGPG(file = file.apiKey.enc,
          gpg = NULL,
          keep = TRUE)
```

# nsoApiBrowser

UI for NSO Web Services

<!-- This is running on shinyapps.io: https://rjsdmx.shinyapps.io/sdmxBrowser/ -->

![nsoApiBrowser screenshot](assets/nsoApiBrowser.png)
