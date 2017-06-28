<!-- [![Travis-CI Build Status](https://travis-ci.org/bowerth/nsoApi.svg?branch=master)](https://travis-ci.org/bowerth/nsoApi) -->
nsoApi
======

An R package to retrieve and harmonize data from national statistical
offices (NSO) application programming interfaces (API).

-   [vignette containig use
    cases](https://github.com/bowerth/nsoApi/blob/master/vignettes/nsoApi.md)
-   [slides presented at satRdays 2016 in
    Budapest](http://rdata.work/slides/nsoapi/)
-   [open data tables
    gitbook](http://www.gitbook.com/read/book/bowerth/opendata-tables)

build package site  
`Rscript -e 'pkgdown::build_site()'`

build articles  
`Rscript -e 'pkgdown::build_articles()'`

serve page  
`cd docs && python -m SimpleHTTPServer`

render README  
`Rscript -e 'rmarkdown::render("README.Rmd", output_format = "md_document")'`

nsoApiBrowser
-------------

Shiny app for nsoApi package

<img src="https://github.com/bowerth/nsoApi/raw/master/assets/nsoApiBrowser.png" width="100%" />

[![Join the chat at
https://gitter.im/bowerth/nsoApi](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/bowerth/nsoApi?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
