---
title: "nsoApi"
author: "Bo Werth"
date: "2016-09-01"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Providers

Some of the APIs require authentication:


|cou |provider |label                          |access   |
|:---|:--------|:------------------------------|:--------|
|USA |BEA      |Bureau of Economic Analysis    |register |
|NLD |CBS      |Statistics Netherlands         |free     |
|DEU |GENESIS  |Destatis                       |paid     |
|GBR |ONS      |Office for National Statistics |register |
|NOR |SSB      |Statistics Norway              |free     |
|AUT |STATAT   |Statistics Austria             |free     |
|CAN |WDS      |Statistics Canada              |IP       |

# Setup

Create an R script containing a list with your keys or just modify the the example script and save it with gpg encryption at `inst/apiKey.R.gpg`.


```r
apiKey <- list(
  BEA = list(
    userid = "29UG8ZKT-JN9R5N2Q")
  ,
  GENESIS = list(
    kennung = "8XXHKY78",
    passwort = "3QYYTQ89")
  ,
  ONS = list(
    apikey = "k647uahw")
)
```

To prevent compromising the API keys, it is suggested to encrypt the file. If you are on Windows, you will need to install GnuPG first. The nsoApiGPG function allows to specify a directory to an executable using the `gpg` parameter.


```r
file.apiKey.enc <- system.file("apiKey.R.gpg", package = "nsoApi")
nsoApi::nsoApiGPG(file = file.apiKey.enc,
                  gpg = NULL,
                  passphrase = Sys.getenv("NSOAPIGPG"),
                  keep = FALSE
                  )
```

```
## [1] TRUE
```

## BEA Bureau of Economic Analysis


```r
api.param <- list(
  DATASETNAME = "GDPbyIndustry",
  METHOD = "GETPARAMETERVALUES",
  RESULTFORMAT = "JSON",
  USERID = apiKey$BEA$userid,
  PARAMETERNAME = "TABLEID"
)
tablelist <- beaAPI(api.param = api.param, curl = curl)
tablelist$BEAAPI$Request$RequestParam
```

```
##   ParameterName                       ParameterValue
## 1  RESULTFORMAT                                 JSON
## 2        METHOD                   GETPARAMETERVALUES
## 3   DATASETNAME                        GDPBYINDUSTRY
## 4 PARAMETERNAME                              TABLEID
## 5        USERID 7023E825-15FF-488D-B8D9-D70E6F67D439
```

Show first 5 tables


|Key |Desc                                                                      |
|:---|:-------------------------------------------------------------------------|
|1   |Value Added by Industry (A) (Q)                                           |
|5   |Value Added by Industry as a Percentage of Gross Domestic Product (A) (Q) |
|6   |Components of Value Added by Industry (A)                                 |
|7   |Components of Value Added by Industry as a Percentage of Value Added (A)  |
|8   |Chain-Type Quantity Indexes for Value Added by Industry (A) (Q)           |


```r
api.param <- list(
  DATASETNAME = "GDPBYINDUSTRY",
  FREQUENCY = "A",
  INDUSTRY = "11",
  METHOD = "GETDATA",
  RESULTFORMAT = "JSON",
  USERID = apiKey$BEA$userid,
  TABLEID = "1",
  YEAR = paste(as.character(c(2008:2015)), collapse = ",")
)
datalist <- beaAPI(api.param = api.param, curl = curl)
knitr::kable(datalist$BEAAPI$Results$Data)
```



|TableID |Frequency |Year |Quarter |Industry |IndustrYDescription                         |DataValue |NoteRef |
|:-------|:---------|:----|:-------|:--------|:-------------------------------------------|:---------|:-------|
|1       |A         |2008 |2008    |11       |Agriculture, forestry, fishing, and hunting |154.5     |1       |
|1       |A         |2009 |2009    |11       |Agriculture, forestry, fishing, and hunting |137.7     |1       |
|1       |A         |2010 |2010    |11       |Agriculture, forestry, fishing, and hunting |160.2     |1       |
|1       |A         |2011 |2011    |11       |Agriculture, forestry, fishing, and hunting |197.2     |1       |
|1       |A         |2012 |2012    |11       |Agriculture, forestry, fishing, and hunting |185.8     |1       |
|1       |A         |2013 |2013    |11       |Agriculture, forestry, fishing, and hunting |225.4     |1       |
|1       |A         |2014 |2014    |11       |Agriculture, forestry, fishing, and hunting |215.4     |1       |
|1       |A         |2015 |2015    |11       |Agriculture, forestry, fishing, and hunting |196.0     |1       |


```r
knitr::kable(datalist$BEAAPI$Results$Data)
```



|TableID |Frequency |Year |Quarter |Industry |IndustrYDescription                         |DataValue |NoteRef |
|:-------|:---------|:----|:-------|:--------|:-------------------------------------------|:---------|:-------|
|1       |A         |2008 |2008    |11       |Agriculture, forestry, fishing, and hunting |154.5     |1       |
|1       |A         |2009 |2009    |11       |Agriculture, forestry, fishing, and hunting |137.7     |1       |
|1       |A         |2010 |2010    |11       |Agriculture, forestry, fishing, and hunting |160.2     |1       |
|1       |A         |2011 |2011    |11       |Agriculture, forestry, fishing, and hunting |197.2     |1       |
|1       |A         |2012 |2012    |11       |Agriculture, forestry, fishing, and hunting |185.8     |1       |
|1       |A         |2013 |2013    |11       |Agriculture, forestry, fishing, and hunting |225.4     |1       |
|1       |A         |2014 |2014    |11       |Agriculture, forestry, fishing, and hunting |215.4     |1       |
|1       |A         |2015 |2015    |11       |Agriculture, forestry, fishing, and hunting |196.0     |1       |

## CBS Statistics Netherlands

First, we define the API URI and the dataset ID


```r
api <- "http://opendata.cbs.nl/ODataApi/OData/"
DSD <- "82572ENG" # Input-Output: "83068ENG"
```

### CBS Metadata

Next, we return the members of a dimension


```r
scheme <- "SectorBranchesSIC2008"
## print query
cbsODataAPI(api=api, DSD=DSD, scheme=scheme, query=TRUE)
```

[1] "http://opendata.cbs.nl/ODataApi/OData/82572ENG/SectorBranchesSIC2008"

```r
cbs_dimension <- cbsODataAPI(api=api, DSD=DSD, scheme=scheme, query=FALSE)
knitr::kable(cbs_dimension[1:n_table, 1:2])
```



|Key    |Title                               |
|:------|:-----------------------------------|
|300025 |A-U All economic activities         |
|301000 |A Agriculture, forestry and fishing |
|301100 |01 Agriculture                      |
|304300 |02 Forestry and logging             |
|305000 |03 Fishing and aquaculture          |

### CBS Data

Finally, we return a dataset


```r
scheme <- "TypedDataSet"
# print query
cbsODataAPI(api=api, DSD=DSD, scheme=scheme, query=TRUE)
```

[1] "http://opendata.cbs.nl/ODataApi/OData/82572ENG/TypedDataSet"

```r
cbs_data <- cbsODataAPI(api=api, DSD=DSD, scheme=scheme, query=FALSE)
knitr::kable(cbs_data[1:n_table, 1:6])
```



| ID|SectorBranchesSIC2008 |Periods  | OutputBasicPrices_1| IntermediateConsumption_2| GrossValueAddedBasicPrices_3|
|--:|:---------------------|:--------|-------------------:|-------------------------:|----------------------------:|
|  0|300025                |1995JJ00 |              587866|                    292992|                       294874|
|  1|300025                |1996JJ00 |              624009|                    316394|                       307615|
|  2|300025                |1997JJ00 |              671992|                    343515|                       328477|
|  3|300025                |1998JJ00 |              714756|                    364847|                       349909|
|  4|300025                |1999JJ00 |              762732|                    390927|                       371805|

### CBS Tables

List existing tables in CBS API. This takes 1-2 minutes to complete


```r
cbs_tables <- nsoApi::cbsODataTables()
knitr::kable(cbs_tables[1:n_table, ])
```



|Identifier |Title                                                                      |Frequency |Period        |
|:----------|:--------------------------------------------------------------------------|:---------|:-------------|
|82010NED   |Zeggenschap bedrijven in Nederland; banen en lonen, bedrijfsgrootte        |Perjaar   |2008 t/m 2011 |
|82011NED   |Zeggenschap bedrijven in Nederland; banen en lonen, bedrijfstak (SBI 2008) |Perjaar   |2008 t/m 2011 |
|81179ned   |Zeggenschap bedrijven in Nederland; banen en lonen (SBI'93) 2006-2008      |Stopgezet |2006-2008     |
|81251ned   |Banen werknemers en afstand woon-werk; woon- en werkregio's                |Perjaar   |2006 - 2014   |
|80339ned   |Banen van werknemers; bedrijfsgrootte en economische activiteit, 2006-2009 |Stopgezet |2006 - 2009   |

## Destatis GENESIS

### Genesis Metadata


```r
api.param.datenexport <- list(
    method = "DatenExport",
    kennung = apiKey$GENESIS$kennung,
    passwort = apiKey$GENESIS$passwort,
    namen = "81000BJ002",
    bereich = "oeffentlich",
    format = "csv",
    werte = "false",
    metadaten = "false",
    zusatz = "true",
    startjahr = "2013",
    endjahr = "2014",
    zeitscheiben = "",
    regionalschluessel = "",
    sachmerkmal = "",
    sachschluessel = "",
    stand = "01.01.2001",
    sprache = "de"
)
## retrieve metadata
xml_list_datenexport_meta <- genesisAPI(api.param = api.param.datenexport,
                                   service = "ExportService",
                                   curl = curl)
meta_df <- genesisXMLtoDF(xml.list = xml_list_datenexport_meta,
                          meta = TRUE)
knitr::kable(meta_df)
```



|code   |label                                          |
|:------|:----------------------------------------------|
|JAHR   |Jahr                                           |
|DINSG  |Deutschland insgesamt                          |
|BWS001 |Bruttowertschöpfung                            |
|VGRPB5 |Preisbasis (jeweilige Preise / preisbereinigt) |
|WZ08G2 |WZ2008: Wirtschaftsbereiche der VGR            |

|code      |label                                              |
|:---------|:--------------------------------------------------|
|DG        |Deutschland                                        |
|VGRJPM    |in jeweiligen Preisen (Mrd. EUR)                   |
|VGRPKM    |preisbereinigt, Kettenindex (2010=100)             |
|WZ08-A    |Land- und Forstwirtschaft, Fischerei               |
|WZ08-B-18 |Produzierendes Gewerbe ohne Baugewerbe             |
|WZ08-C    |Verarbeitendes Gewerbe                             |
|WZ08-F    |Baugewerbe                                         |
|WZ08-G-01 |Handel, Verkehr, Gastgewerbe                       |
|WZ08-J    |Information und Kommunikation                      |
|WZ08-K    |Erbringung von Finanz- und Versicherungsleistungen |
|WZ08-L    |Grundstücks- und Wohnungswesen                     |
|WZ08-M-02 |Unternehmensdienstleister                          |
|WZ08-O-03 |Öffentliche Dienstleister, Erziehung, Gesundheit   |
|WZ08-R-02 |Sonstige Dienstleister                             |


### Genesis Data


```r
api.param.datenexport <- list(
    method = "DatenExport",
    kennung = apiKey$GENESIS$kennung,
    passwort = apiKey$GENESIS$passwort,
    namen = "81000BJ002",
    bereich = "oeffentlich",
    format = "csv",
    werte = "true",
    metadaten = "false",
    zusatz = "false",
    startjahr = "2013",
    endjahr = "2014",
    zeitscheiben = "",
    regionalschluessel = "",
    sachmerkmal = "",
    sachschluessel = "",
    stand = "01.01.2001",
    sprache = "de"
)
## retrieve data
xml_list_datenexport <- genesisAPI(api.param = api.param.datenexport,
                                   service = "ExportService",
                                   curl = curl)
## convert to data frame
data_df <- genesisXMLtoDF(xml.list = xml_list_datenexport)
knitr::kable(data_df[1:n_table,])
```



|VGRPB5 |WZ08G2    |DINSG | ZI-WERT|UNIT   |    WERT|
|:------|:---------|:-----|-------:|:------|-------:|
|VGRJPM |WZ08-A    |DG    |    2013|BWS001 |  24.005|
|VGRJPM |WZ08-B-18 |DG    |    2013|BWS001 | 654.188|
|VGRJPM |WZ08-C    |DG    |    2013|BWS001 | 572.186|
|VGRJPM |WZ08-F    |DG    |    2013|BWS001 | 113.247|
|VGRJPM |WZ08-G-01 |DG    |    2013|BWS001 | 395.395|

### Genesis Tables


```r
genesis_tables <- nsoApi::genesisTables(
                namefilter = 11, # for testing - takes a few minutes to complete
                ## namefilter = c(11:14, 2, 3, 41:49, 5:9),
                fields = c("code", "beschriftungstext"),
                kennung = apiKey$GENESIS$kennung,
                passwort = apiKey$GENESIS$passwort
                )
```

"11*" filter: 4 entries 

```r
genesis_tables$beschriftungstext <- gsub("\n", " ", genesis_tables$beschriftungstext)
knitr::kable(genesis_tables)
```



|code       |beschriftungstext                                                             |
|:----------|:-----------------------------------------------------------------------------|
|11111BJ001 |Feststellung des Gebietsstands, Gebietsflche, Deutschland insgesamt, Stichtag |
|11111KJ001 |Feststellung des Gebietsstands, Gebietsflche, Kreise, Stichtag                |
|11111LJ001 |Feststellung des Gebietsstands, Gebietsflche, Bundeslnder, Stichtag           |
|NA         |NA                                                                            |

## ONS UK Office for National Statistics

### ONS Data


```r
api.param <- list(context = "Census",
                  geog = "2011WARDH",
                  totals = "false",
                  apikey = "xPuqnMzZ01")
ons_data <- onsCsvData(api.param = api.param, dataset = "QS208EW")
knitr::kable(ons_data[1:n_table, 1:7])
```



|geographic_id | total_all_categories_religion| christian| buddhist|  hindu| jewish|  muslim|
|:-------------|-----------------------------:|---------:|--------:|------:|------:|-------:|
|K04000001     |                      56075912|  33243175|   247743| 816633| 263346| 2706066|
|E92000001     |                      53012456|  31479876|   238626| 806199| 261282| 2660116|
|W92000004     |                       3063456|   1763299|     9117|  10434|   2064|   45950|
|E12000001     |                       2596886|   1753334|     6316|   7772|   4503|   46764|
|E12000002     |                       7052177|   4742860|    20695|  38259|  30417|  356458|

## ONS Tables ##


```r
ons_tables <- onsAPI(method = "contexts", api.key = apiKey$ONS$apikey)
knitr::kable(ons_tables)
```



| contextId|contextName    |
|---------:|:--------------|
|         1|Census         |
|         2|Socio-Economic |
|         3|Economic       |
|         4|Social         |

```r
ons_collections <- onsAPI(method = "collections", context = "Economic", api.key = apiKey$ONS$apikey)
titles <- sapply(ons_collections$name[, 1], function(x) x[1, 2])
titles <- iconv(titles, "latin1", "ASCII", sub="")
ons_collections_df <- data.frame(ID = ons_collections$id, Title = titles)
knitr::kable(ons_collections_df[1:n_table, ])
```



|ID       |Title                                                   |
|:--------|:-------------------------------------------------------|
|A02Level |Labour Force Survey Summary Levels (Explorable Dataset) |
|A02Rate  |Labour Force Survey Summary Rates (Explorable Dataset)  |
|ABS      |Annual Business Survey                                  |
|ABS1     |Annual Business Survey                                  |
|ASHE07E  |Earnings by place of work                               |

## STATAT Statistics Austria

### STATAT Data


```r
dataset <- "OGD_vgr001_VGRJahresR_1"
statat_data <- statatAPI(dataset = dataset)
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

```r
knitr::kable(statat_data[1:n_table, ])
```



|date     |C_VGRW04_0 |variable    |   value|
|:--------|:----------|:-----------|-------:|
|A10-1976 |VGRW04-1   |F_ISIS_4575 | 41853.8|
|A10-1976 |VGRW04-2   |F_ISIS_4575 | 14974.9|
|A10-1976 |VGRW04-3   |F_ISIS_4575 | 16556.6|
|A10-1976 |VGRW04-4   |F_ISIS_4575 | 17803.1|
|A10-1976 |VGRW04-5   |F_ISIS_4575 |  -150.7|


```r
baseURL <- "http://data.statistik.gv.at/web/catalog.jsp"
dataset_title <-
  baseURL %>%
  RCurl::getURL() %>%
  XML::htmlParse(baseURL) %>%
  XML::readHTMLTable() %>%
  lapply(function(x) x[["Title"]]) %>%
  unlist %>%
  stringr::str_replace(pattern = "\\n.+", replacement = "")
## latin1 encoded - containing umlaute!!
dataset_id <-
  baseURL %>%
  RCurl::getURL() %>%
  stringr::str_match_all("meta.jsp[?]dataset=.+[\"\"]") %>% # only dataset ID
  unlist() %>%
  stringr::str_replace(pattern = "meta.jsp[?]dataset=", replacement = "") %>%
  stringr::str_replace(pattern = "\"", replacement = "")
df <- data.frame(ID = dataset_id, Title = dataset_title)
knitr::kable(df[1:n_table, ])
```



|ID                       |Title                                                                                        |
|:------------------------|:--------------------------------------------------------------------------------------------|
|OGD_touextsai_Tour_HKL_1 |Nächtigungsstatistik ab November 1973 - Nächtigungen nach Herkunftsländern und Bundesländern |
|OGD_touextsai_Tour_UA_1  |Nächtigungsstatistik ab November 1973 - Nächtigungen nach Unterkunftsarten und Bundesländern |
|OGD_vpi10_VPI_2010_1     |Verbraucherpreisindex Basis 2010                                                             |
|OGD_vpi05_VPI_2005_1     |Verbraucherpreisindex Basis 2005                                                             |
|OGD_vpi15_VPI_2015_1     |Verbraucherpreisindex Basis 2015                                                             |

