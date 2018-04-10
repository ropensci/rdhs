
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Travis-CI Build Status](https://travis-ci.org/OJWatson/rdhs.png?branch=master)](https://travis-ci.org/OJWatson/rdhs) [![codecov.io](https://codecov.io/github/OJWatson/rdhs/coverage.svg?branch=master)](https://codecov.io/github/OJWatson/rdhs?branch=master)

`rdhs` is a package for management and analysis of [Demographic and Health Survey (DHS)](www.dhsprogram.com) data. This includes functionality to:

1.  Access standard indicator data (i.e. [DHS STATcompiler](https://www.statcompiler.com/)) in R via the [DHS API](https://api.dhsprogram.com/).
2.  Identify surveys and datasets relevant to a particular analysis.
3.  Download survey datasets from the [DHS website](https://dhsprogram.com/data/available-datasets.cfm).
4.  Load datasets and associated metadata into R.
5.  Extract variables and combining datasets for pooled multi-survey analyses.

Installation
------------

You can install rdhs from github with:

``` r
# install.packages("devtools")
devtools::install_github("OJWatson/rdhs")
```

Basic Functionality
-------------------

This is a basic example which shows you how to follow the 5 steps above to quickly identify, download and extract datasets you are interested in.

Let's say we want to get all the survey data from the Democratic Republic of Congo and Tanzania in the last 5 years (since 2013), which covers the use of rapid diagnostic tests (RDTs) for malaria. To begin we'll interact with the DHS API to identify our datasets.

The DHS API has many *endpoints* that can be accessed using anyone of `dhs_<endpoint>()` functions. Each one interacts with a different endpoint of the [DHS API](https://api.dhsprogram.com/). Their website gives great information about the different search terms and filters that can be used, and we have tried to include all of this within the documentation of each function.

To start our extraction we'll query the surveyCharacteristics endpoint:

``` r

library(rdhs)
## make a call with no arguments
sc <- dhs_surveyCharacteristics()
sc[grepl("Malaria",sc$SurveyCharacteristicName),]
#>    SurveyCharacteristicID SurveyCharacteristicName
#> 1:                     96            Malaria - DBS
#> 2:                     90     Malaria - Microscopy
#> 3:                     89            Malaria - RDT
#> 4:                     57          Malaria module 
#> 5:                      8 Malaria/bednet questions
```

There are 87 different survey characteristics, with one specific survey characteristic for Malaria RDTs. We'll use this to then find the surveys that include this characteristic. We can also at this point filter for our desired countries and years. The DHS API allows for countries to be filtered using by their countryIds, which is one of the arguments in `dhs_surveys()`. To have a look at what each countries countryId is we can use another of the API endpoints first:

``` r
## what are the countryIds
ids <- dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))
str(ids)
#> Classes 'data.table' and 'data.frame':   94 obs. of  2 variables:
#>  $ DHS_CountryCode: chr  "AF" "AL" "AO" "AM" ...
#>  $ CountryName    : chr  "Afghanistan" "Albania" "Angola" "Armenia" ...
#>  - attr(*, ".internal.selfref")=<externalptr>

# lets find all the surveys that fit our search criteria
survs <- dhs_surveys(surveyCharacteristicIds = 89,countryIds = c("CD","TZ"),surveyYearStart = 2013)

# and lastly use this to find the datasets we will want to download and let's download the spss (.sav) datasets (have a look in the dhs_datasets documentation for all argument options, and fileformat abbreviations etc.)
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "SV")
str(datasets)
#> Classes 'data.table' and 'data.frame':   15 obs. of  13 variables:
#>  $ FileFormat          : chr  "SPSS dataset (.sav)" "SPSS dataset (.sav)" "SPSS dataset (.sav)" "SPSS dataset (.sav)" ...
#>  $ FileSize            : int  225788 7147499 3423479 8385666 11832635 4797858 1735199 8364572 9162774 11654800 ...
#>  $ DatasetType         : chr  "HIV Datasets" "Survey Datasets" "Survey Datasets" "Survey Datasets" ...
#>  $ SurveyNum           : int  421 421 421 421 421 421 421 421 485 485 ...
#>  $ SurveyId            : chr  "CD2013DHS" "CD2013DHS" "CD2013DHS" "CD2013DHS" ...
#>  $ FileType            : chr  "HIV Test Results Recode" "Births Recode" "Couples' Recode" "Household Recode" ...
#>  $ FileDateLastModified: chr  "November, 14 2014 12:48:33" "November, 17 2014 15:42:20" "November, 17 2014 15:42:29" "September, 19 2016 09:55:46" ...
#>  $ SurveyYearLabel     : chr  "2013-14" "2013-14" "2013-14" "2013-14" ...
#>  $ SurveyType          : chr  "DHS" "DHS" "DHS" "DHS" ...
#>  $ SurveyYear          : int  2013 2013 2013 2013 2013 2013 2013 2013 2015 2015 ...
#>  $ DHS_CountryCode     : chr  "CD" "CD" "CD" "CD" ...
#>  $ FileName            : chr  "CDAR61SV.ZIP" "CDBR61SV.ZIP" "CDCR61SV.ZIP" "CDHR61SV.ZIP" ...
#>  $ CountryName         : chr  "Congo Democratic Republic" "Congo Democratic Republic" "Congo Democratic Republic" "Congo Democratic Republic" ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

we can now use this to download our datasets for further analysis. The raw data can be very useful for a lot of analysis, however, the DHS does publish a set of standard health indicators that have been statistically calculated to give country, subnational estimates that can be further refined by education and wealth brackets. To do this we need to query the `dhs_data()` endpoint. We can then either search for specific indicators, or by querying for indicators that have been tagged within specific areas.

``` r
## what are the indicaators
indicators <- dhs_indicators()
str(indicators)
#> Classes 'data.table' and 'data.frame':   2943 obs. of  22 variables:
#>  $ Definition             : chr  "Age-specific fertility rate for the three years preceding the survey for age group 15-19 expressed per 1,000 women" "Age-specific fertility rate for the three years preceding the survey for age group 20-24 expressed per 1,000 women" "Age-specific fertility rate for the three years preceding the survey for age group 25-29 expressed per 1,000 women" "Age-specific fertility rate for the three years preceding the survey for age group 30-34 expressed per 1,000 women" ...
#>  $ NumberScale            : int  0 0 0 0 0 0 0 1 1 0 ...
#>  $ IndicatorType          : chr  "I" "I" "I" "I" ...
#>  $ MeasurementType        : chr  "Rate" "Rate" "Rate" "Rate" ...
#>  $ IsQuickStat            : int  0 0 0 0 0 0 0 1 0 0 ...
#>  $ ShortName              : chr  "ASFR 15-19" "ASFR 20-24" "ASFR 25-29" "ASFR 30-34" ...
#>  $ IndicatorId            : chr  "FE_FRTR_W_A15" "FE_FRTR_W_A20" "FE_FRTR_W_A25" "FE_FRTR_W_A30" ...
#>  $ Level1                 : chr  "Fertility" "Fertility" "Fertility" "Fertility" ...
#>  $ IndicatorTotalId       : chr  "" "" "" "" ...
#>  $ Level2                 : chr  "Fertility rates" "Fertility rates" "Fertility rates" "Fertility rates" ...
#>  $ Level3                 : chr  "Women" "Women" "Women" "Women" ...
#>  $ SDRID                  : chr  "FEFRTRWA15" "FEFRTRWA20" "FEFRTRWA25" "FEFRTRWA30" ...
#>  $ IndicatorOldId         : chr  "19165001" "19165002" "19165003" "19165004" ...
#>  $ TagIds                 : chr  "74, 1, 7, 81, 80" "74" "74" "74" ...
#>  $ DenominatorWeightedId  : chr  "" "" "" "" ...
#>  $ Label                  : chr  "Age specific fertility rate: 15-19" "Age specific fertility rate: 20-24" "Age specific fertility rate: 25-29" "Age specific fertility rate: 30-34" ...
#>  $ IndicatorOrder         : int  11763010 11763020 11763030 11763040 11763050 11763060 11763070 11763080 11763090 11763100 ...
#>  $ Denominator            : chr  "Per thousand women years exposed in the period 1-36 months prior to interview" "Per thousand women years exposed in the period 1-36 months prior to interview" "Per thousand women years exposed in the period 1-36 months prior to interview" "Per thousand women years exposed in the period 1-36 months prior to interview" ...
#>  $ QuickStatOrder         : chr  "" "" "" "" ...
#>  $ IndicatorSpecial1Id    : chr  "" "" "" "" ...
#>  $ DenominatorUnweightedId: chr  "" "" "" "" ...
#>  $ IndicatorSpecial2Id    : chr  "" "" "" "" ...
#>  - attr(*, ".internal.selfref")=<externalptr>

# quite a lot of indicators, so let's query by tags. First let's look at what the tags are:
tags <- dhs_tags()
tags[grepl("Malaria",tags$TagName),]
#>    TagType                   TagName TagID TagOrder
#> 1:       0       Malaria Parasitemia    36      540
#> 2:       2 Select Malaria Indicators    79     1000

# and let's then grab this data
data <- dhs_data(tagIds = 36,countryIds = c("CD","TZ"),breakdown="subnational",surveyYearStart = 2013)
str(data)
#> Classes 'data.table' and 'data.frame':   300 obs. of  27 variables:
#>  $ DataId                : int  3121901 2733430 1943241 1587484 1343913 1878573 2443671 2770492 1882836 1475239 ...
#>  $ Indicator             : chr  "Malaria prevalence according to RDT" "Malaria prevalence according to RDT" "Malaria prevalence according to RDT" "Malaria prevalence according to RDT" ...
#>  $ SurveyId              : chr  "CD2013DHS" "CD2013DHS" "CD2013DHS" "CD2013DHS" ...
#>  $ IsPreferred           : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Value                 : num  17.1 47.1 20.2 27.4 49.1 2.9 44.1 12 38.9 49.4 ...
#>  $ SDRID                 : chr  "MLPMALCRDT" "MLPMALCRDT" "MLPMALCRDT" "MLPMALCRDT" ...
#>  $ Precision             : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ RegionId              : chr  "CDDHS2013503010" "CDDHS2013503020" "CDDHS2013503030" "CDDHS2013503040" ...
#>  $ SurveyYearLabel       : chr  "2013-14" "2013-14" "2013-14" "2013-14" ...
#>  $ SurveyType            : chr  "DHS" "DHS" "DHS" "DHS" ...
#>  $ SurveyYear            : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
#>  $ IndicatorOrder        : int  125136010 125136010 125136010 125136010 125136010 125136010 125136010 125136010 125136010 125136010 ...
#>  $ DHS_CountryCode       : chr  "CD" "CD" "CD" "CD" ...
#>  $ CILow                 : chr  "" "" "" "" ...
#>  $ CountryName           : chr  "Congo Democratic Republic" "Congo Democratic Republic" "Congo Democratic Republic" "Congo Democratic Republic" ...
#>  $ IndicatorType         : chr  "I" "I" "I" "I" ...
#>  $ CharacteristicId      : int  503010 503020 503030 503040 503050 503061 503062 503063 503070 503080 ...
#>  $ CharacteristicCategory: chr  "Region" "Region" "Region" "Region" ...
#>  $ IndicatorId           : chr  "ML_PMAL_C_RDT" "ML_PMAL_C_RDT" "ML_PMAL_C_RDT" "ML_PMAL_C_RDT" ...
#>  $ CharacteristicOrder   : int  1503010 1503020 1503030 1503040 1503050 1503061 1503062 1503063 1503070 1503080 ...
#>  $ CharacteristicLabel   : chr  "Kinshasa" "Bas-Congo" "Bandundu" "Equateur" ...
#>  $ ByVariableLabel       : chr  "" "" "" "" ...
#>  $ DenominatorUnweighted : chr  "406" "375" "1081" "1281" ...
#>  $ DenominatorWeighted   : chr  "532" "347" "1414" "1236" ...
#>  $ CIHigh                : chr  "" "" "" "" ...
#>  $ IsTotal               : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ ByVariableId          : int  0 0 0 0 0 0 0 0 0 0 ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

Depending on your analysis this maybe more than enough detail. It is also worth mentioning that this data can also be accessed via [DHS STATcompiler](https://www.statcompiler.com/) if you prefer a click and collect version. However, hopefully one can see that selecting a lot of different indicators for multiple countries and breakdowns should be a lot easier using the `rdhs` API interaction.

------------------------------------------------------------------------

We can now go ahead and download our datasets. To do this we need to first create a `client`. The client is an R6 class (similar to R's built in reference classes and make caching survey and API queries more reproducible) and will be used to log in to your DHS account, download datasets for you, and help query those datasets for the question you are interested in. The client will also cache all of these processes, which really helps increase the reproducibility of your analysis.

To create our client we use the `client()` function and you need to specify your log in credentials for the DHS website. This is best provided, for security reasons, by giving a path to a file that contains your email, password and project title that you used when setting up your account with the DHS website. This should take the form of a file path that contains 3 lines, something like this:

-   email="dummy@gmail.com"
-   password="dummypass"
-   project="Dummy Project"

It also takes an argument for its `root`, which is the directory path where the client and associated caches will be stored. If left bank, a suitable directory will be created within your user cache directory for your operating system.

``` r

## create a client
client <- client(credentials = "credentials")
client
#> <client>
#>   Public:
#>     available_datasets: function (clear_cache_first = FALSE) 
#>     clear_namespace: function (namespace) 
#>     dhs_api_request: function (api_endpoint, query = list(), api_key = private$api_key, 
#>     download_datasets: function (dataset_filenames, download_option = "rds", reformat = TRUE, 
#>     extract: function (questions, add_geo = TRUE) 
#>     get_cache_date: function () 
#>     get_root: function () 
#>     initialize: function (api_key = NULL, root = NULL, credentials = NULL) 
#>     save_client: function () 
#>     set_cache_date: function (date) 
#>     survey_questions: function (dataset_filenames, search_terms = NULL, essential_terms = NULL, 
#>     survey_variables: function (dataset_filenames, variables, essential_variables = NULL) 
#>   Private:
#>     api_endpoints: data indicators countries surveys surveycharacteristics  ...
#>     api_key: ICLSPH-527168
#>     cache_date: 2018-04-09 21:44:57
#>     check_available_datasets: function (filenames) 
#>     credentials_path: C:\Users\Oliver\GoogleDrive\AcademicWork\Imperial\git\rd ...
#>     na_s: ^na -|^na-|.*-na$|.* - na$| \{NA\}$
#>     package_version: package_version, numeric_version
#>     root: C:\Users\Oliver\AppData\Local\Oliver\rdhs\Cache
#>     storr: storr, R6
#>     url: https://api.dhsprogram.com/rest/dhs/
```

Before we use our client to download our datasets, it is worth mentioning that the client can be passed as an argument to any of the API functions. This will then cache the results for you, so that if you are working remotely or without a good internet connection you can still reclaim your previous API requests:

``` r

# before it's cached we provide the client so the results is cached within our client
microbenchmark::microbenchmark(dhs_datasets(client = client),times = 1)
#> Unit: milliseconds
#>                           expr      min       lq     mean   median
#>  dhs_datasets(client = client) 60.19347 60.19347 60.19347 60.19347
#>        uq      max neval
#>  60.19347 60.19347     1

# with it cached it will be returned much quicker with the client argument
microbenchmark::microbenchmark(dhs_datasets(client = client),times = 1)
#> Unit: milliseconds
#>                           expr      min       lq     mean   median
#>  dhs_datasets(client = client) 33.36871 33.36871 33.36871 33.36871
#>        uq      max neval
#>  33.36871 33.36871     1

# without it cached again for comparison
microbenchmark::microbenchmark(dhs_datasets(),times = 1)
#> Unit: seconds
#>            expr      min       lq     mean   median       uq      max
#>  dhs_datasets() 4.510543 4.510543 4.510543 4.510543 4.510543 4.510543
#>  neval
#>      1
```

Now back to our dataset downloads. If we have a look back at our datasets object, we'll see there are 19 datasets listed. However, not all of them will be relevant to our malaria RDT questions. One approach is to head to the DHS website and have a look at the [DHS Recodes](https://dhsprogram.com/publications/publication-dhsg4-dhs-questionnaires-and-manuals.cfm), and look at the recodes that relate to the surveys. The other alternative is to download all the surveys and then query the variables within them. This is what we'll demonstrate here as it also demonstrates more of the package's functionality:

``` r

# download datasets
downloads <- client$download_datasets(datasets$FileName)

str(downloads)
#> List of 15
#>  $ CDAR61SV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDAR61SV.rds"
#>  $ CDBR61SV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDBR61SV.rds"
#>  $ CDCR61SV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDCR61SV.rds"
#>  $ CDHR61SV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDHR61SV.rds"
#>  $ CDIR61SV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDIR61SV.rds"
#>  $ CDKR61SV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDKR61SV.rds"
#>  $ CDMR61SV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDMR61SV.rds"
#>  $ CDPR61SV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDPR61SV.rds"
#>  $ TZHR7HSV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/TZHR7HSV.rds"
#>  $ TZIR7HSV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/TZIR7HSV.rds"
#>  $ TZKR7HSV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/TZKR7HSV.rds"
#>  $ TZBR7HSV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/TZBR7HSV.rds"
#>  $ TZCR7HSV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/TZCR7HSV.rds"
#>  $ TZMR7HSV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/TZMR7HSV.rds"
#>  $ TZPR7HSV: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/TZPR7HSV.rds"
```

The client function `download_datasets` will download the datasets for you, save them in your client's root directory and then either unzip them and read them in for you, and save the resultant data.frame as a .rds object within the client's root directory. The other main reason for reading the dataset in straight away, which is the default option, is that it creates a table of all the survey variables and their definitions and caches them as well. This then allows us to quickly query for particular search terms or survey variables:

``` r

# rapid diagnostic test search
questions <- client$survey_questions(datasets$FileName,search_terms = "malaria rapid test")

str(questions)
#> 'data.frame':    74 obs. of  6 variables:
#>  $ variable        : chr  "hml35$01" "hml35$02" "hml35$03" "hml35$04" ...
#>  $ description     : chr  "Result of malaria rapid test" "Result of malaria rapid test" "Result of malaria rapid test" "Result of malaria rapid test" ...
#>  $ dataset_filename: chr  "CDHR61SV" "CDHR61SV" "CDHR61SV" "CDHR61SV" ...
#>  $ dataset_path    : chr  "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDHR61SV.rds" "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDHR61SV.rds" "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDHR61SV.rds" "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDHR61SV.rds" ...
#>  $ country_code    : chr  "CD" "CD" "CD" "CD" ...
#>  $ survey_year     : chr  "2013" "2013" "2013" "2013" ...
```

What we see from the questions is that the question "Result of malaria rapid test" appears in a few different datasets. This is because the household member recode datasets (CDPR61SV, TZPR7HSV) stores information about the children in a household, with one row per child, whereas the household recode (CDHR61SV, TZHR7HSV) stores information about the household, and thus flattens the information from each child into different subvariables (hml35$01/02 etc). As such it is easier to extract this information from the member recodes. To do this we pass our questions object to the client function `extract`, which will create a list with each dataset and its extracted data as a data.frame. We also have the option to add any geographic data available, which will download the geographic data files for you and add this data to you resultant extract:

``` r

# let's just use the PR files thus
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "SV",fileType = "PR")

# and grab the questions from this
questions <- client$survey_questions(datasets$FileName,search_terms = "malaria rapid test")

# and now extract the data
extract <- client$extract(questions,add_geo = TRUE)
#> Starting Survey 1 out of 2 surveys:CDPR61SV
#> Loading required package: sp
#> Warning: package 'sp' was built under R version 3.4.3
#> Starting Survey 2 out of 2 surveys:TZPR7HSV
```

------------------------------------------------------------------------

Further vignettes
-----------------

TODO: An example workflow using `rdhs` to calculate trends in anemia prevalence is available [here](INSERT%20LINK).

TODO: Full functionality is described in the tutorial [here](https://rawgit.com/OJWatson/rdhs/c33321a/vignettes/rdhs.html).

Motivation
----------

The Demographic and Health Surveys (DHS) Program has collected and disseminated population survey data from over 90 countries for over 30 years. In many countries, DHS provide the key data that mark progress towards targets such as the Sustainable Development Goals (SDGs) and inform health policy such as detailing trends in child mortality and characterising the distribution of use of insecticide-treated bed nets in Africa. Though standard health indicators are routinely published in survey final reports, much of the value of DHS is derived from the ability to download and analyse standardized microdata datasets for subgroup analysis, pooled multi-country analysis, and extended research studies. The suite of tools within `rdhs` hopes to extend the accessibility of these datasets to more researchers within the global health community, who are increasingly using R for their statistical analysis, and is the output of conversations with numerous research groups globally. The end result aims to increase the end user accessibility to the raw data and create a tool that supports reproducible global health research, as well as simplifying commonly required analytical pipelines.
