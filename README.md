
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

To start our extraction we'll query the *surveyCharacteristics* endpoint using `dhs_surveyCharacteristics()`:

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

There are 87 different survey characteristics, with one specific survey characteristic for Malaria RDTs. We'll use this to then find the surveys that include this characteristic. We can also at this point filter for our desired countries and years. The DHS API allows for countries to be filtered using by their *countryIds*, which is one of the arguments in `dhs_surveys()`. To have a look at what each countries countryId is we can use another of the API endpoints first:

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

# quite a lot of indicators! It might be easier to first query by tags. First let's look at what the tags are:
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
#>     download_datasets: function (dataset_filenames, download_option = "rds", reformat = FALSE, 
#>     extract: function (questions, add_geo = TRUE) 
#>     get_cache_date: function () 
#>     get_root: function () 
#>     initialize: function (api_key = NULL, root = NULL, credentials = NULL) 
#>     save_client: function () 
#>     set_cache_date: function (date) 
#>     survey_questions: function (dataset_filenames, search_terms = NULL, essential_terms = NULL, 
#>     survey_variables: function (dataset_filenames, variables, essential_variables = NULL, 
#>   Private:
#>     api_endpoints: data indicators countries surveys surveycharacteristics  ...
#>     api_key: ICLSPH-527168
#>     cache_date: 2018-04-10 18:44:42
#>     check_available_datasets: function (filenames) 
#>     credentials_path: C:\Users\Oliver\GoogleDrive\AcademicWork\Imperial\git\rd ...
#>     na_s: ^na -|^na-|.*-na$|.* - na$| \{NA\}$
#>     package_version: package_version, numeric_version
#>     root: C:\Users\Oliver\AppData\Local\Oliver\rdhs\Cache
#>     storr: storr, R6
#>     url: https://api.dhsprogram.com/rest/dhs/
```

Before we use our client to download our datasets, it is worth mentioning that the client can be passed as an argument to any of the API functions we have just seen. This will then cache the results for you, so that if you are working remotely or without a good internet connection you can still return your previous API requests:

``` r
# before it's cached we provide the client so the results is cached within our client
s <- dhs_surveys(client = client)

# with it cached it will be returned much quicker with the client argument
microbenchmark::microbenchmark(dhs_surveys(client = client),times = 1)
#> Unit: milliseconds
#>                          expr      min       lq     mean   median       uq
#>  dhs_surveys(client = client) 46.99686 46.99686 46.99686 46.99686 46.99686
#>       max neval
#>  46.99686     1

# without it cached again for comparison
microbenchmark::microbenchmark(dhs_surveys(),times = 1)
#> Unit: milliseconds
#>           expr      min       lq     mean   median       uq      max neval
#>  dhs_surveys() 471.5151 471.5151 471.5151 471.5151 471.5151 471.5151     1
```

Now back to our dataset downloads. If we have a look back at our datasets object, we'll see there are 19 datasets listed. However, not all of them will be relevant to our malaria RDT questions. One approach is to head to the DHS website and have a look at the [DHS Recodes](https://dhsprogram.com/publications/publication-dhsg4-dhs-questionnaires-and-manuals.cfm), and look at the recodes that relate to the surveys. The other alternative is to download all the surveys and then query the variables within them. This is what we'll demonstrate here as it also demonstrates more of the package's functionality:

So first we will download all these datasets:

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
#>  - attr(*, "reformat")= logi FALSE
```

The function returns a list with a file path to where the downloaded dataset has been saved to. We can then read in one of these datasets:

``` r
# read in our dataset
cdpr <- readRDS(downloads$CDPR61SV)

# what is our cdpr object?
names(cdpr)
#> [1] "dataset"        "variable_names"
```

The dataset returned here is a list that contains the *dataset* but also a data.frame called *variable\_names*. This contains all the survey questions within the dataset, and what their survey variable is:

``` r
# let's look at the variable_names
head(cdpr$variable_names)
#>   variable                                                  description
#> 1     hhid                                          Case Identification
#> 2    hvidx                                                  Line number
#> 3    hv000                                       Country code and phase
#> 4    hv001                                               Cluster number
#> 5    hv002                                             Household number
#> 6    hv003 Respondent's line number (answering Household questionnaire)

# and then the dataset
class(cdpr$dataset)
#> [1] "tbl_df"     "tbl"        "data.frame"
str(cdpr$dataset$hv024)
#> Class 'labelled'  atomic [1:95949] 4 4 4 4 4 4 4 4 4 4 ...
#>   ..- attr(*, "label")= chr "Province"
#>   ..- attr(*, "format.spss")= chr "F2.0"
#>   ..- attr(*, "display_width")= int 7
#>   ..- attr(*, "labels")= Named num [1:11] 1 2 3 4 5 6 7 8 9 10 ...
#>   .. ..- attr(*, "names")= chr [1:11] "Kinshasa" "Bandundu" "Bas-Congo" "Equateur" ...
```

This is the default behaviour for the The client function `download_datasets` - it will download the datasets for you, and then by default save them in your client's root directory and then unzip them and read them in for you, and save the resultant data.frame as a .rds object within the client's root directory. You can control this behaviour using the `download_option` argument as such:

-   `client$download_datasets(download_option = "zip")` - Just the downloaded zip will be saved
-   `client$download_datasets(download_option = "rds")` - Just the read in rds will be saved
-   `client$download_datasets(download_option = "both")` - The zip is downloaded and saved as well as the read in rds

The other main reason for reading the dataset in straight away as the default option is that the created table of all the survey variables and their definitions is cached then and there, which then allows us to quickly query for particular search terms or survey variables:

``` r
# rapid diagnostic test search
questions <- client$survey_questions(datasets$FileName,search_terms = "malaria rapid test")

table(questions$dataset_filename)
#> 
#> CDHR61SV CDPR61SV TZHR7HSV TZPR7HSV 
#>       24        1       48        1
```

What we see from the questions is that the question "Result of malaria rapid test" appears in a few different datasets. This is because the household member recode datasets (CDPR61SV, TZPR7HSV) stores information about the children in a household, with one row per child, whereas the household recode (CDHR61SV, TZHR7HSV) stores information about the household, and thus flattens the information from each child into different subvariables (hml35$01/02 etc). As such it is easier to extract this information from the household member recodes. To do this we pass our questions object to the client function `extract`, which will create a list with each dataset and its extracted data as a data.frame. We also have the option to add any geographic data available, which will download the geographic data files for you and add this data to you resultant extract:

``` r
# let's just use the PR files thus
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "SV",fileType = "PR")
downloads <- client$download_datasets(datasets$FileName)

# and grab the questions from this again along with also questions detailing the province
questions <- client$survey_questions(datasets$FileName,search_terms = c("malaria rapid test"))

# and now extract the data
extract <- client$extract(questions,add_geo = TRUE)
#> Starting Survey 1 out of 2 surveys:CDPR61SV
#> Loading required package: sp
#> Warning: package 'sp' was built under R version 3.4.3
#> Starting Survey 2 out of 2 surveys:TZPR7HSV

# what does our extract look like
str(extract)
#> List of 2
#>  $ CDPR61SV:'data.frame':    95949 obs. of  7 variables:
#>   ..$ hml35   :Classes 'labelled_spss', 'labelled'  atomic [1:95949] NA NA NA NA NA NA NA 1 0 NA ...
#>   .. .. ..- attr(*, "label")= chr "Result of malaria rapid test"
#>   .. .. ..- attr(*, "na_values")= num 9
#>   .. .. ..- attr(*, "format.spss")= chr "F1.0"
#>   .. .. ..- attr(*, "display_width")= int 7
#>   .. .. ..- attr(*, "labels")= Named num [1:2] 0 1
#>   .. .. .. ..- attr(*, "names")= chr [1:2] "Negative" "Positive"
#>   ..$ CLUSTER : atomic [1:95949] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. ..- attr(*, "label")= chr "Cluster number"
#>   .. ..- attr(*, "format.spss")= chr "F8.0"
#>   .. ..- attr(*, "display_width")= int 10
#>   ..$ ALT_DEM : int [1:95949] 407 407 407 407 407 407 407 407 407 407 ...
#>   ..$ LATNUM  : num [1:95949] 0.22 0.22 0.22 0.22 0.22 ...
#>   ..$ LONGNUM : num [1:95949] 21.8 21.8 21.8 21.8 21.8 ...
#>   ..$ ADM1NAME: chr [1:95949] "Tshuapa" "Tshuapa" "Tshuapa" "Tshuapa" ...
#>   ..$ DHSREGNA: chr [1:95949] "Equateur" "Equateur" "Equateur" "Equateur" ...
#>  $ TZPR7HSV:'data.frame':    64880 obs. of  7 variables:
#>   ..$ hml35   :Classes 'labelled_spss', 'labelled'  atomic [1:64880] NA NA NA NA NA NA NA 0 NA NA ...
#>   .. .. ..- attr(*, "label")= chr "Result of malaria rapid test"
#>   .. .. ..- attr(*, "na_values")= num 9
#>   .. .. ..- attr(*, "format.spss")= chr "F1.0"
#>   .. .. ..- attr(*, "display_width")= int 7
#>   .. .. ..- attr(*, "labels")= Named num [1:2] 0 1
#>   .. .. .. ..- attr(*, "names")= chr [1:2] "Negative" "Positive"
#>   ..$ CLUSTER : atomic [1:64880] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. ..- attr(*, "label")= chr "Cluster number"
#>   .. ..- attr(*, "format.spss")= chr "F6.0"
#>   ..$ ALT_DEM : int [1:64880] 1236 1236 1236 1236 1236 1236 1236 1236 1236 1236 ...
#>   ..$ LATNUM  : num [1:64880] -4.73 -4.73 -4.73 -4.73 -4.73 ...
#>   ..$ LONGNUM : num [1:64880] 35.9 35.9 35.9 35.9 35.9 ...
#>   ..$ ADM1NAME: chr [1:64880] "Dodoma" "Dodoma" "Dodoma" "Dodoma" ...
#>   ..$ DHSREGNA: chr [1:64880] "Central" "Central" "Central" "Central" ...
```

The resultant extract is a list, with a new element for each different dataset that you have extracted. The responses from the dataset are by default stored as a *labelled* class from the [haven package](https://github.com/tidyverse/haven). This class preserves the original semantics and can easily be coerced to factors with `haven::as_factor()`. Special missing values are also preserved. For more info on the *labelled* class have a look at their github.

We can also query our datasets for the survey question variables. In the example above the survey question was *Result of malaria rapid test* and the variable was *hml35*. So if you knew the survey variables that you wanted (either by looking at the Recode file or by looking through the *variable\_names* included in the datasets) then we could search against these. So let's grab the regions using *hv024* using the client function `survey_variables()`:

``` r
# and grab the questions from this now utilising the survey variables
questions <- client$survey_variables(datasets$FileName,variables = c("hv024","hml35"))

# and now extract the data
extract2 <- client$extract(questions,add_geo = TRUE)
#> Starting Survey 1 out of 2 surveys:CDPR61SV
#> Starting Survey 2 out of 2 surveys:TZPR7HSV

# quick check
head(extract2$CDPR61SV)
#>   hv024 hml35 CLUSTER ALT_DEM   LATNUM  LONGNUM ADM1NAME DHSREGNA
#> 1     4    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> 2     4    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> 3     4    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> 4     4    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> 5     4    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> 6     4    NA       1     407 0.220128 21.79508  Tshuapa Equateur
head(extract2$TZPR7HSV)
#>   hv024 hml35 CLUSTER ALT_DEM    LATNUM  LONGNUM ADM1NAME DHSREGNA
#> 1     1    NA       1    1236 -4.726384 35.91285   Dodoma  Central
#> 2     1    NA       1    1236 -4.726384 35.91285   Dodoma  Central
#> 3     1    NA       1    1236 -4.726384 35.91285   Dodoma  Central
#> 4     1    NA       1    1236 -4.726384 35.91285   Dodoma  Central
#> 5     1    NA       1    1236 -4.726384 35.91285   Dodoma  Central
#> 6     1    NA       1    1236 -4.726384 35.91285   Dodoma  Central
```

We can now combine our two dataframes for further analysis using the `rdhs` package function `rbind_labelled()`. This function works specifically with our lists of labelled data.frames:

``` r
# first let's bind our first extraction, without the hv024
extract_bound <- rbind_labelled(extract)

head(extract_bound)
#>            hml35 CLUSTER ALT_DEM   LATNUM  LONGNUM ADM1NAME DHSREGNA
#> CDPR61SV.1    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> CDPR61SV.2    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> CDPR61SV.3    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> CDPR61SV.4    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> CDPR61SV.5    NA       1     407 0.220128 21.79508  Tshuapa Equateur
#> CDPR61SV.6    NA       1     407 0.220128 21.79508  Tshuapa Equateur

# now let's try our second extraction
extract2_bound <- rbind_labelled(extract2)
#> Warning in rbind_labelled(extract2): Some variables have non-matching value labels: hv024.
#> Inheriting labels from first data frame.
```

This hasn't quite done what we might want in the second instance. The *hv024* variable stores the regions for these 2 countries, which will not be the same and thus the labels will be different between the two of them. Without specifying any additional arguments `rbind_labelled()` will simply use the first data.frames labelling as the default, which will mean that some of the Tanzanian provinces will have been encoded as DRC provinces - not good! (This is a similar problem in nature to say trying to add new character strings to a factored data.frame).

There are a few work arounds. Firstly, we can specify a *labels* argument to the function which will detail how we should handle different variables. *labels* is a names list that specifies how to handle each variable. If we simply want to keep all the labels then we us the string "concatenate":

``` r
# lets try concatenating the hv024
better_bound <- rbind_labelled(extract2,labels = list("hv024"="concatenate"))

head(better_bound$hv024)
#> <Labelled integer>
#> [1] 6 6 6 6 6 6
#> 
#> Labels:
#>  value            label
#>      1           Arusha
#>      2         Bandundu
#>      3        Bas-Congo
#>      4    Dar Es Salaam
#>      5           Dodoma
#>      6         Equateur
#>      7            Geita
#>      8           Iringa
#>      9           Kagera
#>     10 Kasai-Occidental
#>     11   Kasai-Oriental
#>     12  Kaskazini Pemba
#>     13 Kaskazini Unguja
#>     14          Katanga
#>     15           Katavi
#>     16           Kigoma
#>     17      Kilimanjaro
#>     18         Kinshasa
#>     19     Kusini Pemba
#>     20    Kusini Unguja
#>     21            Lindi
#>     22          Maniema
#>     23          Manyara
#>     24             Mara
#>     25            Mbeya
#>     26  Mjini Magharibi
#>     27         Morogoro
#>     28           Mtwara
#>     29           Mwanza
#>     30           Njombe
#>     31        Nord-Kivu
#>     32        Orientale
#>     33            Pwani
#>     34            Rukwa
#>     35           Ruvuma
#>     36        Shinyanga
#>     37           Simiyu
#>     38          Singida
#>     39         Sud-Kivu
#>     40           Tabora
#>     41            Tanga
```

We could also specify new labels for a variable. For example, imagine the two datasets encoded their RDT responses differently, with the first one as `c("No","Yes")` and the other as `c("Negative","Positive")`. These would be for our purposes the same response, and so we could either leave it and all our results would use the `c("No","Yes")` labelling. But we may want to use the latter as it's more informative/correct, or we may want to be crystal clear and use `c("NegativeTest","PositiveTest")`. we can do that like this:

``` r
# lets try concatenating the hv024 and providing new labels
better_bound <- rbind_labelled(extract2,
                               labels = list("hv024"="concatenate",
                                             "hml35"=c("NegativeTest"=0,"PositiveTest"=1)))

# and our new label
head(better_bound$hml35)
#> <Labelled double>
#> [1] NA NA NA NA NA NA
#> 
#> Labels:
#>  value        label
#>      0 NegativeTest
#>      1 PositiveTest
```

The other option is to not use the labelled class at all. We can control this when we download our datasets, using the argument `reformat=TRUE`. This will ensure that no factors or labels are used and it is just the raw data:

``` r
# grab the questions but specifying the reformat argument
questions <- client$survey_variables(datasets$FileName,variables = c("hv024","hml35"),
                                     reformat=TRUE)

# and now extract the data
extract3 <- client$extract(questions,add_geo = TRUE)
#> Starting Survey 1 out of 2 surveys:CDPR61SV
#> Starting Survey 2 out of 2 surveys:TZPR7HSV

# group our results
bound_no_labels <- rbind_labelled(extract3)

# what does our hv024 look like now
table(bound_no_labels$hv024)
#> 
#>           Arusha         Bandundu        Bas-Congo    Dar Es Salaam 
#>             1883            12175             4596             2980 
#>           Dodoma         Equateur            Geita           Iringa 
#>             1878            14231             2859             1736 
#>           Kagera Kasai-Occidental   Kasai-Oriental  Kaskazini Pemba 
#>             2161             7593            11033             1847 
#> Kaskazini Unguja          Katanga           Katavi           Kigoma 
#>             1830            11827             2347             2465 
#>      Kilimanjaro         Kinshasa     Kusini Pemba    Kusini Unguja 
#>             1796             7156             1919             1709 
#>            Lindi          Maniema          Manyara             Mara 
#>             1821             4851             2222             2657 
#>            Mbeya  Mjini Magharibi         Morogoro           Mtwara 
#>             1748             2636             1798             1654 
#>           Mwanza           Njombe        Nord-Kivu        Orientale 
#>             2428             1691             5740            11360 
#>            Pwani            Rukwa           Ruvuma        Shinyanga 
#>             1702             2127             1886             2616 
#>           Simiyu          Singida         Sud-Kivu           Tabora 
#>             3254             2274             5387             2907 
#>            Tanga 
#>             2049
```

The *hv024* column in now just characters, which is possibly the best option depending on your downstream analysis/preferences. It's for this reason that the geographic data that is added is never turned into factors or labels.

------------------------------------------------------------------------

Further vignettes
-----------------

TODO: An example workflow using `rdhs` to calculate trends in anemia prevalence is available [here](INSERT%20LINK).

TODO: Full functionality is described in the tutorial [here](https://rawgit.com/OJWatson/rdhs/c33321a/vignettes/rdhs.html).

Motivation
----------

The Demographic and Health Surveys (DHS) Program has collected and disseminated population survey data from over 90 countries for over 30 years. In many countries, DHS provide the key data that mark progress towards targets such as the Sustainable Development Goals (SDGs) and inform health policy such as detailing trends in child mortality and characterising the distribution of use of insecticide-treated bed nets in Africa. Though standard health indicators are routinely published in survey final reports, much of the value of DHS is derived from the ability to download and analyse standardized microdata datasets for subgroup analysis, pooled multi-country analysis, and extended research studies. The suite of tools within `rdhs` hopes to extend the accessibility of these datasets to more researchers within the global health community, who are increasingly using R for their statistical analysis, and is the output of conversations with numerous research groups globally. The end result aims to increase the end user accessibility to the raw data and create a tool that supports reproducible global health research, as well as simplifying commonly required analytical pipelines.
