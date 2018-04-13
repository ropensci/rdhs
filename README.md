---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/OJWatson/rdhs.png?branch=master)](https://travis-ci.org/OJWatson/rdhs)
[![codecov.io](https://codecov.io/github/OJWatson/rdhs/coverage.svg?branch=master)](https://codecov.io/github/OJWatson/rdhs?branch=master)

`rdhs` is a package for management and analysis of [Demographic and Health Survey (DHS)](www.dhsprogram.com) data. This includes functionality to:

1. Access standard indicator data (i.e. [DHS STATcompiler](https://www.statcompiler.com/)) in R via the [DHS API](https://api.dhsprogram.com/).
1. Identify surveys and datasets relevant to a particular analysis.
1. Download survey datasets from the [DHS website](https://dhsprogram.com/data/available-datasets.cfm).
1. Load datasets and associated metadata into R.
1. Extract variables and combining datasets for pooled multi-survey analyses.

---

## Motivation

The Demographic and Health Surveys (DHS) Program has collected population survey data from over 90 countries for over 30 years. In many countries, DHS provide the key data that mark progress towards targets such as the Sustainable Development Goals (SDGs) and inform health policy. Though standard health indicators are routinely published in survey final reports, much of the value of DHS is derived from the ability to download and analyse standardized microdata datasets for subgroup analysis, pooled multi-country analysis, and extended research studies. The suite of tools within `rdhs` improves the accessibility of these datasets for statistical analysis with R, with aim to support reproducible global health research and simplify common analytical pipelines.

## Installation

Install `rdhs` from github with `devtools`:


```r
# install.packages("devtools")
devtools::install_github("OJWatson/rdhs")
```

## Getting started

TODO: An example workflow using `rdhs` to calculate trends in anemia prevalence is available [here](INSERT LINK).

TODO: Full functionality is described in the tutorial [here](INSERT LINK).

## Basic Functionality

### Query the [DHS API](https://api.dhsprogram.com/).

Obtain survey esimates for Malaria prevalence among children from the Democratic Republic of Congo and Tanzania in the last 5 years (since 2013) that included rapid diagnostic tests (RDTs).


```r
dhs_indicators(indicatorIds = "ML_PMAL_C_RDT", returnFields=c("IndicatorId", "ShortName"))
#>                              ShortName   IndicatorId
#> 1: Malaria prevalence according to RDT ML_PMAL_C_RDT
dhs_data(countryIds = c("CD","TZ"), indicatorIds = "ML_PMAL_C_RDT", surveyYearStart = 2013,
       returnFields=c("Indicator", "SurveyId", "Value", "SurveyYearLabel", "CountryName"))
#>                              Indicator  SurveyId SurveyYearLabel Value
#> 1: Malaria prevalence according to RDT CD2013DHS         2013-14  30.8
#> 2: Malaria prevalence according to RDT TZ2015DHS         2015-16  14.4
#>                  CountryName
#> 1: Congo Democratic Republic
#> 2:                  Tanzania
```

### Identify survey datasets

Now, obtain survey microdatasets to analyze these same indicators. Query the *surveyCharacteristics* endpoint to identify the survey characteristic ID for malaria RDT testing.


```r
## call with no arguments to return all characterstics
sc <- dhs_surveyCharacteristics()
sc[grepl("Malaria", sc$SurveyCharacteristicName), ]
#>    SurveyCharacteristicID SurveyCharacteristicName
#> 1:                     96            Malaria - DBS
#> 2:                     90     Malaria - Microscopy
#> 3:                     89            Malaria - RDT
#> 4:                     57          Malaria module 
#> 5:                      8 Malaria/bednet questions
```

Use `dhs_surveys()` identify surveys for the countries and years of interest.


```r
## what are the countryIds - we can find that using this API request
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))

## find all the surveys that match the search criteria
survs <- dhs_surveys(surveyCharacteristicIds = 89, countryIds = c("CD","TZ"), surveyYearStart = 2013)
```
Lastly, identify the datasets required for download. By default, the recommended option is to download the flat file (.dat) datasets. The household member recode (`PR`) reports the RDT status for children under five.


```r
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")
str(datasets)
#> Classes 'data.table' and 'data.frame':	2 obs. of  13 variables:
#>  $ FileFormat          : chr  "Flat ASCII data (.dat)" "Flat ASCII data (.dat)"
#>  $ FileSize            : int  6595349 6622108
#>  $ DatasetType         : chr  "Survey Datasets" "Survey Datasets"
#>  $ SurveyNum           : int  421 485
#>  $ SurveyId            : chr  "CD2013DHS" "TZ2015DHS"
#>  $ FileType            : chr  "Household Member Recode" "Household Member Recode"
#>  $ FileDateLastModified: chr  "September, 19 2016 09:58:23" "December, 09 2016 14:10:59"
#>  $ SurveyYearLabel     : chr  "2013-14" "2015-16"
#>  $ SurveyType          : chr  "DHS" "DHS"
#>  $ SurveyYear          : int  2013 2015
#>  $ DHS_CountryCode     : chr  "CD" "TZ"
#>  $ FileName            : chr  "CDPR61FL.ZIP" "TZPR7HFL.ZIP"
#>  $ CountryName         : chr  "Congo Democratic Republic" "Tanzania"
#>  - attr(*, ".internal.selfref")=<externalptr>
```

### Download datasets

A DHS `client` will be used to log in to your DHS account, download datasets, and help query datasets for survey variables of interest. Create the client using the `dhs_client()` function. Establishing the client requires login credentials for the DHS website (email, password, and project name) and a directory to store cached datasets. (See introduction vignette for specific format for credentials).


```r
## create a client
client <- dhs_client(credentials = "credentials", root="~/Downloads/rdhs_cache")
```

Note that the client can be provided as an argument to any of the API functions used above. This will cache the results of the API request, such that previous API requests (e.g. survey metadata) can be returned when working remotely or with poor internet connection.


```r
# before it's cached, provide the client so the results is cached within our client
s <- dhs_surveys(client = client)

# after caching, results will be available instantly
s <- dhs_surveys(client = client)
```

Download datasets by providing a list of desired dataset filenames.


```r
# download datasets
downloads <- client$get_datasets(datasets$FileName)

str(downloads)
#> List of 2
#>  $ CDPR61FL: chr "~/Downloads/rdhs_cache/datasets/CDPR61FL.rds"
#>  $ TZPR7HFL: chr "~/Downloads/rdhs_cache/datasets/TZPR7HFL.rds"
#>  - attr(*, "reformat")= logi FALSE
```

### Load datasets into R

The `get_datasets()` function returns a vector with a file path to the saved location of the downloaded datasets. These are read using `readRDS()`:


```r
# read in first dataset
cdpr <- readRDS(downloads["CDPR61FL"])
#> Error in readRDS(downloads["CDPR61FL"]): bad 'file' argument
```

Value labels are stored as attributes to each of the columns of the data frame using the `labelled` class (see `haven::labelled` or our introduction vignette for more details). Variable labels are stored in the `label` attribute.

### Extract variables and pool datasets

The client also caches all variable labels to quickly query variables in each survey *without* loading the datasets.


```r
# rapid diagnostic test search
vars <- client$survey_questions(datasets$FileName, search_terms = "malaria rapid test")
```

Then extract these variables from the datasets. Optionally, geographic data may be added.


```r
# and now extract the data
extract <- client$extract(vars, add_geo = TRUE)
#> Starting Survey 1 out of 2 surveys:CDPR61FL
#> Starting Survey 2 out of 2 surveys:TZPR7HFL
```

The returned object is a list of extracted datasets.

Dataset extracts can alternate be specified by providing a vector of surveys and vector of variable names:


```r
# and grab the questions from this now utilising the survey variables
vars <- client$survey_variables(datasets$FileName, variables = c("hv024","hml35"))

# and now extract the data
extract2 <- client$extract(vars, add_geo = TRUE)
#> Starting Survey 1 out of 2 surveys:CDPR61FL
#> Starting Survey 2 out of 2 surveys:TZPR7HFL
```

Finally, the two datasets are pooled using the function `rbind_labelled()`. This function works specifically with our lists of labelled `data.frame`s. Labels are specified for each variable: for `hv024` all labels are retained (concatenate) but for `hml35` labels across both datasets to be "NegativeTest" and "PositiveTest".


```r
# now let's try our second extraction
extract2_bound <- rbind_labelled(extract2,
                                 labels = list("hv024" = "concatenate",
                                               "hml35" = c("NegativeTest"=0, "PositiveTest"=1)))
```
