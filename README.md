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
#> Error in dhs_indicators(indicatorIds = "ML_PMAL_C_RDT", returnFields = c("IndicatorId", : could not find function "dhs_indicators"
dhs_data(countryIds = c("CD","TZ"), indicatorIds = "ML_PMAL_C_RDT", surveyYearStart = 2013,
       returnFields=c("Indicator", "SurveyId", "Value", "SurveyYearLabel", "CountryName"))
#> Error in dhs_data(countryIds = c("CD", "TZ"), indicatorIds = "ML_PMAL_C_RDT", : could not find function "dhs_data"
```

### Identify survey datasets

Now, obtain survey microdatasets to analyze these same indicators. Query the *surveyCharacteristics* endpoint to identify the survey characteristic ID for malaria RDT testing.


```r
## call with no arguments to return all characterstics
sc <- dhs_surveyCharacteristics()
#> Error in dhs_surveyCharacteristics(): could not find function "dhs_surveyCharacteristics"
sc[grepl("Malaria", sc$SurveyCharacteristicName), ]
#> Error in eval(expr, envir, enclos): object 'sc' not found
```

Use `dhs_surveys()` identify surveys for the countries and years of interest.


```r
## what are the countryIds - we can find that using this API request
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))
#> Error in dhs_countries(returnFields = c("CountryName", "DHS_CountryCode")): could not find function "dhs_countries"

## find all the surveys that match the search criteria
survs <- dhs_surveys(surveyCharacteristicIds = 89, countryIds = c("CD","TZ"), surveyYearStart = 2013)
#> Error in dhs_surveys(surveyCharacteristicIds = 89, countryIds = c("CD", : could not find function "dhs_surveys"
```
Lastly, identify the datasets required for download. By default, the recommended option is to download the flat file (.dat) datasets. The household member recode (`PR`) reports the RDT status for children under five.


```r
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")
#> Error in dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR"): could not find function "dhs_datasets"
str(datasets)
#> Error in str(datasets): object 'datasets' not found
```

### Download datasets

A DHS `client` will be used to log in to your DHS account, download datasets, and help query datasets for survey variables of interest. Create the client using the `client_dhs()` function. Establishing the client requires login credentials for the DHS website (email, password, and project name) and a directory to store cached datasets. (See introduction vignette for specific format for credentials).


```r
## create a client
client <- client_dhs(credentials = "credentials", root="~/Downloads/rdhs_cache")
#> Error in client_dhs(credentials = "credentials", root = "~/Downloads/rdhs_cache"): could not find function "client_dhs"
```

Note that the client can be provided as an argument to any of the API functions used above. This will cache the results of the API request, such that previous API requests (e.g. survey metadata) can be returned when working remotely or with poor internet connection.


```r
# before it's cached, provide the client so the results is cached within our client
s <- dhs_surveys(client = client)
#> Error in dhs_surveys(client = client): could not find function "dhs_surveys"

# after caching, results will be available instantly
s <- dhs_surveys(client = client)
#> Error in dhs_surveys(client = client): could not find function "dhs_surveys"
```

Download datasets by providing a list of desired dataset filenames.


```r
# download datasets
downloads <- client$get_datasets(datasets$FileName)
#> Error in eval(expr, envir, enclos): object 'client' not found

str(downloads)
#> Error in str(downloads): object 'downloads' not found
```

### Load datasets into R

The `get_datasets()` function returns a vector with a file path to the saved location of the downloaded datasets. These are read using `readRDS()`:


```r
# read in first dataset
cdpr <- readRDS(downloads["CDPR61FL"])
#> Error in readRDS(downloads["CDPR61FL"]): object 'downloads' not found
```

Value labels are stored as attributes to each of the columns of the data frame using the `labelled` class (see `haven::labelled` or our introduction vignette for more details). Variable labels are stored in the `label` attribute.

### Extract variables and pool datasets

The client also caches all variable labels to quickly query variables in each survey *without* loading the datasets.


```r
# rapid diagnostic test search
vars <- client$survey_questions(datasets$FileName, search_terms = "malaria rapid test")
#> Error in eval(expr, envir, enclos): object 'client' not found
```

Then extract these variables from the datasets. Optionally, geographic data may be added.


```r
# and now extract the data
extract <- client$extract(vars, add_geo = TRUE)
#> Error in eval(expr, envir, enclos): object 'client' not found
```

The returned object is a list of extracted datasets.

Dataset extracts can alternate be specified by providing a vector of surveys and vector of variable names:


```r
# and grab the questions from this now utilising the survey variables
vars <- client$survey_variables(datasets$FileName, variables = c("hv024","hml35"))
#> Error in eval(expr, envir, enclos): object 'client' not found

# and now extract the data
extract2 <- client$extract(vars, add_geo = TRUE)
#> Error in eval(expr, envir, enclos): object 'client' not found
```

Finally, the two datasets are pooled using the function `rbind_labelled()`. This function works specifically with our lists of labelled `data.frame`s. Labels are specified for each variable: for `hv024` all labels are retained (concatenate) but for `hml35` labels across both datasets to be "NegativeTest" and "PositiveTest".


```r
# now let's try our second extraction
extract2_bound <- rbind_labelled(extract2,
                                 labels = list("hv024" = "concatenate",
                                               "hml35" = c("NegativeTest"=0, "PositiveTest"=1)))
#> Error in rbind_labelled(extract2, labels = list(hv024 = "concatenate", : could not find function "rbind_labelled"
```


There is also an option to process downloaded datasets with labelled variables coded as strings, rather than labelled variables. This is specifed by the argument `reformat=TRUE`.


```r
# identify questions but specifying the reformat argument
questions <- client$survey_variables(datasets$FileName, variables = c("hv024", "hml35"),
                                     reformat=TRUE)
#> Error in eval(expr, envir, enclos): object 'client' not found

# and now extract the data
extract3 <- client$extract(questions, add_geo = TRUE)
#> Error in eval(expr, envir, enclos): object 'client' not found

# group our results
extract3_bound <- rbind_labelled(extract3)
#> Error in rbind_labelled(extract3): could not find function "rbind_labelled"

# our hv024 variable is now just character strings, so you can decide when/how to factor/label it later
str(extract3_bound)
#> Error in str(extract3_bound): object 'extract3_bound' not found
```
