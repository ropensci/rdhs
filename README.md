# rdhs

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/OJWatson/rdhs.png?branch=master)](https://travis-ci.org/OJWatson/rdhs)
[![codecov.io](https://codecov.io/github/OJWatson/rdhs/coverage.svg?branch=master)](https://codecov.io/github/OJWatson/rdhs?branch=master)
[![Documentation via pkgdown](https://github.com/OJWatson/rdhs/raw/master/tools/pkgdownshield.png)](https://ojwatson.github.io/rdhs)

![](https://github.com/OJWatson/rdhs/raw/master/tools/logo.png)  

## Motivation

The Demographic and Health Surveys (DHS) Program has collected population survey data from over 90 countries for over 30 years. In many countries, DHS provide the key data that mark progress towards targets such as the Sustainable Development Goals (SDGs) and inform health policy. Though standard health indicators are routinely published in survey final reports, much of the value of DHS is derived from the ability to download and analyse standardized microdata datasets for subgroup analysis, pooled multi-country analysis, and extended research studies. The suite of tools within `rdhs` improves the accessibility of these datasets for statistical analysis with R, with aim to support reproducible global health research and simplify common analytical pipelines.

To be able to download survey datasets from the DHS website, you will need to set up an account with them to enable you to request access to the datasets. Instructions on how to do this can be found [here](https://dhsprogram.com/data/Access-Instructions.cfm). The email, password, and project name that were used to create the account will then need to be provided to `rdhs` when attempting to download datasets. 

---

`rdhs` is a package for management and analysis of [Demographic and Health Survey (DHS)](www.dhsprogram.com) data. This includes functionality to:

1. Access standard indicator data (i.e. [DHS STATcompiler](https://www.statcompiler.com/)) in R via the [DHS API](https://api.dhsprogram.com/).
1. Identify surveys and datasets relevant to a particular analysis.
1. Download survey datasets from the [DHS website](https://dhsprogram.com/data/available-datasets.cfm).
1. Load datasets and associated metadata into R.
1. Extract variables and combining datasets for pooled multi-survey analyses.

## Installation

Install `rdhs` from github with `devtools`. If you have not got a working development environment then please see the [toolkit vignette](https://ojwatson.github.io/rdhs/articles/toolkit.html)


```r
# install.packages("devtools")
# devtools::install_github("OJWatson/rdhs")
library(rdhs)
```

## Getting started

* Full functionality is described in the tutorial [here](https://ojwatson.github.io/rdhs/articles/introduction.html).

* An example workflow using `rdhs` to calculate trends in anemia prevalence is available [here](https://ojwatson.github.io/rdhs/articles/anemia.html).

* To run the full test suite (including tests for downloading datasets) see tutorial [here](https://ojwatson.github.io/rdhs/articles/testing.html).

## Basic Functionality

### Query the [DHS API](https://api.dhsprogram.com/).

Obtain survey estimates for Malaria prevalence among children from the Democratic Republic of Congo and Tanzania in the last 5 years (since 2013) that included rapid diagnostic tests (RDTs).


```r
dhs_indicators(indicatorIds = "ML_PMAL_C_RDT", returnFields=c("IndicatorId", "ShortName"))
#>                             ShortName   IndicatorId
#> 1 Malaria prevalence according to RDT ML_PMAL_C_RDT

dhs_data(countryIds = c("CD","TZ"), indicatorIds = "ML_PMAL_C_RDT", surveyYearStart = 2013,
       returnFields=c("Indicator", "SurveyId", "Value", "SurveyYearLabel", "CountryName"))
#>                             Indicator  SurveyId SurveyYearLabel Value
#> 1 Malaria prevalence according to RDT CD2013DHS         2013-14  30.8
#> 2 Malaria prevalence according to RDT TZ2015DHS         2015-16  14.4
#>                 CountryName
#> 1 Congo Democratic Republic
#> 2                  Tanzania
```

### Identify survey datasets

Now, obtain survey microdatasets to analyze these same indicators. Query the *surveyCharacteristics* endpoint to identify the survey characteristic ID for malaria RDT testing.


```r
## call with no arguments to return all characterstics
sc <- dhs_survey_characteristics()
sc[grepl("Malaria", sc$SurveyCharacteristicName), ]
#>    SurveyCharacteristicID SurveyCharacteristicName
#> 57                     96            Malaria - DBS
#> 58                     90     Malaria - Microscopy
#> 59                     89            Malaria - RDT
#> 60                     57          Malaria module 
#> 61                      8 Malaria/bednet questions
```

Use `dhs_surveys()` identify surveys for the countries and years of interest.


```r
## what are the countryIds - we can find that using this API request
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))

## find all the surveys that match the search criteria
survs <- dhs_surveys(surveyCharacteristicIds = 89, countryIds = c("CD","TZ"), surveyYearStart = 2013)
```

Lastly, identify the datasets required for download. By default, the recommended option is to download either the spss (.sav), `fileFormat = "SV"`, or the flat file (.dat), `fileFormat = "FL"` datasets. The flat is quicker, but there are still one or two very old datasets that don't read correctly, whereas the .sav files are slower to read in but so far no datasets have been found that don't read in correctly. The household member recode (`PR`) reports the RDT status for children under five.


```r
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")
str(datasets)
#> 'data.frame':	2 obs. of  13 variables:
#>  $ FileFormat          : chr  "Flat ASCII data (.dat)" "Flat ASCII data (.dat)"
#>  $ FileSize            : int  6595349 6622102
#>  $ DatasetType         : chr  "Survey Datasets" "Survey Datasets"
#>  $ SurveyNum           : int  421 485
#>  $ SurveyId            : chr  "CD2013DHS" "TZ2015DHS"
#>  $ FileType            : chr  "Household Member Recode" "Household Member Recode"
#>  $ FileDateLastModified: chr  "September, 19 2016 09:58:23" "August, 07 2018 17:36:25"
#>  $ SurveyYearLabel     : chr  "2013-14" "2015-16"
#>  $ SurveyType          : chr  "DHS" "DHS"
#>  $ SurveyYear          : int  2013 2015
#>  $ DHS_CountryCode     : chr  "CD" "TZ"
#>  $ FileName            : chr  "CDPR61FL.ZIP" "TZPR7AFL.ZIP"
#>  $ CountryName         : chr  "Congo Democratic Republic" "Tanzania"
```

### Download datasets

We can now go ahead and download our datasets. To be able to download survey datasets from the DHS website, you will need to set up an account with them to enable you to request access to the datasets. Instructions on how to do this can be found [here](https://dhsprogram.com/data/Access-Instructions.cfm). The email, password, and project name that were used to create the account will then need to be provided to `rdhs` when attempting to download datasets. 

Once we have created an account, we need to set up our credentials using the function `set_rdhs_config()`. This will require providing as arguments your `email` and `project` for which you want to download datasets from. You will then be prompted for your password.

You can also specify a directory for datasets and API calls to be cached to using `cache_path`. In order to comply with CRAN, this function will also ask you for your permission to write to files outside your temporary directory, and you must type out the filename for the `config_path` - "rdhs.json". (See [introduction vignette](https://ojwatson.github.io/rdhs/articles/introduction.html) for specific format for config, or `?set_rdhs_config`). 


```r
## login
set_rdhs_config(email = "rdhs.tester@gmail.com",
                project = "Testing Malaria Investigations",
                config_path = "rdhs.json",
                global=FALSE)
#> Writing your configuration to:
#>    -> rdhs.json
#> rdhs.json has been added to your .gitignore
```

The path to your config is saved between sessions so you only have to set this once. With your credentials set, all API requests will be cached within the `cache_path` directory provided so that these can be returned when working remotely or with a poor internet connection.


```r
# the first time this will take a few seconds 
microbenchmark::microbenchmark(dhs_datasets(surveyYearStart = 1977),times = 1)
#> Unit: seconds
#>                                  expr      min       lq     mean   median
#>  dhs_datasets(surveyYearStart = 1977) 4.215179 4.215179 4.215179 4.215179
#>        uq      max neval
#>  4.215179 4.215179     1

# after caching, results will be available instantly
microbenchmark::microbenchmark(dhs_datasets(surveyYearStart = 1977),times = 1)
#> Unit: milliseconds
#>                                  expr      min       lq     mean   median
#>  dhs_datasets(surveyYearStart = 1977) 2.358711 2.358711 2.358711 2.358711
#>        uq      max neval
#>  2.358711 2.358711     1
```

Now download datasets by providing a list of desired dataset filenames.


```r
# download datasets
downloads <- get_datasets(datasets$FileName)

str(downloads)
#> List of 2
#>  $ CDPR61FL: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/CDPR61FL.rds"
#>  $ TZPR7AFL: chr "C:\\Users\\Oliver\\AppData\\Local\\Oliver\\rdhs\\Cache/datasets/TZPR7AFL.rds"
#>  - attr(*, "reformat")= logi FALSE
```

### Load datasets into R

The `get_datasets()` function returns a vector with a file path to the saved location of the downloaded datasets. These are read using `readRDS()`:


```r
# read in first dataset
cdpr <- readRDS(downloads$CDPR61FL)
```

Value labels are stored as attributes to each of the columns of the data frame using the `labelled` class (see `haven::labelled` or our introduction vignette for more details). Variable labels are stored in the `label` attribute.

### Extract variables and pool datasets

The client also caches all variable labels to quickly query variables in each survey *without* loading the datasets.


```r
# rapid diagnostic test search
vars <- search_variable_labels(datasets$FileName, search_terms = "malaria rapid test")
```

Then extract these variables from the datasets. Optionally, geographic data may be added.


```r
# and now extract the data
extract <- extract_dhs(vars, add_geo = FALSE)
#> Starting Survey 1 out of 2 surveys:CDPR61FL
#> Starting Survey 2 out of 2 surveys:TZPR7AFL
```

The returned object is a list of extracted datasets.

Dataset extracts can alternate be specified by providing a vector of surveys and vector of variable names:


```r
# and grab the questions from this now utilising the survey variables
vars <- search_variables(datasets$FileName, variables = c("hv024","hml35"))

# and now extract the data
extract <- extract_dhs(vars, add_geo = FALSE)
#> Starting Survey 1 out of 2 surveys:CDPR61FL
#> Starting Survey 2 out of 2 surveys:TZPR7AFL
```

Finally, the two datasets are pooled using the function `rbind_labelled()`. This function works specifically with our lists of labelled `data.frame`s. Labels are specified for each variable: for `hv024` all labels are retained (concatenate) but for `hml35` labels across both datasets to be "Neg" and "Pos".


```r
# now let's try our second extraction
extract <- rbind_labelled(extract,
                          labels = list("hv024" = "concatenate",
                                        "hml35" = c("Neg"=0, "Pos"=1)))
```


There is also an option to process downloaded datasets with labelled variables coded as strings, rather than labelled variables. This is specified by the argument `reformat=TRUE`.


```r
# identify questions but specifying the reformat argument
questions <- search_variables(datasets$FileName, variables = c("hv024", "hml35"),
                                     reformat=TRUE)

# and now extract the data
extract <- extract_dhs(questions, add_geo = FALSE)
#> Starting Survey 1 out of 2 surveys:CDPR61FL
#> Starting Survey 2 out of 2 surveys:TZPR7AFL

# group our results
extract <- rbind_labelled(extract)

# our hv024 variable is now just character strings, so you can decide when/how to factor/label it later
str(extract)
#> Classes 'dhs_dataset' and 'data.frame':	160829 obs. of  4 variables:
#>  $ hv024   : chr  "equateur" "equateur" "equateur" "equateur" ...
#>   ..- attr(*, "label")= chr "Province"
#>  $ hml35   : chr  NA NA NA NA ...
#>   ..- attr(*, "label")= chr "Result of malaria rapid test"
#>  $ SurveyId: chr  "CD2013DHS" "CD2013DHS" "CD2013DHS" "CD2013DHS" ...
#>  $ DATASET : chr  "CDPR61FL" "CDPR61FL" "CDPR61FL" "CDPR61FL" ...
```
