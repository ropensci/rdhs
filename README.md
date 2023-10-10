
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdhs <img src="https://raw.githubusercontent.com/ropensci/rdhs/main/tools/logo.png" align="right" style="padding-left:10px;background-color:white;" />

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/ropensci/rdhs/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/rdhs/actions)
[![codecov.io](https://codecov.io/github/ropensci/rdhs/coverage.svg?branch=main)](https://app.codecov.io/github/ropensci/rdhs?branch=main)
[![Documentation via
pkgdown](https://github.com/ropensci/rdhs/raw/main/tools/pkgdownshield.png)](https://docs.ropensci.org/rdhs/)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/rdhs)](https://cran.r-project.org/package=rdhs)
[![Downloads from Rstudio
mirror](https://cranlogs.r-pkg.org/badges/grand-total/rdhs)](https://www.r-pkg.org:443/pkg/rdhs)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/rdhs)](https://cran.r-project.org/package=rdhs)
[![rOpenSci](https://badges.ropensci.org/238_status.svg)](https://github.com/ropensci/software-review/issues/238)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2423635.svg)](https://doi.org/10.5281/zenodo.2423635)

## Motivation

The Demographic and Health Surveys (DHS) Program has collected
population survey data from over 90 countries for over 30 years. In many
countries, DHS provide the key data that mark progress towards targets
such as the Sustainable Development Goals (SDGs) and inform health
policy. Though standard health indicators are routinely published in
survey final reports, much of the value of DHS is derived from the
ability to download and analyse standardized microdata datasets for
subgroup analysis, pooled multi-country analysis, and extended research
studies. The suite of tools within `rdhs` improves the accessibility of
these datasets for statistical analysis with R, with aim to support
reproducible global health research and simplify common analytical
pipelines.

> For questions regarding how to analyse DHS survey data, please read
> the DHS website’s data section first. If you have any questions after
> this then please create an
> [issue](https://github.com/ropensci/rdhs/issues) with your question.
> It is really likely that your question will help other people and so
> posting them publically as an issue may help others with similar
> questions.

-----

`rdhs` is a package for management and analysis of [Demographic and
Health Survey (DHS)](https://www.dhsprogram.com) data. This includes
functionality to:

1.  Access standard indicator data (i.e. [DHS
    STATcompiler](https://www.statcompiler.com/)) in R via the [DHS
    API](https://api.dhsprogram.com/).
2.  Identify surveys and datasets relevant to a particular analysis.
3.  Download survey datasets from the [DHS
    website](https://dhsprogram.com/data/available-datasets.cfm).
4.  Load datasets and associated metadata into R.
5.  Extract variables and combining datasets for pooled multi-survey
    analyses.

## Installation

You can install the latest version from
[`CRAN`](https://cran.r-project.org/package=rdhs) using:

``` r
install.packages("rdhs")
```

You can also install the development version of `rdhs` with the latest
patches from github with:

``` r
#install.packages("devtools")
devtools::install_github("ropensci/rdhs")
```

``` r
# Load the package
library(rdhs)
```

## Getting started

To be able to **download survey datasets from the DHS website**, you
will need to **set up an account with the DHS website**, which will
enable you to request access to the datasets. Instructions on how to do
this can be found
[here](https://dhsprogram.com/data/Access-Instructions.cfm). The email,
password, and project name that were used to create the account will
then need to be provided to `rdhs` when attempting to download datasets.

-----

  - Request dataset access from the DHS website
    [here](https://dhsprogram.com/data/Access-Instructions.cfm).

  - Full functionality is described in the tutorial
    [here](https://docs.ropensci.org/rdhs/articles/introduction.html).

  - An example workflow using `rdhs` to calculate trends in anemia
    prevalence is available
    [here](https://docs.ropensci.org/rdhs/articles/anemia.html).

## Basic Functionality

### Query the [DHS API](https://api.dhsprogram.com/).

Obtain survey estimates for Malaria prevalence among children from the
Democratic Republic of Congo and Tanzania in the last 5 years (since
2013) that included rapid diagnostic tests
(RDTs).

``` r
dhs_indicators(indicatorIds = "ML_PMAL_C_RDT", returnFields=c("IndicatorId", "ShortName"))
#>                             ShortName   IndicatorId
#> 1 Malaria prevalence according to RDT ML_PMAL_C_RDT

dhs_data(countryIds = c("CD","TZ"), indicatorIds = "ML_PMAL_C_RDT", surveyYearStart = 2013,
       returnFields=c("Indicator", "SurveyId", "Value", "SurveyYearLabel", "CountryName"))
#>                             Indicator  SurveyId SurveyYearLabel Value
#> 1 Malaria prevalence according to RDT CD2013DHS         2013-14  30.8
#> 2 Malaria prevalence according to RDT TZ2015DHS         2015-16  14.4
#> 3 Malaria prevalence according to RDT TZ2017MIS            2017   7.3
#>                 CountryName
#> 1 Congo Democratic Republic
#> 2                  Tanzania
#> 3                  Tanzania
```

### Identify survey datasets

Now, obtain survey microdatasets to analyze these same indicators. Query
the *surveyCharacteristics* endpoint to identify the survey
characteristic ID for malaria RDT testing.

``` r
## call with no arguments to return all characterstics
sc <- dhs_survey_characteristics()
sc[grepl("Malaria", sc$SurveyCharacteristicName), ]
#>    SurveyCharacteristicID SurveyCharacteristicName
#> 58                     96            Malaria - DBS
#> 59                     90     Malaria - Microscopy
#> 60                     89            Malaria - RDT
#> 61                     57 Malaria bednet inventory
```

Use `dhs_surveys()` identify surveys for the countries and years of
interest.

``` r
## what are the countryIds - we can find that using this API request
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))

## find all the surveys that match the search criteria
survs <- dhs_surveys(surveyCharacteristicIds = 89, countryIds = c("CD","TZ"), surveyYearStart = 2013)
```

Lastly, identify the datasets required for download. By default, the
recommended option is to download either the spss (.sav), `fileFormat =
"SV"`, or the flat file (.dat), `fileFormat = "FL"` datasets. The flat
is quicker, but there are still one or two very old datasets that don’t
read correctly, whereas the .sav files are slower to read in but so far
no datasets have been found that don’t read in correctly. The household
member recode (`PR`) reports the RDT status for children under
five.

``` r
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")
str(datasets)
#> 'data.frame':    3 obs. of  13 variables:
#>  $ FileFormat          : chr  "Flat ASCII data (.dat)" "Flat ASCII data (.dat)" "Flat ASCII data (.dat)"
#>  $ FileSize            : int  6595349 6491292 2171918
#>  $ DatasetType         : chr  "Survey Datasets" "Survey Datasets" "Survey Datasets"
#>  $ SurveyNum           : int  421 485 529
#>  $ SurveyId            : chr  "CD2013DHS" "TZ2015DHS" "TZ2017MIS"
#>  $ FileType            : chr  "Household Member Recode" "Household Member Recode" "Household Member Recode"
#>  $ FileDateLastModified: chr  "September, 19 2016 09:58:23" "September, 28 2019 17:58:28" "June, 11 2019 15:38:22"
#>  $ SurveyType          : chr  "DHS" "DHS" "MIS"
#>  $ SurveyYearLabel     : chr  "2013-14" "2015-16" "2017"
#>  $ SurveyYear          : chr  "2013" "2015" "2017"
#>  $ DHS_CountryCode     : chr  "CD" "TZ" "TZ"
#>  $ FileName            : chr  "CDPR61FL.ZIP" "TZPR7BFL.ZIP" "TZPR7IFL.ZIP"
#>  $ CountryName         : chr  "Congo Democratic Republic" "Tanzania" "Tanzania"
```

### Download datasets

We can now go ahead and download our datasets. To be able to download
survey datasets from the DHS website, you will need to set up an account
with them to enable you to request access to the datasets. Instructions
on how to do this can be found
[here](https://dhsprogram.com/data/Access-Instructions.cfm). The email,
password, and project name that were used to create the account will
then need to be provided to `rdhs` when attempting to download datasets.

Once we have created an account, we need to set up our credentials using
the function `set_rdhs_config()`. This will require providing as
arguments your `email` and `project` for which you want to download
datasets from. You will then be prompted for your password.

You can also specify a directory for datasets and API calls to be cached
to using `cache_path`. In order to comply with CRAN, this function will
also ask you for your permission to write to files outside your
temporary directory, and you must type out the filename for the
`config_path` - “rdhs.json”. (See [introduction
vignette](https://docs.ropensci.org/rdhs/articles/introduction.html) for
specific format for config, or `?set_rdhs_config`).

``` r
## login
set_rdhs_config(email = "rdhs.tester@gmail.com",
                project = "rdhs R package development",
                config_path = "rdhs.json",
                global = FALSE)
#> Writing your configuration to:
#>    -> rdhs.json
```

The path to your config is saved between sessions so you only have to
set this once. With your credentials set, all API requests will be
cached within the `cache_path` directory provided so that these can be
returned when working remotely or with a poor internet connection.

``` r
# the first time this will take a few seconds 
microbenchmark::microbenchmark(dhs_datasets(surveyYearStart = 1986),times = 1)
#> Unit: milliseconds
#>                                  expr     min      lq    mean  median      uq
#>  dhs_datasets(surveyYearStart = 1986) 46.3744 46.3744 46.3744 46.3744 46.3744
#>      max neval
#>  46.3744     1

# after caching, results will be available instantly
microbenchmark::microbenchmark(dhs_datasets(surveyYearStart = 1986),times = 1)
#> Unit: milliseconds
#>                                  expr      min       lq     mean   median
#>  dhs_datasets(surveyYearStart = 1986) 1.410894 1.410894 1.410894 1.410894
#>        uq      max neval
#>  1.410894 1.410894     1
```

Now download datasets by providing a list of desired dataset filenames.

``` r
# download datasets
downloads <- get_datasets(datasets$FileName)

str(downloads)
#> List of 3
#>  $ CDPR61FL: chr "/home/oj/.cache/rdhs/datasets/CDPR61FL.rds"
#>  $ TZPR7BFL: chr "/home/oj/.cache/rdhs/datasets/TZPR7BFL.rds"
#>  $ TZPR7IFL: chr "/home/oj/.cache/rdhs/datasets/TZPR7IFL.rds"
#>  - attr(*, "reformat")= logi FALSE
```

### Load datasets into R

The `get_datasets()` function returns a vector with a file path to the
saved location of the downloaded datasets. These are read using
`readRDS()`:

``` r
# read in first dataset
cdpr <- readRDS(downloads$CDPR61FL)
```

Value labels are stored as attributes to each of the columns of the data
frame using the `labelled` class (see `haven::labelled` or our
introduction vignette for more details). Variable labels are stored in
the `label` attribute.

### Extract variables and pool datasets

The client also caches all variable labels to quickly query variables in
each survey *without* loading the datasets.

``` r
# rapid diagnostic test search
vars <- search_variable_labels(datasets$FileName, search_terms = "malaria rapid test")
```

Then extract these variables from the datasets. Optionally, geographic
data may be added.

``` r
# and now extract the data
extract <- extract_dhs(vars, add_geo = FALSE)
#> Starting Survey 1 out of 3 surveys:CDPR61FL
#> Starting Survey 2 out of 3 surveys:TZPR7BFL
#> Starting Survey 3 out of 3 surveys:TZPR7IFL
```

The returned object is a list of extracted datasets.

Dataset extracts can alternate be specified by providing a vector of
surveys and vector of variable names:

``` r
# and grab the questions from this now utilising the survey variables
vars <- search_variables(datasets$FileName, variables = c("hv024","hml35"))

# and now extract the data
extract <- extract_dhs(vars, add_geo = FALSE)
#> Starting Survey 1 out of 3 surveys:CDPR61FL
#> Starting Survey 2 out of 3 surveys:TZPR7BFL
#> Starting Survey 3 out of 3 surveys:TZPR7IFL
```

Finally, the two datasets are pooled using the function
`rbind_labelled()`. This function works specifically with our lists of
labelled `data.frame`s. Labels are specified for each variable: for
`hv024` all labels are retained (concatenate) but for `hml35` labels
across both datasets to be “Neg” and “Pos”.

``` r
# now let's try our second extraction
extract <- rbind_labelled(extract,
                          labels = list("hv024" = "concatenate",
                                        "hml35" = c("Neg"=0, "Pos"=1)))
```

There is also an option to process downloaded datasets with labelled
variables coded as strings, rather than labelled variables. This is
specified by the argument `reformat=TRUE`.

``` r
# identify questions but specifying the reformat argument
questions <- search_variables(datasets$FileName, variables = c("hv024", "hml35"),
                                     reformat=TRUE)

# and now extract the data
extract <- extract_dhs(questions, add_geo = FALSE)
#> Starting Survey 1 out of 3 surveys:CDPR61FL
#> Starting Survey 2 out of 3 surveys:TZPR7BFL
#> Starting Survey 3 out of 3 surveys:TZPR7IFL

# group our results
extract <- rbind_labelled(extract)

# our hv024 variable is now just character strings, so you can decide when/how to factor/label it later
str(extract)
#> Classes 'dhs_dataset' and 'data.frame':  208595 obs. of  4 variables:
#>  $ hv024   : chr  "equateur" "equateur" "equateur" "equateur" ...
#>   ..- attr(*, "label")= chr "Province"
#>  $ hml35   : chr  NA NA NA NA ...
#>   ..- attr(*, "label")= chr "Result of malaria rapid test"
#>  $ SurveyId: chr  "CD2013DHS" "CD2013DHS" "CD2013DHS" "CD2013DHS" ...
#>  $ DATASET : chr  "CDPR61FL" "CDPR61FL" "CDPR61FL" "CDPR61FL" ...
```

-----
