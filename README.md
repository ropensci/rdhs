# rdhs
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/OJWatson/rdhs.png?branch=master)](https://travis-ci.org/OJWatson/rdhs)
[![codecov.io](https://codecov.io/github/OJWatson/rdhs/coverage.svg?branch=master)](https://codecov.io/github/OJWatson/rdhs?branch=master)


![]()<img src="img/logo.svg" width="50%">

### What is this?

`rdhs` is a package for management and analysis of [Demographic and Health Survey (DHS)](www.dhsprogram.com) data. This includes functionality to:

1. Access standard indicator data (i.e. [DHS STATcompiler](https://www.statcompiler.com/)) in R via the [DHS API](https://api.dhsprogram.com/).
1. Identify surveys and datasets relevant to a particular analysis.
1. Download survey datasets from the [DHS website](https://dhsprogram.com/data/available-datasets.cfm).
1. Load datasets and associated metadata into R.
1. Extract variables and combining datasets for pooled multi-survey analyses.

*** 

### Getting started

Download and install development version of `rdhs` from github using [*devtools*](https://github.com/hadley/devtools).

```r
install.packages("devtools")
devtools::install_github("OJWatson/rdhs")
library(rdhs)
```

An example workflow using `rdhs` to calculate trends in anemia prevalence is avaiable [here](INSERT LINK).

Full functionality is described in the tutorial [here](https://rawgit.com/OJWatson/rdhs/c33321a/vignettes/rdhs.html).

***

### Basic functionality

* TODO: Illustrate very basic functionalities using DHS model datasetes *

***

#### Asking a question

For bug reports, feature requests, contributions, use github's [issue system.](https://github.com/OJWatson/rdhs/issues)
