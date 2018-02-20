# rdhs

[![Travis-CI Build Status](https://travis-ci.org/OJWatson/rdhs.png?branch=master)](https://travis-ci.org/OJWatson/rdhs)
[![codecov.io](https://codecov.io/github/OJWatson/rdhs/coverage.svg?branch=master)](https://codecov.io/github/OJWatson/rdhs?branch=master)


![]()<img src="img/logo.svg" width="50%">

### What is this?

rdhs is a package designed to assist in handling the [DHS database](www.dhsprogram.com). This inlcudes providing an
API client for the DHS API that allows DHS survey indicators to be queried. This allows desired indicators to be easily
found and passed onto subsequent functionality within rdhs for downloading datasets and conducting basic data munging tasks.

***
> To view the tutorial please click [here](https://cdn.rawgit.com/OJWatson/rdhs/vignettes/rdhs.html)

***

### Installing *rdhs*

To install the development version from github the package [*devtools*](https://github.com/hadley/devtools) is required.

```r
install.packages("devtools")
library(devtools)
```

Now we can install the pacakge from the github repository:

```r
devtools::install_github("OJWatson/rdhs")
library(rdhs)
```

***

#### Asking a question

For bug reports, feature requests, contributions, use github's [issue system.](https://github.com/OJWatson/rdhs/issues)
