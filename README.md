# rdhs

[![Travis-CI Build Status](https://travis-ci.org/OJWatson/rdhs.png?branch=mrc-branch)](https://travis-ci.org/OJWatson/rdhs)


![]()<img src="img/logo.svg" width="50%">

### What is this?

rdhs is a package designed to assist in handling the [DHS database](www.dhsprogram.com). This inlcudes providing an
API client for the DHS API that allows DHS survey indicators to be queried. This allows desired indicators to be easily
found and passed onto subsequent functionality within rdhs for downloading datasets and conducting basic data munging tasks.

***
> To view the tutorial please click [here](https://cdn.rawgit.com/OJWatson/rdhs/tutorials/package_tutorial/rdhs-package-tutorial.html)

***

### Installing *rdhs*

To install the development version from github the package [*devtools*](https://github.com/hadley/devtools) is required.

```r
install.packages("devtools")
library(devtools)
```
Once devtools is installed it is best to restart our R session. To do this either close RStudio or restart R (ctrl + shift + F10). Once your R session
has been restarted the package can be installed and loaded using:

```r
devtools::install_github("OJWatson/rdhs")
library(rdhs)
```

***

#### Asking a question

For bug reports, feature requests, contributions, use github's [issue system.](https://github.com/OJWatson/rdhs/issues)
