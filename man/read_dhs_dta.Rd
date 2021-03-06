% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_dhs_dta.R
\name{read_dhs_dta}
\alias{read_dhs_dta}
\title{Read DHS Stata data set}
\usage{
read_dhs_dta(zfile, mode = "haven", all_lower = TRUE, ...)
}
\arguments{
\item{zfile}{Path to `.zip` file containing Stata dataset, usually ending
in filename `XXXXXXDT.zip`}

\item{mode}{Read mode for Stata `.dta` file. Defaults to "haven", see
'Details' for other options.}

\item{all_lower}{Logical indicating whether all value labels should be lower
case. Default to `TRUE`.}

\item{...}{Other arguments to be passed to \code{\link{read_zipdata}}.
Here this will be arguments to pass to either \code{\link[haven]{read_dta}}
or \code{\link[foreign]{read.dta}} depending on the mode provided}
}
\value{
A data frame. If mode = 'map', value labels for each variable are
  stored as the `labelled` class from `haven`.
}
\description{
This function reads a DHS recode dataset from the zipped Stata dataset.
By default (`mode = "haven"`), it reads in the stata data set using
\code{\link[haven]{read_dta}}
}
\details{
The default `mode="haven"` uses  \code{\link[haven]{read_dta}}
to read in the dataset. We have chosen this option as it is more consistent
with respect to variable labels and descriptions than others.
The other options either use use \code{\link[foreign]{read.dta}}
or they use the `.MAP` dictionary file provided with the DHS Stata datasets
to reconstruct the variable labels and value labels. In this case, value
labels are stored are stored using the the `labelled` class from `haven`.
See `?haven::labelled` for more information. Variable labels are stored in
the "label" attribute of each variable, the same as `haven::read_dta()`.

Currently, `mode="map"` is only implemented for 111
character fixed-width .MAP files, which comprises
the vast majority of recode data files from DHS Phases V,
VI, and VII and some from Phase IV. Parsers
for other .MAP formats will be added in future.

Other available modes read labels from the Stata dataset
with various options available in R:

* `mode="map"` uses the `.MAP` dictionary file provided with the DHS Stata
datasets to reconstruct the variable labels and value labels. In this case,
value labels are stored are stored using the the `labelled` class
from `haven`. See `?haven::labelled` for more information. Variable labels
are stored in the "label" attribute of each variable, the same as
`haven::read_dta()`.

* `mode="haven"`: use `haven::read_dta()` to read dataset.
This option retains the native value codings
with value labels affixed with the 'labelled' class.

* `mode="foreign"`: use `foreign::read.dta()`,
with default options convert.factors=TRUE to add
variable labels. Note that variable labels will
not be added if labels are not present for all
values, but variable labels are available via the "val.labels" attribute.

* `mode="foreignNA"`: use `foreign::read.dta(..., convert.factors=NA)`,
which converts any values without labels to 'NA'. This risks data loss
if labelling is incomplete in Stata datasets.

* `mode="raw"`: use `foreign::read.dta(..., convert.factors=FALSE)`,
which simply loads underlying value coding. Variable labels and value
labels are still available through dataset attributes (see examples).
}
\examples{
mrdt_zip <- tempfile()
download.file("https://dhsprogram.com/data/model_data/dhs/zzmr61dt.zip",
              mrdt_zip, mode="wb")

mr <- rdhs::read_dhs_dta(mrdt_zip,mode="map")
attr(mr$mv213, "label")
class(mr$mv213)
head(mr$mv213)
table(mr$mv213)
table(haven::as_factor(mr$mv213))

## If Stata file codebook is complete, `mode="map"` and `"haven"`
## should be the same.
mr_hav <- rdhs::read_dhs_dta(mrdt_zip, mode="haven")
attr(mr_hav$mv213, "label")
class(mr_hav$mv213)
head(mr_hav$mv213)  # "9=missing" omitted from .dta codebook
table(mr_hav$mv213)
table(haven::as_factor(mr_hav$mv213))

## Parsing codebook when using foreign::read.dta()
# foreign issues with duplicated factors
# Specifying foreignNA can help but often will not as below.
# Thus we would recommend either using mode = "haven" or mode = "raw"
\dontrun{
mr_for <- rdhs::read_dhs_dta(mrdt_zip, mode="foreign")
mr_for <- rdhs::read_dhs_dta(mrdt_zip, mode = "foreignNA")
}
## Don't convert factors
mr_raw <- rdhs::read_dhs_dta(mrdt_zip, mode="raw")
table(mr_raw$mv213)

}
\seealso{
\code{\link[foreign]{read.dta}}, \code{\link[haven]{labelled}},
  \code{\link[haven]{read_dta}}.

For more information on the DHS filetypes and contents of
distributed dataset .ZIP files, see
\url{https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10334}.
}
