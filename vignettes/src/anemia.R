#' ---
#' title: "Anemia prevalence among women: an `rdhs` example"
#' author: "OJ Watson, Jeff Eaton"
#' date: "`r Sys.Date()`"
#' output: pdf_document
#' #  html_document:
#' #    smart: false
#' vignette: >
#'   %\VignetteIndexEntry{Vignette Title}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---

#' Anemia is a common cause of fatigue, and women of childbearing age
#' are at particularly high risk for anemia. The package `rdhs` can be used
#' to compare estimates of the prevalence of any anemia among women from
#' Demographic and Health Surveys (DHS) conducted in Armenia, Cambodia,
#' and Lesotho.
#'
#' # Setup
#' Load the `rdhs` package and establish a `dhs_client`. *Note: hopefully in future it will not be required to establish `dhs_client` to query the API.*
## devtools::install_github("OJWatson/rdhs")
## library(rdhs)
devtools::load_all()
library(data.table)
library(ggplot2)
library(survey)
library(haven)

client <- rdhs::client(api_key = "ICLSPH-527168",
                           credentials = "~/Documents/Data/DHS/rdhs/credentials")


#' # Using calculated indicators from STATcompiler
#'
#' Anemia prevalence among women is reported as a core indicator through the DHS STATcompiler (https://www.statcompiler.com/).
#' These indicators can be accessed directly from R via the DHS API with the function `dhs_data()`.
#'
#' Query the API for a list of all StatCompiler indicators, and then search the indicators for those
#' that have `"anemia"` in the indicator name. API calls return `data.table` objects. If package `data.table`
#' is not loaded, they will be printed as `data.frame`s instead.
#'
#' *To do: wrapper function `dhs_indicators()` and search function*

indicators <- client$dhs_api_request(api_endpoint = "indicators")
tail(indicators[grepl("anemia", Label), .(IndicatorId, ShortName, Label)])

#' The indicator ID `"AN_ANEM_W_ANY"` provides the percentage of women with any anemia.
#' The function `dhs_data()` wiill query the indicator dataset for this indicator
#' for our three countries of interest. First, use `dhs_countries()` to query the
#' list of DHS countries to identify the DHS country code for each country.

countries <- client$dhs_api_request(api_endpoint = "countries")
dhscc <- countries[CountryName %in% c("Armenia", "Cambodia", "Lesotho"), DHS_CountryCode]
dhscc
#'
#' Now query the indicators dataset for the women with any anemia indicator for these three countries.
statcomp <- client$dhs_api_request(api_endpoint = "data",
                                   query = list(indicatorIds = "AN_ANEM_W_ANY",
                                                countryIds = dhscc))
statcomp[,.(Indicator, CountryName, SurveyYear, Value, DenominatorWeighted)]

##+ fig.height=2.5, fig.width=4, fig.align="center"
ggplot(statcomp, aes(SurveyYear, Value, col=CountryName)) +
  geom_point() + geom_line()


#' # Analyse DHS microdata
#'
#' ## Identify surveys that include anemia testing
#'
#' The DHS API provides the facility to filter surveys according to particular characteristics.
#' We first query the list of survey characteristics and identify the `SurveyCharacteristicID`
#' that indicates the survey included anemia testing. The first command below queries the API
#' for the full liest of survey characteristics, and the second uses `grepl()` to search
#' `SurveyCharacteristicName`s that include the word 'anemia'.

surveychar <- client$dhs_api_request(api_endpoint = "surveycharacteristics")
surveychar[grepl("anemia", SurveyCharacteristicName, ignore.case=TRUE)]

#' The `SurveyCharacteristicID = 41` indicates that the survey included anemia testing. Next we
#' query the API to identify the surveys that have this characteristic and were conducted
#' in our countries of interest.

surveys <- client$dhs_api_request(api_endpoint = "surveys",
                                  query = list(surveyCharacteristicIds = 41,
                                               countryIds = dhscc))

surveys[,.(SurveyId, CountryName, SurveyYear, NumberOfWomen, SurveyNum, FieldworkEnd)]

#' Finally, query the API identify the individual recode (IR) survey datasets for each of these surveys

datasets <- client$dhs_api_request(api_endpoint = "datasets",
                                   query = list(SurveyIds = surveys$SurveyId,
                                                fileType = "IR",
                                                fileFormat="flat"))
datasets[, .(SurveyId, SurveyNum, FileDateLastModified, FileName)]


#' ## Download datasets
#'
#' *In future, we will use the `dhs_client` to download and manage the IR datasets. For now, use
#' datasets saved in a local archive.*
#'
#' Confirm that all identified datasets are in the local archive.

all(tolower(datasets$FileName) %in% tolower(list.files("~/Documents/Data/DHS/flat")))

#' ## Identify survey variables
#'
#' Anemia is defined as having a hemoglobin (Hb) <12.0 g/dL for non-pregnant women
#' or Hb <11.0 g/dL for currently pregnant women^[https://www.measureevaluation.org/prh/rh_indicators/womens-health/womens-nutrition/percent-of-women-of-reproductive-age-with-anemia].
#' To calculate anemia prevalence from DHS microdata, we must identify the DHS recode
#' survey variables for hemoglobin measurement and pregnancy status. This could be
#' done by consulting the DHS recode manual or the .MAP files accompanying survey
#' datasets. It is convenient though to do this in R by loading the first
#' individual recode dataset and searching the metadata for the variable
#' names corresponding to the hemoglobin measurement and pregnancy status.

ir <- read_dhs_flat(file.path("~/Documents/Data/DHS/flat", datasets$FileName[1]))
head(grep("hemoglobin", sapply(ir, attr, "label"), value=TRUE, ignore.case=TRUE))

#' Variable `v042` records the household selection for hemoglobin testing.
#' Variable `v455` reports the outcome of hemoglobin measurement and `v456`
#' the result of altitude adjusted hemoglobin levels.
table(as_factor(ir$v042))
table(as_factor(ir$v455))
summary(ir$v456)

#' Variable `v454` reports the current pregnancy status used for determining the
#' anemia threshold.
grep("currently.*pregnant", sapply(ir, attr, "label"), value=TRUE, ignore.case=TRUE)
table(as_factor(ir$v454))

#' We also keep a number of other variables related to the survey design and potentially
#' interesting covariates: country code and phase (`v000`), cluster number (`v001`),
#' sample weight (`v005`), age (`v012`), region (`v024`), urban/rural residence (`v025`),
#' and education level (`v106`).

vars <- c("SurveyId", "CountryName", "SurveyYear", "v000", "v001", "v005",
          "v012", "v024", "v025", "v106", "v042", "v454", "v455", "v456")

#' ## Extract survey data

datlst <- list()

for(i in 1:nrow(datasets)){

  print(paste(i, datasets$SurveyId[i]))
  ir <- read_dhs_flat(file.path("~/Documents/Data/DHS/flat/", datasets$FileName[i]))

  ir$SurveyId <- datasets$SurveyId[i]
  ir$CountryName <- datasets$CountryName[i]
  ir$SurveyYear <- datasets$SurveyYear[i]

  datlst[[datasets$SurveyId[i]]] <- ir[vars]
}

#' We use `rbind_labelled()` to combine datasets with labelled columns. The argument
#' `labels` describes to combine variable levels for all datasets for `v024` (region)
#' while providing a consistent set of value labels to be used for `v454` (currently
#' pregnant) for all datasets.
#'
dat <- rbind_labelled(datlst,
                      labels = list(v024 = "concatenate",
                                    v454 = c("no/don't know" = 0L,
                                             "yes" = 1L, "missing" = 9L)))

sapply(dat, is.labelled)
dat$v456 <- zap_labels(dat$v456)
dat <- as_factor(dat)

#' ## Data tabulations
#'
#' It is a good idea to check basic tabulations of the data, especially by
#' survey to identify and nuances Exploratory analysis of variables
with(dat, table(SurveyId, v025, useNA="ifany"))
with(dat, table(SurveyId, v024, useNA="ifany"))
with(dat, table(SurveyId, v106, useNA="ifany"))
with(dat, table(SurveyId, v454, useNA="ifany"))
with(dat, table(SurveyId, v455, useNA="ifany"))

with(dat, table(v042, v454, useNA="ifany"))

#' ## Calculate anemia prevalence

#' Create indicator variable for 'any anemia'. The threshold depends on pregnancy status.
dat$v456[dat$v456 == 999] <- NA
with(dat, table(v455, is.na(v456)))
dat$anemia <- as.integer(dat$v456  < ifelse(dat$v454 == "yes", 110, 120))
dat$anemia_denom <- as.integer(!is.na(dat$anemia))

#' Specify survey design using the `survey` package.
dat$w <- dat$v005/1e6
des <- svydesign(~v001+SurveyId, data=dat, weights=~w)

anemia_prev <- svyby(~anemia, ~SurveyId, des, svyciprop, na.rm=TRUE, vartype="ci")
anemia_denom <- svyby(~anemia_denom, ~SurveyId, des, svytotal, na.rm=TRUE)

anemia_prev <- merge(anemia_prev, anemia_denom[c("SurveyId", "anemia_denom")])
res <- statcomp[,.(SurveyId, CountryName, SurveyYear, Value, DenominatorUnweighted, DenominatorWeighted)][anemia_prev, on="SurveyId"]

res$anemia <- 100*res$anemia
res$ci_l <- 100*res$ci_l
res$ci_u <- 100*res$ci_u
res$anemia_denom0 <- round(res$anemia_denom)

#' The table below compares the prevalence of any anemia calcuated from survey microdata
#' with the estimates from DHS StatCompiler and the weighted denominators for each
#' calculation. The estimates are identical for most cases. There are some small
#' differences to be ironed out.
knitr::kable(res[,.(CountryName, SurveyYear, Value, anemia, ci_l, ci_u, DenominatorWeighted, anemia_denom0)], digits=1)

##+ fig.height=2.5, fig.width=4, fig.align="center"
ggplot(res, aes(x=SurveyYear, y=anemia, ymin=ci_l, ymax=ci_u,
                col=CountryName, fill=CountryName)) +
  geom_ribbon(alpha=0.4, linetype="blank") + geom_point() + geom_line()



#'
#' # Regression analysis: relationship between education and anemia
#'
#' A key use of the survey microdata are to conduct secondary analysis of pooled data
#' from several surveys, such as regression analysis. Here we investigate the
#' relationship between anemia prevalence and education level (`v106`) for women using
#' logistic regression, adjusting for urban/rural (`v025`) and fixed effects for each
#' survey.

des <- update(des, v106 = relevel(v106, "primary"))
summary(svyglm(anemia ~ SurveyId + v025 + v106, des, family="binomial"))

#' The results suggest a strong gradient in lower anemia prevalence associated with higher education among women.
