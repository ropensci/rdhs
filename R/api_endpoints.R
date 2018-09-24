#' API request of DHS Indicator Data
#'
#' @title API request of DHS Indicator Data
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE
#' @param countryIds Specify a comma separated list of country ids to filter
#'   by. For a list of countries use
#' \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
#' @param indicatorIds Specify a comma separated list of indicator ids to
#'   filter by. For a list of indicators use
#' \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
#' @param surveyIds Specify a comma separated list of survey ids to filter by.
#'   For a list of surveys use
#' \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param selectSurveys Specify to filter Data from the latest survey by adding
#'   `selectSurveys="latest"` in conjunction with a Country Code and/or Survey
#'   Type. Please Note: Not all indicators are present in the latest surveys.
#'   To filter your API Indicator Data call to return the latest survey data in
#'   which a specific set of indicators is present, add
#'   `selectSurveys="byIndicator"` in conjunction with Indicator IDs, Country
#'   Code, and/or Survey Type instead
#'   of using `selectSurveys="latest"`.
#' @param surveyYear Specify a comma separated list of survey years to
#'   filter by.
#' @param surveyYearStart Specify a range of Survey Years to filter Data on.
#'   surveyYearStart is an inclusive value. Can be used alone or in conjunction
#'   with surveyYearEnd.
#' @param surveyYearEnd Specify a range of Survey Years to filter Data on.
#'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
#'   with surveyYearStart.
#' @param surveyType Specify a survey type to filter by.
#' @param surveyCharacteristicIds Specify a survey characteristic id to filter
#'   data on surveys with the specified survey characteristic. For a list of
#'   survey characteristics use
#' \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param characteristicCategory Specify a survey characteristic category to
#'   filter data on surveys with the specified survey characteristic category.
#'   This query is case insensitive, but it only recognizes exact phrase
#'   matches. For example, `characteristicCategory="wealth"` will return
#'   results that have a characteristic category of `Wealth` while
#'   `characteristicCategory="wealth quintile"' will return results that have
#'   a characteristic category of `Wealth Quintile`.
#' @param characteristicLabel Specify a survey characteristic category to
#'   filter data on surveys with the specified survey characteristic category.
#'   This query is case insensitive, but it only recognizes exact phrase
#'   matches. You can use characteristicLabel on its own or in conjunction with
#'   characteristicCategory.
#' @param tagIds Specify a tag id to filter data on indicators with the
#'   specified tag. For a list of tags use \code{dhs_tags()}
#' @param breakdown Data can be requested at different levels via the breakdown
#'   parameter. By default national data is returned and provides totals on a
#'   national level. `breakdown="subnational"` data provides values on a
#'   subnational level. `breakdown="background"` provides totals on categorized
#'   basis. Examples are urban/rural, education and wealth level.
#'   `breakdown="all"` provides all the data including disaggregated data.
#' @param returnGeometry Coordinates can be requested from the API by including
#'   `returnGeometry=TRUE` in your request. The default for this value is
#'   false.
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format is
#'   JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request.
#'   By default the API will return page 1.
#'
#' @return Returns a `data.table` of 27 (or less if `returnFields` is provided)
#'   data for your particular query. Details of properties returned with each
#'   row of data are provided at
#' \url{https://api.dhsprogram.com/rest/dhs/data/fields}
#'
#' @export
#' @examples
#'
#' # A common use for the indicator data API will be to search for a specific
#' # health indicator for a given country. For example to return the total
#' # malaria prevalence according to RDT, given by the indicator ML_PMAL_C_RDT,
#' # in Senegal since 2010:
#'
#' dat <- dhs_data(
#' indicatorIds="ML_PMAL_C_RDT",
#' countryIds="SN",
#' surveyYearStart="2006"
#' )
#'
#' # A complete list of examples for how each argument to the data api
#' # endpoint can be provided is given below, which is a copy of each of
#' # the examples listed in the API at:
#' \url{https://api.dhsprogram.com/#/api-data.cfm}
#'
#' \dontrun{
#' dat <- dhs_data(countryIds="EG",all_results=FALSE)
#' dat <- dhs_data(indicatorIds="FE_FRTR_W_TFR",all_results=FALSE)
#' dat <- dhs_data(surveyIds="SN2010DHS",all_results=FALSE)
#' dat <- dhs_data(selectSurveys="latest",all_results=FALSE)
#' dat <- dhs_data(selectSurveys="byIndicator", indicatorIds="FE_CEBA_W_CH0",
#' all_results=FALSE)
#' dat <- dhs_data(surveyYear="2010",all_results=FALSE)
#' dat <- dhs_data(surveyYearStart="2006",all_results=FALSE)
#' dat <- dhs_data(surveyYearStart="1991", surveyYearEnd="2006",
#' all_results=FALSE)
#' dat <- dhs_data(surveyType="DHS",all_results=FALSE)
#' dat <- dhs_data(surveyCharacteristicIds="32",all_results=FALSE)
#' dat <- dhs_data(characteristicCategory="wealth quintile",all_results=FALSE)
#' dat <- dhs_data(breakdown="all", countryIds="AZ", characteristicLabel="6+",
#' all_results=FALSE)
#' dat <- dhs_data(tagIds="1",all_results=FALSE)
#' dat <- dhs_data(breakdown="subnational",all_results=FALSE)
#' dat <- dhs_data(breakdown="background",all_results=FALSE)
#' dat <- dhs_data(breakdown="all",all_results=FALSE)
#' dat <- dhs_data(f="html",all_results=FALSE)
#' dat <- dhs_data(f="geojson", returnGeometry="true",all_results=FALSE)
#' }

dhs_data <- function(countryIds=NULL,
                     indicatorIds=NULL,
                     surveyIds=NULL,
                     selectSurveys=NULL,
                     surveyYear=NULL,
                     surveyYearStart=NULL,
                     surveyYearEnd=NULL,
                     surveyType=NULL,
                     surveyCharacteristicIds=NULL,
                     characteristicCategory=NULL,
                     characteristicLabel=NULL,
                     tagIds=NULL,
                     breakdown=NULL,
                     returnGeometry=NULL,
                     f=NULL,
                     returnFields=NULL,
                     perPage=NULL,
                     page=NULL,
                     client=NULL,
                     force=FALSE,
                     all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/data"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Indicators
#'
#' @title API request of DHS Indicators
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param countryIds Specify a comma separated list of country ids to filter
#'   by. For a list of countries use
#' \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
#' @param indicatorIds Specify a comma separated list of indicators ids to
#'   filter by. For a list of indicators use
#' \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
#' @param surveyIds Specify a comma separated list of survey ids to filter by.
#'   For a list of surveys use
#' \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param surveyYear Specify a survey year to filter by.
#' @param surveyYearStart Specify a range of Survey Years to filter Indicators
#'   on. surveyYearStart is an inclusive value. Can be used alone or in
#'   conjunction with surveyYearEnd.
#' @param surveyYearEnd Specify a range of Survey Years to filter Indicators
#'   on. surveyYearEnd is an inclusive value. Can be used alone or in
#'   conjunction with surveyYearStart.
#' @param surveyType Specify a comma separated list of survey years to
#'   filter by.
#' @param surveyCharacteristicIds Specify a survey characteristic id to filter
#'   indicators in surveys with the specified survey characteristic. For a list
#'   of survey characteristics use
#'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param tagIds Specify a tag id to filter indicators with the specified tag.
#'   For a list of tags use \code{dhs_tags()}
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
#'   is JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request. By
#'   default the API will return page 1.
#'
#' @return Returns a `data.table` of 18 (or less if `returnFields` is provided)
#'   indicators with attributes for each indicator. A detailed description of
#'   all the attributes returned is provided at
#'   \url{https://api.dhsprogram.com/rest/dhs/indicators/fields}
#'
#' @export
#'
#' @examples
#'
#' # A common use for the indicators data API will be to search for a list of
#' # health indicators within a given characteristic category, such as anemia
#' # testing, HIV prevalence, micronutrients etc. For example to return all the
#' # indicators relating to malaria testing by RDTs:
#'
#' dat <- dhs_indicators(surveyCharacteristicIds="90")
#'
#' # A list of the different `surveyCharacteristicIds` can be found
#' # [here](https://api.dhsprogram.com/rest/dhs/surveycharacteristics?f=html)
#'
#' # A complete list of examples for how each argument to the indicator API
#' # endpoint can be provided is given below, which is a copy of each of
#' # the examples listed in the API at:
#' \url{https://api.dhsprogram.com/#/api-indicators.cfm}
#'
#' \dontrun{
#' dat <- dhs_indicators(countryIds="EG",all_results=FALSE)
#' dat <- dhs_indicators(indicatorIds="FE_FRTR_W_TFR",all_results=FALSE)
#' dat <- dhs_indicators(surveyIds="SN2010DHS",all_results=FALSE)
#' dat <- dhs_indicators(surveyYear="2010",all_results=FALSE)
#' dat <- dhs_indicators(surveyYearStart="2006",all_results=FALSE)
#' dat <- dhs_indicators(surveyYearStart="1991", surveyYearEnd="2006",
#' all_results=FALSE)
#' dat <- dhs_indicators(surveyType="DHS",all_results=FALSE)
#' dat <- dhs_indicators(surveyCharacteristicIds="32",all_results=FALSE)
#' dat <- dhs_indicators(tagIds="1",all_results=FALSE)
#' dat <- dhs_indicators(f="html",all_results=FALSE)
#' }

dhs_indicators <- function(countryIds=NULL,
                           indicatorIds=NULL,
                           surveyIds=NULL,
                           surveyYear=NULL,
                           surveyYearStart=NULL,
                           surveyYearEnd=NULL,
                           surveyType=NULL,
                           surveyCharacteristicIds=NULL,
                           tagIds=NULL,
                           f=NULL,
                           returnFields=NULL,
                           perPage=NULL,
                           page=NULL,
                           client=NULL,
                           force=FALSE,
                           all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/indicators"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS UI Updates
#'
#' @title API request of DHS UI Updates
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param lastUpdate Specify a date or Unix time to filter the updates by. Only
#'   results for interfaces that has been updated on or after the specified
#'   date will be returned.
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
#'   is JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request. By
#'   default the API will return page 1.
#'
#' @return Returns a `data.table` of 3 (or less if `returnFields` is provided)
#'   interfaces that have been added/updated or removed. A detailed description
#'   of all the attributes returned is provided at
#'   \url{https://api.dhsprogram.com/rest/dhs/uiupdates/fields}
#'
#' @export
#' @examples
#'
#' # The main use for the ui updates API will be to search for the last time
#' # there was a change to the UI. For example to return all the
#' # changes since 2018:
#'
#' dat <- dhs_ui_updates(lastUpdate="20180101")
#'
#' # A complete list of examples for how each argument to the ui updates API
#' # endpoint can be provided is given below, which is a copy of each of
#' # the examples listed in the API at:
#' \url{https://api.dhsprogram.com/#/api-uiupdates.cfm}
#'
#' \dontrun{
#' dat <- dhs_ui_updates(lastUpdate="20150901",all_results=FALSE)
#' dat <- dhs_ui_updates(f="html",all_results=FALSE)
#' }

dhs_ui_updates <- function(lastUpdate=NULL,
                          f=NULL,
                          returnFields=NULL,
                          perPage=NULL,
                          page=NULL,
                          client=NULL,
                          force=FALSE,
                          all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/uiupdates"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Info
#'
#' @title API request of DHS Info
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param infoType Specify a type of info to obtain the information requested.
#'   Default is version. `infoType="version"`` (default) Provides the version
#'   of the API.
#'   Example: https://api.dhsprogram.com/rest/dhs/info?infoType=version
#'   `infoType="citation"` Provides the citation for the API to include with
#'   your application or data.
#'   Example: https://api.dhsprogram.com/rest/dhs/info?infoType=citation
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
#'   is JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request. By
#'   default the API will return page 1.
#'
#' @return Returns a `data.table` of 2 (or less if `returnFields` is provided)
#'   fields describing the type of information that was requested and a value
#'   corresponding to the information requested.
#'   \url{https://api.dhsprogram.com/rest/dhs/info/fields}
#'
#' @export
#' @examples
#'
#' # The main use for the info API  will be to confirm the version of the API
#' # being used to providing the most current citation for the data.
#'
#' dat <- dhs_info(infoType="version")
#'
#' # A complete list of examples for how each argument to the info API
#' # endpoint can be provided is given below, which is a copy of each of
#' # the examples listed in the API at:
#' \url{https://api.dhsprogram.com/#/api-info.cfm}
#'
#' \dontrun{
#' dat <- dhs_info(infoType="version",all_results=FALSE)
#' dat <- dhs_info(infoType="citation",all_results=FALSE)
#' dat <- dhs_info(f="html",all_results=FALSE)
#' }

dhs_info <- function(infoType=NULL,
                     f=NULL,
                     returnFields=NULL,
                     perPage=NULL,
                     page=NULL,
                     client=NULL,
                     force=FALSE,
                     all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/info"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Countries
#'
#' @title API request of DHS Countries
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param countryIds Specify a comma separated list of country ids to filter
#'   by. For a list of countries use
#' \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
#' @param indicatorIds Specify a comma separated list of indicators ids to
#'   filter by. For a list of indicators use
#' \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
#' @param surveyIds Specify a comma separated list of survey ids to filter by.
#'   For a list of surveys use \code{dhs_surveys(returnFields=c("SurveyId",
#'   "SurveyYearLabel","SurveyType","CountryName"))}
#' @param surveyYear Specify a comma separated list of survey years to
#'   filter by.
#' @param surveyYearStart Specify a range of Survey Years to filter Countries
#'   on. surveyYearStart is an inclusive value. Can be used alone or in
#'   conjunction with surveyYearEnd.
#' @param surveyYearEnd Specify a range of Survey Years to filter Countries on.
#'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
#'   with surveyYearStart.
#' @param surveyType Specify a survey type to filter by.
#' @param surveyCharacteristicIds Specify a survey characteristic id to filter
#'   countries in surveys with the specified survey characteristic. For a list
#'   of survey characteristics use
#'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param tagIds Specify a tag id to filter countries with surveys containing
#'   indicators with the specified tag. For a list of tags use
#'   \code{dhs_tags()}
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
#'   is JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request. By
#'   default the API will return page 1.
#'
#' @return Returns a `data.table` of 12 (or less if `returnFields` is provided)
#'   countries with their corresponding details. A detailed description of all
#'   the attributes returned is provided at
#'  \url{https://api.dhsprogram.com/rest/dhs/countries/fields}
#'
#' @export
#' @examples
#'
#' # A common use for the countries API endpoint is to query which countries
#' # ask questions about a given topic. For example to find all countries that
#' # record data on malaria prevalence by RDT:
#'
#' dat <- dhs_countries(indicatorIds = "ML_PMAL_C_RDT")
#'
#' # Additionally you may want to know all the countries that have conducted
#' MIS (malaria indicator surveys):
#'
#' dat <- dhs_countries(surveyType="MIS")
#'
#' # A complete list of examples for how each argument to the countries API
#' # endpoint can be provided is given below, which is a copy of each of
#' # the examples listed in the API at:
#'
#' \url{https://api.dhsprogram.com/#/api-countries.cfm}
#'
#' \dontrun{
#' dat <- dhs_countries(countryIds="EG",all_results=FALSE)
#' dat <- dhs_countries(indicatorIds="FE_FRTR_W_TFR",all_results=FALSE)
#' dat <- dhs_countries(surveyIds="SN2010DHS",all_results=FALSE)
#' dat <- dhs_countries(surveyYear="2010",all_results=FALSE)
#' dat <- dhs_countries(surveyYearStart="2006",all_results=FALSE)
#' dat <- dhs_countries(surveyYearStart="1991", surveyYearEnd="2006",
#' all_results=FALSE)
#' dat <- dhs_countries(surveyType="DHS",all_results=FALSE)
#' dat <- dhs_countries(surveyCharacteristicIds="32",all_results=FALSE)
#' dat <- dhs_countries(tagIds="1",all_results=FALSE)
#' dat <- dhs_countries(f="html",all_results=FALSE)
#' }

dhs_countries <- function(countryIds=NULL,
                          indicatorIds=NULL,
                          surveyIds=NULL,
                          surveyYear=NULL,
                          surveyYearStart=NULL,
                          surveyYearEnd=NULL,
                          surveyType=NULL,
                          surveyCharacteristicIds=NULL,
                          tagIds=NULL,
                          f=NULL,
                          returnFields=NULL,
                          perPage=NULL,
                          page=NULL,
                          client=NULL,
                          force=FALSE,
                          all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/countries"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Surveys
#'
#' @title API request of DHS Surveys
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param countryIds Specify a comma separated list of country ids to
#'  filter by. For a list of countries use
#'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
#' @param indicatorIds Specify a comma separated list of indicators ids to
#'   filter by. For a list of indicators use
#'   \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
#' @param selectSurveys Specify to filter data from the latest survey by
#'   including `selectSurveys=TRUE` in your request. Note: Please use this
#'   parameter in conjunction with countryCode, surveyType, or indicatorIds
#'   for best results.
#' @param surveyIds Specify a comma separated list of survey ids to filter by.
#'   For a list of surveys use
#'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param surveyYear Specify a comma separated list of survey years to
#'   filter by.
#' @param surveyYearStart Specify a range of Survey Years to filter Surveys on.
#'   surveyYearStart is an inclusive value. Can be used alone or in conjunction
#'   with surveyYearEnd.
#' @param surveyYearEnd Specify a range of Survey Years to filter Surveys on.
#'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
#'   with surveyYearStart.
#' @param surveyType Specify a survey type to filter by.
#' @param surveyStatus Every survey is assigned a surveys status and can be
#'   queried based on the surveyStatus parameter. `surveyStatus="available"`
#'   (default) provides a list of all surveys for which the DHS API contains
#'   Indicator Data. `surveyStatus="Completed"` provides a list of all
#'   completed surveys. NOTE: Data may not be available for every completed
#'   survey. `surveyStatus="Ongoing"` provides a list of all ongoing surveys.
#'   `surveyStatus="all"` provides a list of all surveys.
#' @param surveyCharacteristicIds Specify a survey characteristic id to filter
#'   surveys with the specified survey characteristic. For a list of survey
#'   characteristics use \code{dhs_surveys(returnFields=c("SurveyId",
#'   "SurveyYearLabel","SurveyType","CountryName"))}
#' @param tagIds Specify a tag id to filter surveys containing indicators with
#'   the specified tag. For a list of tags use \code{dhs_tags()}
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
#'   is JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#' default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request. By
#' default the API will return page 1.
#'
#' @return Returns a `data.table` of 28 (or less if `returnFields` is provided)
#'   surveys with detailed information for each survey. A detailed description
#'   of all the attributes returned is provided at
#'   \url{https://api.dhsprogram.com/rest/dhs/surveys/fields}
#'
#' @export
#' @examples
#'
#' # A common use for the surveys API endpoint is to query which countries
#' # have conducted surveys since a given year, e.g. since 2010
#'
#' dat <- dhs_surveys(surveyYearStart="2010")
#'
#' # Additionally, some countries conduct non DHS surveys, but the data for
#' thse is also available within the DHS website/API. To query these:
#'
#' dat <- dhs_surveys(surveyType="MIS")
#'
#' # Lastly, you may be interested to know about anything peculiar about a
#' # particular survey's implementation. This can be found by looking within
#' # the footnotes variable within the data frame returned. For example, the
#' # Madagascar 2013 MIS:
#'
#' dat$Footnotes[dat$SurveyId == "MD2013MIS"]
#'
#' # A complete list of examples for how each argument to the surveys API
#' # endpoint can be provided is given below, which is a copy of each of
#' # the examples listed in the API at:
#'
#' \url{https://api.dhsprogram.com/#/api-surveys.cfm}
#' \dontrun{
#' dat <- dhs_surveys(countryIds="EG",all_results=FALSE)
#' dat <- dhs_surveys(indicatorIds="FE_FRTR_W_TFR",all_results=FALSE)
#' dat <- dhs_surveys(selectSurveys="latest",all_results=FALSE)
#' dat <- dhs_surveys(surveyIds="SN2010DHS",all_results=FALSE)
#' dat <- dhs_surveys(surveyYear="2010",all_results=FALSE)
#' dat <- dhs_surveys(surveyYearStart="2006",all_results=FALSE)
#' dat <- dhs_surveys(surveyYearStart="1991", surveyYearEnd="2006",
#' all_results=FALSE)
#' dat <- dhs_surveys(surveyType="DHS",all_results=FALSE)
#' dat <- dhs_surveys(surveyStatus="Surveys",all_results=FALSE)
#' dat <- dhs_surveys(surveyStatus="Completed",all_results=FALSE)
#' dat <- dhs_surveys(surveyStatus="Ongoing",all_results=FALSE)
#' dat <- dhs_surveys(surveyStatus="All",all_results=FALSE)
#' dat <- dhs_surveys(surveyCharacteristicIds="32",all_results=FALSE)
#' dat <- dhs_surveys(tagIds="1",all_results=FALSE)
#' dat <- dhs_surveys(f="html",all_results=FALSE)
#' }

dhs_surveys <- function(countryIds=NULL,
                        indicatorIds=NULL,
                        selectSurveys=NULL,
                        surveyIds=NULL,
                        surveyYear=NULL,
                        surveyYearStart=NULL,
                        surveyYearEnd=NULL,
                        surveyType=NULL,
                        surveyStatus=NULL,
                        surveyCharacteristicIds=NULL,
                        tagIds=NULL,
                        f=NULL,
                        returnFields=NULL,
                        perPage=NULL,
                        page=NULL,
                        client=NULL,
                        force=FALSE,
                        all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/surveys"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Survey Characteristics
#'
#' @title API request of DHS Survey Characteristics
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param countryIds Specify a comma separated list of country ids to filter
#'   by. For a list of countries use
#'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
#' @param indicatorIds Specify a comma separated list of indicators ids to
#'   filter by. For a list of indicators use
#'   \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
#' @param surveyIds Specify a comma separated list of survey ids to filter by.
#'   For a list of surveys use
#'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param surveyYear Specify a comma separated list of survey years to
#'   filter by.
#' @param surveyYearStart Specify a range of Survey Years to filter Survey
#'   Characteristics on. surveyYearStart is an inclusive value. Can be used
#'   alone or in conjunction with surveyYearEnd.
#' @param surveyYearEnd Specify a range of Survey Years to filter Survey
#'   Characteristics on. surveyYearEnd is an inclusive value. Can be used alone
#'   or in conjunction with surveyYearStart.
#' @param surveyType Specify a survey type to filter by.
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
#'   is JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request. By
#'   default the API will return page 1.
#'
#' @return Returns a `data.table` of 2 (or less if `returnFields` is provided)
#'   survey characteristics. A survey can be labelled with one or more of these
#'   survey characteristics. A description of all the attributes returned is
#'   provided at
#'   \url{https://api.dhsprogram.com/rest/dhs/surveycharacteristics/fields}
#'
#' @export
#' @examples
#'
#' # A good use for the survey characteristics API endpoint is to query what the
#' # IDs are for each survey characteristic. These are useful for passing as
#' # arguments to other API endpoints.For example to show all the ids:
#'
#' dat <- dhs_survey_characteristics()
#'
#' # Or if your analysis is foucssed on a particular country, and you want to
#' # see all the characteristics surveyed for e.g. Senegal
#'
#' dat <- dhs_countries(countryIds="SN")
#'
#' # A complete list of examples for how each argument to the survey
#' # characteristics API endpoint can be provided is given below, which is a
#' # copy of each of the examples listed in the API at:
#'
#' \url{https://api.dhsprogram.com/#/api-surveycharacteristics.cfm}
#' \dontrun{
#' dat <- dhs_survey_characteristics(countryIds="EG",all_results=FALSE)
#' dat <- dhs_survey_characteristics(indicatorIds="FE_FRTR_W_TFR",
#' all_results=FALSE)
#' dat <- dhs_survey_characteristics(surveyIds="SN2010DHS,all_results=FALSE")
#' dat <- dhs_survey_characteristics(surveyYear="2010,all_results=FALSE")
#' dat <- dhs_survey_characteristics(surveyYearStart="2006",all_results=FALSE)
#' dat <- dhs_survey_characteristics(surveyYearStart="1991",
#' surveyYearEnd="2006",all_results=FALSE)
#' dat <- dhs_survey_characteristics(surveyType="DHS",all_results=FALSE)
#' dat <- dhs_survey_characteristics(f="html",all_results=FALSE)
#' }

dhs_survey_characteristics <- function(countryIds=NULL,
                                      indicatorIds=NULL,
                                      surveyIds=NULL,
                                      surveyYear=NULL,
                                      surveyYearStart=NULL,
                                      surveyYearEnd=NULL,
                                      surveyType=NULL,
                                      f=NULL,
                                      returnFields=NULL,
                                      perPage=NULL,
                                      page=NULL,
                                      client=NULL,
                                      force=FALSE,
                                      all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/surveycharacteristics"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Publications
#'
#' @title API request of DHS Publications
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param countryIds Specify a comma separated list of country ids to filter
#'   by. For a list of countries use
#'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
#' @param selectSurveys Specify to filter data from the latest survey by
#'   including `selectSurveys=TRUE` in your request. Note: Please use this
#'   parameter in conjunction with countryCode, surveyType, or indicatorIds
#'   for best results.
#' @param indicatorIds Specify a comma separated list of indicators ids to
#'   filter by. For a list of indicators use
#'   \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
#' @param surveyIds Specify a comma separated list of survey ids to filter by.
#'   For a list of surveys use
#'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param surveyYear Specify a comma separated list of survey years to
#'   filter by.
#' @param surveyYearStart Specify a range of Survey Years to filter
#'   Publications on. surveyYearStart is an inclusive value. Can be used alone
#'   or in conjunction with surveyYearEnd.
#' @param surveyYearEnd Specify a range of Survey Years to filter Publications
#'   on. surveyYearEnd is an inclusive value. Can be used alone or in
#'   conjunction with surveyYearStart.
#' @param surveyType Specify a survey type to filter by.
#' @param surveyCharacteristicIds Specify a survey characteristic id to filter
#'   publications with countries with the specified survey characteristics.
#'   For a list of survey characteristics use
#'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param tagIds Specify a tag id to filter publications with surveys
#'   containing indicators with the specified tag. For a list of tags use
#'   \code{dhs_tags()}
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format is
#'   JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request.
#'   By default the API will return page 1.
#'
#' @return Returns a `data.table` of 10 (or less if `returnFields` is provided)
#'   publications with detailed information for each publication. A detailed
#'   description of all the attributes returned is provided at
#'   \url{https://api.dhsprogram.com/rest/dhs/publications/fields}
#'
#' @export
#' @examples
#'
#' # A main use for the publications API endpoint is to find which surveys have
#' # a published report resulting from the conducted survey:
#'
#' dat <- dhs_publications()
#'
#' # A complete list of examples for how each argument to the publications
#' # API endpoint can be provided is given below, which is a
#' # copy of each of the examples listed in the API at:
#'
#' \url{https://api.dhsprogram.com/#/api-publications.cfm}
#' \dontrun{
#' dat <- dhs_publications(countryIds="EG",all_results=FALSE)
#' dat <- dhs_publications(selectSurveys="latest",all_results=FALSE)
#' dat <- dhs_publications(indicatorIds="FE_FRTR_W_TFR",all_results=FALSE)
#' dat <- dhs_publications(surveyIds="SN2010DHS",all_results=FALSE)
#' dat <- dhs_publications(surveyYear="2010",all_results=FALSE)
#' dat <- dhs_publications(surveyYearStart="2006",all_results=FALSE)
#' dat <- dhs_publications(surveyYearStart="1991", surveyYearEnd="2006",
#' all_results=FALSE)
#' dat <- dhs_publications(surveyType="DHS",all_results=FALSE)
#' dat <- dhs_publications(surveyCharacteristicIds="32",all_results=FALSE)
#' dat <- dhs_publications(tagIds=1,all_results=FALSE)
#' dat <- dhs_publications(f="html",all_results=FALSE)
#' }

dhs_publications <- function(countryIds=NULL,
                             selectSurveys=NULL,
                             indicatorIds=NULL,
                             surveyIds=NULL,
                             surveyYear=NULL,
                             surveyYearStart=NULL,
                             surveyYearEnd=NULL,
                             surveyType=NULL,
                             surveyCharacteristicIds=NULL,
                             tagIds=NULL,
                             f=NULL,
                             returnFields=NULL,
                             perPage=NULL,
                             page=NULL,
                             client=NULL,
                             force=FALSE,
                             all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/publications"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Datasets
#'
#' @title API request of DHS Datasets
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param countryIds Specify a comma separated list of country ids to filter
#'   by. For a list of countries use
#'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
#' @param selectSurveys Specify to filter data from the latest survey by
#'   including `selectSurveys=TRUE` in your request. Note: Please use this
#'   parameter in conjunction with countryCode, surveyType, or indicatorIds for
#'   best results.
#' @param surveyIds Specify a comma separated list of survey ids to filter by.
#'   For a list of surveys use
#'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param surveyYear Specify a comma separated list of survey years to
#'   filter by.
#' @param surveyYearStart Specify a range of Survey Years to filter Datasets
#'   on. surveyYearStart is an inclusive value. Can be used alone or in
#'   conjunction with surveyYearEnd.
#' @param surveyYearEnd Specify a range of Survey Years to filter Datasets on.
#'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
#'   with surveyYearStart.
#' @param surveyType Specify a survey type to filter by.
#' @param fileFormat Specify the file format for the survey. Can use file
#'   format type name (SAS, Stata, SPSS, Flat, Hierarchical) or file format
#'   code. View list of file format codes -
#'   \url{https://dhsprogram.com/data/File-Types-and-Names.cfm}
#' @param fileType Specify the type of dataset generated for the survey
#'   (e.g. household, women, men, children, couples, etc.). View list of file
#'   type codes - \url{https://dhsprogram.com/data/File-Types-and-Names.cfm}
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format is
#'   JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request.
#'   By default the API will return page 1.
#'
#' @return Returns a `data.table` of 13 (or less if `returnFields` is provided)
#'   datasets with their corresponding details. A detailed description of all
#'   the attributes returned is provided at
#'   \url{https://api.dhsprogram.com/rest/dhs/datasets/fields}
#'
#' @export
#' @examples
#'
#' # The API endpoint for the datasets available within the DHS website
#' # is a very useful endpoint, which is used a lot within `rdhs`. For example,
#' # it is used to find the file names and size of the dataset files, as well
#' # as when they were last modified. This enables us to see which datasets
#' # have been updated and may thus be out of date. For example to find all
#' # datasets that have been modified in 2018:
#'
#' dat <- dhs_datasets()
#' dates <- rdhs:::mdy_hms(dat$FileDateLastModified)
#' years <- as.POSIXlt(dates, tz = tz(dates))$year + 1900
#' modified_in_2018 <- which(years == 2018)
#'
#' # A complete list of examples for how each argument to the datasets
#' # API endpoint can be provided is given below, which is a
#' # copy of each of the examples listed in the API at:
#'
#' \url{https://api.dhsprogram.com/#/api-datasets.cfm}
#' \dontrun{
#' dat <- dhs_datasets(countryIds="EG",all_results=FALSE)
#' dat <- dhs_datasets(selectSurveys="latest",all_results=FALSE)
#' dat <- dhs_datasets(surveyIds="SN2010DHS",all_results=FALSE)
#' dat <- dhs_datasets(surveyYear="2010",all_results=FALSE)
#' dat <- dhs_datasets(surveyYearStart="2006",all_results=FALSE)
#' dat <- dhs_datasets(surveyYearStart="1991", surveyYearEnd="2006",
#' all_results=FALSE)
#' dat <- dhs_datasets(surveyType="DHS",all_results=FALSE)
#' dat <- dhs_datasets(fileFormat="stata",all_results=FALSE)
#' dat <- dhs_datasets(fileFormat="DT",all_results=FALSE)
#' dat <- dhs_datasets(fileType="KR",all_results=FALSE)
#' dat <- dhs_datasets(f="geojson",all_results=FALSE)
#' }

dhs_datasets <- function(countryIds=NULL,
                         selectSurveys=NULL,
                         surveyIds=NULL,
                         surveyYear=NULL,
                         surveyYearStart=NULL,
                         surveyYearEnd=NULL,
                         surveyType=NULL,
                         fileFormat=NULL,
                         fileType=NULL,
                         f=NULL,
                         returnFields=NULL,
                         perPage=NULL,
                         page=NULL,
                         client=NULL,
                         force=FALSE,
                         all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/datasets"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Geometry
#'
#' @title API request of DHS Geometry
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param countryIds Specify a comma separated list of country ids to filter
#'   by. For a list of countries use
#'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
#' @param surveyIds Specify a comma separated list of survey ids to filter by.
#'   For a list of surveys use
#'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param surveyYear Specify a comma separated list of survey years to
#'   filter by.
#' @param surveyYearStart Specify a range of Survey Years to filter Geometry
#'   on. surveyYearStart is an inclusive value. Can be used alone or in
#'   conjunction with surveyYearEnd.
#' @param surveyYearEnd Specify a range of Survey Years to filter Geometry on.
#'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
#'   with surveyYearStart.
#' @param surveyType Specify a survey type to filter by.
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
#'   is JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request.
#'   By default the API will return page 1.
#'
#' @return Returns a `data.table` of 7 (or less if `returnFields` is provided)
#'   geometry with their corresponding details. A detailed description of all
#'   the attributes returned is provided at
#'   \url{https://api.dhsprogram.com/rest/dhs/geometry/fields}
#'
#' @export
#' @examples
#'
#' # The geometry API endpoint returns the spatial geometry for countries, and
#' # can then be used to recreate the spatial polygon for a given country. For
#' # example to return the coordinates for the Senegal 2010 DHS survey:
#'
#' dat <- dhs_geometry(surveyIds="SN2010DHS")
#'
#' # At the moment there is no function to convert the coordinates returned by
#' # the API but this will be in the next version of rdhs. For those interested
#' # look at the geojson vignette for an alternative way to produce plots.
#'
#' # A complete list of examples for how each argument to the geometry
#' # API endpoint can be provided is given below, which is a
#' # copy of each of the examples listed in the API at:
#'
#' \url{https://api.dhsprogram.com/#/api-geometry.cfm}
#' \dontrun{
#' dat <- dhs_geometry(countryIds="EG",all_results=FALSE)
#' dat <- dhs_geometry(surveyIds="SN2010DHS",all_results=FALSE)
#' dat <- dhs_geometry(surveyYear="2010",all_results=FALSE)
#' dat <- dhs_geometry(surveyYearStart="2006",all_results=FALSE)
#' dat <- dhs_geometry(surveyYearStart="1991", surveyYearEnd="2006",
#' all_results=FALSE)
#' dat <- dhs_geometry(surveyType="DHS",all_results=FALSE)
#' dat <- dhs_geometry(f="geojson",all_results=FALSE)
#' }


dhs_geometry <- function(countryIds=NULL,
                         surveyIds=NULL,
                         surveyYear=NULL,
                         surveyYearStart=NULL,
                         surveyYearEnd=NULL,
                         surveyType=NULL,
                         f=NULL,
                         returnFields=NULL,
                         perPage=NULL,
                         page=NULL,
                         client=NULL,
                         force=FALSE,
                         all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/geometry"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Tags
#'
#' @title API request of DHS Tags
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#' then the specified page only will be returned. Default = TRUE.
#' @param countryIds Specify a comma separated list of country ids to filter
#'   by. For a list of countries use
#'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
#' @param indicatorIds Specify a comma separated list of indicators ids to
#'   filter by. For a list of indicators use
#'   \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
#' @param surveyIds Specify a comma separated list of survey ids to filter by.
#'   For a list of surveys use
#'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
#'   "SurveyType","CountryName"))}
#' @param surveyYear Specify a comma separated list of survey years to
#'   filter by.
#' @param surveyYearStart Specify a range of Survey Years to filter Tags on.
#'   surveyYearStart is an inclusive value. Can be used alone or in
#'   conjunction with surveyYearEnd.
#' @param surveyYearEnd Specify a range of Survey Years to filter Tags on.
#'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
#'   with surveyYearStart.
#' @param surveyType Specify a survey type to filter by.
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
#'   is JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request. By
#'   default the API will return page 1.
#'
#' @return Returns a `data.table` of 4 (or less if `returnFields` is provided)
#'   tags with detailed information. An indicators can be tagged with one or
#'   more tags to help identify certain topics an indicator can be identified
#'   by. A description of the attributes returned is provided at
#'   \url{https://api.dhsprogram.com/rest/dhs/tags/fields}
#'
#' @export
#' @examples
#'
#' # A good use for the tags API endpoint is to query what the
#' # IDs are for each tag. These are useful for passing as
#' # arguments to other API endpoints.For example to show all the ids:
#'
#' dat <- dhs_tags()
#'
#' # Or if your analysis is foucssed on a particular country, and you want to
#' # see all the characteristics surveyed for e.g. Senegal
#'
#' dat <- dhs_tags(countryIds="SN")
#'
#' # A complete list of examples for how each argument to the survey
#' # tags API endpoint can be provided is given below, which is a
#' # copy of each of the examples listed in the API at:
#'
#' \url{https://api.dhsprogram.com/#/api-tags.cfm}
#' \dontrun{
#' dat <- dhs_tags(countryIds="EG",all_results=FALSE)
#' dat <- dhs_tags(indicatorIds="FE_FRTR_W_TFR",all_results=FALSE)
#' dat <- dhs_tags(surveyIds="SN2010DHS",all_results=FALSE)
#' dat <- dhs_tags(surveyYear="2010",all_results=FALSE)
#' dat <- dhs_tags(surveyYearStart="2006",all_results=FALSE)
#' dat <- dhs_tags(surveyYearStart="1991", surveyYearEnd="2006",
#' all_results=FALSE)
#' dat <- dhs_tags(surveyType="DHS",all_results=FALSE)
#' dat <- dhs_tags(f="html",all_results=FALSE)
#' }

dhs_tags <- function(countryIds=NULL,
                     indicatorIds=NULL,
                     surveyIds=NULL,
                     surveyYear=NULL,
                     surveyYearStart=NULL,
                     surveyYearEnd=NULL,
                     surveyType=NULL,
                     f=NULL,
                     returnFields=NULL,
                     perPage=NULL,
                     page=NULL,
                     client=NULL,
                     force=FALSE,
                     all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/tags"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}



#' API request of DHS Data Updates
#'
#' @title API request of DHS Data Updates
#' @param client If the API request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}
#' @param force Should we force fetching the API results, and ignore any
#'   cached results we have. Default = FALSE
#' @param all_results Boolean for if all results should be returned. If FALSE
#'   then the specified page only will be returned. Default = TRUE.
#' @param lastUpdate Specify a date or Unix time to filter the updates by.
#'   Only results for data that have been updated on or after the specified
#'   date will be returned.
#' @param f You can specify the format of the data returned from the query as
#'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format is
#'   JSON.
#' @param returnFields Specify a list of attributes to be returned.
#' @param perPage Specify the number of results to be returned per page. By
#'   default the API will return 100 results.
#' @param page Allows specifying a page number to obtain for the API request.
#'   By default the API will return page 1.
#'
#' @return Returns a `data.table` of 9 (or less if `returnFields` is provided)
#'   indicators or surveys that have been added/updated or removed. A detailed
#'   description of all the attributes returned is provided at
#'   \url{https://api.dhsprogram.com/rest/dhs/dataupdates/fields}
#'
#' @export
#' @examples
#'
#' # The API endpoint for the data updates available within the DHS
#' # is a very useful endpoint, which is used a lot within `rdhs`. For example,
#' # we use it to keep the end user's cache up to date. For example to find all
#' # updates that have occurred in 2018:
#'
#' dat <- dhs_data_updates(lastUpdate="20180101")
#'
#' # A complete list of examples for how each argument to the data updates
#' # API endpoint can be provided is given below, which is a
#' # copy of each of the examples listed in the API at:
#'
#' \url{https://api.dhsprogram.com/#/api-dataupdates.cfm}
#' \dontrun{
#' dat <- dhs_data_updates(lastUpdate="20150901",all_results=FALSE)
#' dat <- dhs_data_updates(f="html",all_results=FALSE)
#' }

dhs_data_updates <- function(lastUpdate=NULL,
                            f=NULL,
                            returnFields=NULL,
                            perPage=NULL,
                            page=NULL,
                            client=NULL,
                            force=FALSE,
                            all_results=TRUE) {

  # create query with all provided arguments
  query <- args_to_query(environment())

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/dataupdates"

  # pass request on accordingly
  handle_api_request(endpoint, query, all_results, client, force)
}
