##' API request of DHS Indicator Data
##'
##' @title API request of DHS Indicator Data
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE
##' @param countryIds Specify a comma separated list of country ids to filter
##'   by. For a list of countries use
##' \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
##' @param indicatorIds Specify a comma separated list of indicator ids to
##'   filter by. For a list of indicators use
##' \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
##' @param surveyIds Specify a comma separated list of survey ids to filter by.
##'   For a list of surveys use
##' \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param selectSurveys Specify to filter Data from the latest survey by adding
##'   `selectSurveys="latest"` in conjunction with a Country Code and/or Survey
##'   Type. Please Note: Not all indicators are present in the latest surveys.
##'   To filter your API Indicator Data call to return the latest survey data in
##'   which a specific set of indicators is present, add
##'   `selectSurveys="byIndicator"` in conjunction with Indicator IDs, Country
##'   Code, and/or Survey Type instead
##'   of using `selectSurveys="latest"`.
##' @param surveyYear Specify a comma separated list of survey years to
##'   filter by.
##' @param surveyYearStart Specify a range of Survey Years to filter Data on.
##'   surveyYearStart is an inclusive value. Can be used alone or in conjunction
##'   with surveyYearEnd.
##' @param surveyYearEnd Specify a range of Survey Years to filter Data on.
##'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
##'   with surveyYearStart.
##' @param surveyType Specify a survey type to filter by.
##' @param surveyCharacteristicIds Specify a survey characteristic id to filter
##'   data on surveys with the specified survey characteristic. For a list of
##'   survey characteristics use
##' \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param characteristicCategory Specify a survey characteristic category to
##'   filter data on surveys with the specified survey characteristic category.
##'   This query is case insensitive, but it only recognizes exact phrase
##'   matches. For example, `characteristicCategory="wealth"` will return
##'   results that have a characteristic category of `Wealth` while
##'   `characteristicCategory="wealth quintile"' will return results that have
##'   a characteristic category of `Wealth Quintile`.
##' @param characteristicLabel Specify a survey characteristic category to
##'   filter data on surveys with the specified survey characteristic category.
##'   This query is case insensitive, but it only recognizes exact phrase
##'   matches. You can use characteristicLabel on its own or in conjunction with
##'   characteristicCategory.
##' @param tagIds Specify a tag id to filter data on indicators with the
##'   specified tag. For a list of tags use \code{dhs_tags()}
##' @param breakdown Data can be requested at different levels via the breakdown
##'   parameter. By default national data is returned and provides totals on a
##'   national level. `breakdown="subnational"` data provides values on a
##'   subnational level. `breakdown="background"` provides totals on categorized
##'   basis. Examples are urban/rural,education and wealth level.
##'   `breakdown="all"` provides all the data including disaggregated data.
##' @param returnGeometry Coordinates can be requested from the API by including
##'   `returnGeometry=TRUE` in your request. The default for this value is
##'   false.
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format is
##'   JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset.
##'   By default the API will return page 1.
##'
##' @return Returns a `data.table` of 27 (or less if `returnFields` is provided)
##'   data for your particular query. Details of properties returned with each
##'   row of data are provided at
##' \url{https://api.dhsprogram.com/rest/dhs/data/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_data(countryIds="EG",allResults=FALSE)
##' dat <- dhs_data(indicatorIds="FE_FRTR_W_TFR",allResults=FALSE)
##' dat <- dhs_data(surveyIds="SN2010DHS",allResults=FALSE)
##' dat <- dhs_data(selectSurveys="latest",allResults=FALSE)
##' dat <- dhs_data(selectSurveys="byIndicator", indicatorIds="FE_CEBA_W_CH0",
##' allResults=FALSE)
##' dat <- dhs_data(surveyYear="2010",allResults=FALSE)
##' dat <- dhs_data(surveyYearStart="2006",allResults=FALSE)
##' dat <- dhs_data(surveyYearStart="1991", surveyYearEnd="2006",
##' allResults=FALSE)
##' dat <- dhs_data(surveyType="DHS",allResults=FALSE)
##' dat <- dhs_data(surveyCharacteristicIds="32",allResults=FALSE)
##' dat <- dhs_data(characteristicCategory="wealth quintile",allResults=FALSE)
##' dat <- dhs_data(breakdown="all", countryIds="AZ", characteristicLabel="6+",
##' allResults=FALSE)
##' dat <- dhs_data(tagIds="1",allResults=FALSE)
##' dat <- dhs_data(breakdown="subnational",allResults=FALSE)
##' dat <- dhs_data(breakdown="background",allResults=FALSE)
##' dat <- dhs_data(breakdown="all",allResults=FALSE)
##' dat <- dhs_data(f="html",allResults=FALSE)
##' dat <- dhs_data(f="geojson", returnGeometry="true",allResults=FALSE)
##' }

dhs_data <- function(client=NULL,
                     allResults=TRUE,
                     countryIds=NULL,
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
                     page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/data"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Indicators
##'
##' @title API request of DHS Indicators
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param countryIds Specify a comma separated list of country ids to filter
##'   by. For a list of countries use
##' \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
##' @param indicatorIds Specify a comma separated list of indicators ids to
##'   filter by. For a list of indicators use
##' \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
##' @param surveyIds Specify a comma separated list of survey ids to filter by.
##'   For a list of surveys use
##' \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param surveyYear Specify a survey year to filter by.
##' @param surveyYearStart Specify a range of Survey Years to filter Indicators
##'   on. surveyYearStart is an inclusive value. Can be used alone or in
##'   conjunction with surveyYearEnd.
##' @param surveyYearEnd Specify a range of Survey Years to filter Indicators
##'   on. surveyYearEnd is an inclusive value. Can be used alone or in
##'   conjunction with surveyYearStart.
##' @param surveyType Specify a comma separated list of survey years to
##'   filter by.
##' @param surveyCharacteristicIds Specify a survey characteristic id to filter
##'   indicators in surveys with the specified survey characteristic. For a list
##'   of survey characteristics use
##'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param tagIds Specify a tag id to filter indicators with the specified tag.
##'   For a list of tags use \code{dhs_tags()}
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
##'   is JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset. By
##'   default the API will return page 1.
##'
##' @return Returns a `data.table` of 18 (or less if `returnFields` is provided)
##'   indicators with attributes for each indicator. A detailed description of
##'   all the attributes returned is provided at
##'   \url{https://api.dhsprogram.com/rest/dhs/indicators/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_indicators(countryIds="EG",allResults=FALSE)
##' dat <- dhs_indicators(indicatorIds="FE_FRTR_W_TFR",allResults=FALSE)
##' dat <- dhs_indicators(surveyIds="SN2010DHS",allResults=FALSE)
##' dat <- dhs_indicators(surveyYear="2010",allResults=FALSE)
##' dat <- dhs_indicators(surveyYearStart="2006",allResults=FALSE)
##' dat <- dhs_indicators(surveyYearStart="1991", surveyYearEnd="2006",
##' allResults=FALSE)
##' dat <- dhs_indicators(surveyType="DHS",allResults=FALSE)
##' dat <- dhs_indicators(surveyCharacteristicIds="32",allResults=FALSE)
##' dat <- dhs_indicators(tagIds="1",allResults=FALSE)
##' dat <- dhs_indicators(f="html",allResults=FALSE)
##' }

dhs_indicators <- function(client=NULL,
                           allResults=TRUE,
                           countryIds=NULL,
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
                           page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/indicators"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS UI Updates
##'
##' @title API request of DHS UI Updates
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param lastUpdate Specify a date or Unix time to filter the updates by. Only
##'   results for interfaces that has been updated on or after the sepcified
##'   date will be returned.
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
##'   is JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset. By
##'   default the API will return page 1.
##'
##' @return Returns a `data.table` of 3 (or less if `returnFields` is provided)
##'   interfaces that have been added/updated or removed. A detailed description
##'   of all the attributes returned is provided at
##'   \url{https://api.dhsprogram.com/rest/dhs/uiupdates/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_uiUpdates(lastUpdate="20150901",allResults=FALSE)
##' dat <- dhs_uiUpdates(f="html",allResults=FALSE)
##' }

dhs_uiUpdates <- function(client=NULL,
                          allResults=TRUE,
                          lastUpdate=NULL,
                          f=NULL,
                          returnFields=NULL,
                          perPage=NULL,
                          page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/uiupdates"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Info
##'
##' @title API request of DHS Info
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param infoType Specify a type of info to obtain the information requested.
##'   Default is version. `infoType="version"`` (default) Provides the version
##'   of the API.
##'   Example: https://api.dhsprogram.com/rest/dhs/info?infoType=version
##'   `infoType="citation"` Provides the citation for the API to include with
##'   your application or data.
##'   Example: https://api.dhsprogram.com/rest/dhs/info?infoType=citation
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
##'   is JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset. By
##'   default the API will return page 1.
##'
##' @return Returns a `data.table` of 2 (or less if `returnFields` is provided)
##'   fields describing the type of information that was requested and a value
##'   corresponding to the information requested.
##'   \url{https://api.dhsprogram.com/rest/dhs/info/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_info(infoType="version",allResults=FALSE)
##' dat <- dhs_info(infoType="citation",allResults=FALSE)
##' dat <- dhs_info(f="html",allResults=FALSE)
##' }

dhs_info <- function(client=NULL,
                     allResults=TRUE,
                     infoType=NULL,
                     f=NULL,
                     returnFields=NULL,
                     perPage=NULL,
                     page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/info"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Countries
##'
##' @title API request of DHS Countries
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param countryIds Specify a comma separated list of country ids to filter
##'   by. For a list of countries use
##' \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
##' @param indicatorIds Specify a comma separated list of indicators ids to
##'   filter by. For a list of indicators use
##' \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
##' @param surveyIds Specify a comma separated list of survey ids to filter by.
##'   For a list of surveys use \code{dhs_surveys(returnFields=c("SurveyId",
##'   "SurveyYearLabel","SurveyType","CountryName"))}
##' @param surveyYear Specify a comma separated list of survey years to
##'   filter by.
##' @param surveyYearStart Specify a range of Survey Years to filter Countries
##'   on. surveyYearStart is an inclusive value. Can be used alone or in
##'   conjunction with surveyYearEnd.
##' @param surveyYearEnd Specify a range of Survey Years to filter Countries on.
##'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
##'   with surveyYearStart.
##' @param surveyType Specify a survey type to filter by.
##' @param surveyCharacteristicIds Specify a survey characteristic id to filter
##'   countries in surveys with the specified survey characteristic. For a list
##'   of survey characteristics use
##'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param tagIds Specify a tag id to filter countries with surveys containing
##'   indicators with the specified tag. For a list of tags use
##'   \code{dhs_tags()}
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
##'   is JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset. By
##'   default the API will return page 1.
##'
##' @return Returns a `data.table` of 12 (or less if `returnFields` is provided)
##'   countries with their corresponding details. A detailed description of all
##'   the attributes returned is provided at
##'  \url{https://api.dhsprogram.com/rest/dhs/countries/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_countries(countryIds="EG",allResults=FALSE)
##' dat <- dhs_countries(indicatorIds="FE_FRTR_W_TFR",allResults=FALSE)
##' dat <- dhs_countries(surveyIds="SN2010DHS",allResults=FALSE)
##' dat <- dhs_countries(surveyYear="2010",allResults=FALSE)
##' dat <- dhs_countries(surveyYearStart="2006",allResults=FALSE)
##' dat <- dhs_countries(surveyYearStart="1991", surveyYearEnd="2006",
##' allResults=FALSE)
##' dat <- dhs_countries(surveyType="DHS",allResults=FALSE)
##' dat <- dhs_countries(surveyCharacteristicIds="32",allResults=FALSE)
##' dat <- dhs_countries(tagIds="1",allResults=FALSE)
##' dat <- dhs_countries(f="html",allResults=FALSE)
##' }

dhs_countries <- function(client=NULL,
                          allResults=TRUE,
                          countryIds=NULL,
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
                          page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/countries"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Surveys
##'
##' @title API request of DHS Surveys
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param countryIds Specify a comma separated list of country ids to
##'  filter by. For a list of countries use
##'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
##' @param indicatorIds Specify a comma separated list of indicators ids to
##'   filter by. For a list of indicators use
##'   \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
##' @param selectSurveys Specify to filter data from the latest survey by
##'   including `selectSurveys=TRUE` in your request. Note: Please use this
##'   parameter in conjunction with countryCode, surveyType, or indicatorIds
##'   for best results.
##' @param surveyIds Specify a comma separated list of survey ids to filter by.
##'   For a list of surveys use
##'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param surveyYear Specify a comma separated list of survey years to
##'   filter by.
##' @param surveyYearStart Specify a range of Survey Years to filter Surveys on.
##'   surveyYearStart is an inclusive value. Can be used alone or in conjunction
##'   with surveyYearEnd.
##' @param surveyYearEnd Specify a range of Survey Years to filter Surveys on.
##'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
##'   with surveyYearStart.
##' @param surveyType Specify a survey type to filter by.
##' @param surveyStatus Every survey is assigned a surveys status and can be
##'   queried based on the surveyStatus parameter. `surveyStatus="available"`
##'   (default) provides a list of all surveys for which the DHS API contains
##'   Indicator Data. `surveyStatus="Completed"` provides a list of all
##'   completed surveys. NOTE: Data may not be available for every completed
##'   survey. `surveyStatus="Ongoing"` provides a list of all ongoing surveys.
##'   `surveyStatus="all"` provides a list of all surveys.
##' @param surveyCharacteristicIds Specify a survey characteristic id to filter
##'   surveys with the specified survey characteristic. For a list of survey
##'   characteristics use \code{dhs_surveys(returnFields=c("SurveyId",
##'   "SurveyYearLabel","SurveyType","CountryName"))}
##' @param tagIds Specify a tag id to filter surveys containing indicators with
##'   the specified tag. For a list of tags use \code{dhs_tags()}
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
##'   is JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##' default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset. By
##' default the API will return page 1.
##'
##' @return Returns a `data.table` of 28 (or less if `returnFields` is provided)
##'   surveys with detailed information for each survey. A detailed description
##'   of all the attributes returned is provided at
##'   \url{https://api.dhsprogram.com/rest/dhs/surveys/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_surveys(countryIds="EG",allResults=FALSE)
##' dat <- dhs_surveys(indicatorIds="FE_FRTR_W_TFR",allResults=FALSE)
##' dat <- dhs_surveys(selectSurveys="latest",allResults=FALSE)
##' dat <- dhs_surveys(surveyIds="SN2010DHS",allResults=FALSE)
##' dat <- dhs_surveys(surveyYear="2010",allResults=FALSE)
##' dat <- dhs_surveys(surveyYearStart="2006",allResults=FALSE)
##' dat <- dhs_surveys(surveyYearStart="1991", surveyYearEnd="2006",
##' allResults=FALSE)
##' dat <- dhs_surveys(surveyType="DHS",allResults=FALSE)
##' dat <- dhs_surveys(surveyStatus="Surveys",allResults=FALSE)
##' dat <- dhs_surveys(surveyStatus="Completed",allResults=FALSE)
##' dat <- dhs_surveys(surveyStatus="Ongoing",allResults=FALSE)
##' dat <- dhs_surveys(surveyStatus="All",allResults=FALSE)
##' dat <- dhs_surveys(surveyCharacteristicIds="32",allResults=FALSE)
##' dat <- dhs_surveys(tagIds="1",allResults=FALSE)
##' dat <- dhs_surveys(f="html",allResults=FALSE)
##' }

dhs_surveys <- function(client=NULL,
                        allResults=TRUE,
                        countryIds=NULL,
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
                        page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/surveys"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Survey Characteristics
##'
##' @title API request of DHS Survey Characteristics
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param countryIds Specify a comma separated list of country ids to filter
##'   by. For a list of countries use
##'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
##' @param indicatorIds Specify a comma separated list of indicators ids to
##'   filter by. For a list of indicators use
##'   \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
##' @param surveyIds Specify a comma separated list of survey ids to filter by.
##'   For a list of surveys use
##'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param surveyYear Specify a comma separated list of survey years to
##'   filter by.
##' @param surveyYearStart Specify a range of Survey Years to filter Survey
##'   Characteristics on. surveyYearStart is an inclusive value. Can be used
##'   alone or in conjunction with surveyYearEnd.
##' @param surveyYearEnd Specify a range of Survey Years to filter Survey
##'   Characteristics on. surveyYearEnd is an inclusive value. Can be used alone
##'   or in conjunction with surveyYearStart.
##' @param surveyType Specify a survey type to filter by.
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
##'   is JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset. By
##'   default the API will return page 1.
##'
##' @return Returns a `data.table` of 2 (or less if `returnFields` is provided)
##'   survey characteristics. A survey can be labeled with one or more of these
##'   survey characteristics. A description of all the attributes returned is
##'   provided at
##'   \url{https://api.dhsprogram.com/rest/dhs/surveycharacteristics/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_surveyCharacteristics(countryIds="EG",allResults=FALSE)
##' dat <- dhs_surveyCharacteristics(indicatorIds="FE_FRTR_W_TFR",
##' allResults=FALSE)
##' dat <- dhs_surveyCharacteristics(surveyIds="SN2010DHS,allResults=FALSE")
##' dat <- dhs_surveyCharacteristics(surveyYear="2010,allResults=FALSE")
##' dat <- dhs_surveyCharacteristics(surveyYearStart="2006",allResults=FALSE)
##' dat <- dhs_surveyCharacteristics(surveyYearStart="1991",
##' surveyYearEnd="2006",allResults=FALSE)
##' dat <- dhs_surveyCharacteristics(surveyType="DHS",allResults=FALSE)
##' dat <- dhs_surveyCharacteristics(f="html",allResults=FALSE)
##' }

dhs_surveyCharacteristics <- function(client=NULL,
                                      allResults=TRUE,
                                      countryIds=NULL,
                                      indicatorIds=NULL,
                                      surveyIds=NULL,
                                      surveyYear=NULL,
                                      surveyYearStart=NULL,
                                      surveyYearEnd=NULL,
                                      surveyType=NULL,
                                      f=NULL,
                                      returnFields=NULL,
                                      perPage=NULL,
                                      page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/surveycharacteristics"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Publications
##'
##' @title API request of DHS Publications
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param countryIds Specify a comma separated list of country ids to filter
##'   by. For a list of countries use
##'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
##' @param selectSurveys Specify to filter data from the latest survey by
##'   including `selectSurveys=TRUE` in your request. Note: Please use this
##'   parameter in conjunction with countryCode, surveyType, or indicatorIds
##'   for best results.
##' @param indicatorIds Specify a comma separated list of indicators ids to
##'   filter by. For a list of indicators use
##'   \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
##' @param surveyIds Specify a comma separated list of survey ids to filter by.
##'   For a list of surveys use
##'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param surveyYear Specify a comma separated list of survey years to
##'   filter by.
##' @param surveyYearStart Specify a range of Survey Years to filter
##'   Publications on. surveyYearStart is an inclusive value. Can be used alone
##'   or in conjunction with surveyYearEnd.
##' @param surveyYearEnd Specify a range of Survey Years to filter Publications
##'   on. surveyYearEnd is an inclusive value. Can be used alone or in
##'   conjunction with surveyYearStart.
##' @param surveyType Specify a survey type to filter by.
##' @param surveyCharacteristicIds Specify a survey characteristic id to filter
##'   publications with countries with the specified survey characteristics.
##'   For a list of survey characteristics use
##'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param tagIds Specify a tag id to filter publications with surveys
##'   containing indicators with the specified tag. For a list of tags use
##'   \code{dhs_tags()}
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format is
##'   JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset.
##'   By default the API will return page 1.
##'
##' @return Returns a `data.table` of 10 (or less if `returnFields` is provided)
##'   publications with detailed information for each publication. A detailed
##'   description of all the attributes returned is provided at
##'   \url{https://api.dhsprogram.com/rest/dhs/publications/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_publications(countryIds="EG",allResults=FALSE)
##' dat <- dhs_publications(selectSurveys="latest",allResults=FALSE)
##' dat <- dhs_publications(indicatorIds="FE_FRTR_W_TFR",allResults=FALSE)
##' dat <- dhs_publications(surveyIds="SN2010DHS",allResults=FALSE)
##' dat <- dhs_publications(surveyYear="2010",allResults=FALSE)
##' dat <- dhs_publications(surveyYearStart="2006",allResults=FALSE)
##' dat <- dhs_publications(surveyYearStart="1991", surveyYearEnd="2006",
##' allResults=FALSE)
##' dat <- dhs_publications(surveyType="DHS",allResults=FALSE)
##' dat <- dhs_publications(surveyCharacteristicIds="32",allResults=FALSE)
##' dat <- dhs_publications(tagIds=1,allResults=FALSE)
##' dat <- dhs_publications(f="html",allResults=FALSE)
##' }

dhs_publications <- function(client=NULL,
                             allResults=TRUE,
                             countryIds=NULL,
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
                             page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/publications"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Datasets
##'
##' @title API request of DHS Datasets
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param countryIds Specify a comma separated list of country ids to filter
##'   by. For a list of countries use
##'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
##' @param selectSurveys Specify to filter data from the latest survey by
##'   including `selectSurveys=TRUE` in your request. Note: Please use this
##'   parameter in conjunction with countryCode, surveyType, or indicatorIds for
##'   best results.
##' @param surveyIds Specify a comma separated list of survey ids to filter by.
##'   For a list of surveys use
##'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param surveyYear Specify a comma separated list of survey years to
##'   filter by.
##' @param surveyYearStart Specify a range of Survey Years to filter Datasets
##'   on. surveyYearStart is an inclusive value. Can be used alone or in
##'   conjunction with surveyYearEnd.
##' @param surveyYearEnd Specify a range of Survey Years to filter Datasets on.
##'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
##'   with surveyYearStart.
##' @param surveyType Specify a survey type to filter by.
##' @param fileFormat Specify the file format for the survey. Can use file
##'   format type name (SAS, Stata, SPSS, Flat, Hierarchical) or file format
##'   code. View list of file format codes -
##'   \url{https://dhsprogram.com/data/File-Types-and-Names.cfm}
##' @param fileType Specify the type of dataset generated for the survey
##'   (e.g. household, women, men, children, couples, etc.). View list of file
##'   type codes - \url{https://dhsprogram.com/data/File-Types-and-Names.cfm}
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format is
##'   JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset.
##'   By default the API will return page 1.
##'
##' @return Returns a `data.table` of 13 (or less if `returnFields` is provided)
##'   datasets with their corresponding details. A detailed description of all
##'   the attributes returned is provided at
##'   \url{https://api.dhsprogram.com/rest/dhs/datasets/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_datasets(countryIds="EG",allResults=FALSE)
##' dat <- dhs_datasets(selectSurveys="latest",allResults=FALSE)
##' dat <- dhs_datasets(surveyIds="SN2010DHS",allResults=FALSE)
##' dat <- dhs_datasets(surveyYear="2010",allResults=FALSE)
##' dat <- dhs_datasets(surveyYearStart="2006",allResults=FALSE)
##' dat <- dhs_datasets(surveyYearStart="1991", surveyYearEnd="2006",
##' allResults=FALSE)
##' dat <- dhs_datasets(surveyType="DHS",allResults=FALSE)
##' dat <- dhs_datasets(fileFormat="stata",allResults=FALSE)
##' dat <- dhs_datasets(fileFormat="DT",allResults=FALSE)
##' dat <- dhs_datasets(fileType="KR",allResults=FALSE)
##' dat <- dhs_datasets(f="geojson",allResults=FALSE)
##' }

dhs_datasets <- function(client=NULL,
                         allResults=TRUE,
                         countryIds=NULL,
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
                         page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/datasets"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Geometry
##'
##' @title API request of DHS Geometry
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param countryIds Specify a comma separated list of country ids to filter
##'   by. For a list of countries use
##'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
##' @param surveyIds Specify a comma separated list of survey ids to filter by.
##'   For a list of surveys use
##'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param surveyYear Specify a comma separated list of survey years to
##'   filter by.
##' @param surveyYearStart Specify a range of Survey Years to filter Geometry
##'   on. surveyYearStart is an inclusive value. Can be used alone or in
##'   conjunction with surveyYearEnd.
##' @param surveyYearEnd Specify a range of Survey Years to filter Geometry on.
##'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
##'   with surveyYearStart.
##' @param surveyType Specify a survey type to filter by.
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
##'   is JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset.
##'   By default the API will return page 1.
##'
##' @return Returns a `data.table` of 7 (or less if `returnFields` is provided)
##'   geometry with their corresponding details. A detailed description of all
##'   the attributes returned is provided at
##'   \url{https://api.dhsprogram.com/rest/dhs/geometry/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_geometry(countryIds="EG",allResults=FALSE)
##' dat <- dhs_geometry(surveyIds="SN2010DHS",allResults=FALSE)
##' dat <- dhs_geometry(surveyYear="2010",allResults=FALSE)
##' dat <- dhs_geometry(surveyYearStart="2006",allResults=FALSE)
##' dat <- dhs_geometry(surveyYearStart="1991", surveyYearEnd="2006",
##' allResults=FALSE)
##' dat <- dhs_geometry(surveyType="DHS",allResults=FALSE)
##' dat <- dhs_geometry(f="geojson",allResults=FALSE)
##' }


dhs_geometry <- function(client=NULL,
                         allResults=TRUE,
                         countryIds=NULL,
                         surveyIds=NULL,
                         surveyYear=NULL,
                         surveyYearStart=NULL,
                         surveyYearEnd=NULL,
                         surveyType=NULL,
                         f=NULL,
                         returnFields=NULL,
                         perPage=NULL,
                         page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/geometry"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Tags
##'
##' @title API request of DHS Tags
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##' then the specifed page only will be returned. Default = TRUE.
##' @param countryIds Specify a comma separated list of country ids to filter
##'   by. For a list of countries use
##'   \code{dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))}
##' @param indicatorIds Specify a comma separated list of indicators ids to
##'   filter by. For a list of indicators use
##'   \code{dhs_indicators(returnFields=c("IndicatorId","Label","Definition"))}
##' @param surveyIds Specify a comma separated list of survey ids to filter by.
##'   For a list of surveys use
##'   \code{dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel",
##'   "SurveyType","CountryName"))}
##' @param surveyYear Specify a comma separated list of survey years to
##'   filter by.
##' @param surveyYearStart Specify a range of Survey Years to filter Tags on.
##'   surveyYearStart is an inclusive value. Can be used alone or in
##'   conjunction with surveyYearEnd.
##' @param surveyYearEnd Specify a range of Survey Years to filter Tags on.
##'   surveyYearEnd is an inclusive value. Can be used alone or in conjunction
##'   with surveyYearStart.
##' @param surveyType Specify a survey type to filter by.
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format
##'   is JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset. By
##'   default the API will return page 1.
##'
##' @return Returns a `data.table` of 4 (or less if `returnFields` is provided)
##'   tags with detailed information. An indicators can be tagged with one or
##'   more tags to help identify certain topics an indicator can be indentified
##'   by. A description of the attributes returned is provided at
##'   \url{https://api.dhsprogram.com/rest/dhs/tags/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_tags(countryIds="EG",allResults=FALSE)
##' dat <- dhs_tags(indicatorIds="FE_FRTR_W_TFR",allResults=FALSE)
##' dat <- dhs_tags(surveyIds="SN2010DHS",allResults=FALSE)
##' dat <- dhs_tags(surveyYear="2010",allResults=FALSE)
##' dat <- dhs_tags(surveyYearStart="2006",allResults=FALSE)
##' dat <- dhs_tags(surveyYearStart="1991", surveyYearEnd="2006",
##' allResults=FALSE)
##' dat <- dhs_tags(surveyType="DHS",allResults=FALSE)
##' dat <- dhs_tags(f="html",allResults=FALSE)
##' }

dhs_tags <- function(client=NULL,
                     allResults=TRUE,
                     countryIds=NULL,
                     indicatorIds=NULL,
                     surveyIds=NULL,
                     surveyYear=NULL,
                     surveyYearStart=NULL,
                     surveyYearEnd=NULL,
                     surveyType=NULL,
                     f=NULL,
                     returnFields=NULL,
                     perPage=NULL,
                     page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/tags"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}



##' API request of DHS Data Updates
##'
##' @title API request of DHS Data Updates
##' @param client If the api request should be cached, then provide a client
##'   object created by \code{\link{client_dhs}}
##' @param allResults Boolean for if all results should be returned. If FALSE
##'   then the specifed page only will be returned. Default = TRUE.
##' @param lastUpdate Specify a date or Unix time to filter the updates by.
##'   Only results for data that have been updated on or after the specified
##'   date will be returned.
##' @param f You can specify the format of the data returned from the query as
##'   HTML, JSON, PJSON, geoJSON, JSONP, XML or CSV. The default data format is
##'   JSON.
##' @param returnFields Specify a list of attributes to be returned.
##' @param perPage Specify the number of results to be returned per page. By
##'   default the API will return 100 results.
##' @param page Allows specifying a page number to obtain for the recordset.
##'   By default the API will return page 1.
##'
##' @return Returns a `data.table` of 9 (or less if `returnFields` is provided)
##'   indicators or surveys that have been added/updated or removed. A detailed
##'   description of all the attributes returned is provided at
##'   \url{https://api.dhsprogram.com/rest/dhs/dataupdates/fields}
##'
##' @export
##' @examples
##' ## not run only because they take a very long time and interact with an API
##' \dontrun{
##' dat <- dhs_dataUpdates(lastUpdate="20150901",allResults=FALSE)
##' dat <- dhs_dataUpdates(f="html",allResults=FALSE)
##' }

dhs_dataUpdates <- function(client=NULL,
                            allResults=TRUE,
                            lastUpdate=NULL,
                            f=NULL,
                            returnFields=NULL,
                            perPage=NULL,
                            page=NULL) {

  # create query with all provided arguments
  args <- ls()
  query <- lapply(args, function(x) eval(parse(text = x)))
  names(query) <- args

  # specific endpoint for each function
  endpoint <- "https://api.dhsprogram.com/rest/dhs/dataupdates"

  # pass request on accordingly
  handle_api_request(endpoint, query, allResults, client)
}
