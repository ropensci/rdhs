## rdhs 0.4.0

* The user has to now give permission for `rdhs` to write to files outside of 
your R temporary directory. This will be prompted the first time access any 
functionality that requires this, and then stored in you .Renviron file.

* Startup messages are silent by default now. They can be made loud using:
`Sys.setenv("rdhs_STARTUP_LOUD" = TRUE)`.

* The initial check against the API's last update date is now set to timeout
after 30 seconds. This is because the package will access the dataUpdates endpoint
when it is attached. Therefore if the API is frozen you will still be able to 
load the package. If 30 seconds is too long, this can be changed using: 
`Sys.setenv("rdhs_TIMEOUT" = 30)`

* API requests will return a `data.frame` by default, but this can be changed now
using: 
`Sys.setenv("rdhs_DATA_TABLE" = TRUE)`

* API requests and datasets can now be made ignoring any cached responses. API 
requests using the `force = TRUE` argument (#23) will ignore any cached responses.
`get_datasets(clear_cache = TRUE)` will now download datasets after clearing the
cached available datasets. This allows you to be able to donwload new datasets that
you have recently requested (#29 @kaisero).

* geojson objects can now be requested from the API (#28) so that you can return
geojson objects for mapping purposes (#28) e.g. :

```
d <- dhs_data(countryIds = "SN", surveyYearStart = 2014, 
              breakdown = "subnational", returnGeometry = TRUE,
              f = "geojson")
              
# convert to spatial object
sp <- geojsonio::as.json(d) %>% geojsonio::geojson_sp

```

* Large amount of style standardisation.

## rdhs 0.3.0

* Change to the UI - end user interaction is now through normal functions, as opposed
to `client_dhs` methods

* These are faciliated through the package internal environment, which means you now call
`set_dhs_credentials()` once with your credentials and the package handles everything else in 
the backend. 

## rdhs 0.2.1

* Patch to authenticate_dhs. failed to connect before if project names were very short. Fixed in 7e4ef81.

## rdhs 0.2.0

* New vignettes: `anemia`

* Substantial change to API interaction. All endpoints can now be queried using
bespoke functions taking the form `dhs_<endpoint>()`.

* As a result of API changes, the DHS client is called with rdhs::client_dhs() now.

* Flat datasets can now be read in with a variety of download read in options

## rdhs 0.1.0

* Initial share on Feb, 24th 2018 to colleagues at UNC
