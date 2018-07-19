## rdhs 0.5.0

* The way in which people provide their login details and set up their preferences
has substantially changed. `set_dhs_credentials` is now `set_rdhs_config`. This
function requires your email and project to be provided as argument, and will then
prompt you for your password. You also provide as arguments `cache_path` that will
specify where your API calls and datasets are to be cached. You can also control
where the config is saved using `config_path`. For more details, see the documentation
for `?set_rdhs_config` or within the tutorial [here](https://ojwatson.github.io/rdhs/articles/introduction.html).

* This change means now that when you use any of the API functions without setting
up a config, your API calls are cached within a temporary directory. After you have
set up a config, rdhs will look for your config location. This will happen by first
checking to see if you created a local config (`global = FALSE` within `set_rdhs_config`), 
which will be a file called "rdhs.json" in your current directory. If not there then it
will look for a global one (`global = TRUE` (the default)), which will be a file 
called "~/.rdhs.json". If that is not there it will check in your user cache directory
for your operating system. To see what config `rdhs` is using at any point just use
`get_rdhs_config`.

* These changes also affect a lot of internal changes, and have removed system 
environment variables being used as much. As such "rdhs_STARTUP_LOUD" and
"rdhs_TIMEOUT" are now controlled by arguments you set via `set_rdhs_config`. 

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
