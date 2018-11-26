## rdhs 0.6.0

* New `as_factor.labelled` for backward compatibility with `haven <2.0.0` 
`labelled` classes.

* `model_datasets` now internal and exported dataset (#60).


## rdhs 0.5.2

* New vignettes: `country_codes`

* Documentation typos corrected (#55)

## rdhs 0.5.0

* New `set_rdhs_config` for providing login credentials. This deprecates
`set_dhs_credentials`. 

* New `get_rdhs_config` shows the credentials currently used by `rdhs`

## rdhs 0.4.0

* Permissionn from user now required for file saving.

* API requests can now ignore any cached responses (`force = TRUE`) argument 
(#23):

```R
dat <- dhs_countries(force = TRUE)
```

* `get_datasets(clear_cache = TRUE)` will clear the cached available datasets,
enabling newly requested datasets to be downloaded (@kaisero, #29).

* geojson objects can now be requested from the API so that you can return
geojson objects for mapping purposes (#28) e.g. :

```R
d <- dhs_data(countryIds = "SN", surveyYearStart = 2014, 
              breakdown = "subnational", returnGeometry = TRUE,
              f = "geojson")
              
# convert to spatial object
sp <- geojsonio::as.json(d) %>% geojsonio::geojson_sp

```

## rdhs 0.3.0

* New `dhs_data()`, `dhs_countries()` and other API functions (`dhs_x()`). 

## rdhs 0.2.1

* `authenticate_dhs()` now works with short project names.

## rdhs 0.2.0

* New vignettes: `anemia`

* New `read_dhs_flat()` for reading flat datasets.

## rdhs 0.1.0

* Initial share on Feb, 24th 2018 to colleagues at UNC.
