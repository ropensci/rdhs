## rdhs 0.7.1

* Remove class `"dhs_dataset"` from downloaded micro data sets. This class is not 
  anywhere and it creates an error for dplyr_v1.0. 
  
  Cached datasets will need to be re-downloaded after updating to clear the 
  dhs_dataset clas.

* Replace `readLines()` with `brio::read_lines()` to make parsers robust to 
  Windows encoding issues (similar to https://stackoverflow.com/questions/18789330/r-on-windows-character-encoding-hell).

* Use `"sf"` as default download method for `download_boundaries(..., method = "sf")`.
  Add arguments `quiet_download` and `quiet_parse = TRUE` to 
  `download_boundaries()`. `quiet_download` (default `FALSE`) controls `download.file()` 
  messages. `quiet_parse` (default `TRUE`) controls messages from `sf::st_read()` when
  `method = "sf"`.

## rdhs 0.7.0

* Add CITATION info.
* New `download_boundaries` for downloading spatial boundaries using (#71)
* New `dhs_gps_data_format` for DHS GPS Information (#74)
* Tibbles can be specified correctly as data.frame format (#89)
* Config creation on Windows 10 fixed (#91)
* Typos and messaging fixed (#78, #84, #87, #92)
* `unzip_special` correctly detects 4Gb files (#43)

## rdhs 0.6.3

* Addresses CRAN fail on windows 
* New `delabel_df` for converting labelled data frames to characters (#54)

## rdhs 0.6.2

* Duplicate labels when parsing flat data files corrected (#79)

## rdhs 0.6.1

* `extraction(add_geo=TRUE)` correction for Kenya 2014 surveys (#67)
* Geospatial covariate data sets now supported correctly (#64)

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
