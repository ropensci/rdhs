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
