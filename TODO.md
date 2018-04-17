TODO Functionality list:

---

1. Anemia vignette should full embrace survey_variables() and extract() functionality
2. Need to add / fix parsers for a few edge cases: SNIR02FL, KEIR03FL, BOIR01FL
3. Some harmonization of terminology across package (questions vs. labels, etc.)
4. Additional functionality for merging datasets (IR, MR, AR, etc.)
5. Handling of survey strata
6. Additional functionality for statistical analysis of survey data

Additional dev todo's

1. Wrap all API examples in a skip_if_no_auth style bracket, this way they can be run on travis but not CRAN
2. ui's for:
    - get_datasets
    - get_downloaded_datasets
    - get_available_datasets
    - extract
    - survey_variables
    - survey_questions
3. place vignettes in vignette_src directory, where they are built, then copy those .md to vignette and write as .Rmd

Next major version

1. write a fake data function to create all the data formats we have seen and the exceptions so these can be tested without internet
2. testing with mocking rather than just full skipping API ones, and then use either the fake data or downlaoded and saved model datasets
