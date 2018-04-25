
### TODO Functionality list:

---

1. Need to add / fix parsers for a few edge cases: SNIR02FL, KEIR03FL, BOIR01FL
2. Additional functionality for merging datasets (IR, MR, AR, etc.)
3. Handling of survey strata - ask DHS for strata of each survey so that new sample weighed indicators can be estimated
4. Additional functionality for statistical analysis of survey data, e.. wrapping in prevR, the above indicators, plotting etc


#### Next major version


1. Use the model datasets with a dummy login account that the DHS will set up to create a better demo for users
2. DRAKE THE FOLLOWING: Somehow wrap each step in the example analytical pipeline within internal package environments, so that the 
user can use a new function to return the precise function call tree used to produce the final dataset/statistics etc,
without accidentally altering it etc. It can then be packageversion/date branded so it can definitely be reproduced using
old datasets if they get updated, this aiding the reproducibility of their analysis.
