TODO Functionality list:

---

1. Functionality to chain together pipeline in tutorial into something that can be cached and then repeated without fail by someone else.
2. More testing and annotation
3. Merge across survey types
4. Something that does some validated fuzzy matching when survey codes and descriptions are not the same across surveys. Should combine columns that have the same code and description, and then match based on valid descriptions with some fizzy text match and then highlight all matches made this way for you to review and check. There are issues however when questions are "in the last x weeks" so although it's the same question it carries fundamentally different data.
5. Function to take multiple datasets and group_by cluster number and summarise all numeric data to produce clustered data - need to figure out what to do when it comes to categorical data and how to check the correct survey_design stratification for each dataset
6. 500 server errors catch in client load
