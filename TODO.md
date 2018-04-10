TODO:


2.	Vignette and function to take extracted survey results and calculate design effect adjusted estimates for numeric variables, and proportions for categorical variables at the cluster, sub-admin1, admin 1 level.
3.	Functionality to chain together pipeline in tutorial into something that can be cached and then repeated without fail by someone else.
6.	More testing and annotation
8. Something to merge across survey types (this shouldn't actually be hard I'm just buzy/bit bored/should do phd work to get round to this soon)
9. Something that does some validated fuzzy matching when survey codes and descriptions are not the same across surveys. Should combine columns that have the same code and description, and then match based on valid descriptions with some fizzy text match and then highlight all matches made this way for you to review and check. There are issues however when questions are "in the last x weeks" so although it's the same question it carries fundamentally different data.
10.Function to take multiple datasets and group_by cluster number and summarise all numeric data to produce clustered data - need to figure out what to do when it comes to categorical data as will become long if every character variable is simply table(x)/sum(table(x)) etc.

21. zip test break
24. Merging with HIV pacakge
25. recreate hrp2 discordance

26. 500 server errors catch in client load

...
