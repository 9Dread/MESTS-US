# MESTS-US: A Dataset of Model-Estimated Standardized Test Scores Across the US

This project is an updating prototype of an ML model-estimation of the spatial distribution of high school standardized test score performance. We gather and combine average score performance (SAT and ACT) at the high-school level for 10 states in total. ACT scores are converted to SAT scores as per common practice in other work. We also gather test counts and participation rates, as these are known nuisance parameters, but they have yet to be implemented in this analysis. They are also missing in some states; we impute them if that is the case. We use spatial and tabular methods to join schools in our training set to our predictive variables.

## Purpose

[Recent research](https://opportunityinsights.org/wp-content/uploads/2023/07/CollegeAdmissions_Paper.pdf) has found that Ivy-plus admissions disproportionately favor wealthy people due (1) to legacy admissions, (2) to athletic recruitment (since athletes are wealthier on average), and (3) to non-academic ratings e.g. extracurricular activities. However, none of these factors are related to higher performance after college, suggesting that they are on average unrelated to the potential of a student. On the other hand, high school test scores are strongly related to post-college outcomes. Since Ivy-plus colleges have a causal impact on their students' chances at reaching the top 1% of the wealth distribution, the authors argue that changing admissions practices could diversify the leaders of society.

To contribute to this, we estimate average performance at high schools across the country. There is plenty of literature that has found a clear relationship between socioeconomic status and academic performance, and we find work that is related to ours in [Stanford's education opportunity project](https://edopportunity.org/). However, to our knowledge our dataset of standardized test scores for high schools across the US is the first of its kinda, and it has significant use potential given standardized test scores' relevance in college admissions.

We hope that, given our model estimates of schools' average performance, we can have more publically-available information about how well a typical student performs at schools, and that this information can drive further educational research. In the future, with more sophisticated methodology, individual students may even be compared to their expected performance, contributing to more equitable college admissions across the country. 

## Status

This is an updating repository. We currently have a working model and have produced score estimates for most of the high schools across the US. They are visualized as follows:

![Spatial point plot of total, math, and reading scores across the US](https://github.com/9Dread/educationopportunity/blob/main/Figures/Points.PNG?raw=true)
![Plot of county-level average total, math, and erw scores across the US](https://github.com/9Dread/educationopportunity/blob/main/Figures/Counties.PNG?raw=true)
![Plot of county-level average total scores across the US](https://raw.githubusercontent.com/9Dread/educationopportunity/refs/heads/main/Figures/Counties_Total.PNG)

We also have model performance metrics, SHAP variable importance beeswarm plots, and prediction-value-change variable importance metrics for the Total, Math, and ERW models; see [Figures](https://github.com/9Dread/educationopportunity/blob/main/Figures).

## Coming Soon

This project is unfinished. As stated previously, we want to implement participation rates as a nuisance parameter for more unbiased estimates. We also plan to:
* Analyze factors that contribute to strong average performance at the county level
* Train another model on variables that change over time for time series visualization
* Conduct model validation experiments on other data sources
* Release a paper on our findings
* Release a supplementary visualization map application
* Conduct case studies i.e. analyze specific regions/cities of interest, corroborating with other research on said regions/cities

## Reproducibility

To reproduce our model and figures, one can open this repository and sequentially run all the code in the **numbered** R scripts in the *Scripts* folder.

Note that, while reproduction is possible, the repository has not yet been adjusted to make it seamless. For instance, the repository currently contains all of the base data needed (and all of the datasets created thereafter by our code), so it is pretty large. Some folders are unorganized and there are also some unused R scripts for testing. We will make adjustments for better reproducibility once the project nears its finish.

