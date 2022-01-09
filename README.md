# DataAnalyst

- Many job postings on indeed don't include reference salary for job seekers. This package uses scraped job posting data which has reference salary as training data to train AdaBoost models and apply it for predicting salary of a job posting. 

- To be more specific, it sampled 100 sets of bootstrap dataset and fit 100 AdaBoost models and get 100 predicted hourly salaries using job information such as job location (abbreviations for states in the U.S.), job type (remote or on site), job description, job required degree, job required working years and etc. 

- The 100 predicted salaries lead to the point prediction (the median) of the salary and also an interval prediction (0.25 and 0.75 quantiles). The prediction function is provided to users, and it's also wrapped into a shiny app, which allows the user to interactively input basic information of a job posting and the shiny app could give them a visualization of the predicted salary about the specific job posting. 

## Usage

```{r eval=FALSE}
library(DataAnalyst)
predict_wage("Requires sql", "Remote", "NC", "mixed", "Master", "Yes", 33)
predict_wage("Requires python", "on site", "CO", "Full time", "PhD", "No", 100)
```

```{r eval=FALSE}
library(DataAnalyst)
DAwage_shiny()
```