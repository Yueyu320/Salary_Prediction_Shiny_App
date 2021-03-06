% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_data.R
\docType{data}
\name{tidy_data}
\alias{tidy_data}
\title{JobInfo dataset}
\format{
A data frame with 645 observations on 23 variables.
\describe{
\item{salary}{dollars per hour}
\item{job_remote}{the job is remote or on-site}
\item{job_des}{detailed description of job}
\item{job_loc}{job location}
\item{job_type}{job types}
\item{sql}{if SQL is required}
\item{power}{if PowerBI is required}
\item{c}{if C/C++ is required}
\item{micro}{if Microsoft skills are required}
\item{sas}{if SAS is required}
\item{spss}{if SPSS is required}
\item{xml}{if XML is required}
\item{etl}{if ETL is required}
\item{comm}{if Communication skill is required}
\item{python}{if Python is required}
\item{java}{if Java is required}
\item{r}{if R is required}
\item{tableau}{if Tableau is required}
\item{spark}{if Spark is required}
\item{working_years}{required working years}
\item{degree}{required degree}
\item{data_analyst}{if this job is related to data analyst}
#'\item{review}{number of reviews for this job}
}
}
\source{
Scraping from Indeed website
}
\usage{
tidy_data
}
\description{
The data was extracted from the Indeed website, and comprises salary and 22 related variables.
}
\examples{
summary(tidy_data)

}
\keyword{datasets}
