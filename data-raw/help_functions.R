library(stringr)
library(tidyverse)

########## A function to transform word to number, however, this can't handle word larger than 15
w2n <- function(num_word){
  switch(  
    num_word,
    "zero" = 0,
    "one" = 1,
    "two" = 2,
    "three" = 3,
    "four" = 4,
    "five" = 5,
    "six" = 6,
    "seven" = 7,
    "eight" = 8,
    "nine" = 9,
    "ten" = 10,
    "eleven" = 11,
    "twelve" = 12,
    "thirteen" = 13,
    "fourteen" = 14,
    "fifteen" = 15
  )  
}
##########

########## A function to extract the number of years from a job description
get_year <- function(jd){
  loc <- str_locate_all(jd, "years")[[1]]
  
  # If the word "years" is not detected, we regard the job do not require working experience, i.e. working years = 0
  if (length(loc) == 0){
    return (0)
  }
  
  year_vec <- c()
  
  for (i in 1:nrow(loc)){
    year_tmp <- substr(jd, loc[i, 1] - 10, loc[i, 2]) %>% # Go backward 10 characters to search the number of years
      str_extract_all("[0-9]+") %>%
      {.[[1]]}
    
    # If no pattern like [0-9]+ is detected, we continue to detect number written in word like "one", "two" ...
    if (length(year_tmp) == 0){
      year_tmp <- substr(jd, loc[i, 1] - 10, loc[i, 2]) %>%
        str_to_lower() %>%
        str_extract_all(" zero | one | two | three | four | five | six | seven | eight | nine | ten | eleven | twelve | thirteen | fourteen | fifteen ") %>%
        {.[[1]]} %>%
        str_trim()
      
      # If also no pattern like "one", "two", "three" ... was detected, year_tmp will be a character(0), we regard it as no working year required and code it as 0
      year_tmp <- ifelse(length(year_tmp) == 0, 0, w2n(year_tmp))
    }
    
    year_vec <- c(year_vec, as.numeric(year_tmp))
  }
  
  year_vec <- year_vec[year_vec <= 15] # Cut off at 15, because some "years" are not requirement for applicants, but rather the description of the company, and these "years" can be super large, like 100
  
  return(max(c(year_vec, 0)))
}
##########

########## Functions to extract the required degree and skill sets from a job description

detect_sql <- function(des) {
  return(str_detect(des, regex("sql", ignore_case = TRUE)))
}
detect_power <- function(des) {
  return(str_detect(des, regex("power bi | powerbi", ignore_case = TRUE)))
}
detect_c <- function(des) {
  return(str_detect(des, regex("c++ | C++ | C language", ignore_case = TRUE)))
}
detect_micro <- function(des) {
  return(str_detect(des, regex("microsoft | excel | word | powerpoint | power point", 
                               ignore_case = TRUE)))
}
detect_sas <- function(des) {
  return(str_detect(des, regex("SAS")))
}
detect_spss <- function(des) {
  return(str_detect(des, regex("SPSS")))
}
detect_xml <- function(des) {
  return(str_detect(des, regex("XML")))
}
detect_etl <- function(des) {
  return(str_detect(des, regex("ETL")))
}
detect_comm <- function(des) {
  return(str_detect(des, regex("communicat", ignore_case = TRUE)))
}
detect_python <- function(des) {
  return(str_detect(des, regex("python", ignore_case = TRUE)))
}
detect_java <- function(des) {
  return(str_detect(des, regex("java", ignore_case = TRUE)))
}
detect_r <- function(des) {
  return(str_detect(des, regex(" R | R language")))
}
detect_tableau <- function(des) {
  return(str_detect(des, regex("tableau", ignore_case = TRUE)))
}
detect_spark <- function(des) {
  return(str_detect(des, regex("spark", ignore_case = TRUE)))
}

detect_bachelor <- function(des) {
  return(str_detect(des, regex("BA | BS | Bachelor", ignore_case = TRUE)))
}

detect_master <- function(des){
  return(str_detect(des, regex("MS | MA | master", ignore_case = TRUE)))
}

detect_phd <- function(des){
  return(str_detect(des, regex("phd | Ph.D. |doctor", ignore_case = TRUE)))
}
##########

########## A function used to detect if this job is related to data analyst
detect_data <- function(title) {
  return(str_detect(title, regex("data",ignore_case = TRUE)))
}
##########

########## These four functions detect salary units, four possible values:year, month, day, hour
detect_year <- function(salary) {
  return(str_detect(salary, regex("year",ignore_case = TRUE)))
}
detect_month <- function(salary) {
  return(str_detect(salary, regex("month",ignore_case = TRUE)))
}
detect_day <- function(salary) {
  return(str_detect(salary, regex("day",ignore_case = TRUE)))
}
detect_hour <- function(salary) {
  return(str_detect(salary, regex("hour",ignore_case = TRUE)))
}
##########

########## A function to transform salary unit to hour
get_salary <- function(salary) {
  salary=str_replace_all(salary, ",", "")
  Msalary=max(as.numeric(str_extract_all(salary,"[0-9]+")[[1]]))
  if (detect_year(salary)==1){
    Msalary=Msalary/1800
  }
  if (detect_month(salary)==1){
    Msalary=Msalary/160
  }
  if (detect_day(salary)==1){
    Msalary=Msalary/8
  }
  return (Msalary)
}
##########

########## A function to get the number of reviews
get_review=function(review) {
  review=str_replace_all(review, ",", "")
  review=as.numeric(str_extract_all(review,"[0-9]+")[[1]])
  return (review)
}
##########

########## A function to get the mixed job type
detect_mixed=function(type){
  return(str_detect(type,"-"))
}
##########

