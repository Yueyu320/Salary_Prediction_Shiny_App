 #' @title The wage prediction function
 #'
 #' @description Function implementing the hourly wage in USD prediction function.
 #'
 #' The function takes a combination of job-related string and numeric value as inputs and
 #' predicts the hour wage in USD using imported information for the job. And the function utilizes AdaBoost
 #' model with tuned parameters to predict hour wages in USD.
 #'
 #' @param desc A string containing job-related description
 #' @param remote A string taking only "on site" or "Remote" as input (Case sensitive)
 #' @param loc A string taking abbreviations for states in the U.S. or "unknown" as inputs (ex: CO, NC, DC...(Case sensitive))
 #' @param type A string taking only "Contract", "Full time", "mixed", "Part time", "Per diem"(day worker), "Temporary" or "unknown" as input (Case sensitive)
 #' @param degree A string taking only "Bachelor", "Master", "NotSpecified" or "PhD" as input (Case sensitive)
 #' @param data A string taking only "Yes" or "No" as input (Case sensitive), "Yes" means this job is related to data analyst type
 #' @param review A numeric value (the number of reviews for the company where the job is)
 #'
 #' @details The inputs are expected to be numeric or in the string types and contain only one value for each input.
 #' And remote, loc, type, degree as well as data has to pick one value from available options provided in the
 #' parameter description section as input. Any violations of these assumptions will result in an error.
 #'
 #' @return predict_wage returns a list containing median predicted hourly wage in USD, minimum predicted hourly wage in USD, maximum predicted
 #' hourly wage in USD as well as 100 predicted hourly wages (vector) using bootstrap method.
 #'
 #' @examples
 #'
 #' predict_wage("Requires sql", "Remote", "NC", "mixed", "Master", "Yes", 33)
 #' predict_wage("Requires python", "on site", "CO", "Full time", "PhD", "No", 100)
 #'
 #' @export

predict_wage = function(desc, remote, loc, type, degree, data, review) {
  `%>%` <- magrittr::`%>%`
  if (length(desc) > 1 | length(remote) > 1 | length(loc) > 1 | length(type) > 1 | length(degree) > 1 | length(data) > 1 | length(review) > 1) {
    stop("Couldn't handle more than one input!")
  }
  if (!is.numeric(review) | !is.character(desc)) {
    stop("Plz check review or desc: review has to be a numeric value and desc has to be in the string type!")
  }
  if (!(remote %in% c("Remote", "on site"))) {
    stop("remote variable has to be 'Remote' or 'on site' (Case Sensitive)!")
  }
  states <- append(datasets::state.abb, "DC")
  states <- append(states, "unknown")
  if (!(loc %in% states)) {
    stop("loc variable has to be the abbreviation of states in the U.S. or 'unknown' (ex: CO)!")
  }
  if (!(type %in% c("mixed", "Full time", "unknown", "Contract", "Part time", "Per diem", "Temporary"))) {
    stop("type variable has to be one of 'mixed', 'Full time', 'unknown', 'Contract', 'Part time', 'Per diem','Temporary' types (Case sensitive)!")
  }
  if (!(degree %in% c("Bachelor", "Master", "NotSpecified", "PhD"))) {
    stop("degree variable has to be one of 'Bachelor', 'Master', 'NotSpecified', 'PhD' types (Case sensitive!)")
  }
  if (!(data %in% c("Yes", "No"))) {
    stop("data variable has to be one of 'Yes' or 'No' types (Case sensitive)!")
  }
  tidy_data <- DataAnalyst::tidy_data
  #all_data: delete unnecessary columns of tidy_data. Categorical features in this data frame are not dummified, but it can be used to train tree based models
  data_all <- tidy_data %>%
    dplyr::select(-c(job_des, spark, xml))
  # According to the Cross Validation result, we choose depth = 4 and ntrees = 75
  new_data <- cbind(description(desc),
                    data.frame(job_loc = as.factor(location(loc)),
                         job_remote = as.factor(remote),
                         job_type = as.factor(type),
                         degree = as.factor(degree),
                         data_analyst = as.factor(data_analyst(data)),
                         review = as.numeric(review))
  )


  # Use bootstrap to get interval estimate of a new data
  nBoot <- 100
  set.seed(123)
  ypred <- rep(NA, nBoot)
  for (i in 1:nBoot){
    # sample with replacement to get bootstrap samples
    boot.id <- sample(1:nrow(data_all), size = nrow(data_all), replace = TRUE)
    train.tmp <- data_all[boot.id, ]
    ada.fit <- gbm::gbm(log(salary) ~ ., data = train.tmp, distribution = "gaussian", n.trees = 75, interaction.depth = 4)
    ypred[i] <- exp(stats::predict(ada.fit, newdata = new_data, n.trees = 75))
  }

  return(list(hourly_wage_median = stats::median(ypred), hourly_wage_min = min(ypred), hourly_wage_max = max(ypred), hourly_wage_predictions = ypred))
}

location = function(loc) {
  # classify each state to a specific area
  if (loc %in% c("WA","OR","CA","NV","UT","AZ","ID","MT","WY","CO","NM","AK","HI")) {
    loc = "west"
  }
  if (loc %in% c("ND","SD","NE","KS","MN","IA","MO","WI","IL","IN","MI","OH")) {
    loc = "midwest"
  }
  if (loc %in% c("TX","OK","AR","LA","MS","AL","TN","KY","WV","MD","DE","DC","VA","NC","SC","GA","FL")) {
    loc = "south"
  }
  if (loc %in% c("PA","NY","NJ","CT","RI","MA","VT","NH","ME")) {
    loc = "northeast"
  }
  else {
    loc = "unknown"
  }
  return(loc)
}

data_analyst = function(data) {
  if (data == "Yes") {
    data = 1
  }
  if (data == "No") {
    data = 0
  }
  return(data)
}

detect_sql <- function(des) {
  return(stringr::str_detect(des, stringr::regex("sql", ignore_case = TRUE)))
}
detect_power <- function(des) {
  return(stringr::str_detect(des, stringr::regex("power bi | powerbi", ignore_case = TRUE)))
}
detect_c <- function(des) {
  return(stringr::str_detect(des, stringr::regex("c++ | C++ | C language", ignore_case = TRUE)))
}
detect_micro <- function(des) {
  return(stringr::str_detect(des, stringr::regex("microsoft | excel | word | powerpoint | power point",
                               ignore_case = TRUE)))
}
detect_sas <- function(des) {
  return(stringr::str_detect(des, stringr::regex("SAS")))
}
detect_spss <- function(des) {
  return(stringr::str_detect(des, stringr::regex("SPSS")))
}
detect_etl <- function(des) {
  return(stringr::str_detect(des, stringr::regex("ETL")))
}
detect_comm <- function(des) {
  return(stringr::str_detect(des, stringr::regex("communicat", ignore_case = TRUE)))
}
detect_python <- function(des) {
  return(stringr::str_detect(des, stringr::regex("python", ignore_case = TRUE)))
}
detect_java <- function(des) {
  return(stringr::str_detect(des, stringr::regex("java", ignore_case = TRUE)))
}
detect_r <- function(des) {
  return(stringr::str_detect(des, stringr::regex(" R | R language")))
}
detect_tableau <- function(des) {
  return(stringr::str_detect(des, stringr::regex("tableau", ignore_case = TRUE)))
}

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

get_year <- function(jd){
  loc <- stringr::str_locate_all(jd, "years")[[1]]

  # If the word "years" is not detected, we regard the job do not require working experience, i.e. working years = 0
  if (length(loc) == 0){
    return (0)
  }

  year_vec <- c()
  `%>%` <- magrittr::`%>%`

  for (i in 1:nrow(loc)){
    year_tmp <- substr(jd, loc[i, 1] - 10, loc[i, 2]) %>% # Go backward 10 characters to search the number of years
      stringr::str_extract_all("[0-9]+") %>%
      {.[[1]]}

    # If no pattern like [0-9]+ is detected, we continue to detect number written in word like "one", "two" ...
    if (length(year_tmp) == 0){
      year_tmp <- substr(jd, loc[i, 1] - 10, loc[i, 2]) %>%
        stringr::str_to_lower() %>%
        stringr::str_extract_all(" zero | one | two | three | four | five | six | seven | eight | nine | ten | eleven | twelve | thirteen | fourteen | fifteen ") %>%
        {.[[1]]} %>%
        stringr::str_trim()

      # If also no pattern like "one", "two", "three" ... was detected, year_tmp will be a character(0), we regard it as no working year required and code it as 0
      year_tmp <- ifelse(length(year_tmp) == 0, 0, w2n(year_tmp))
    }

    year_vec <- c(year_vec, as.numeric(year_tmp))
  }

  year_vec <- year_vec[year_vec <= 15] # Cut off at 15, because some "years" are not requirement for applicants, but rather the description of the company, and these "years" can be super large, like 100

  return(max(c(year_vec, 0)))
}

description <- function(desc) {
  sql <- data.frame(sql = ifelse(detect_sql(desc), 1, 0))
  power <- data.frame(power = ifelse(detect_power(desc), 1, 0))
  c <- data.frame(c = ifelse(detect_power(desc), 1, 0))
  micro <- data.frame(micro = ifelse(detect_micro(desc), 1, 0))
  sas <- data.frame(sas = ifelse(detect_sas(desc), 1, 0))
  spss <- data.frame(spss = ifelse(detect_spss(desc), 1, 0))
  etl <- data.frame(etl = ifelse(detect_etl(desc), 1, 0))
  # communication
  comm <- data.frame(comm = ifelse(detect_comm(desc), 1, 0))
  python <- data.frame(python = ifelse(detect_python(desc), 1, 0))
  java <- data.frame(java = ifelse(detect_java(desc), 1, 0))
  r <- data.frame(r = ifelse(detect_r(desc), 1, 0))
  tableau <- data.frame(tableau = ifelse(detect_tableau(desc), 1, 0))
  working_years <- data.frame(working_years = get_year(desc))
  desc <- cbind(sql, power, c, micro, sas, spss, etl, comm, python, java,
                      r, tableau, working_years)
  return(desc)
  }
