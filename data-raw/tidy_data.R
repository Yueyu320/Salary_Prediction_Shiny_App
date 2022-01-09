## code to prepare `tidy_data` dataset goes here

remotes::install_github("rstudio/chromote")
library(hayalbaz)
library(chromote)
library(rvest)
library(tidyverse)
library(stringr)
library(devtools)
source("help_functions.R")

#n: the number of pages

scrape_data <- function(n){
  test <- puppet$new()
  # 10 items per page
  starts <- seq(0, n, by = 10)
  full_links <- tibble()
  for(start1 in starts) {
    cat(start1, "\n")
    url <- paste0("https://www.indeed.com/jobs?q=analyst&start=", start1)
    test$goto(url)
    test$wait_on_load()
    
    a <- test$get_source()
    
    links <- a %>%
      read_html() %>%
      html_nodes(".mosaic-zone a") %>%
      html_attr("href") %>%
      tibble(link = .) %>%
      filter(str_detect(link, "&vjs="))
    
    full_links <- rbind(full_links, links)
  }
  # remove duplicated values
  job_links <- tibble(link = unique(full_links$link))
  # add prefix to each link
  job_links <- job_links %>%
    mutate(link = paste0("https://www.indeed.com", link))
  # table used to store each job's information
  job_table <- data.frame()
  for (i in seq(1, nrow(job_links))){
    link <- job_links[i,] %>% pull()
    job_page <- read_html(link)
    tmp <- data.frame(
      job_title = ifelse(length(job_page %>%
                                  html_elements(".jobsearch-JobInfoHeader-title") %>%
                                  html_text2()) == 0, NA, job_page %>%
                           html_elements(".jobsearch-JobInfoHeader-title") %>%
                           html_text2()),
      job_salary = ifelse(length(job_page %>%
                                   html_elements(".jobsearch-JobMetadataHeader-item .icl-u-xs-mr--xs") %>%
                                   html_text2()) == 0, NA, job_page %>%
                            html_elements(".jobsearch-JobMetadataHeader-item .icl-u-xs-mr--xs") %>%
                            html_text2()),
      job_reviews = ifelse(length(job_page %>%
                                    html_elements(".icl-Ratings-link .icl-Ratings-count") %>%
                                    html_text2()) == 0, 0, job_page %>%
                             html_elements(".icl-Ratings-link .icl-Ratings-count") %>%
                             html_text2()),
      job_remote = ifelse(length(job_page %>%
                                   html_elements(".jobsearch-DesktopStickyContainer-companyrating~ div+ div") %>%
                                   html_text2()) == 0, "on site", job_page %>%
                            html_elements(".jobsearch-DesktopStickyContainer-companyrating~ div+ div") %>%
                            html_text2()),
      job_des = ifelse(length(job_page %>%
                                html_elements("#jobDescriptionText") %>%
                                html_text2()) == 0, NA, job_page %>%
                         html_elements("#jobDescriptionText") %>%
                         html_text2()),
      job_loc = ifelse(length(job_page %>%
                                html_elements(".jobsearch-DesktopStickyContainer-companyrating+ div") %>%
                                html_text2()) == 0, NA, job_page %>%
                         html_elements(".jobsearch-DesktopStickyContainer-companyrating+ div") %>%
                         html_text2()),
      job_type = ifelse(length(job_page %>%
                                 html_elements(" .jobsearch-JobMetadataHeader-item.icl-u-xs-mt--xs") %>%
                                 html_text2()) == 0, NA, job_page %>%
                          html_elements(" .jobsearch-JobMetadataHeader-item.icl-u-xs-mt--xs") %>%
                          html_text2())
      
    )
    job_table <- rbind(job_table, tmp)
  }
  # remove NA
  job_table <- job_table[!is.na(job_table$job_salary),]
  # remove duplicated values (based on description)
  job_table <- job_table[!duplicated(job_table$job_des), ]
  return(job_table)
}

job_table <- scrape_data(300)

#data: a data frame contains job related information (title, salary, review, remote, description, location, type)

data_preprocessing <- function(data){
  # extract working years, skill sets and required degree from job description
  descr <- data$job_des
  working_years <- c()
  skill_data <- data.frame()
  degree_data <- data.frame()
  for (i in seq_along(descr)) {
    sql <- data.frame(sql = ifelse(detect_sql(descr[i]), 1, 0))
    power <- data.frame(power = ifelse(detect_power(descr[i]), 1, 0))
    c <- data.frame(c = ifelse(detect_power(descr[i]), 1, 0))
    micro <- data.frame(micro = ifelse(detect_micro(descr[i]), 1, 0))
    sas <- data.frame(sas = ifelse(detect_sas(descr[i]), 1, 0))
    spss <- data.frame(spss = ifelse(detect_spss(descr[i]), 1, 0))
    xml <- data.frame(xml = ifelse(detect_xml(descr[i]), 1, 0))
    etl <- data.frame(etl = ifelse(detect_etl(descr[i]), 1, 0))
    # communication
    comm <- data.frame(comm = ifelse(detect_comm(descr[i]), 1, 0))
    python <- data.frame(python = ifelse(detect_python(descr[i]), 1, 0))
    java <- data.frame(java = ifelse(detect_java(descr[i]), 1, 0))
    r <- data.frame(r = ifelse(detect_r(descr[i]), 1, 0))
    tableau <- data.frame(tableau = ifelse(detect_tableau(descr[i]), 1, 0))
    spark <- data.frame(spark = ifelse(detect_spark(descr[i]), 1, 0))
    skill <- cbind(sql, power, c, micro, sas, spss, xml, etl, comm, python, java,
                   r, tableau, spark)
    skill_data <- rbind(skill_data, skill)
    
    bachelor <- data.frame(bachelor = ifelse(detect_bachelor(descr[i]), 1, 0))
    master <- data.frame(master = ifelse(detect_master(descr[i]), 2, 0))
    Phd <- data.frame(Phd = ifelse(detect_phd(descr[i]), 3, 0))
    degree <- cbind(bachelor, master, Phd)
    degree_data <- rbind(degree_data, degree)
    
    working_years <- c(working_years, get_year(descr[i]))
  }
  # append new columns
  new_data <- cbind(data, skill_data, degree_data, working_years)
  # create a new column degree and remove original ones
  # this new column is categorical and it contains four values:0, 1, 2, 3
  # 0 means no degree requirement; 1 means highest required degree is Bachelor
  # 2 means highest required degree is Master; 3 means highest required degree is PhD
  new_data <- new_data %>%
    mutate(degree = apply(degree_data, MARGIN = 1, max)) %>%
    select(-bachelor, -master, -Phd)
  # transform salary unit into hour; extract the number of reviews; identify if it is a data related job
  new_data <- new_data %>%
    mutate(data_analyst=as.numeric(sapply(job_title,detect_data, USE.NAMES = FALSE)),
           salary=round(sapply(job_salary,get_salary, USE.NAMES = FALSE), 2),
           review=sapply(job_reviews,get_review, USE.NAMES = FALSE))
  # change job_remote to two types
  new_data$job_remote[new_data$job_remote=="Temporarily remote"] <- "Remote"
  # change job_type
  new_data$job_type[is.na(new_data$job_type)] <- "unknown"
  new_data$job_type[new_data$job_type=="- Full-time"] <- "Full time"
  new_data$job_type[new_data$job_type=="- Contract"] <- "Contract"
  new_data$job_type[new_data$job_type=="- Part-time"] <- "Part time"
  new_data$job_type[new_data$job_type=="- Per diem"] <- "Per diem"
  new_data$job_type[new_data$job_type=="- Temporary"] <- "Temporary"
  new_data$job_type[sapply(new_data$job_type,detect_mixed)] <- "mixed"
  # change job_loc to their own states
  new_data$job_loc[sapply(new_data$job_loc,FUN=function(i){return (str_detect(i,"DC"))})] <- "DC"
  for (abb in state.abb){
    new_data$job_loc[sapply(new_data$job_loc,FUN=function(i){return (str_detect(i,abb))})] <- abb
  }
  for (j in seq_along(state.name)){
    new_data$job_loc[sapply(new_data$job_loc,FUN=function(i){
      return (str_detect(i,state.name[j]))})] <- state.abb[j]
  }
  # classify each state to a specific area
  west <- c("WA","OR","CA","NV","UT","AZ","ID","MT","WY","CO","NM","AK","HI")
  midwest <- c("ND","SD","NE","KS","MN","IA","MO","WI","IL","IN","MI","OH")
  south <- c("TX","OK","AR","LA","MS","AL","TN","KY","WV","MD","DE","DC","VA","NC","SC","GA","FL")
  northeast <- c("PA","NY","NJ","CT","RI","MA","VT","NH","ME")
  new_data$job_loc[new_data$job_loc %in% west] <- "west"
  new_data$job_loc[new_data$job_loc %in% midwest] <- "midwest"
  new_data$job_loc[new_data$job_loc %in% south] <- "south"
  new_data$job_loc[new_data$job_loc %in% northeast] <- "northeast"
  # If job location is not in these four areas, we set it to "unknown"
  new_data$job_loc[!(new_data$job_loc %in% c("west", "midwest", "south", "northeast"))] <- "unknown"
  # remove redundant columns
  new_data <- new_data %>%
    select(-job_title, -job_salary, -job_reviews)
  # Transform categorical variables from numerical type to factor type
  degree_set <- c("NotSpecified", "Bachelor", "Master", "PhD")
  new_data <- new_data %>%
    mutate(degree = degree_set[degree + 1] %>% factor()) # Transform the number notation of degree to word notation
  
  # transfer variables into factor except working_years, salary and review
  new_data[, -c(which(colnames(new_data) == "working_years"), 
                which(colnames(new_data) == "salary"), 
                which(colnames(new_data) == "review"))] <- new_data[, -c(which(colnames(new_data) == "working_years"), 
                                                                         which(colnames(new_data) == "salary"), 
                                                                         which(colnames(new_data) == "review"))] %>%
                                                                lapply(as.factor)
  return (new_data)
}

tidy_data <- data_preprocessing(job_table)

usethis::use_data(tidy_data, overwrite = TRUE)
