#if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies)

#######Prepare variable list of background information -- one set with values, other one without
#source("U:/respondi sinus daten/background_info/prepare_var_list.R")

######################################
#web_pageviews ---create one df from chunked up original data
#source("U:/respondi sinus daten/code/read_webpages.R")

####Draw sample data for students
#source("U:/respondi sinus daten/code/draw_samples.R")

######Prep survey data....some preparation done in stata script "prep_survey_data.do"
#source(("U:/respondi sinus daten/code/prep_survey_data.R"))
#load("U:/respondi sinus daten/survey_daten/survey_data_all.RData")

######Prep background data....some preparation done in stata script "prep_background_data.do"
#source(("U:/respondi sinus daten/code/prep_background_data.R"))
#load("U:/respondi sinus daten/background_info/background.RData")


######################################
#preparation of covariates for mobile views data
#source(("U:/respondi sinus daten/code/prep_mobile_views.R"))
######################################

######################################
#preparation of covariates for web visits data
#source(("U:/respondi sinus daten/code/prep_web_visits.R"))
######################################

######################################
#preparation of covariates for web pageviews data
#source(("U:/respondi sinus daten/code/prep_web_pageviews.R"))
######################################

