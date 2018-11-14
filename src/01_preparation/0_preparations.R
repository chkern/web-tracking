#if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies)
setwd("Y:\\Respondi\\RESPONDI_w3\\")  

########################################
# Subset the tracking data to spells before beginning of first wave
# DO NOT RUN AGAIN
source("./code/0_2_subset_w3_DONOTRUN.R")


######################################
#web_pageviews ---create one df from chunked up original data
source("./code/1_read_webpages.R")

######Prep survey data....some preparation done in stata script "3_1_1_prep_survey_data.do"
source("./code/3_1_2_prep_survey_data.R")

######Prep background data....some preparation done in stata script "3_2_1_prep_background_data.do"
source("./code/3_2_2_prep_background_data.R")

######################################
#preparation of covariates for mobile views data
source("./code/3_3_1_prep_mobile_views.R")
######################################

######################################
#preparation of covariates for web pageviews data
source("./code/3_3_3_1_prep_web_pageviews.R")
source("./code/3_3_3_2_prep_web_pageviews.R")
######################################

######################################
#preparation and merge of news media data
source("./code/3_3_4_prep_news_media.R")
######################################


######################################
#preparation and merge of Oeffentlich-rechtliche data
source("./code/3_3_5_prep_oeffrecht.R")
######################################


######################################
#preparation and merge of fake news data
source("./code/3_3_6_1_fake_news.R")
######################################


######################################
#preparation and merge of fake news data
source("./code/4_prep_all_datasets.R")
#####################################

