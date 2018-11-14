#install.packages("pacman")
library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies, urltools, parallel, stringr, httr)
#devtools::install_github("jayjacobs/tldextract")
#library(tldextract)

setwd("Z:\\Respondi\\RESPONDI_w3\\")


web_pageviews <- readRDS(".\\original daten\\web_pageviews.rds")

df1 <- web_pageviews[1:3000000,]
saveRDS(df1, file = ".\\data_pieces\\web_pageviews1.rds")
rm(df1)

df2 <- web_pageviews[3000001:6000000,]
saveRDS(df2, file = ".\\data_pieces\\web_pageviews2.rds")
rm(df2)

df3 <- web_pageviews[6000001:9000000,]
saveRDS(df3, file = ".\\data_pieces\\web_pageviews3.rds")
rm(df3)

df4 <- web_pageviews[9000001:12000000,]
saveRDS(df4, file = ".\\data_pieces\\web_pageviews4.rds")
rm(df4)

df5 <- web_pageviews[12000001:15000000,]
saveRDS(df5, file = ".\\data_pieces\\web_pageviews5.rds")
rm(df5)

df6 <- web_pageviews[15000001:16559164,]
saveRDS(df6, file = ".\\data_pieces\\web_pageviews6.rds")
rm(df6)
###################################################################################
web_pageviews <- readRDS(".\\data_pieces\\web_pageviews1.rds")

#web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
#web_pageviews$domain <- gsub("/","",web_pageviews$domain)
#suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$DECODED <-url_decode(web_pageviews$url)
web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
web_pageviews$domain <- gsub("/","",web_pageviews$domain)
suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$url <- NULL
web_pageviews$DECODED <- NULL
web_pageviews$domain <- NULL
web_pageviews$host <- suffix_df$host
web_pageviews$subdomain <- suffix_df$subdomain
web_pageviews$domain <- suffix_df$domain
web_pageviews$suffix <- suffix_df$suffix
rm(suffix_df)

saveRDS(web_pageviews, file = ".\\data_pieces\\web_pageviews1_prep.rds")
rm(web_pageviews)
#####################################################################################

web_pageviews <- readRDS(".\\data_pieces\\web_pageviews2.rds")

#web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
#web_pageviews$domain <- gsub("/","",web_pageviews$domain)
#suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$DECODED <-url_decode(web_pageviews$url)
web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
web_pageviews$domain <- gsub("/","",web_pageviews$domain)
suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$url <- NULL
web_pageviews$DECODED <- NULL
web_pageviews$domain <- NULL
web_pageviews$host <- suffix_df$host
web_pageviews$subdomain <- suffix_df$subdomain
web_pageviews$domain <- suffix_df$domain
web_pageviews$suffix <- suffix_df$suffix
rm(suffix_df)

saveRDS(web_pageviews, file = ".\\data_pieces\\web_pageviews2_prep.rds")
rm(web_pageviews)
#####################################################################################
web_pageviews <- readRDS(".\\data_pieces\\web_pageviews3.rds")

#web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
#web_pageviews$domain <- gsub("/","",web_pageviews$domain)
#suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$DECODED <-url_decode(web_pageviews$url)
web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
web_pageviews$domain <- gsub("/","",web_pageviews$domain)
suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$url <- NULL
web_pageviews$DECODED <- NULL
web_pageviews$domain <- NULL
web_pageviews$host <- suffix_df$host
web_pageviews$subdomain <- suffix_df$subdomain
web_pageviews$domain <- suffix_df$domain
web_pageviews$suffix <- suffix_df$suffix
rm(suffix_df)

saveRDS(web_pageviews, file = ".\\data_pieces\\web_pageviews3_prep.rds")
rm(web_pageviews)
#####################################################################################
web_pageviews <- readRDS(".\\data_pieces\\web_pageviews4.rds")

#web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
#web_pageviews$domain <- gsub("/","",web_pageviews$domain)
#suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$DECODED <-url_decode(web_pageviews$url)
web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
web_pageviews$domain <- gsub("/","",web_pageviews$domain)
suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$url <- NULL
web_pageviews$DECODED <- NULL
web_pageviews$domain <- NULL
web_pageviews$host <- suffix_df$host
web_pageviews$subdomain <- suffix_df$subdomain
web_pageviews$domain <- suffix_df$domain
web_pageviews$suffix <- suffix_df$suffix
rm(suffix_df)

#\\nas2.ad.uni-mannheim.de\vol_swnsswml\Home\swnsswml\Respondi\web_pageviews
saveRDS(web_pageviews, file = ".\\data_pieces\\web_pageviews4_prep.rds")
rm(web_pageviews)
#####################################################################################
web_pageviews <- readRDS(".\\data_pieces\\web_pageviews5.rds")

#web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
#web_pageviews$domain <- gsub("/","",web_pageviews$domain)
#suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$DECODED <-url_decode(web_pageviews$url)
web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
web_pageviews$domain <- gsub("/","",web_pageviews$domain)
suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$url <- NULL
web_pageviews$DECODED <- NULL
web_pageviews$domain <- NULL
web_pageviews$host <- suffix_df$host
web_pageviews$subdomain <- suffix_df$subdomain
web_pageviews$domain <- suffix_df$domain
web_pageviews$suffix <- suffix_df$suffix
rm(suffix_df)

saveRDS(web_pageviews, file = ".\\data_pieces\\web_pageviews5_prep.rds")
rm(web_pageviews)
#####################################################################################
web_pageviews <- readRDS(".\\data_pieces\\web_pageviews6.rds")

#web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
#web_pageviews$domain <- gsub("/","",web_pageviews$domain)
#suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$DECODED <-url_decode(web_pageviews$url)
web_pageviews$domain <- str_extract(web_pageviews$url, "(.*?)(\\/|$)")
web_pageviews$domain <- gsub("/","",web_pageviews$domain)
suffix_df <- suffix_extract(web_pageviews$domain, suffix_refresh())

web_pageviews$url <- NULL
web_pageviews$DECODED <- NULL
web_pageviews$domain <- NULL
web_pageviews$host <- suffix_df$host
web_pageviews$subdomain <- suffix_df$subdomain
web_pageviews$domain <- suffix_df$domain
web_pageviews$suffix <- suffix_df$suffix
rm(suffix_df)

saveRDS(web_pageviews, file = ".\\data_pieces\\web_pageviews6_prep.rds")
rm(web_pageviews)
#####################################################################################
rm(web_pageviews)
#####################################################################################
df1 <- readRDS(file = ".\\data_pieces\\web_pageviews1_prep.rds")
df2 <- readRDS(file = ".\\data_pieces\\web_pageviews2_prep.rds")
df3 <- readRDS(file = ".\\data_pieces\\web_pageviews3_prep.rds")
df4 <- readRDS(file = ".\\data_pieces\\web_pageviews4_prep.rds")
df5 <- readRDS(file = ".\\data_pieces\\web_pageviews5_prep.rds")
df6 <- readRDS(file = ".\\data_pieces\\web_pageviews6_prep.rds")

web_pageviews <- rbind(df1, df2, df3, df4, df5, df6)
rm(df1, df2, df3, df4, df5, df6)
web_pageviews$domain <- paste(web_pageviews$domain, web_pageviews$suffix, sep=".")

saveRDS(web_pageviews, file = ".\\data\\web_pageviews_prep.rds")
