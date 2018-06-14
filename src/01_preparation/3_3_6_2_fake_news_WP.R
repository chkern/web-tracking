library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies,urltools, caret, magrittr, randomForest,doParallel, snow)

##################################################################################
##Some information and descriptive stuff regarding Sinus Milieus
##################################################################################

setwd("Z:\\Respondi\\")
# read in data with fakenews
fake_sites <- read.csv(file=".\\media_landscape\\fakenews_propaganda_sites.csv", header=TRUE, sep = ",")
fake_sites <- fake_sites %>% 
  select(-c(X, X.1))
fake_sites <- fake_sites[c(1:89),]
fake_sites <- as.data.frame(fake_sites)

# get more info than URL because of wordpress sites where info is in subdomain
suffix_fake <- suffix_extract(fake_sites$fake_sites, suffix_refresh())

#gen identifier that will tell us after merge if fakenews domain
suffix_fake$fake <- 1

# prepare mobile views
web_pageviews <- readRDS(file = "./data/web_pageviews_prep.rds")

# prepare fakesites in same manner
suffix_fake$domain3 <- paste(suffix_fake$domain, suffix_fake$suffix, sep=".")

#########select all wordpress/blogspot sites because domain will only be wordpress and we need elements before
###do prep for those separately. Later, drop them from other dataset and add these rows
mv_wordpress <- web_pageviews[grep("wordpress", web_pageviews$domain), ]
# drop those that have no subdomain....irrelevant
mv_wordpress$subdomain[is.na(mv_wordpress$subdomain)] <- 0
mv_wordpress <- filter(mv_wordpress, subdomain!=0)

mv_blogspot <- web_pageviews[grep("blogspot", web_pageviews$domain), ]
mv_blogspot$domain <- gsub(".blogspot.de","",mv_blogspot$domain)
mv_blogspot$domain <- gsub(".blogspot.com","",mv_blogspot$domain)
mv_blogspot$domain <- gsub(".blogspot.co.at","",mv_blogspot$domain)
mv_blogspot$domain <- gsub(".blogspot.co.uk","",mv_blogspot$domain)
mv_blogspot$domain <- gsub(".blogspot.hu","",mv_blogspot$domain)
mv_blogspot$domain <- gsub(".blogspot.ru","",mv_blogspot$domain)
mv_blogspot$domain <- gsub(".blogspot.iu","",mv_blogspot$domain)
# actually, dont have to do this. all fakenews blogspots are .de
# drop .files extension because not in mobile views
mv_wordpress$subdomain <- gsub(".files","",mv_wordpress$subdomain)

# select all fakesites with wordpress
suffix_fake_wordpress <- suffix_fake[grep("wordpress", suffix_fake$host), ]
suffix_fake_wordpress <- select(suffix_fake_wordpress, subdomain, fake)
suffix_fake_wordpress <- rename(suffix_fake_wordpress, subdomain2 = subdomain)

suffix_fake_blogspot <- suffix_fake[grep("blogspot", suffix_fake$host), ]
suffix_fake_blogspot <- select(suffix_fake_blogspot, domain, fake)
suffix_fake_blogspot <- rename(suffix_fake_blogspot, subdomain2 = domain)

mv_wordpress$subdomain2 <- mv_wordpress$subdomain
mv_blogspot$subdomain2 <- mv_blogspot$domain


# finally, put two together
mv_wordpress <- merge(mv_wordpress, suffix_fake_wordpress, by="subdomain2", all.x=TRUE)
mv_blogspot <- merge(mv_blogspot, suffix_fake_blogspot, by="subdomain2", all.x=TRUE)



####
suffix_fake_wordpress <- suffix_fake[grep("wordpress", suffix_fake$host), ]
suffix_fake_blogspot <- suffix_fake[grep("blogspot", suffix_fake$host), ]

suffix_fake_nowordpress <- anti_join(suffix_fake, suffix_fake_wordpress,  by=c("host"))
suffix_fake_nowordpress_no_blogspot <- anti_join(suffix_fake_nowordpress, suffix_fake_blogspot,  by=c("host"))

suffix_fake <- suffix_fake_nowordpress_no_blogspot

rm(suffix_fake_nowordpress, suffix_fake_nowordpress_no_blogspot,  suffix_fake_wordpress, suffix_fake_blogspot)

# now merge info to all mobile_views sites that are NOT wordpress
mo_vie_no_wordpr <- anti_join(web_pageviews, mv_wordpress, by=c("panelist_id", "active_seconds", "used_at"))
mo_vie_no_wordpr_no_blogspot <- anti_join(mo_vie_no_wordpr, mv_blogspot, by=c("panelist_id", "active_seconds", "used_at"))
rm(mo_vie_no_wordpr)


suffix_fake <- select(suffix_fake, domain3, fake)
suffix_fake <- rename(suffix_fake, domain = domain3)
mo_vie_no_wordpr_no_blogspot <- merge(mo_vie_no_wordpr_no_blogspot, suffix_fake, by="domain", all = TRUE)
mo_vie_no_wordpr_no_blogspot$panelist_id[is.na(mo_vie_no_wordpr_no_blogspot$panelist_id)] <- 0
mo_vie_no_wordpr_no_blogspot <- filter(mo_vie_no_wordpr_no_blogspot, panelist_id!=0)

mv_blogspot$subdomain2 <- NULL
mv_wordpress$subdomain2 <- NULL


web_pageviews <- rbind(mo_vie_no_wordpr_no_blogspot, mv_blogspot, mv_wordpress)

rm(mo_vie_no_wordpr_no_blogspot, mv_blogspot, mv_wordpress, suffix_fake, fake_sites)
web_pageviews <- select(web_pageviews, -c("host", "subdomain", "suffix"))
web_pageviews$fake[is.na(web_pageviews$fake)] <- 0

table(web_pageviews$fake)

d_totals <- web_pageviews %>%
  group_by(panelist_id) %>% 
  summarise(
    tot_n = n(),
    tot_d = sum(active_seconds, na.rm = TRUE)
  )

fakeshare <- web_pageviews %>% 
  group_by(panelist_id, fake) %>%
  summarise(
    fake_tot_n = n(),
    fake_tot_d = sum(active_seconds, na.rm = TRUE))  

fakeshare <- merge(fakeshare, d_totals, by="panelist_id")

fakeshare <- fakeshare %>%
  mutate(
    fake_rel_n = fake_tot_n/tot_n,
    fake_rel_d = fake_tot_d/tot_d
  ) %>% 
  filter(fake==1)

APP_names <- names(fakeshare)
APP_names <- paste0('wp_', APP_names)
fakeshare <- set_names(fakeshare, nm = APP_names)
fakeshare <- rename(fakeshare, panelist_id = wp_panelist_id)
rm(APP_names)

fakeshare <- select(fakeshare, -c("wp_tot_n", "wp_tot_d"))


mobile_views <- readRDS(file = "./original daten/web_pageviews.rds")

mobile_views <- mobile_views %>% 
  group_by(panelist_id) %>% 
  filter(row_number()==1) %>% 
  select(panelist_id)

fakeshare <- merge(mobile_views, fakeshare, by="panelist_id", all=TRUE)
rm(mobile_views, d_totals, web_pageviews)

fakeshare[is.na(fakeshare)] <- 0

saveRDS(fakeshare, file =  "./data/fake_wp_final.RDS")

