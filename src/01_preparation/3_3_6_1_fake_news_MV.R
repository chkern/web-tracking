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
mobile_views <- readRDS(file = "./original daten/mobile_views.rds")

# only domains
mobile_views <- mobile_views %>%
  filter(is.na(app_n)) %>%
  filter(is.na(app_os))

# get domain plus subdomain for wordpress sites
mobile_views$DECODED <-url_decode(mobile_views$url)
mobile_views$domain2 <- str_extract(mobile_views$url, "(.*?)(\\/|$)")
mobile_views$domain2 <- gsub("/","",mobile_views$domain2)
mobile_views$domain2 <- gsub("www.","",mobile_views$domain2)
suffix_df <- suffix_extract(mobile_views$domain2, suffix_refresh())
mobile_views$DECODED <- NULL

suf_nam <- names(suffix_df)
suf_nam <- paste0('suf_', suf_nam)
suffix_df <- set_names(suffix_df, nm = suf_nam)
rm(suf_nam)

mobile_views <- cbind(mobile_views, suffix_df)
rm(suffix_df)


# prepare fakesites in same manner
suffix_fake$domain3 <- paste(suffix_fake$domain, suffix_fake$suffix, sep=".")


#########select all wordpress/blogspot sites because domain will only be wordpress and we need elements before
###do prep for those separately. Later, drop them from other dataset and add these rows
mv_wordpress <- mobile_views[grep("wordpress", mobile_views$domain), ]
mv_blogspot <- mobile_views[grep("blogspot", mobile_views$domain), ]

# drop .files extension because not in mobile views
mv_wordpress$suf_subdomain <- gsub(".files","",mv_wordpress$suf_subdomain)

# select all fakesites with wordpress
suffix_fake_wordpress <- suffix_fake[grep("wordpress", suffix_fake$host), ]
suffix_fake_wordpress <- select(suffix_fake_wordpress, subdomain, fake)
suffix_fake_wordpress <- rename(suffix_fake_wordpress, suf_subdomain = subdomain)


suffix_fake_blogspot <- suffix_fake[grep("blogspot", suffix_fake$host), ]
suffix_fake_blogspot <- select(suffix_fake_blogspot, subdomain, fake)
suffix_fake_blogspot <- rename(suffix_fake_blogspot, suf_subdomain = subdomain)
# finally, put two together
mv_wordpress <- merge(mv_wordpress, suffix_fake_wordpress, by="suf_subdomain", all.x=TRUE)
mv_blogspot <- merge(mv_blogspot, suffix_fake_blogspot, by="suf_subdomain", all.x=TRUE)



####
suffix_fake_wordpress <- suffix_fake[grep("wordpress", suffix_fake$host), ]
suffix_fake_blogspot <- suffix_fake[grep("blogspot", suffix_fake$host), ]

suffix_fake_nowordpress <- anti_join(suffix_fake, suffix_fake_wordpress,  by=c("host"))
suffix_fake_nowordpress_no_blogspot <- anti_join(suffix_fake_nowordpress, suffix_fake_blogspot,  by=c("host"))

suffix_fake <- suffix_fake_nowordpress_no_blogspot

rm(suffix_fake_nowordpress, suffix_fake_nowordpress_no_blogspot,  suffix_fake_wordpress, suffix_fake_blogspot)

# now merge info to all mobile_views sites that are NOT wordpress
mo_vie_no_wordpr <- anti_join(mobile_views, mv_wordpress, by=c("panelist_id", "url", "used_at"))
mo_vie_no_wordpr_no_blogspot <- anti_join(mo_vie_no_wordpr, mv_blogspot, by=c("panelist_id", "url", "used_at"))
rm(mo_vie_no_wordpr)


suffix_fake <- select(suffix_fake, domain3, fake)
suffix_fake <- rename(suffix_fake, domain = domain3)
mo_vie_no_wordpr_no_blogspot <- merge(mo_vie_no_wordpr_no_blogspot, suffix_fake, by="domain", all = TRUE)
mo_vie_no_wordpr_no_blogspot$panelist_id[is.na(mo_vie_no_wordpr_no_blogspot$panelist_id)] <- 0
mo_vie_no_wordpr_no_blogspot <- filter(mo_vie_no_wordpr_no_blogspot, panelist_id!=0)

mobile_views <- rbind(mo_vie_no_wordpr_no_blogspot, mv_blogspot, mv_wordpress)

rm(mo_vie_no_wordpr_no_blogspot, mv_blogspot, mv_wordpress, suffix_fake)
mobile_views <- select(mobile_views, -(starts_with("suf_")), -domain2)
rm(fake_sites)
mobile_views$fake[is.na(mobile_views$fake)] <- 0

table(mobile_views$fake)

d_totals <- mobile_views %>%
  group_by(panelist_id) %>% 
  summarise(
    tot_n = n(),
    tot_d = sum(duration, na.rm = TRUE)
  )

fakeshare <- mobile_views %>% 
  group_by(panelist_id, fake) %>%
  summarise(
    fake_tot_n = n(),
    fake_tot_d = sum(duration, na.rm = TRUE))  

fakeshare <- merge(fakeshare, d_totals, by="panelist_id")

fakeshare <- fakeshare %>%
  mutate(
    fake_rel_n = fake_tot_n/tot_n,
    fake_rel_d = fake_tot_d/tot_d
  ) %>% 
  filter(fake==1)

APP_names <- names(fakeshare)
APP_names <- paste0('mv_', APP_names)
fakeshare <- set_names(fakeshare, nm = APP_names)
fakeshare <- rename(fakeshare, panelist_id = mv_panelist_id)
rm(APP_names, d_totals)

fakeshare <- select(fakeshare, -c("mv_tot_n", "mv_tot_d"))

mobile_views <- readRDS(file = "./original daten/mobile_views.rds")

mobile_views <- mobile_views %>% 
  group_by(panelist_id) %>% 
  filter(row_number()==1) %>% 
  select(panelist_id)

fakeshare <- merge(mobile_views, fakeshare, by="panelist_id", all=TRUE)
rm(mobile_views)

fakeshare[is.na(fakeshare)] <- 0

saveRDS(fakeshare, file =  "./data/fake_mv_final.RDS")

