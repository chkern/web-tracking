#draw samples of URL and APP data

#draw 500 respondents sample
mobile_views <- readRDS(file = "U:/respondi sinus daten/original daten/mobile_views.rds")
web_visits <- readRDS(file = "U:/respondi sinus daten/original daten/web_visits.rds")
web_pages <- readRDS("U:/respondi sinus daten/original daten/web_pageviews.rds")


ids <- sample(unique(mobile_views$panelist_id), 500)


mobile_sample_userhistory <- mobile_views[mobile_views$panelist_id %in% ids, ]
webpage_sample_userhistory <- web_pages[web_pages$panelist_id %in% ids, ]
webvisits_sample_userhistory <- web_visits[web_visits$panelist_id %in% ids, ]


write.table(mobile_sample_userhistory, file = "U:/respondi sinus daten/500_r_sample/mobile_sample_userhistory.csv", sep="\t")
write.table(webpage_sample_userhistory, file = "U:/respondi sinus daten/500_r_sample/webpage_sample_userhistory.csv", sep="\t")
write.table(webvisits_sample_userhistory, file = "U:/respondi sinus daten/500_r_sample/webvisits_sample_userhistory.csv", sep="\t")

#draw 10k observations sample from each file
mobile_views <- readRDS(file = "U:/respondi sinus daten/original daten/mobile_views.rds")
web_visits <- readRDS(file = "U:/respondi sinus daten/original daten/web_visits.rds")
web_pages <- readRDS("U:/respondi sinus daten/original daten/web_pageviews.rds")

mobile_views_10k <- mobile_views[sample(1:nrow(mobile_views), 10000,
                          replace=FALSE),] 
web_visits_10k <- web_visits[sample(1:nrow(web_visits), 10000,
                                replace=FALSE),] 
web_pages_10k <- web_pages[sample(1:nrow(web_pages), 10000,
                           replace=FALSE),] 
length(unique(mobile_views_10k$panelist_id)) 


write.table(mobile_views_10k, file = "U:/respondi sinus daten/10_k_sample/mobile_views_10k.csv", sep="\t")
write.table(web_visits_10k, file = "U:/respondi sinus daten/10_k_sample/web_visits_10k.csv", sep="\t")
write.table(web_pages_10k, file = "U:/respondi sinus daten/10_k_sample/web_pages_10k.csv", sep="\t")
