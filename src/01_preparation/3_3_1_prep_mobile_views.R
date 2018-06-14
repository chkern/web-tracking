library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies)
setwd("Z:\\Respondi\\")

mobile_views <- readRDS(file = ".\\original daten\\mobile_views.rds")

length(unique(mobile_views$panelist_id))

############################################################################
table(mobile_views$d_kind)

############################################################################
#Ever Connection Type X used?
kindofdev <- mobile_views %>%
  group_by(panelist_id,d_kind) %>%
  filter(row_number()==1)

#create dummies if ever used type X
kindofdev$ever_mob <- as.logical(kindofdev$d_kind == "mobile")
kindofdev$ever_tab <- as.logical(kindofdev$d_kind == "tablet")

#aggregate to individual level
kindofdev2 <- kindofdev %>%
  group_by(panelist_id) %>%
  summarise(
    ever_mob =  max(ever_mob),
    ever_tab  =  max(ever_tab)
  )


saveRDS(kindofdev2, file = "./data_pieces/mob_v_kindofdevice.RDS")
rm(kindofdev, kindofdev2)
##########################################################################
#How long Connection type X used? and how often?
#Data has up to three obs per person. We want one
#Total duration per connection
duration <- mobile_views %>%
  group_by(panelist_id, d_kind) %>%
  summarise(
    kind_dur_p = sum(duration, na.rm = TRUE),
    kind_n_p = n()
  )
#Create dummy that indicates what type of connection this observation is
MYDUMMIES <- dummy(duration$d_kind, sep = "_")
MYDUMMIES <- as.data.frame(MYDUMMIES)
#Attach ID , duration and count
MYDUMMIES$panelist_id <- duration$panelist_id
MYDUMMIES$kind_dur_p <- duration$kind_dur_p
MYDUMMIES$kind_n_p <- duration$kind_n_p

rm(duration)

#intermediate step: total duration irrespective of connection plus total number of observations
duration3 <- mobile_views %>%
  group_by(panelist_id) %>%
  summarise(
    kind_totdur_p_user = sum(duration, na.rm = TRUE),
    kind_totn_p = n()
  )
#merge to dataset with connection specific duration and count
duration <- merge(MYDUMMIES, duration3, by="panelist_id")
rm(MYDUMMIES)
#relative duration of type x: conn of type X / total con duration 
duration2 <- mutate(duration,
                    kind_rel_dur = kind_dur_p/kind_totdur_p_user,
                    kind_rel_n = kind_n_p/kind_totn_p)
rm(duration)

#still, multiple obs per user --- condense to one
##All wifi connections
duration_mob <- filter(duration2, d_kind_mobile==1)
duration_mob <- select(duration_mob, -(starts_with("d_kind")))
duration_mob <- rename(duration_mob,
                       kind_mob_dur = kind_dur_p,
                       kind_mob_rel_dur= kind_rel_dur,
                       kind_mob_tot_dur = kind_totdur_p_user,
                       kind_mob_n = kind_n_p,
                       kind_mob_rel_n= kind_rel_n,
                       kind_mob_tot_n = kind_totn_p
)
duration_mob <- select(duration_mob, -(kind_mob_tot_n))
duration_mob <- select(duration_mob, -(kind_mob_tot_dur))

##all cellular connections
duration_tab <- filter(duration2, d_kind_tablet==1)
duration_tab <- select(duration_tab, -(starts_with("d_kind")))
duration_tab <- rename(duration_tab,
                       kind_tab_dur = kind_dur_p,
                       kind_tab_rel_dur = kind_rel_dur,
                       kind_tab_tot_dur = kind_totdur_p_user,
                       kind_tab_n = kind_n_p,
                       kind_tab_rel_n= kind_rel_n,
                       kind_tab_tot_n = kind_totn_p
)
duration_tab <- select(duration_tab, -(kind_tab_tot_n))
duration_tab <- select(duration_tab, -(kind_tab_tot_dur))

##put them all in one DF
duration <- merge(duration_mob, duration_tab, by="panelist_id", all= TRUE)
rm(duration_mob,duration_tab,duration2)

duration <- merge(duration, duration3, by="panelist_id", all=TRUE)

rm(duration3)

#replace missings with zero <- obs with no count in connection X get an NA otherwise
duration[is.na(duration)] <- 0

saveRDS(duration, file = "./data_pieces/mob_v_duration_kind.RDS")
rm(duration)

############################################################################
############################################################################
############################################################################
table(mobile_views$d_usage_type)

############################################################################
#Ever Connection Type X used?
USETYPE <- mobile_views %>%
  group_by(panelist_id,d_usage_type) %>%
  filter(row_number()==1)

#create dummies if ever used type X
USETYPE$ever_ho <- as.logical(USETYPE$d_usage_type == "home")
USETYPE$ever_wo <- as.logical(USETYPE$d_usage_type == "work")

#aggregate to individual level
USETYPE2 <- USETYPE %>%
  group_by(panelist_id) %>%
  summarise(
    ever_ho =  max(ever_ho),
    ever_wo  =  max(ever_wo)
  )


saveRDS(USETYPE2, file = "./data_pieces/mob_v_usetype.RDS")
rm(USETYPE, USETYPE2)
##########################################################################
#How long Connection type X used? and how often?
#Data has up to three obs per person. We want one
#Total duration per connection
duration <- mobile_views %>%
  group_by(panelist_id, d_usage_type) %>%
  summarise(
    utype_dur_p = sum(duration, na.rm = TRUE),
    utype_n_p = n()
  )
#Create dummy that indicates what type of connection this observation is
MYDUMMIES <- dummy(duration$d_usage_type, sep = "_")
MYDUMMIES <- as.data.frame(MYDUMMIES)
#Attach ID , duration and count
MYDUMMIES$panelist_id <- duration$panelist_id
MYDUMMIES$utype_dur_p <- duration$utype_dur_p
MYDUMMIES$utype_n_p <- duration$utype_n_p

rm(duration)

#intermediate step: total duration irrespective of connection plus total number of observations
duration3 <- mobile_views %>%
  group_by(panelist_id) %>%
  summarise(
    utype_totdur_p_user = sum(duration, na.rm = TRUE),
    utype_totn_p = n()
  )
#merge to dataset with connection specific duration and count
duration <- merge(MYDUMMIES, duration3, by="panelist_id")
rm(MYDUMMIES)
#relative duration of type x: conn of type X / total con duration 
duration2 <- mutate(duration,
                    utype_rel_dur = utype_dur_p/utype_totdur_p_user,
                    utype_rel_n = utype_n_p/utype_totn_p)
rm(duration)

#still, multiple obs per user --- condense to one
##All wifi connections
duration_ho <- filter(duration2, d_usage_type_home==1)
duration_ho <- select(duration_ho, -(starts_with("d_usage")))
duration_ho <- rename(duration_ho,
                      utype_ho_dur = utype_dur_p,
                      utype_ho_rel_dur= utype_rel_dur,
                      utype_ho_tot_dur = utype_totdur_p_user,
                      utype_ho_n = utype_n_p,
                      utype_ho_rel_n= utype_rel_n,
                      utype_ho_tot_n = utype_totn_p
)
duration_ho <- select(duration_ho, -(utype_ho_tot_n))
duration_ho <- select(duration_ho, -(utype_ho_tot_dur))

##all cellular connections
duration_wo <- filter(duration2, d_usage_type_work==1)
duration_wo <- select(duration_wo, -(starts_with("d_usage")))
duration_wo <- rename(duration_wo,
                      utype_wo_dur = utype_dur_p,
                      utype_wo_rel_dur = utype_rel_dur,
                      utype_wo_tot_dur = utype_totdur_p_user,
                      utype_wo_n = utype_n_p,
                      utype_wo_rel_n= utype_rel_n,
                      utype_wo_tot_n = utype_totn_p
)
duration_wo <- select(duration_wo, -(utype_wo_tot_n))
duration_wo <- select(duration_wo, -(utype_wo_tot_dur))

##put them all in one DF
duration <- merge(duration_ho, duration_wo, by="panelist_id", all= TRUE)
rm(duration_ho,duration_wo,duration2)

duration <- merge(duration, duration3, by="panelist_id", all=TRUE)

rm(duration3)

#replace missings with zero <- obs with no count in connection X get an NA otherwise
duration[is.na(duration)] <- 0

saveRDS(duration, file = "./data_pieces/mob_v_duration_usetype.RDS")
rm(duration)
####################################################################################
####################################################################################

table(mobile_views$device_version)


############################################################################
#Ever Connection Type X used?
Connection <- mobile_views %>%
  group_by(panelist_id,connection) %>%
  filter(row_number()==1)

#create dummies if ever used type X
Connection$ever_wifi <- as.logical(Connection$connection == "wifi")
Connection$ever_cell <- as.logical(Connection$connection == "cellular")
Connection$ever_unknown <- as.logical(Connection$connection == "unknown")
#aggregate to individual level
Connection2 <- Connection %>%
  group_by(panelist_id) %>%
  summarise(
    ever_wifi =  max(ever_wifi),
    ever_cell  =  max(ever_cell),
    ever_unknown =  max(ever_unknown)
  )


saveRDS(Connection2, file = "./data_pieces/mob_v_connection.RDS")
rm(Connection, Connection2)
##########################################################################
#How long Connection type X used? and how often?
  #Data has up to three obs per person. We want one
#Total duration per connection
duration <- mobile_views %>%
  group_by(panelist_id, connection) %>%
  summarise(
    con_dur_p = sum(duration, na.rm = TRUE),
    con_n_p = n()
  )
#Create dummy that indicates what type of connection this observation is
MYDUMMIES <- dummy(duration$connection, sep = "_")
MYDUMMIES <- as.data.frame(MYDUMMIES)
#Attach ID , duration and count
MYDUMMIES$panelist_id <- duration$panelist_id
MYDUMMIES$con_dur_p <- duration$con_dur_p
MYDUMMIES$con_n_p <- duration$con_n_p

rm(duration)

#intermediate step: total duration irrespective of connection plus total number of observations
duration3 <- mobile_views %>%
  group_by(panelist_id) %>%
  summarise(
    con_totdur_p_user = sum(duration, na.rm = TRUE),
    con_totn_p = n()
    )
#merge to dataset with connection specific duration and count
duration <- merge(MYDUMMIES, duration3, by="panelist_id")
rm(MYDUMMIES)
#relative duration of type x: conn of type X / total con duration 
duration2 <- mutate(duration,
                    con_rel_dur = con_dur_p/con_totdur_p_user,
                    con_rel_n = con_n_p/con_totn_p)
rm(duration)

#still, multiple obs per user --- condense to one
##All wifi connections
duration_wifi <- filter(duration2, connection_wifi==1)
duration_wifi <- select(duration_wifi, -(starts_with("connection")))
duration_wifi <- rename(duration_wifi,
                        con_wifi_dur = con_dur_p,
                        con_wifi_rel_dur= con_rel_dur,
                        con_wifi_tot_dur = con_totdur_p_user,
                        con_wifi_n = con_n_p,
                        con_wifi_rel_n= con_rel_n,
                        con_wifi_tot_n = con_totn_p
                        )
duration_wifi <- select(duration_wifi, -(con_wifi_tot_n))
duration_wifi <- select(duration_wifi, -(con_wifi_tot_dur))

##all cellular connections
duration_cellular <- filter(duration2, connection_cellular==1)
duration_cellular <- select(duration_cellular, -(starts_with("connection")))
duration_cellular <- rename(duration_cellular,
                        con_cell_dur = con_dur_p,
                        con_cell_rel_dur = con_rel_dur,
                        con_cell_tot_dur = con_totdur_p_user,
                        con_cell_n = con_n_p,
                        con_cell_rel_n= con_rel_n,
                        con_cell_tot_n = con_totn_p
                        )
duration_cellular <- select(duration_cellular, -(con_cell_tot_n))
duration_cellular <- select(duration_cellular, -(con_cell_tot_dur))

##all unknown type connections
duration_unknown <- filter(duration2, connection_unknown==1)
duration_unknown <- select(duration_unknown, -(starts_with("connection")))
duration_unknown <- rename(duration_unknown,
                        con_unknown_dur = con_dur_p,
                        con_unknown_rel_dur = con_rel_dur,
                        con_unknown_tot_dur = con_totdur_p_user,
                        con_unknown_n = con_n_p,
                        con_unknown_rel_n= con_rel_n,
                        con_unknown_tot_n = con_totn_p
                        )
duration_unknown <- select(duration_unknown, -(con_unknown_tot_n))
duration_unknown <- select(duration_unknown, -(con_unknown_tot_dur))

##put them all in one DF
duration <- merge(duration_wifi, duration_cellular, by="panelist_id", all= TRUE)
duration <- merge(duration, duration_unknown, by="panelist_id", all=TRUE)
rm(duration_cellular,duration_unknown,duration_wifi,duration2)

duration <- merge(duration, duration3, by="panelist_id", all=TRUE)

rm(duration3)

#replace missings with zero <- obs with no count in connection X get an NA otherwise
duration[is.na(duration)] <- 0

saveRDS(duration, file = "./data_pieces/mob_v_duration_connection.RDS")
rm(duration)

#########################################################################
###scheme http vs https
#split up in two DF to assign "78" to cases without label
scheme_1 <- filter(mobile_views, scheme=="https" | scheme=="http")
scheme_other <- filter(mobile_views, !(scheme=="https" | scheme=="http"))
scheme_other$scheme <- 78
#put back together
mobile_views <- rbind(scheme_other, scheme_1)
rm(scheme_1,scheme_other)

#count n of obs by scheme and duration
scheme <- mobile_views %>%
  group_by(panelist_id, scheme) %>%
  summarise(
    scheme_n_p = n(),
    scheme_dur_p = sum(duration, na.rm = TRUE)
  )

#count n of obs by user only
scheme2 <- mobile_views %>%
  group_by(panelist_id) %>%
  summarise(
    scheme_tot_n = n(),
    scheme_tot_dur = sum(duration, na.rm = TRUE)
  )
#put both information together
scheme <- merge(scheme, scheme2, by= "panelist_id")
rm(scheme2)

scheme_http <- filter(scheme, scheme=="http") %>%
  select(-(scheme)) %>%
  mutate(
    http_rel_n = scheme_n_p/scheme_tot_n,
    http_rel_dur = scheme_dur_p/scheme_tot_dur
    ) %>%
  rename(
    http_n = scheme_n_p,
    http_dur = scheme_dur_p
  )

scheme_https <- filter(scheme, scheme=="https") %>%
  select(-(scheme)) %>%
  mutate(
    https_rel_n = scheme_n_p/scheme_tot_n,
    https_rel_dur = scheme_dur_p/scheme_tot_dur
  ) %>%
  select(-(scheme_tot_dur)) %>%
  select(-(scheme_tot_n)) %>%
  rename(
    https_n = scheme_n_p,
    https_dur = scheme_dur_p
  )

scheme_other <- filter(scheme, !(scheme=="https" | scheme=="http")) %>%
  select(-(scheme)) %>%
  mutate(
    other_rel_n = scheme_n_p/scheme_tot_n,
    other_rel_dur = scheme_dur_p/scheme_tot_dur
  ) %>%
  select(-(scheme_tot_dur)) %>%
  select(-(scheme_tot_n)) %>%
  rename(
    other_n = scheme_n_p,
    other_dur = scheme_dur_p
  )
  

scheme <- merge(scheme_http , scheme_https , by = "panelist_id", all = TRUE)
scheme <- merge(scheme , scheme_other , by = "panelist_id", all = TRUE)
scheme[is.na(scheme)] <- 0

rm(scheme_http,scheme_https,scheme_other) 

saveRDS(scheme, file = "./data_pieces/mob_v_scheme.RDS")
rm(scheme)


############################################################################
#OS Version of device?
version <- mobile_views %>%
  group_by(panelist_id,device_version) %>%
  filter(row_number()==1)

#create dummies if ever used type X
MYDUMMIES <- dummy(version$device_version, sep = "_")
MYDUMMIES <- as.data.frame(MYDUMMIES)
MYDUMMIES$panelist_id <- version$panelist_id

Version3 <- MYDUMMIES %>%
  group_by(panelist_id) %>%
  summarise(
    v_10.0.2 = max(device_version_10.0.2),
    v_10.1.1 = max(device_version_10.1.1),
    v_10.14.0 = max(device_version_10.14.0),
    v_10.15.0 = max(device_version_10.15.0),
    v_10.15.1 = max(device_version_10.15.1),
    v_10.2.0 = max(device_version_10.2.0),
    v_10.2.1 = max(device_version_10.2.1),
    v_10.3.0 = max(device_version_10.3.0),
    v_10.3.1 = max(device_version_10.3.1),
    v_10.3.2 = max(device_version_10.3.2),
    v_10.3.3 = max(device_version_10.3.3),
    v_10.4.0 = max(device_version_10.4.0),
    v_10.9.0 = max(device_version_10.9.0),
    v_11.0.0 = max(device_version_11.0.0),
    v_11.0.2 = max(device_version_11.0.2),
    v_11.0.3 = max(device_version_11.0.3),
    v_2.3.5 = max(device_version_2.3.5),
    v_4.0.3 = max(device_version_4.0.3),
    v_4.0.4 = max(device_version_4.0.4),
    v_4.1.1 = max(device_version_4.1.1),
    v_4.1.2 = max(device_version_4.1.2),
    v_4.2.2 = max(device_version_4.2.2),
    v_4.3 = max(device_version_4.3),
    v_4.4.2 = max(device_version_4.4.2),
    v_4.4.3 = max(device_version_4.4.3),
    v_4.4.4 = max(device_version_4.4.4),
    v_5.0 = max(device_version_5.0),
    v_5.0.1 = max(device_version_5.0.1),
    v_5.0.2 = max(device_version_5.0.2),
    v_5.1 = max(device_version_5.1),
    v_5.1.1 = max(device_version_5.1.1),
    v_6.0 = max(device_version_6.0),
    v_6.0.1 = max(device_version_6.0.1),
    v_7.0 = max(device_version_7.0),
    v_7.1.1 = max(device_version_7.1.1),
    v_7.1.2 = max(device_version_7.1.2),
    v_9.3.3 = max(device_version_9.3.3),
    v_NULL = max(device_version_NULL)
  )
rm(version,MYDUMMIES)

#user has more than one device?
Version3$v_upgraded <- rowSums( Version3[,2:39] )
table(Version3$v_upgraded)
#aggregate to individual level

saveRDS(Version3, file = "./data_pieces/mob_v_version.RDS")
rm(Version3)
################################################################################
#Ever device of manufacturer X used?
table(mobile_views$device_manufacturer)

Manufacturer <- mobile_views %>%
  group_by(panelist_id,device_manufacturer) %>%
  filter(row_number()==1)

#Manufacturer2 <- mobile_views %>%
#  group_by(panelist_id,device_manufacturer,device_model) %>%
#  filter(row_number()==1)
#rm(Manufacturer2,Manufacturer,MYDUMMIES)
MYDUMMIES <- dummy(Manufacturer$device_manufacturer, sep = "__")
MYDUMMIES <- as.data.frame(MYDUMMIES)
MYDUMMIES$panelist_id <- Manufacturer$panelist_id
MYDUMMIES <- MYDUMMIES %>%
  rename(device_manufacturer__Elebest = 'device_manufacturer__Elebest Germany',
         device_manufacturer__HMD = 'device_manufacturer__HMD Global',
         device_manufacturer__LG_E = 'device_manufacturer__LG Electronics',
         device_manufacturer__Pearl = 'device_manufacturer__Pearl GmbH',
         device_manufacturer__TCT2 = 'device_manufacturer__TCT Mobile Europe SAS',
         device_manufacturer__tplink = 'device_manufacturer__TP-LINK'
  )

Manufacturer3 <- MYDUMMIES %>%
  group_by(panelist_id) %>%
  summarise(
    dev_Acer = max(device_manufacturer__Acer),
    dev_Allwinner = max(device_manufacturer__Allwinner),
    dev_alps = max(device_manufacturer__alps),
    dev_Amazon = max(device_manufacturer__Amazon),
    dev_android = max(device_manufacturer__android),
    dev_Android = max(device_manufacturer__Android),
    dev_Apple = max(device_manufacturer__Apple),
    dev_archos = max(device_manufacturer__archos),
    dev_asus = max(device_manufacturer__asus),
    dev_blackberry = max(device_manufacturer__blackberry),
    dev_BlackBerry = max(device_manufacturer__BlackBerry),
    dev_Blackview = max(device_manufacturer__Blackview),
    dev_bq = max(device_manufacturer__bq),
    dev_Compal = max(device_manufacturer__Compal),
    dev_Coolpad = max(device_manufacturer__Coolpad),
    dev_CUBOT = max(device_manufacturer__CUBOT),
    dev_DOOGEE = max(device_manufacturer__DOOGEE),
    dev_E1031H1C = max(device_manufacturer__E1031H1C),
    dev_elebest = max(device_manufacturer__Elebest),
    dev_Elephone = max(device_manufacturer__Elephone),
    dev_GIGABYTE = max(device_manufacturer__GIGABYTE),
    dev_Gigaset = max(device_manufacturer__Gigaset),
    dev_HMD = max(device_manufacturer__HMD),
    dev_HOMTOM = max(device_manufacturer__HOMTOM),
    dev_htc = max(device_manufacturer__htc),
    dev_HTC = max(device_manufacturer__HTC),
    dev_HUAWEI = max(device_manufacturer__HUAWEI),
    dev_Intel = max(device_manufacturer__Intel),
    dev_iRULU = max(device_manufacturer__iRULU),
    dev_Jlinksz = max(device_manufacturer__Jlinksz),
    dev_JYT = max(device_manufacturer__JYT),
    dev_LeMobile = max(device_manufacturer__LeMobile),
    dev_lenovo = max(device_manufacturer__Lenovo),
    dev_LENOVO = max(device_manufacturer__LENOVO),
    dev_LGE = max(device_manufacturer__LGE),
    dev_LG_E = max(device_manufacturer__LG_E),
    dev_malata = max(device_manufacturer__malata),
    dev_MEDION = max(device_manufacturer__MEDION),
    dev_Mobistel = max(device_manufacturer__Mobistel),
    dev_motorola = max(device_manufacturer__motorola),
    dev_nubia = max(device_manufacturer__nubia),
    dev_NULL = max(device_manufacturer__NULL),
    dev_ODYS = max(device_manufacturer__ODYS),
    dev_OnePlus = max(device_manufacturer__OnePlus),
    dev_OPPO = max(device_manufacturer__OPPO),
    dev_OUKITEL = max(device_manufacturer__OUKITEL),
    dev_Pearl = max(device_manufacturer__Pearl ),
    dev_Prestigio = max(device_manufacturer__Prestigio),
    dev_rockchip = max(device_manufacturer__rockchip),
    dev_samsung = max(device_manufacturer__samsung),
    dev_SHIFT4.2 = max(device_manufacturer__SHIFT4.2),
    dev_SHIFT5.2 = max(device_manufacturer__SHIFT5.2),
    dev_SISWOO = max(device_manufacturer__SISWOO),
    dev_Sony = max(device_manufacturer__Sony),
    dev_SurfTab = max(device_manufacturer__SurfTab),
    dev_TCL = max(device_manufacturer__TCL),
    dev_TCT = max(device_manufacturer__TCT),
    dev_TCT2 = max(device_manufacturer__TCT2),
    dev_TechniSat = max(device_manufacturer__TechniSat),
    dev_thl = max(device_manufacturer__thl),
    dev_tplink = max(device_manufacturer__tplink),
    dev_Trekstor = max(device_manufacturer__Trekstor),
    dev_TrekStor = max(device_manufacturer__TrekStor),
    dev_ulefone = max(device_manufacturer__ulefone),
    dev_UMI = max(device_manufacturer__UMI),
    dev_UMIDIGI = max(device_manufacturer__UMIDIGI),
    dev_Vodafone = max(device_manufacturer__Vodafone),
    dev_WIKO = max(device_manufacturer__WIKO),
    dev_Wileyfox = max(device_manufacturer__Wileyfox),
    dev_Xiaomi = max(device_manufacturer__Xiaomi),
    dev_YIFANG = max(device_manufacturer__YIFANG),
    dev_ZTE = max(device_manufacturer__ZTE)
  )
rm(Manufacturer,MYDUMMIES)

#user has more than one device?
Manufacturer3$num_dev <- rowSums( Manufacturer3[,2:73] )
table(Manufacturer3$num_dev)

#mobile_views <- merge(mobile_views,Manufacturer3, by = "panelist_id")
saveRDS(Manufacturer3, file = "./data_pieces/mob_v_manufacturer.RDS")
rm(Manufacturer3)

#########################################################################
mobile_views <- readRDS(file = "./original daten/mobile_views.rds")

topappsclass <- read.csv(file = "./data/topappsclass_clean.csv", header = TRUE)
topappsclass$X <- NULL

topappsclass <- rename(topappsclass, app_n = App)

APPS <- mobile_views %>%
  filter(!(is.na(app_n)))

APPS <- select(APPS, panelist_id, app_n, duration, app_cat)

rm(mobile_views)

mv_2 <- merge(APPS, topappsclass, by = "app_n", all.x = TRUE)

summary(mv_2$Classification)

APPS_classified <- rename(mv_2, apps_clfied = Classification)
rm(APPS, mv_2, topappsclass)

totals <- APPS_classified %>% 
  group_by(panelist_id) %>% 
  summarise(
    tot_n = n(),
    tot_d = sum(duration, na.rm = TRUE))

#create category for not classified apps
levels(APPS_classified$apps_clfied) <- c(levels(APPS_classified$apps_clfied),"not classified")
APPS_classified$apps_clfied[is.na(APPS_classified$apps_clfied)]  <- "not classified"
summary(APPS_classified$apps_clfied)

APPS_class_2 <- APPS_classified %>% 
  group_by(panelist_id, apps_clfied) %>% 
  summarise(
    per_app_tot_n = n(),
    per_app_tot_d = sum(duration, na.rm = TRUE)
  )

APPS_class_2 <- merge(APPS_class_2, totals, by = "panelist_id")

rm(totals)

APPS_class_2 <- APPS_class_2 %>% 
  ungroup %>% 
  mutate(
    per_app_rel_n = per_app_tot_n/tot_n,
    per_app_rel_d = per_app_tot_d/tot_d
  )


APPS2_clfd1 <- select(APPS_class_2, panelist_id, apps_clfied, per_app_tot_n)
APPS2_clfd1 <- spread(APPS2_clfd1, apps_clfied, per_app_tot_n)
APPS2_clfd1[is.na(APPS2_clfd1)] <- 0

APPS2_clfd2 <- select(APPS_class_2, panelist_id, apps_clfied, per_app_tot_d)
APPS2_clfd2 <- spread(APPS2_clfd2, apps_clfied, per_app_tot_d)
APPS2_clfd2[is.na(APPS2_clfd2)] <- 0

APPS2_clfd3 <- select(APPS_class_2, panelist_id, apps_clfied, per_app_rel_n)
APPS2_clfd3 <- spread(APPS2_clfd3, apps_clfied, per_app_rel_n)
APPS2_clfd3[is.na(APPS2_clfd3)] <- 0

APPS2_clfd4 <- select(APPS_class_2, panelist_id, apps_clfied, per_app_rel_d)
APPS2_clfd4 <- spread(APPS2_clfd4, apps_clfied, per_app_rel_d)
APPS2_clfd4[is.na(APPS2_clfd4)] <- 0

appnames1 <- names(APPS2_clfd1)
appnames2 <- names(APPS2_clfd2)
appnames3 <- names(APPS2_clfd3)
appnames4 <- names(APPS2_clfd4)

appnames1 <- paste0('t_n_' , appnames1)
appnames2 <- paste0('t_d_' , appnames2)
appnames3 <- paste0('r_n_' , appnames3)
appnames4 <- paste0('r_d_' , appnames4)

APPS2_clfd1 <- set_names(APPS2_clfd1, nm = appnames1)
APPS2_clfd2 <- set_names(APPS2_clfd2, nm = appnames2)
APPS2_clfd3 <- set_names(APPS2_clfd3, nm = appnames3)
APPS2_clfd4 <- set_names(APPS2_clfd4, nm = appnames4)

APPS2_clfd1 <- rename(APPS2_clfd1, panelist_id = t_n_panelist_id)
APPS2_clfd2 <- rename(APPS2_clfd2, panelist_id = t_d_panelist_id)
APPS2_clfd3 <- rename(APPS2_clfd3, panelist_id = r_n_panelist_id)
APPS2_clfd4 <- rename(APPS2_clfd4, panelist_id = r_d_panelist_id)

rm(appnames1, appnames2,appnames3,appnames4)
APPS_classified <- merge(APPS2_clfd1, APPS2_clfd2, by="panelist_id")
APPS_classified <- merge(APPS_classified, APPS2_clfd3, by="panelist_id")
APPS_classified <- merge(APPS_classified, APPS2_clfd4, by="panelist_id")

rm(APPS2_clfd1, APPS2_clfd2,APPS2_clfd3,APPS2_clfd4, APPS_class_2)
saveRDS(APPS_classified, file = "data\\apps_classified.rds")

#########################################################################
mobile_views <- readRDS(file = ".\\original daten\\mobile_views.rds")
#How many different domains?
length(unique(mobile_views$domain))

#Total visits per domain?
#filter everything that has no domain
DOMAINS <- mobile_views %>%
  filter(is.na(app_n)) %>%
  filter(is.na(app_os))
DOMAINS <- select(DOMAINS, panelist_id:domain)
rm(mobile_views)
#Total number of domains and total duration per user --- constant within user
n_domains_user <- DOMAINS %>%
  group_by(panelist_id) %>%
  summarise(
    dom_tot_p_n = n(),
    dom_tot_p_dur = sum(duration, na.rm = TRUE)
  )

DOMAINS <- merge(DOMAINS, n_domains_user, by="panelist_id")

saveRDS(n_domains_user, file = "./data_pieces/mob_v_n_domains_user.RDS")
rm(n_domains_user)
#mobile_views <- merge(mobile_views, n_domains_user, by="panelist_id")
#mobile_views <- mobile_views[1:50000,]
#n_domains_user %>% group_by(panelist_id) %>% filter(row_number() == 1)

#############################
#dom_tot_p_n: how many different domains visited by this user? constant within user
#dom_tot_p_n: how much time did this user spent online? constant within user


#limit to 20 most used domains
SITES20 <- DOMAINS %>%
  group_by(panelist_id, domain) %>%
  summarise(count_visits = n(),
            time_spent = sum(duration, na.rm=TRUE))
SITES20 <- merge(DOMAINS, SITES20, by=c("panelist_id", "domain"))

#share of visits and duration per user per domain
X2 <- transmute(SITES20,
                dom_rel_p_n = count_visits/dom_tot_p_n,
                dom_rel_p_dur = time_spent/dom_tot_p_dur)
SITES20 <- cbind(SITES20,X2)
#X2 <- mobile_views %>% 
#  group_by(panelist_id, domain) %>% 
#  filter(row_number() == 1) %>%
#  select(panelist_id, domain, starts_with("dom_rel"))
#SITES20 <- merge(SITES20, X2, by = c("panelist_id", "domain"))

rm(X2)

SITES20 <- SITES20 %>%
  group_by(panelist_id, domain) %>%
  filter(row_number()==1)

SITES20 <- SITES20 %>%
  arrange(-(panelist_id), -(time_spent),-(count_visits) )%>%
  ungroup() %>%
  group_by(panelist_id) %>%
  arrange(panelist_id, -(time_spent),-(count_visits) ) %>%
  filter(row_number() >= 1, row_number() <= 5) 
#how many different domains remain?
length(unique(SITES20$domain)) #8k


SITES20_count <- SITES20
SITES20_count <- SITES20_count %>%
  select(panelist_id, domain, count_visits)
SITES20_count <- spread(SITES20_count, domain, count_visits)

SITES20_time <- SITES20
SITES20_time <- SITES20_time %>%
  select(panelist_id, domain, time_spent)
SITES20_time <- spread(SITES20_time, domain, time_spent)

SITES20_d_rel_n <- SITES20
SITES20_d_rel_n <- SITES20_d_rel_n %>%
  select(panelist_id, domain, dom_rel_p_n)
SITES20_d_rel_n <- spread(SITES20_d_rel_n, domain, dom_rel_p_n)

SITES20_d_rel_dur <- SITES20
SITES20_d_rel_dur <- SITES20_d_rel_dur %>%
  select(panelist_id, domain, dom_rel_p_dur)
SITES20_d_rel_dur <- spread(SITES20_d_rel_dur, domain, dom_rel_p_dur)


CNT_names <- names(SITES20_count)
TM_names <- names(SITES20_time)
DN_names <- names(SITES20_d_rel_n)
DD_names <- names(SITES20_d_rel_dur)

CNT_names <- paste0('CNT_', CNT_names)
TM_names <- paste0('TM_', TM_names)
DN_names <- paste0('DN_', DN_names)
DD_names <- paste0('DD_', DD_names)

SITES20_count <- set_names(SITES20_count, nm = CNT_names)
SITES20_time <- set_names(SITES20_time, nm = TM_names)
SITES20_d_rel_n <- set_names(SITES20_d_rel_n, nm = DN_names)
SITES20_d_rel_dur <- set_names(SITES20_d_rel_dur, nm = DD_names)

SITES20_count <- rename(SITES20_count, panelist_id = CNT_panelist_id)
SITES20_time <- rename(SITES20_time, panelist_id = TM_panelist_id)
SITES20_d_rel_n <- rename(SITES20_d_rel_n, panelist_id = DN_panelist_id)
SITES20_d_rel_dur <- rename(SITES20_d_rel_dur, panelist_id = DD_panelist_id)

rm(CNT_names, TM_names, DN_names, DD_names)
SITES20 <- merge(SITES20_count, SITES20_time, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_d_rel_n, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_d_rel_dur, by="panelist_id")

rm(SITES20_count, SITES20_time,SITES20_d_rel_n,SITES20_d_rel_dur)

table(is.na(SITES20$DN_google.de))
SITES20[is.na(SITES20)] <- 0

saveRDS(SITES20, file = "./data_pieces/mob_v_SITES20.RDS")
rm(DOMAINS, SITES20)
####################################################################################

###INFORMATION ON APP USAGE
mobile_views <- readRDS(file = "./original daten/mobile_views.rds")

#How many different Apps?
length(unique(mobile_views$app_n))
#filter everything that has no app name
APPS <- mobile_views %>%
  filter(!(is.na(app_n)))

#drop unnecessary content
rm(mobile_views)
APPS$scheme <- NULL
APPS$connection <- NULL
APPS$domain <- NULL
APPS$device_manufacturer <- NULL
APPS$device_model <- NULL
APPS$device_version <- NULL
APPS$url <- NULL

#Total number of Apps and total duration per user --- constant within user
n_apps_user <- APPS %>%
  group_by(panelist_id) %>%
  summarise(
    app_tot_p_n = n(),
    app_tot_p_dur = sum(duration, na.rm = TRUE)
  )
saveRDS(n_apps_user, file = ".\\data_pieces\\mob_v_n_apps_user.RDS")
APPS <- merge(APPS, n_apps_user, by="panelist_id")

#n_domains_user %>% group_by(panelist_id) %>% filter(row_number() == 1)

#############################
#dom_tot_p_n: how many different domains visited by this user? constant within user
#dom_tot_p_n: how much time did this user spent online? constant within user


#limit to 20 most used domains
SITES20 <- APPS %>%
  group_by(panelist_id, app_n) %>%
  summarise(count_visits = n(),
            time_spent = sum(duration, na.rm=TRUE))

SITES20 <- merge(SITES20, n_apps_user, by=c("panelist_id"))
rm(n_apps_user)


#share of visits and duration per user per domain
X2 <- transmute(SITES20,
                app_rel_p_n = count_visits/app_tot_p_n,
                app_rel_p_dur = time_spent/app_tot_p_dur)
SITES20 <- cbind(SITES20,X2)
#X2 <- APPS %>% 
#  group_by(panelist_id, app_n) %>% 
#  filter(row_number() == 1) %>%
#  select(panelist_id, app_n, starts_with("dom_rel"))
#SITES20 <- merge(SITES20, X2, by = c("panelist_id", "app_n"))
rm(X2)

SITES20 <- SITES20 %>%
  arrange(-(panelist_id), -(time_spent),-(count_visits) )%>%
  ungroup() %>%
  group_by(panelist_id) %>%
  arrange(panelist_id, -(time_spent),-(count_visits) ) %>%
  filter(row_number() >= 1, row_number() <= 10)
#how many different domains remain?
length(unique(SITES20$app_n)) #8k

SITES20_count <- SITES20
SITES20_count <- SITES20_count %>%
  select(panelist_id, app_n, count_visits)
SITES20_count <- spread(SITES20_count, app_n, count_visits)

SITES20_time <- SITES20
SITES20_time <- SITES20_time %>%
  select(panelist_id, app_n, time_spent)
SITES20_time <- spread(SITES20_time, app_n, time_spent)

SITES20_d_rel_n <- SITES20
SITES20_d_rel_n <- SITES20_d_rel_n %>%
  select(panelist_id, app_n, app_rel_p_n)
SITES20_d_rel_n <- spread(SITES20_d_rel_n, app_n, app_rel_p_n)

SITES20_d_rel_dur <- SITES20
SITES20_d_rel_dur <- SITES20_d_rel_dur %>%
  select(panelist_id, app_n, app_rel_p_dur)
SITES20_d_rel_dur <- spread(SITES20_d_rel_dur, app_n, app_rel_p_dur)


CNT_names <- names(SITES20_count)
TM_names <- names(SITES20_time)
DN_names <- names(SITES20_d_rel_n)
DD_names <- names(SITES20_d_rel_dur)

CNT_names <- paste0('CNT_', CNT_names)
TM_names <- paste0('TM_', TM_names)
DN_names <- paste0('DN_', DN_names)
DD_names <- paste0('DD_', DD_names)

SITES20_count <- set_names(SITES20_count, nm = CNT_names)
SITES20_time <- set_names(SITES20_time, nm = TM_names)
SITES20_d_rel_n <- set_names(SITES20_d_rel_n, nm = DN_names)
SITES20_d_rel_dur <- set_names(SITES20_d_rel_dur, nm = DD_names)

SITES20_count <- rename(SITES20_count, panelist_id = CNT_panelist_id)
SITES20_time <- rename(SITES20_time, panelist_id = TM_panelist_id)
SITES20_d_rel_n <- rename(SITES20_d_rel_n, panelist_id = DN_panelist_id)
SITES20_d_rel_dur <- rename(SITES20_d_rel_dur, panelist_id = DD_panelist_id)

rm(CNT_names, TM_names, DN_names, DD_names)
SITES20 <- merge(SITES20_count, SITES20_time, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_d_rel_n, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_d_rel_dur, by="panelist_id")

rm(SITES20_count, SITES20_time,SITES20_d_rel_n,SITES20_d_rel_dur)

#table(is.na(SITES20$DN_google.de))
SITES20[is.na(SITES20)] <- 0
#rm(list = ls())

#saveRDS(APPS, file = "U:/respondi sinus daten/data_pieces/mob_v_app.RDS")
rm(APPS)
saveRDS(SITES20, file = "./data_pieces/mob_v_APPS20.RDS")
####################################################################################


mobile_views <- readRDS(file = "./original daten/mobile_views.rds")

mobile_views <- mobile_views %>%
  filter(is.na(app_n)) %>%
  filter(is.na(app_os))

mobile_views$hour <- as.integer(substr(mobile_views$used_at, 12, 13))

v_twelvetosix <- mobile_views %>%
  group_by(panelist_id, domain) %>%
  summarise(
    v_betw12and6 = sum(ifelse(hour==00| hour==01| hour==02| hour==03|hour==04|hour==05, 1, 0), na.rm=TRUE)
  )
mobile_views <- merge(mobile_views, v_twelvetosix, by=c("panelist_id", "domain"), all.x=TRUE)
rm(v_twelvetosix)

# Total visits between 12 and 6
tot_v_twelvetosix <- mobile_views %>%
  group_by(panelist_id, domain) %>%
  summarise(
    tot_v_12to6 = as.numeric(sum(v_betw12and6)/(length(domain)))
  ) %>%
  group_by(panelist_id) %>%
  summarise(
    tot_12to6 = sum(tot_v_12to6, na.rm=TRUE)
  ) 
#n_domains_user <- merge(n_domains_user, tot_v_twelvetosix, by=c("panelist_id"), all.x=TRUE)
#rm(tot_v_twelvetosix)

# Total time spent online between 12 and 6
tot_t_12to6 <- mobile_views %>%
  group_by(panelist_id) %>%
  summarise(
    tot_t_time12to6 = sum(as.numeric(ifelse(hour==00| hour==01| hour==02| hour==03|hour==04|hour==05, duration, 0)), na.rm=TRUE)
  ) 
tot_v_twelvetosix <- merge(tot_v_twelvetosix, tot_t_12to6, by=c("panelist_id"), all.x=TRUE)
rm(tot_t_12to6)
mobile_views <- select(mobile_views, -hour)

# Time spent on weekends
mobile_views$date <- (substr(mobile_views$used_at, 1, 10))
mobile_views$day <- as.integer(strftime(mobile_views$date, '%u'))

time_weekend <- mobile_views %>%
  group_by(panelist_id, day) %>%
  summarise(
    time = sum(as.numeric(duration), na.rm=TRUE)
  )

time_weekend <- time_weekend %>%
  filter(day==6 | day==7) %>%
  group_by(panelist_id) %>%
  summarise(
    time_weekend = sum(time, na.rm=TRUE)
  )

tot_v_twelvetosix <- merge(tot_v_twelvetosix, time_weekend, by=c("panelist_id"), all.x=TRUE)
rm(time_weekend)

# Time spent on weekdays
time_weekday <- mobile_views %>%
  group_by(panelist_id, day) %>%
  summarise(
    time = sum(as.numeric(duration), na.rm=TRUE)
  )

time_weekday <- time_weekday %>%
  filter(day==1 | day==2 | day==3 | day==4 | day==5) %>%
  group_by(panelist_id) %>%
  summarise(
    time_weekday = sum(time)
  )
tot_v_twelvetosix <- merge(tot_v_twelvetosix, time_weekday, by=c("panelist_id"), all.x=TRUE)
rm(time_weekday)

saveRDS(tot_v_twelvetosix, file = "./data_pieces/mob_v_FRED_domains.RDS")


###same thing for apps
mobile_views <- readRDS(file = "./original daten/mobile_views.rds")
#filter everything that has no app name
APPS <- mobile_views %>%
  filter(!(is.na(app_n)))

mobile_views$hour <- as.integer(substr(mobile_views$used_at, 12, 13))

v_twelvetosix <- mobile_views %>%
  group_by(panelist_id, app_n) %>%
  summarise(
    v_betw12and6 = sum(ifelse(hour==00| hour==01| hour==02| hour==03|hour==04|hour==05, 1, 0), na.rm=TRUE)
  )
mobile_views <- merge(mobile_views, v_twelvetosix, by=c("panelist_id", "app_n"), all.x=TRUE)
rm(v_twelvetosix)

# Total visits between 12 and 6
tot_v_twelvetosix <- mobile_views %>%
  group_by(panelist_id, app_n) %>%
  summarise(
    tot_v_12to6 = as.numeric(sum(v_betw12and6)/(length(app_n)))
  ) %>%
  group_by(panelist_id) %>%
  summarise(
    tot_12to6 = sum(tot_v_12to6, na.rm=TRUE)
  ) 
#n_domains_user <- merge(n_domains_user, tot_v_twelvetosix, by=c("panelist_id"), all.x=TRUE)
#rm(tot_v_twelvetosix)

# Total time spent online between 12 and 6
tot_t_12to6 <- mobile_views %>%
  group_by(panelist_id) %>%
  summarise(
    tot_t_time12to6 = sum(as.numeric(ifelse(hour==00| hour==01| hour==02| hour==03|hour==04|hour==05, duration, 0)), na.rm=TRUE)
  ) 
tot_v_twelvetosix <- merge(tot_v_twelvetosix, tot_t_12to6, by=c("panelist_id"), all.x=TRUE)
rm(tot_t_12to6)
mobile_views <- select(mobile_views, -hour)

# Time spent on weekends
mobile_views$date <- (substr(mobile_views$used_at, 1, 10))
mobile_views$day <- as.integer(strftime(mobile_views$date, '%u'))

time_weekend <- mobile_views %>%
  group_by(panelist_id, day) %>%
  summarise(
    time = sum(as.numeric(duration), na.rm=TRUE)
  )

time_weekend <- time_weekend %>%
  filter(day==6 | day==7) %>%
  group_by(panelist_id) %>%
  summarise(
    time_weekend = sum(time, na.rm=TRUE)
  )

tot_v_twelvetosix <- merge(tot_v_twelvetosix, time_weekend, by=c("panelist_id"), all.x=TRUE)
rm(time_weekend)

# Time spent on weekdays
time_weekday <- mobile_views %>%
  group_by(panelist_id, day) %>%
  summarise(
    time = sum(as.numeric(duration), na.rm=TRUE)
  )

time_weekday <- time_weekday %>%
  filter(day==1 | day==2 | day==3 | day==4 | day==5) %>%
  group_by(panelist_id) %>%
  summarise(
    time_weekday = sum(time)
  )
tot_v_twelvetosix <- merge(tot_v_twelvetosix, time_weekday, by=c("panelist_id"), all.x=TRUE)
rm(time_weekday)

saveRDS(tot_v_twelvetosix, file = "./data_pieces/mob_v_FRED_apps.RDS")


#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#Put all data pices together
rm(list = ls())

media_usage <- readRDS(file = ".\\data_pieces\\MV_FB_News.RDS")
device <- readRDS(file = ".\\data_pieces\\mob_v_kindofdevice.RDS")
kind <- readRDS(file = ".\\data_pieces\\mob_v_duration_kind.RDS")
usetype_n <- readRDS(file = ".\\data_pieces\\mob_v_usetype.RDS")
usetype_dur<- readRDS(file = ".\\data_pieces\\mob_v_duration_usetype.RDS")
conn_n <- readRDS(file = ".\\data_pieces\\mob_v_connection.RDS")
conn_dur<- readRDS(file = ".\\data_pieces\\mob_v_duration_connection.RDS")
scheme <- readRDS(file = ".\\data_pieces\\mob_v_scheme.RDS")
version <- readRDS(file = ".\\data_pieces\\mob_v_version.RDS")
manufac <- readRDS( file = ".\\data_pieces\\mob_v_manufacturer.RDS")
n_dom <- readRDS( file = ".\\data_pieces\\mob_v_n_domains_user.RDS")
n_app <- readRDS( file = ".\\data_pieces\\mob_v_n_apps_user.RDS")
mv_Sites <- readRDS(file = ".\\data_pieces\\mob_v_SITES20.RDS")
mv_Apps <- readRDS(file = ".\\data_pieces\\mob_v_APPS20.RDS")
time_dom <- readRDS(file = ".\\data_pieces\\mob_v_FRED_domains.RDS")
time_apps <- readRDS(file = ".\\data_pieces\\mob_v_FRED_apps.RDS")


mv_prep <- merge(device, kind, by="panelist_id")
rm(device, kind)
mv_prep <- merge(mv_prep, usetype_n, by="panelist_id")
rm(usetype_n)
mv_prep <- merge(mv_prep, usetype_dur, by="panelist_id")
rm(usetype_dur)
mv_prep <- merge(mv_prep, conn_n, by="panelist_id")
rm(conn_n)
mv_prep <- merge(mv_prep, conn_dur, by="panelist_id")
rm(conn_dur)
mv_prep <- merge(mv_prep, scheme, by="panelist_id")
rm(scheme)
mv_prep <- merge(mv_prep, version, by="panelist_id")
rm(version)
mv_prep <- merge(mv_prep, manufac, by="panelist_id")
rm(manufac)
mv_prep <- merge(mv_prep, n_dom, by="panelist_id", all.x = TRUE)
rm(n_dom)
mv_prep <- merge(mv_prep, n_app, by="panelist_id", all.x = TRUE)
rm(n_app)

app_name <- names(time_apps)
dom_name <- names(time_dom)

app_name <- paste0('app_', app_name)
dom_name <- paste0('dom_', dom_name)

time_apps <- set_names(time_apps, nm = app_name)
time_dom <- set_names(time_dom, nm = dom_name)

time_apps <- rename(time_apps, panelist_id = app_panelist_id)
time_dom <- rename(time_dom, panelist_id = dom_panelist_id)

rm(dom_name, app_name)

time_data <- merge(time_dom, time_apps, by="panelist_id", all = TRUE)
rm(time_dom, time_apps)

time_data[is.na(time_data)] <- 0

mv_prep <- merge(mv_prep, time_data, by="panelist_id", all.x = TRUE)
rm(time_data)
mv_prep <- merge(mv_prep, media_usage, by="panelist_id", all.x = TRUE)
rm(media_usage)

saveRDS(mv_prep, file = ".\\data\\mobile_views_prepared_small.RDS")

mv_prep <- merge(mv_prep, mv_Sites, by="panelist_id", all.x = TRUE)
rm(mv_Sites)
mv_prep <- merge(mv_prep, mv_Apps, by="panelist_id", all.x = TRUE)
rm(mv_Apps)


saveRDS(mv_prep, file = ".\\data\\mobile_views_prepared.RDS")

