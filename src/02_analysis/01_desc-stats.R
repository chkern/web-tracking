##################################################################################

library(tidyverse)
library(ggmosaic)
library(haven)
library(rtf)

##################################################################################
# Prepare data
##################################################################################

# Wave 3
setwd("Y:\\Respondi\\RESPONDI_w3\\")

# load socio demographic information
back <- readRDS("./background_info/background_small.RDS")
# load tracking data (small)
tracking_small <- readRDS(file = "./data/general_usage_info_final.rds")
# load the tracking data (full)
tracking <- readRDS(file = "./data/apps_and_sites_final.rds")
# load the wave 3 survey data
survey_w3 <- read_dta(file = "./survey_daten/survey_data_w3.dta")
# load media data
news_media <- readRDS(file = "./data/news_media_final.RDS")
# load fake data
fake <- readRDS(file = "./data/fake_final.RDS")
# load fake data
oeffrecht <- readRDS(file = "./data/oeffrecht_final.rds")

# put datasets together

X_back_track <- merge(back, fake, by = "panelist_id")
X_back_track <- merge(X_back_track, news_media, by = "panelist_id")
X_back_track <- merge(X_back_track, oeffrecht, by = "panelist_id")
X_back_track <- merge(X_back_track, tracking, by = "panelist_id")
X_back_track <- merge(X_back_track, tracking_small, by = "panelist_id")

# Attach Ys to X variables
Y <- back %>% 
  select(panelist_id, net_inc, age, gender, state, legal_status, num_children, emp_type)

Y$D_lowinc <- as.factor(ifelse(Y$net_inc %in% c("[1] under 500 EUR", "[2] 500 to 1.000 EUR"), "low", "not_low"))
Y$D_lowinc[Y$net_inc %in% c("[0] - please select -", "[98] No own income", "[99] No comment / answer")] <- NA
Y$D_highinc <- as.factor(ifelse(Y$net_inc %in% c("[11] over 5.000 EUR", "[10] 4.500 to 5.000 EUR", "[9] 4.000 to 4.500 EUR", "[8] 3.500 to 4.000 EUR", "[7] 3.000 to 3.500 EUR"), "high", "not_high"))
Y$D_highinc[Y$net_inc %in% c("[0] - please select -", "[98] No own income", "[99] No comment / answer")] <- NA
Y$D_u25 <- as.factor(ifelse(Y$age <= 25, "u25", "o25"))
Y$D_u25 <- relevel(Y$D_u25, ref = "u25")
Y$D_o60 <- as.factor(ifelse(Y$age >= 60, "o60", "u60"))
Y$D_male <- as.factor(ifelse(Y$gender == "[1] male", "male", "female"))
Y$D_male <- relevel(Y$D_male, ref = "male")
Y$D_east <- as.factor(ifelse(Y$state %in% c("[4] Brandenburg", "[8] Mecklenburg-Vorpommern", "[13] Sachsen", "[14] Sachsen-Anhalt", "[16] Thueringen"), "east", "west"))
Y$D_east[Y$state == "[0] - bitte auswählen -"] <- NA
Y$D_married <- as.factor(ifelse(Y$legal_status == "[1] Married", "married", "not_married"))
Y$D_nopartner <- as.factor(ifelse(Y$legal_status %in% c("[4] Single, not living with partner", "[6] Divorced/widowed, not living with partner"), "no_partner", "partner"))
Y$D_nochild <- as.factor(ifelse(Y$num_children == "[98] No children", "no_children", "children"))
Y$D_nochild <- relevel(Y$D_nochild, ref = "no_children")
Y$D_unemp <- as.factor(ifelse(Y$emp_type == "[7] Currently unemployed", "unemployed", "not_unemployed"))
Y$D_unemp[Y$emptype == "[0] - please select -"] <- NA
Y$D_unemp <- relevel(Y$D_unemp, ref = "unemployed")
Y$D_fulltime <- as.factor(ifelse(Y$emp_type == "[1] Work full-time (30+ hours per week)", "fulltime", "not_fulltime"))
Y$D_fulltime[Y$emptype == "[0] - please select -"] <- NA

Y <- Y %>% 
  select(panelist_id, D_lowinc, D_highinc, D_u25, D_o60, D_male, D_east, D_married, D_nopartner, D_nochild, D_unemp, D_fulltime)

X_back_track <- merge(X_back_track, Y, by = "panelist_id")

# Attach Ys to X variables
Y2 <- survey_w3 %>% 
  select(c(voted, AFD, LEFT, CDU, SPD, GREEN, FDP, panelist_id))

Y2$voted <- as.factor(Y2$voted)
levels(Y2$voted) <- c("not_voted", "voted")
Y2$voted <- relevel(Y2$voted, ref = "voted")
Y2$AFD <- as.factor(Y2$AFD)
levels(Y2$AFD) <- c("not_AFD", "AFD")
Y2$AFD <- relevel(Y2$AFD, ref = "AFD")
Y2$LEFT <- as.factor(Y2$LEFT)
levels(Y2$LEFT) <- c("not_LEFT", "LEFT")
Y2$LEFT <- relevel(Y2$LEFT, ref = "LEFT")
Y2$CDU <- as.factor(Y2$CDU)
levels(Y2$CDU) <- c("not_CDU", "CDU")
Y2$CDU <- relevel(Y2$CDU, ref = "CDU")
Y2$SPD <- as.factor(Y2$SPD)
levels(Y2$SPD) <- c("not_SPD", "SPD")
Y2$SPD <- relevel(Y2$SPD, ref = "SPD")
Y2$GREEN <- as.factor(Y2$GREEN)
levels(Y2$GREEN) <- c("not_GREEN", "GREEN")
Y2$GREEN <- relevel(Y2$GREEN, ref = "GREEN")
Y2$FDP <- as.factor(Y2$FDP)
levels(Y2$FDP) <- c("not_FDP", "FDP")
Y2$FDP <- relevel(Y2$FDP, ref = "FDP")

X_back_track2 <- merge(X_back_track, Y2, by = "panelist_id")

# Wave 2

setwd("Y:\\Respondi\\RESPONDI_w2\\")

# load socio demographic information
back <- readRDS("./background_info/background_small.RDS")
# load tracking data (small)
tracking_small <- readRDS(file = "./data/general_usage_info_final.rds")
# load the tracking data (full)
tracking <- readRDS(file = "./data/apps_and_sites_final.rds")
# load the wave 2 survey data
survey_w2 <- read_dta(file = "./survey_daten/survey_data_w2.dta")
# load media data
news_media <- readRDS(file = "./data/news_media_final.RDS")
# load fake data
fake <- readRDS(file = "./data/fake_final.RDS")
# load fake data
oeffrecht <- readRDS(file = "./data/oeffrecht_final.rds")

# put datasets together

X_back_track3 <- merge(back, fake, by = "panelist_id")
X_back_track3 <- merge(X_back_track3, news_media, by = "panelist_id")
X_back_track3 <- merge(X_back_track3, oeffrecht, by = "panelist_id")
X_back_track3 <- merge(X_back_track3, tracking, by = "panelist_id")
X_back_track3 <- merge(X_back_track3, tracking_small, by = "panelist_id")

# Attach Ys to X variables

Y3 <- survey_w2 %>% 
  select(c(undecided, panelist_id))

Y3$undecided <- as.factor(Y3$undecided)
levels(Y3$undecided) <- c("decided", "undecided")
Y3$undecided <- relevel(Y3$undecided, ref = "undecided")

X_back_track3 <- merge(X_back_track3, Y3, by = "panelist_id")
X_back_track2 <- merge(X_back_track2, Y3, by = "panelist_id")

##################################################################################
# Data exploration
##################################################################################

# Descriptive stats (outcomes)

tab_lowinc <- c(table(X_back_track$D_lowinc), prop.table(table(X_back_track$D_lowinc)), sum(!is.na(X_back_track$D_lowinc)))
tab_highinc <- c(table(X_back_track$D_highinc), prop.table(table(X_back_track$D_highinc)), sum(!is.na(X_back_track$D_highinc)))
tab_u25 <- c(table(X_back_track$D_u25), prop.table(table(X_back_track$D_u25)), sum(!is.na(X_back_track$D_u25)))
tab_o60 <- c(table(X_back_track$D_o60), prop.table(table(X_back_track$D_o60)), sum(!is.na(X_back_track$D_o60)))
tab_male <- c(table(X_back_track$D_male), prop.table(table(X_back_track$D_male)), sum(!is.na(X_back_track$D_male)))
tab_east <- c(table(X_back_track$D_east), prop.table(table(X_back_track$D_east)), sum(!is.na(X_back_track$D_east)))
tab_married <- c(table(X_back_track$D_married), prop.table(table(X_back_track$D_married)), sum(!is.na(X_back_track$D_married)))
tab_nopartner <- c(table(X_back_track$D_nopartner), prop.table(table(X_back_track$D_nopartner)), sum(!is.na(X_back_track$D_nopartner)))
tab_nochild <- c(table(X_back_track$D_nochild), prop.table(table(X_back_track$D_nochild)), sum(!is.na(X_back_track$D_nochild)))
tab_unemp <- c(table(X_back_track$D_unemp), prop.table(table(X_back_track$D_unemp)), sum(!is.na(X_back_track$D_unemp)))
tab_fulltime <- c(table(X_back_track$D_fulltime), prop.table(table(X_back_track$D_fulltime)), sum(!is.na(X_back_track$D_fulltime)))

tab_undecided <- c(table(X_back_track3$undecided), prop.table(table(X_back_track3$undecided)), sum(!is.na(X_back_track3$undecided)))
tab_voted <- c(table(X_back_track2$voted), prop.table(table(X_back_track2$voted)), sum(!is.na(X_back_track2$voted)))
tab_AFD <- c(table(X_back_track2$AFD), prop.table(table(X_back_track2$AFD)), sum(!is.na(X_back_track2$AFD)))
tab_LEFT <- c(table(X_back_track2$LEFT), prop.table(table(X_back_track2$LEFT)), sum(!is.na(X_back_track2$LEFT)))
tab_CDU <- c(table(X_back_track2$CDU), prop.table(table(X_back_track2$CDU)), sum(!is.na(X_back_track2$CDU)))
tab_SPD <- c(table(X_back_track2$SPD), prop.table(table(X_back_track2$SPD)), sum(!is.na(X_back_track2$SPD)))
tab_GREEN <- c(table(X_back_track2$GREEN), prop.table(table(X_back_track2$GREEN)), sum(!is.na(X_back_track2$GREEN)))
tab_FDP <- c(table(X_back_track2$FDP), prop.table(table(X_back_track2$FDP)), sum(!is.na(X_back_track2$FDP)))

tab <- rbind(tab_lowinc, tab_highinc, tab_u25, tab_o60, tab_male, tab_east, tab_married, tab_nopartner, tab_nochild, tab_unemp, tab_fulltime, 
             tab_undecided, tab_voted, tab_AFD, tab_LEFT, tab_CDU, tab_SPD, tab_GREEN, tab_FDP)
tab

rtffile <- RTF("desc.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)

p <- ggplot(X_back_track2) +
  geom_mosaic(aes(x = product(undecided, party_affiliation), fill = party_affiliation), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))
p + geom_label(data = ggplot_build(p)$data[[1]], aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label = .wt))

ggsave("d_outcomes.png", width = 8, height = 6)

# Descriptive stats (outcomes and tracking general)

# ggplot(X_back_track3) +
#  geom_mosaic(aes(x = product(mvevermob, party_affiliation), fill = party_affiliation, conds = product(undecided)), na.rm = TRUE) +
#  labs(y = "", x = "") +
#  theme(legend.position = "none")

# ggplot(X_back_track3) +
#  geom_mosaic(aes(x = product(mvevertab, party_affiliation), fill = party_affiliation, conds = product(undecided)), na.rm = TRUE) +
#  labs(y = "", x = "") +
#  theme(legend.position = "none")

# ggplot(X_back_track3) +
#  geom_boxplot(aes(x = party_affiliation, y = mvkindmobdur, fill = party_affiliation), na.rm = TRUE) +
#  labs(y = "", x = "") +
#  theme(legend.position = "none") + 
#  coord_cartesian(ylim = c(0, 2000000)) +
#  facet_grid(undecided ~ .)

# ggplot(X_back_track3) +
#  geom_boxplot(aes(x = party_affiliation, y = mvkindmobn, fill = party_affiliation), na.rm = TRUE) +
#  labs(y = "", x = "") +
#  theme(legend.position = "none") + 
#  coord_cartesian(ylim = c(0, 30000)) +
#  facet_grid(undecided ~ .)
