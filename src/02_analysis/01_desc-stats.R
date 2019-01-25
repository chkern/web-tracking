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

X_back_track3 <- merge(back, fake, by = "panelist_id")
X_back_track3 <- merge(X_back_track3, news_media, by = "panelist_id")
X_back_track3 <- merge(X_back_track3, oeffrecht, by = "panelist_id")
X_back_track3 <- merge(X_back_track3, tracking, by = "panelist_id")
X_back_track3 <- merge(X_back_track3, tracking_small, by = "panelist_id")

# Attach Ys to X variables
Y <- survey_w3 %>% 
  select(c(voted, AFD, LEFT, party_affiliation, panelist_id))

Y$voted <- as.factor(Y$voted)
levels(Y$voted) <- c("not_voted", "voted")
Y$AFD <- as.factor(Y$AFD)
levels(Y$AFD) <- c("not_AFD", "AFD")
Y$LEFT <- as.factor(Y$LEFT)
levels(Y$LEFT) <- c("not_LEFT", "LEFT")
Y$party_affiliation <- as.factor(Y$party_affiliation)
levels(Y$party_affiliation) <- c("CDU","SPD","GREEN","FDP","LEFT","AFD","Other")
levels(Y$party_affiliation) <- c(levels(Y$party_affiliation), "not voted")
Y$party_affiliation[Y$voted == "not_voted"] <- "not voted"

X_back_track3 <- merge(X_back_track3, Y, by = "panelist_id")

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

X_back_track2 <- merge(back, fake, by = "panelist_id")
X_back_track2 <- merge(X_back_track2, news_media, by = "panelist_id")
X_back_track2 <- merge(X_back_track2, oeffrecht, by = "panelist_id")
X_back_track2 <- merge(X_back_track2, tracking, by = "panelist_id")
X_back_track2 <- merge(X_back_track2, tracking_small, by = "panelist_id")

# Attach Ys to X variables

Y2 <- survey_w2 %>% 
  select(c(undecided, panelist_id))

Y2$undecided <- as.factor(Y2$undecided)
levels(Y2$undecided) <- c("decided", "undecided")

X_back_track2 <- merge(X_back_track2, Y2, by = "panelist_id")
X_back_track3 <- merge(X_back_track3, Y2, by = "panelist_id")

##################################################################################
# Data exploration
##################################################################################

# Descriptive stats (outcomes)

tab1 <- c(table(X_back_track2$undecided), prop.table(table(X_back_track2$undecided)), sum(!is.na(X_back_track2$undecided)))
tab2 <- c(table(X_back_track3$voted), prop.table(table(X_back_track3$voted)), sum(!is.na(X_back_track3$voted)))
tab3 <- c(table(X_back_track3$AFD), prop.table(table(X_back_track3$AFD)), sum(!is.na(X_back_track3$AFD)))
tab4 <- c(table(X_back_track3$LEFT), prop.table(table(X_back_track3$LEFT)), sum(!is.na(X_back_track3$LEFT)))

tab <- rbind(tab1, tab2, tab3, tab4)
tab

rtffile <- RTF("desc.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)

p <- ggplot(X_back_track3) +
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
