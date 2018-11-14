setwd("Y:\\Respondi\\RESPONDI_w3\\")  
library(haven)

wave_1 <- read_dta("./survey_daten/survey_data_w1.dta")
wave_2 <- read_dta("./survey_daten/survey_data_w2.dta")
wave_3 <- read_dta("./survey_daten/survey_data_w3.dta")

##get all objects with class "labelled" to correctly assign labels and levels
subset_colclasses <- function(DF, colclasses="labelled") {
  DF[,sapply(DF, function(vec, test) class(vec) %in% test, test=colclasses)]
}

wave_1_lab <- subset_colclasses(wave_1)
wave_1_lab <- as.data.frame(lapply(wave_1_lab, as_factor, levels = "both"))
wave_1_wo <- intersect(colnames(wave_1),colnames(wave_1_lab))
wave_1 <- wave_1[,!(names(wave_1) %in% wave_1_wo)]
wave_1 <- cbind(wave_1,wave_1_lab)
rm(wave_1_wo, wave_1_lab)

wave_2_lab <- subset_colclasses(wave_2)
wave_2_lab <- as.data.frame(lapply(wave_2_lab, as_factor, levels = "both"))
wave_2_wo <- intersect(colnames(wave_2),colnames(wave_2_lab))
wave_2 <- wave_2[,!(names(wave_2) %in% wave_2_wo)]
wave_2 <- cbind(wave_2,wave_2_lab)
rm(wave_2_wo, wave_2_lab)

wave_3_lab <- subset_colclasses(wave_3)
wave_3_lab <- as.data.frame(lapply(wave_3_lab, as_factor, levels = "both"))
wave_3_wo <- intersect(colnames(wave_3),colnames(wave_3_lab))
wave_3 <- wave_3[,!(names(wave_3) %in% wave_3_wo)]
wave_3 <- cbind(wave_3,wave_3_lab)
rm(wave_3_wo, wave_3_lab,subset_colclasses)

save.image("./survey_daten/survey_data_all.RData")
