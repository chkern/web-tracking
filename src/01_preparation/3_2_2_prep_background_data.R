setwd("Y:\\Respondi\\RESPONDI_w3\\")  


background <- read_dta("./background_info/background.dta")

##get all objects with class "labelled" to correctly assign labels and levels
subset_colclasses <- function(DF, colclasses="labelled") {
  DF[,sapply(DF, function(vec, test) class(vec) %in% test, test=colclasses)]
}

background_lab <- subset_colclasses(background)
background_lab <- as.data.frame(lapply(background_lab, as_factor, levels = "both"))
background_wo <- intersect(colnames(background),colnames(background_lab))
background <- background[,!(names(background) %in% background_wo)]
background <- cbind(background,background_lab)
rm(background_wo, background_lab, subset_colclasses)

levels(background$m_1001) <- c(levels(background$m_1001),"Single Household")
background$m_1001[background$md_1172 == "[1] 1 Person"] <- "Single Household"
summary(background$m_1001)

levels(background$md_1223) <- c(levels(background$md_1223),"No answer provided")
background$md_1223[background$md_1181 == "[4] School"] <- "[98] I do not work"
background$md_1223[background$md_1181 == "[5] Student"] <- "[98] I do not work"
background$md_1223[background$md_1181 == "[7] Currently unemployed"] <- "[98] I do not work"
background$md_1223[background$md_1181 == "[8] Pensioner/retired, formerly in full-time work"] <- "[98] I do not work"
background$md_1223[background$md_1181 == "[9] Not working (housewife/house husband)"] <- "[98] I do not work"
background$md_1223[background$md_1181 == "[10] Maternity leave, Parental leave, Sabbatical"] <- "[98] I do not work"
background$md_1223[is.na(background$md_1223)] <- "No answer provided"
summary(background$md_1223)

levels(background$md_1634) <- c(levels(background$md_1634),"No answer provided")
background$md_1634[background$md_1181 == "[4] School"] <- "[98] I do not work"
background$md_1634[background$md_1181 == "[5] Student"] <- "[98] I do not work"
background$md_1634[background$md_1181 == "[7] Currently unemployed"] <- "[98] I do not work"
background$md_1634[background$md_1181 == "[8] Pensioner/retired, formerly in full-time work"] <- "[98] I do not work"
background$md_1634[background$md_1181 == "[9] Not working (housewife/house husband)"] <- "[98] I do not work"
background$md_1634[background$md_1181 == "[10] Maternity leave, Parental leave, Sabbatical"] <- "[98] I do not work"
background$md_1634[is.na(background$md_1634)] <- "No answer provided"
summary(background$md_1634)

levels(background$md_1635) <- c(levels(background$md_1635),"No answer provided")
background$md_1635[background$md_1181 == "[4] School"] <- "[98] I do not work"
background$md_1635[background$md_1181 == "[5] Student"] <- "[98] I do not work"
background$md_1635[background$md_1181 == "[7] Currently unemployed"] <- "[98] I do not work"
background$md_1635[background$md_1181 == "[8] Pensioner/retired, formerly in full-time work"] <- "[98] I do not work"
background$md_1635[background$md_1181 == "[9] Not working (housewife/house husband)"] <- "[98] I do not work"
background$md_1635[background$md_1181 == "[10] Maternity leave, Parental leave, Sabbatical"] <- "[98] I do not work"
background$md_1635[is.na(background$md_1635)] <- "No answer provided"
summary(background$md_1635)

levels(background$md_1660) <- c(levels(background$md_1660),"No answer provided")
background$md_1660[background$md_1181 == "[4] School"] <- "[98] I do not work"
background$md_1660[background$md_1181 == "[5] Student"] <- "[98] I do not work"
background$md_1660[background$md_1181 == "[7] Currently unemployed"] <- "[98] I do not work"
background$md_1660[background$md_1181 == "[8] Pensioner/retired, formerly in full-time work"] <- "[98] I do not work"
background$md_1660[background$md_1181 == "[9] Not working (housewife/house husband)"] <- "[98] I do not work"
background$md_1660[background$md_1181 == "[10] Maternity leave, Parental leave, Sabbatical"] <- "[98] I do not work"
background$md_1660[is.na(background$md_1660)] <- "No answer provided"
summary(background$md_1660)

levels(background$md_1000) <- c(levels(background$md_1000),"No answer provided")
background$md_1000[background$md_1181 == "[4] School"] <- "[98] I do not work"
background$md_1000[background$md_1181 == "[5] Student"] <- "[98] I do not work"
background$md_1000[background$md_1181 == "[7] Currently unemployed"] <- "[98] I do not work"
background$md_1000[background$md_1181 == "[8] Pensioner/retired, formerly in full-time work"] <- "[98] I do not work"
background$md_1000[background$md_1181 == "[9] Not working (housewife/house husband)"] <- "[98] I do not work"
background$md_1000[background$md_1181 == "[10] Maternity leave, Parental leave, Sabbatical"] <- "[98] I do not work"
background$md_1000[is.na(background$md_1000)] <- "No answer provided"
summary(background$md_1000)

save.image("./background_info/background.RData")