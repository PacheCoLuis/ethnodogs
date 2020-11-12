# SANTA CRUZ DESCRIPTIVE DATA #######################

source("01_Read_me_Load_data.R")

# Total households and residents

# From the TA records
sort(unique(TA$hh.key[!is.na(TA$hh.key)]))
SC_totHHs <- length(sort(unique(TA$hh.key[!is.na(TA$hh.key)])))
# TEXT: there were 82 HHs by 2012 [we only covered 76 HHs in HDS sections IV & VI]
#		HHs 11, 77, 78, 79, 81, 82 [the six we missed]
sort(unique(TA$hum.key[!is.na(TA$hum.key)]))
# there are cases with '?' I counted them as residents
length(sort(unique(TA$hum.key[!is.na(TA$hum.key)])))
# TEXT: 484 residents in 82 HHs

# From the HDS records
sort(unique(hds_hms$hh.key[!is.na(hds_hms$hh.key)]))
length(sort(unique(hds_hms$hh.key[!is.na(hds_hms$hh.key)])))
# TEXT: there were 82 HHs by 2012, but we only covered 72 HHs in HDS section I
#		HHs 3, 11, 15, 18, 19, 77, 78, 79, 81, 82 [the ten we missed]
sort(hds_hms$hum.key)
length(sort(hds_hms$hum.key))
# but we need to omit the entries for which(hds_hms$death.date!="Na")
length(sort(hds_hms$hum.key)) - length(which(hds_hms$death.date!="Na"))
# TEXT: 399 residents in 72 HHs
length(which(hds_hms$sex== "Female"))
length(which(hds_hms$sex=="Male"))
hds_hms[which(hds_hms$death.date!="Na"),]
length(which(hds_hms$sex== "Female" & hds_hms$death.date!="Na"))
length(which(hds_hms$se=="Male" & hds_hms$death.date!="Na"))
# TEXT: 211+197-4-5==399 [women+men-deceasedwomen-deceasedmen==total]
# but we are missing ten HHs in this count	

# USE THE FOLLOWING FIGURES
# from HDS and ethnographic records [hand-made counts], as
# the most reliable i.e. not considering deceased babies
# TEXT: 221+207==428 [women+men==total]
SC_totwom <- 221
SC_totmen <- 207
SC_totpop <- 428


# Classification of households

# Percent of TA's HHs surveyed in the HDS
(length(unique(hds_hwd$hh.key))*100) / length(unique(TA$hh.key[!is.na(TA$hh.key)]))
# TEXT: 93% of HHs are included in the analysis (we did not complete the HDS in six of them)

# Number of households in each category	
length(hds_hwd[idx_hunters,"hh.key"]) # 17 HHs
length(hds_hwd[which(hds_hwd$hhclass=="active hunters"),"hh.key"])
length(hds_hwd[idx_ohunter,"hh.key"]) # 24 HHs
length(hds_hwd[which(hds_hwd$hhclass=="occasional hunters"),"hh.key"])
length(hds_hwd[idx_phunter,"hh.key"]) # 18 HHs
length(hds_hwd[which(hds_hwd$hhclass=="past hunters"),"hh.key"])
length(hds_hwd[idx_nonhunt,"hh.key"]) # 17 HHs
length(hds_hwd[which(hds_hwd$hhclass=="non-hunters"),"hh.key"])

# CHECK for consistency	
length(unique(hsd$hoh.key)) == length(hds_hwd$hh.key)
# 0k: 41 in 'hsd' differs from 76 in 'hds_hhw' or 'hds_hwd', since not all HHs engage in hunting	

# Total number of HH with dogs
length(which((hds_hhw$num.dog.female+hds_hhw$num.dog.male)!=0))
( length(which((hds_hhw$num.dog.female+hds_hhw$num.dog.male)!=0))*100 ) / length(hds_hwd$hh.key)
# TEXT: 55 (72.4%) out of 76 surveyed households had dogs in 2012
