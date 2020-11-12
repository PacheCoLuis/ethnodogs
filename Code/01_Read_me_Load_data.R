# README AND LOADING DATA #######################

# "AN ETHNOGRAPHY OF DOGS IN BELIZE" DATA COMES
# From Notes, HA, GPS/HR, HDS, TA, and Estimated Weights samples
#   The hunting sample description (hsd, hpts) objects contain
#      Notes, HA, and GPS/HR information. Mind that HA data comes from two sources
#      here: 1) the hsd, and 2) the HAsub object as coded by Jorge A Valencia G,
#      under the direction of Luis Pacheco-Cobos.
#   HDS and TA data (objects: hds_hhw, hds_hwd, TA)
#      were coded by Chloe Atwater, under the direction of Bruce Winterhalder.
#      LPC checked and edited some of these records when necessary
# for NSF Grant #0827277 in Human and Social Dynamics, Collaborative Research
# "Development and Resilience of Complex Socioeconomic Systems: A Theoretical
# Model and Case Study from the Maya Lowlands." Santa Cruz, Toledo, Belize.


# R version 4.0.2 (2020-06-22) -- "Taking Off Again"
# Platform: x86_64-apple-darwin17.0 (64-bit)
# macOS Catalina Version 10.15.5

library(dplyr)
# ?summarise()
# Could specify summarise(n=n(), .groups="drop_last"|"drop"|"keep"|"rowwise") or
#   When .groups is not specified, you either get "drop_last" when all the results are
#   size 1, or "keep" if the size varies. In addition, a message informs you of that
#   choice, unless the option "dplyr.summarise.inform" is set to FALSE.
# options(dplyr.summarise.inform = FALSE) # suppress summarise info
library(ggplot2)

# HUNTING SAMPLE DESCRIPTION (hsd) NOTES, HA, GPS/HR
hsd <- read.csv("BZ_hunting.csv", header=TRUE, fill=TRUE, sep=',')
# An earlier version of the 'hsd' dataset was shared with J. Koster and
# R. McElreath to contribute with the "Life History of Foraging" analysis
hpts <- read.csv("GPS_HA_Merged_and_Filtered.csv",na.strings="NA")
# No need to exclude LPC and Tuli from the counts. Means, sd, and
# ranges hold practically the same. In addition, LPC was coded as
# and assistant in all GPS/HR trips. If to be removed use
# hpts <- filter(hpts,ID!="hM0A",ID!="dF0A")

# HOUSEHOLD DEMOGRAPHIC SURVEY: SECTION IV & SECTION VI
# SECTION I: HOUSEHOLD MEMBERSHIP
hds_hms <- read.csv("HDS_I_Household_Membership_edited.csv")
# SOURCE below loads HHs' (without dogs | classification) indexing/referencing vectors
source("Load_HDS_Data_ed.R") # loads and handles objects hds_hhw and hds_hwd
# hds_hhw comes from "HDS_IV_Household_Wealth_edited.csv" SECTION IV: HOUSEHOLD WEALTH
# hds_hwd comes from "HDS_VI_Hunting_with_Dogs_edited.csv" SECTION VI: DOGS & HUNTING

# USE hds_hhw[idx_name,"hh.key"] or hds_hwd[idx_name,"hh.key"] to call 'hh.key'
idx_hhndogs # reporting not to have dogs
idx_hunters # active hunters
idx_ohunter # occasional hunters
idx_phunter # past hunters
idx_nonhunt # non-hunters

# ADDING the variable household classification ($hhclass)
# into 'hds_hhw'
hds_hhw$hhclass <- rep(NA,dim(hds_hwd)[1])
hds_hhw$hhclass[idx_hunters] <- "active hunters"
hds_hhw$hhclass[idx_ohunter] <- "occasional hunters"
hds_hhw$hhclass[idx_phunter] <- "past hunters"
hds_hhw$hhclass[idx_nonhunt] <- "non-hunters"
#write.csv(hds_hhw,"hds_hhw_2020.csv",row.names=FALSE)
# into 'hds_hwd'
hds_hwd$hhclass <- rep(NA,dim(hds_hwd)[1])
hds_hwd$hhclass[idx_hunters] <- "active hunters"
hds_hwd$hhclass[idx_ohunter] <- "occasional hunters"
hds_hwd$hhclass[idx_phunter] <- "past hunters"
hds_hwd$hhclass[idx_nonhunt] <- "non-hunters"
#write.csv(hds_hhw,"hds_hwd_2020.csv",row.names=FALSE)

# TIME ALLOCATION (TA) DATA
TA <- read.csv("TACoding_R.csv")
tab.purp <- table (TA$Purpose) ; sort(tab.purp)
# Sum agricultural categories
tab.purp["A-H"]+tab.purp["A-O"]+tab.purp["A-P"]+tab.purp["A-P/P"]+tab.purp["A-W"] # 365
# Sum of crafts, wage labor, and attendance to school
tab.purp["HP-S/C"] ; tab.purp["L-W"] ; tab.purp["SCH"] # 112 ; 149 ; 420
# Sum of hunting and gathering
tab.purp["HG"] # 52
head(TA[which(TA$Purpose=="HG"),c("hh.key","hum.pos","Date","Observers","Notes","Annotation","Location","Others","Reliable")]) ; tail(TA[which(TA$Purpose=="HG"),c("hh.key","hum.pos","Date","Observers","Notes","Annotation","Location","Others","Reliable")])
rm(tab.purp)

# HUNT ALLOCATION (HA) DATA
source("CARGA_DATOS_ed.R")
#write.csv(HAsub,"HAsub_ed.csv",,row.names=FALSE)

# HUNTERS' ESTIMATED WEIGHT ERROR
werror <- read.csv("weight_error_sc.csv", header=TRUE, fill=TRUE, sep=',')

# DATASETS AVAILABLE
ls()
names(hsd)
names(hpts)
names(hds_hms)
names(hds_hhw)
names(hds_hwd)
names(TA)
names(HAsub)
names(werror)
# idx_ ones are for referencing purposes
# Handle "MasterHHDemoCensusDataEntry.xlsx" for futher consultations
