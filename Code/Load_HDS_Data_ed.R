# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # DOGS & GUNS DATA & STATS FROM TWO SECTIONS ON THE HDS # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# L O A D I N G   D A T A S E T S = = = = = = = = = = = = = = = = = =
# The csv files being read below originally came from the "MasterHHDemoCensusDataEntry.xlsx"
# which CC shared with LPC on June2015. Such a master data entry contains all the responses
# to the "Household Census and DemographyFinalDraftInstrument.pdf" or its previous versions

# SECTION IV: HOUSEHOLD WEALTH (HH) = = = = = = = = = = = = = = = = =
hds_hhw <- read.csv("HDS_IV_Household_Wealth_edited.csv")

# SECTION VI: DOGS & HUNTING  = = = = = = = = = = = = = = = = = = = =
hds_hwd <- read.csv("HDS_VI_Hunting_with_Dogs_edited.csv")

# Inconsistencies fixed: check previous code and comments in earlier version
# MIND THAT LPC MANUALLY PLACED THE LAST SIX VARIABLES IN 'hds_hwd' FROM 'hds_hhw'

# List of houses from section IV reporting not to have dogs
a_index <- which(hds_hhw$num.dog.female==0 & hds_hhw$num.dog.male==0)
b_index <- which(is.na(hds_hhw$num.dog.female) & is.na(hds_hhw$num.dog.male))
# Indexes for subsetting HHs with no dogs
idx_hhndogs <- sort(c(a_index,b_index))
rm(a_index,b_index)

# Mind that 'hds_hhw' or 'hds_hwd' indexes differ from HHs' codes '$hh.key'
hds_hwd[,c("hh.key","dog.type","num.gun.own")]
# HH 11 and HHs 77-79 were not coded, resulting in a HH skipped sequence
# for proper matching HHs' key numbers with 'hds_hhw' or 'hds_hwd' indexing
# substract 1 to HHs' keys between 12-76, or substract 4 to HH key 80
# ...or simply call '$hh.key' by their exact names

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Building lists of indexes to facilitate subsetting HHs according to their hunting frequencies:
# rough ethnographic estimate [for the 13 months in the field] to compare with the hunters'
# estimate ["hunt.tpw" in the HDS], in order to fix the following indexing objects:
# "idx_hunters" ; "idx_ohunter" ; "idx_phunter" ; "idx_nonhunt"
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Ethnographic hunting frequency recorded by LPC as "times recorded to go to hunt" (f) is
# f = "number of times recorded to go hunt /  number of weeks in thirteen months"
# OR
# f = "number of times recorded to go hunt /  thirteen months"
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# hsd <- read.csv("BZ_hunting.csv", header=TRUE, fill=TRUE, sep=',')
# sort(table(hsd$Hunter.ID)) # this one sums all hunting trips per individual
# sort(table(hsd$hoh.key)) # this one sums all hunting trips per household
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# TEXT: In order to estimate f we counted all hunting trips per household during 56 weeks
# # HUNTING ABOUT ONCE A WEEK (f from 0.8 to 1)
# # HUNTING ABOUT ONCE EVERY TWO WEEKS (f from 0.3 to 0.5)
# # HUNTING ABOUT ONCE EVERY MONTH AND A HALF (f from 0.09 to 0.25)
# # HUNTING ABOUT ONCE EVERY THREE MONTHS (f from 0.02 to 0.07)
# # Formula: f = round("number of times recorded to go hunt"/56,1)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# TEXT: In order to estimate f we counted all hunting trips per household during 13 months
# # HUNTING ABOUT (f from 3.3 to 5.6) TIMES PER MONTH
# # HUNTING ABOUT (f from 1.2 to 2.2) TIMES PER MONTH
# # HUNTING ABOUT (f from 0.4 to 1.1) TIMES PER MONTH
# # HUNTING ABOUT (f from 0.1 to 0.3) TIMES PER MONTH
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# PICK t VALUE DEPENDING ON WHETHER WE ARE GOING TO ESTIMTAE f PER WEEK (56) OR MONTH (13)
t <- 13 # or 56
# f = round("number of times recorded to go hunt"/t,1)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # NUMBER OF TIMES RECORDED TO GO HUNT (HUNTER OR HOUSEHOLDS) DURING FIELDWORK (53W|13M)

# # > 30 - - - T I M E S - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# which(sort(table(hsd$Hunter.ID))>30) # HUNTERS M26A, M0A, M45A
# round(43/t,1)==3.3 ; round(52/t,1)==4 ; round(58/t,1)==4.5
# which(sort(table(hsd$hoh.key))>30) # HOUSEHOLDS 5, 0, 45, 26
# round(43/t,1)==3.3 ; round(52/t,1)==4 ; round(59/t,1)==4.5 ; round(73/t,1)==5.6

# # >=15 & <30 - - T I M E S - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# which(sort(table(hsd$Hunter.ID))>=15 & sort(table(hsd$Hunter.ID))<30) # HUNTERS M32A, M28A, M5A, M26C 
# round(17/t,1)==1.3 ; round(26/t,1)==2 ; round(29/t,1)==2.2
# which(sort(table(hsd$hoh.key))>=15 & sort(table(hsd$hoh.key))<30) # HOUSEHOLDS 65, 32, 28
# round(15/t,1)==1.2 ; round(17/t,1)==1.3 ; round(26/t,1)==2

# # >=5 & <15 - - T I M E S - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# which(sort(table(hsd$Hunter.ID))>=5 & sort(table(hsd$Hunter.ID))<15) # HUNTERS
# # M29A, M65C, M38A, M57A, M58C, M27A, M58A, M5D, M9A, M66A, M65A, M72A, M73C, M5C, M61A, M49C, M6A
# round(5/t,1)==0.4 ; ... ; round(14/t,1)==1.1
# which(sort(table(hsd$hoh.key))>=5 & sort(table(hsd$hoh.key))<15) # HOUSEHOLDS
# 29, 30, 38, 57, 9, 27, 66, 72, 61, 49, 73, 6, 58, 65
# round(5/t,1)==0.4 ; ... ; round(14/t,1)==1.1

# # < 5 - - T I M E S - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# which(sort(table(hsd$Hunter.ID))<5) # HUNTERS
# # M41A, F45B, M18A, M26, M33A, M39A, M48A, M52A, M54C, M58, M63A, M69A, M70A, M72, M75A, M8A, M30A, M36A
# # M54A, M19E, M30C, M40A, M73A, M2A, M31A, M37C, M4A, M55A, M62A, M71A
# round(1/t,1)==0.1 ; ... ; round(4/t,1)==0.3
# which(sort(table(hsd$hoh.key))<5) # HOUSEHOLDS
# # 8, 18, 33, 39, 48, 52, 63, 69, 70, 75, 36, 19, 40, 54, 2, 4, 31, 37, 55, 62, 71
# round(1/t,2)==0.02 ; ... ; round(4/t,2)==0.07

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # NOTE: F32B is missing? She helped 32A to get the kiib out of the hole at least once
# # QUICK-CHECKED ON 170301 - IT SEEMS LIKE F32B 'source' NEEDS TO BE IDENTIFIED FIRST
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# # HUNTING FROM (f 1 to 5.6) TIMES PER MONTH
# which(sort(table(hsd$hoh.key))>=13) # round(13/t,1)==1 ; round(73/t,1)==5.6
# length(which(sort(table(hsd$hoh.key))>=13)) # 9 HOUSEHOLDS, and 8 HHs without HH 0
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # HUNTING FROM (f 0.5 to 0.9) TIMES PER MONTH
# which(sort(table(hsd$hoh.key))>=6 & sort(table(hsd$hoh.key))<=13) # round(6/t,1)==0.5 ; round(12/t,1)==0.9
# length(which(sort(table(hsd$hoh.key))>=6 & sort(table(hsd$hoh.key))<=13)) # 9 HOUSEHOLDS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # HUNTING FROM (f 0.1 to 0.4) TIMES PER MONTH
# which(sort(table(hsd$hoh.key))>=1 & sort(table(hsd$hoh.key))<=5) # round(1/t,1)==0.1 ; round(5/t,1)==0.4
# length(which(sort(table(hsd$hoh.key))>=1 & sort(table(hsd$hoh.key))<=5)) # 23 HOUSEHOLDS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# # COAERSENING CASES TO BUILD CONSISTENT HUNTING FREQUENCY LABELS

# A C T I V E #########################
# # HUNTING FROM EVERY TWO MONTHS AND A HALF TO ONCE A WEEK (f from 0.11 to 1.3)
# which(sort(table(hsd$hoh.key))>=6) # round(6/t,2)==0.11 ; round(73/t,2)==1.3
# length(which(sort(table(hsd$hoh.key))>=6)) # 18 HOUSEHOLDS, and 17 HHs without HH 0
# # HUNTING FROM (f 0.5 to 5.6) TIMES PER MONTH
# which(sort(table(hsd$hoh.key))>=6) # round(6/t,1)==0.5 ; round(73/t,1)==5.6
# length(which(sort(table(hsd$hoh.key))>=6)) # 18 HOUSEHOLDS, and 17 HHs without HH 0
# the list of 'hoh.key' comes from the first row in sort(which(table(hsd$hoh.key)>=6))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Full-time hunters [or "currently" ACTIVE] - - - - - - - - - - - - - - - - - - #
# TEXT: 17 seventeen HHs with ACTIVE hunters
# 38A temporarily hunted without dogs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
length(which(hds_hwd$curr.or.past=="currently")) # 37...check names list and notes in master "HDS_HwD.R" file
hds_hwd[c(which(hds_hwd$curr.or.past=="currently")),c("hh.key","curr.or.past")]
hds_hwd[c(which(hds_hwd$curr.or.past=="currently")),c("hh.key","hunt.with.dogs","curr.or.past","hunt.tpw","dog.type","dog.origin")]
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Indexes for subsetting active hunters
# manual-match between which(table(hsd$hoh.key)>=6) and hds_hwd[,c("hh.key","curr.or.past")]
# c(0,5,6,9,26,27,28,32,38,45,49,57,58,61,65,66,72,73) # HH 0 should be out of this sample
idx_hunters <- c(5,6,9,25,26,27,31,37,44,48,56,57,60,64,65,71,72)
#c(5,6,9,26,27,28,32,38,45,49,57,58,61,65,66,72,73) == hds_hwd[idx_hunters,"hh.key"] ; length(idx_hunters)
hds_hwd[idx_hunters,c("hh.key","num.gun.own","hunt.with.dogs","curr.or.past","hunt.tpw","dog.type","dog.origin")]
hds_hwd[idx_hunters,c("hh.key","hunt.tpw","dog.type","num.dog.female","num.dog.male","X2011.pup.num","X2011.pup.surv")]

# O C C A S I O N A L #########################
# # HUNTING FROM EVERY SIX TO THREE MONTHS (f from 0.02 to 0.09)
# which(sort(table(hsd$hoh.key))<6) # round(1/t,2)==0.02 ; round(5/t,2)==0.09
# length(which(sort(table(hsd$hoh.key))<6))
# 23 HOUSEHOLDS, and 24 HHs with HH 25 ... [HH 79] could also be included but it [IS NOT CODED]
# # HUNTING FROM (f 0.1 to 0.4) TIMES PER MONTH
# which(sort(table(hsd$hoh.key))<6) # round(1/t,1)==0.1 ; round(5/t,1)==0.4
# length(which(sort(table(hsd$hoh.key))<6))
# 23 HOUSEHOLDS, and 24 HHs with HH 25 ... [HH 79] could also be included but it [IS NOT CODED]
# the list of 'hoh.key' comes from the first row in sort(which(table(hsd$hoh.key)<6))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Part time hunters [OCCASIONAL or "current" or "past" ones]- - - - - - - - - - #
# TEXT: 24 twenty four HHs with OCCASIONAL hunters
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 19, 40 hunt without dogs
# 2, 75 'past' or reported to 'currently' hunt but LPC did not registered it with Notes, HA or GPS/HR
# 48 Gun owner
# 70A Uxben-Ahtzak[Hunter] said he did not use to hunt with dogs, he uses his gun and machete for that
# 25C was learning to hunt as he revealed to ECh in the HDS (see '$DHNotes'), he did hunt with 48A. Though LPC has no ethnographic records of 25C going to hunt.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Indexes for subsetting occasional hunters
# manual-match between which(table(hsd$hoh.key)<6) and hds_hwd[,c("hh.key","curr.or.past")]
# c(2,4,8,18,19,29,30,31,33,36,37,39,40,48,52,54,55,62,63,69,70,71,75) # HH 25 should be out of this sample
idx_ohunter <- c(2,4,8,17,18,24,28,29,30,32,35,36,38,39,47,51,53,54,61,62,68,69,70,74)
#c(2,4,8,18,19,25,29,30,31,33,36,37,39,40,48,52,54,55,62,63,69,70,71,75) == hds_hwd[idx_ohunter,"hh.key"] ; length(idx_ohunter)
hds_hwd[idx_ohunter,c("hh.key","num.gun.own","hunt.with.dogs","curr.or.past","hunt.tpw","dog.type","dog.origin")]
hds_hwd[idx_ohunter,c("hh.key","hunt.tpw","dog.type","num.dog.female","num.dog.male","X2011.pup.num","X2011.pup.surv")]

# P A S T  [H U N T E R S] #########################
# # LABELED ACCORDING TO THEIR 'past' ANSWERS IN THE HDS
# the list of 'hh.key' comes from hds_hwd[which(hds_hwd$curr.or.past=="past"),"hh.key"]
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# PAST hunters [curr.or.past] - - - - - - - - - - - - - - - - - - - - - - - - - #
# TEXT: 18 eighteen HHs with PAST hunters
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 35, 56, 59, 74 Uxben-Ahtzak[Hunters]
# 46A said he did not use to hunt [with dogs] but ECh remembers that 46B informed he did
#     bring plenty of halee and keeh to her (his HH) in the past
# 79A used to hunt with dogs [BUT HIS SURVEY IS NOT CODED YET; WAS NOT IT CONSIDERED AS A HH?]
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Indexes for subsetting past hunters
# no-match required, just subsetting the corresponding and CSV-FIXED 'hds_hwd$curr.or.past'
hds_hwd[which(hds_hwd$curr.or.past=="past"),c("hh.key","curr.or.past")]
# hds_hwd[76,"hh.key"] # HH 80 is for Lucia Maas ... HHs 77, 78 & 79 were not coded [WHY?]
idx_phunter <- which(hds_hwd$curr.or.past=="past")
#hds_hwd[idx_phunter,"hh.key"] ; length(which(hds_hwd$curr.or.past=="past"))
hds_hwd[idx_phunter,c("hh.key","num.gun.own","hunt.with.dogs","curr.or.past","hunt.tpw","dog.type","dog.origin")]
hds_hwd[idx_phunter,c("hh.key","hunt.tpw","dog.type","num.dog.female","num.dog.male","X2011.pup.num","X2011.pup.surv")]


# N O N - H U N T E R S #########################
# # HHs REPORTING NOT TO HUNT OR WITH ALMOST NO ETHNOGRAPHIC EVIDENCE OF IT
# the list of 'hh.key' mainly comes from hds_hwd[which(is.na(hds_hwd$curr.or.past)),"hh.key"]
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Non-hunters [not labeled as curr.or.past] - - - - - - - - - - - - - - - - - - #
# TEXT: 17 seveteen HHs with NON-HUNTERS hunters
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 23A once chopped a kitam 49C was chasing with his dogs [occasional hunter?]
# 11 survey was not coded, and does not have information on HwD either.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# NOTES:  60A did not actively hunt, though LPC found kitam's hair in his
#         garden while helping to thatch his kitchen.
#         23A did not actively hunt, but he could do it given the opportunity.
#         No hunting trips were registered for HH 24, so it was included among non-hunters.
#         HHs 29,48,70,71, coded as <NA> for $curr.or.past, were known to occasionally hunt.
#         1A shared hunting story in which, when younger, he and his friends drowned a keeh
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Indexes for subsetting non-hunters
# manually handling hds_hwd[which(is.na(hds_hwd$curr.or.past)),"hh.key"] to match the list and conditions above
idx_nonhunt <- c(1,3,7,11,12,14,15,21,22,23,33,41,46,50,59,66,76)
# length(idx_nonhunt) # 17 HHs
hds_hwd[idx_nonhunt,c("hh.key","num.gun.own","hunt.with.dogs","curr.or.past","hunt.tpw","dog.type","dog.origin")]
hds_hwd[idx_nonhunt,c("hh.key","hunt.tpw","dog.type","num.dog.female","num.dog.male","X2011.pup.num","X2011.pup.surv")]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
rm(t)

# Indexes for HHs in the 'hds_hwd$" grouped by their hunting type label
idx_hunters;idx_ohunter;idx_phunter;idx_nonhunt
sort(c(idx_hunters,idx_ohunter,idx_phunter,idx_nonhunt))
# HHs codes
hh.check <- sort(c(hds_hwd[idx_hunters,"hh.key"],hds_hwd[idx_ohunter,"hh.key"],
                   hds_hwd[idx_phunter,"hh.key"],hds_hwd[idx_nonhunt,"hh.key"]))
# Checking that all '$hh.key' are included the 'idx' objects created to group hunter types
hds_hwd$hh.key == hh.check # All TRUE
rm(hh.check)

# HUNTER LABELS AND TOTAL NUMBER OF HHs
# ACTIVE      17 (EXCLUDING HH 0)
# OCCASIONAL  24
# PAST        18
# NO-HUNTERS  17
#             _____
# TOTAL       76

# active hunters + occasional hunters + past hunters + non-hunters = 76 HHs
length(c(idx_hunters,idx_ohunter,idx_phunter,idx_nonhunt))

length(idx_hunters) + length(idx_ohunter) + length(idx_phunter) +
  length(idx_nonhunt) == length(hds_hwd$hh.key) # TRUE

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Descriptive stats continue in corresponding sections..."HDS_IV or _VI ... .R"


