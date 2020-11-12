# SUBSISTENCE HUNTING PREY KG TO KCAL #######################

source("05_Subsist_Hunt_Sample.R")

# Table 4: Prey captures recorded during fieldwork + Harvest (kg to kcal) #######
# Switch to longitudinal format, to deal with serial prey catches [1-5]
prey.all <- reshape(harv_succ, varying=list(prey=c("Prey.Catch","Prey.Catch.2","Prey.Catch.3","Prey.Catch.4","Prey.Catch.5"), weight=c("PC.kg","PC.kg.2","PC.kg.3","PC.kg.4","PC.kg.5")), direction="long")
colnames(prey.all)[which(colnames(prey.all)=="Harvest.kg..NAs.Prey....")] <- "PC.kg.guess"
prey.all <- filter(prey.all,!is.na(PC.kg))
prey.all <- prey.all[order(prey.all$Prey.Number,prey.all$Trip.ID.by.date),]

# kcal/kg estimates per prey type (successful trips) #######
# 	Hill & Hawkes (1983: p158), assume that 65% of the prey live weight is edible
#	a general conversion factor to obtain the edible kg (ek) is (65*PC.kg)/100
# 	ek should be multiplied by the amount of estimated cal/kg (ck) per prey
#	prey ek*ck [units: (cal/kg)*kg = cal = kcal] given the food calories
# 	equivalence cal ~ kcal [1Cal = 1000calories = 1kcal] the units in
#	( (65*PC.kg)/100 ) * (cal/kg) would be kcal (the OFT currency)
sort(unique(prey.all$Prey.Catch))
prey.all$kcal <- rep(0,dim(prey.all)[1])
prey.all$kcal[prey.all$Prey.Catch=="bird"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="bird"])/100 )*1900
prey.all$kcal[prey.all$Prey.Catch=="chiic"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="chiic"])/100 )*3000
prey.all$kcal[prey.all$Prey.Catch=="ek en che"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="ek en che"])/100 )*3000
prey.all$kcal[prey.all$Prey.Catch=="garuba"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="garuba"])/100 )*1500
prey.all$kcal[prey.all$Prey.Catch=="halee"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="halee"])/100 )*3000
prey.all$kcal[prey.all$Prey.Catch=="keeh"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="keeh"])/100 )*1250
prey.all$kcal[prey.all$Prey.Catch=="kiib"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="kiib"])/100 )*3000
prey.all$kcal[prey.all$Prey.Catch=="kitam"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="kitam"])/100 )*3000
prey.all$kcal[prey.all$Prey.Catch=="wech"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="wech"])/100 )*3000
prey.all$kcal[prey.all$Prey.Catch=="yuk"] <- ((65*prey.all$PC.kg[prey.all$Prey.Catch=="yuk"])/100 )*1250

# Visualizing
prey.all[,c("Prey.Catch", "PC.kg", "kcal")]
# Saving
write.csv(prey.all,"prey_all.csv",row.names=FALSE)


# SUMMARIZING THE PREY CATCH KILOGRAMS, HUNTS WITHOUT/WITH DOGS
# MIND: harv_succ$Prey.Number, coming from hsd$Prey.Number, indicates the
#       total number of captures per trip, do not use it to sum captures
#       by prey type --- count Prey.Catch instead
prety.stats <- group_by(prey.all,Prey.Catch) %>% summarize(count=n(), round(mean(PC.kg),1),round(sd(PC.kg),1), length(which((Dogs.used=="No")==TRUE)), length(which((Dogs.used=="Yes")==TRUE)))
colnames(prety.stats) <- c("Prey","N","Mean_Kg","SD","Without_dogs","With_dogs")
prety.stats <- prety.stats[order(-prety.stats$Mean_Kg),]
write.table(prety.stats, file="Table-4_Prey-KG_means_sd.csv", append = FALSE, quote=FALSE, sep=" , ", row.names=FALSE)
prety.stats
# CHECK for consistency, the total Ns hold equally
sum(prety.stats$N) == sum(prety.stats$Without_dogs) + sum(prety.stats$With_dogs)

# SUMMARIZING THE PREY CATCH KCAL, HUNTS WITHOUT/WITH DOGS
prety.kcals <- group_by(prey.all,Prey.Catch) %>% summarize(count=n(), round(mean(kcal),0),round(sd(kcal),0), length(which((Dogs.used=="No")==TRUE)), length(which((Dogs.used=="Yes")==TRUE)))
colnames(prety.kcals) <- c("Prey","N","Mean_Kcal","SD","Without_dogs","With_dogs")
prety.kcals <- prety.kcals[order(-prety.kcals$Mean_Kcal),]
write.table(prety.kcals, file="Table-4_Prey-KCAL_means_sd.csv", append = FALSE, quote=FALSE, sep=" , ", row.names=FALSE)
prety.kcals

# NOTE: on weight estimates and actual weight measurements
#       If 'Harvest.kg..NAs.Prey....' [here 'prey.all$PC.kg.guess'] values are
#       filled with characters it is a 'Harvest.guess'=="Yes" meaning that the
#       'Harvest.kg' were obtained from the literature, in such cases hunters
#       reported prey type but could not estimate the approximate prey weight
#       In all other cases weights were either measured using a hanging scale
#       or estimated by hunters. Use 'PC.kg' associated to Source.Uniform==HA
#       records as a rough of the number of weights that hunters did estimate
sort(names(table(prey.all$PC.kg.guess))) # table with 45 dimnames
table(prey.all$PC.kg.guess)[1:37]
names(table(prey.all$PC.kg.guess)[38:45])
# working with numeric values (kg estimates or measures)
kg.estme <- data.frame(table(prey.all$PC.kg.guess)[1:37][names(table(prey.all$PC.kg.guess)[1:37])]) ; sum(kg.estme$Freq)
# 85 cases for which PC.kg were hunters' estimates or actual measurements
length(which(prey.all$Source.Uniform=="HA"))
# ~68 of kg.estme could come from hunters' estimates, which
# leaves ~17 cases with actual prey measurements (weights)
# working with character values (kg guesses)
kg.guess <- data.frame(table(prey.all$PC.kg.guess)[38:45][names(table(prey.all$PC.kg.guess)[38:45])]) ; sum(kg.guess$Freq)
# 36 cases for which PC.kg were taken from Primack et al. (1997) or Koster (2008)
# CHECK for consistency
sum(kg.estme$Freq) + sum(kg.guess$Freq) == sum(table(prey.all$PC.kg.guess))

# Metrics: kcal, hours, prey, ethno-source #######
# kg or kcal per source type successful hunts (mean, sd, n)
prety.sourc <- group_by(prey.all,Source.Uniform) %>% summarize(count=n(), round(sum(PC.kg)))
colnames(prety.sourc) <- c("Source","Prey_N","Sum_Kg")
prety.sourc
prety.sokca <- group_by(prey.all,Source.Uniform) %>% summarize(count=n(), round(sum(kcal)))
colnames(prety.sokca) <- c("Source","Prey_N","Sum_Kcal")
prety.sokca

# Hours per trip (mean, sd, n)
round(mean(hsd$Hours.hunted,na.rm=TRUE),1) ; round(sd(hsd$Hours.hunted,na.rm=TRUE),1)
# mean 4.9 sd 2.1
length(unique(hsd$Trip.ID.by.date))-length(unique(hsd[which(is.na(hsd$Hours.hunted)),"Trip.ID.by.date"]))
# n = 136 dates; after omitting 49 dates for which 'Hours.hunted' are NA...
length(unique(hsd[which(is.na(hsd$Hours.hunted) & hsd$Source.Uniform=="Notes"), "Trip.ID.by.date"]))
# ...37 from 'Notes'
length(unique(hsd[which(is.na(hsd$Hours.hunted) & hsd$Source.Uniform=="HA"), "Trip.ID.by.date"]))
# ...12 from 'HA'

# Evidence on the sources' disparities and more
# BUT FIRST, recalling unsuccessful hunts when converting from wide to long format
prey.suun <- reshape(harv, varying=list(prey=c("Prey.Catch","Prey.Catch.2","Prey.Catch.3","Prey.Catch.4","Prey.Catch.5"), weight=c("PC.kg","PC.kg.2","PC.kg.3","PC.kg.4","PC.kg.5")), direction="long")
colnames(prey.suun)[which(colnames(prey.suun)=="Harvest.kg..NAs.Prey....")] <- "PC.kg.guess"
prey.suun <- filter(prey.suun, Prey.Catch!="")

# kcal/kg estimates per prey type (all trips) #######
# 	Hill & Hawkes (1983: p158), assume that 65% of the prey live weight is edible
#	a general conversion factor to obtain the edible kg (ek) is (65*PC.kg)/100
# 	ek should be multiplied by the amount of estimated cal/kg (ck) per prey
#	prey ek*ck [units: (cal/kg)*kg = cal = kcal] given the food calories
# 	equivalence cal ~ kcal [1Cal = 1000calories = 1kcal] the units in
#	( (65*PC.kg)/100 ) * (cal/kg) would be kcal (the OFT currency)
sort(unique(prey.suun$Prey.Catch))
prey.suun$kcal <- rep(0,dim(prey.suun)[1])
prey.suun$kcal[prey.suun$Prey.Catch=="aim_PCS"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="aim_PCS"])/100 )*0
prey.suun$kcal[prey.suun$Prey.Catch=="bird"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="bird"])/100 )*1900
prey.suun$kcal[prey.suun$Prey.Catch=="chiic"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="chiic"])/100 )*3000
prey.suun$kcal[prey.suun$Prey.Catch=="ek en che"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="ek en che"])/100 )*3000
prey.suun$kcal[prey.suun$Prey.Catch=="garuba"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="garuba"])/100 )*1500
prey.suun$kcal[prey.suun$Prey.Catch=="halee"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="halee"])/100 )*3000
prey.suun$kcal[prey.suun$Prey.Catch=="keeh"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="keeh"])/100 )*1250
prey.suun$kcal[prey.suun$Prey.Catch=="kiib"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="kiib"])/100 )*3000
prey.suun$kcal[prey.suun$Prey.Catch=="kitam"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="kitam"])/100 )*3000
prey.suun$kcal[prey.suun$Prey.Catch=="wech"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="wech"])/100 )*3000
prey.suun$kcal[prey.suun$Prey.Catch=="yuk"] <- ((65*prey.suun$PC.kg[prey.suun$Prey.Catch=="yuk"])/100 )*1250

# Visualizing
prey.suun[,c("Prey.Catch", "PC.kg", "kcal")]
# Saving
write.csv(prey.suun,"prey_suun.csv",row.names=FALSE)

# SUMMARIZING THE PREY COUNTS PER SOURCE
prety.suun <- group_by(prey.suun,Prey.Catch) %>% summarize(count=n(),  length(which((Source.Uniform=="GPS/HR")==TRUE)), length(which((Source.Uniform=="HA")==TRUE)), length(which((Source.Uniform=="Notes")==TRUE)))
colnames(prety.suun) <- c("Prey", "N", "GPS/HR", "HA", "Notes")
write.table(prety.suun, file="Table_Prey-Source.csv", append = FALSE, quote=FALSE, sep=", ", row.names=FALSE)
prety.suun

# Recalling notes on prey missed
#hsd[1:20,c("Trip.ID.by.date", "Prey.Number", "HarvShare_NotAccounted", "Prey.Catch", "Harvest.kg..NAs.Prey....","Prey.Catch.Spa")]
#harv[which(harv$PC.kg==0),c("Trip.ID.by.date", "Prey.Number", "HarvShare_NotAccounted", "Prey.Catch", "Harvest.kg..NAs.Prey....","Prey.Catch.Spa")]
prety.miss <- group_by(prey.suun[which(prey.suun$Prey.Number==0),],Prey.Catch.Spa) %>% summarize(count=n(), length(which((Source.Uniform=="GPS/HR")==TRUE)), length(which((Source.Uniform=="HA")==TRUE)), length(which((Source.Uniform=="Notes")==TRUE)))
colnames(prety.miss) <- c("Prey.Miss.Spa", "N_Aimed", "GPS/HR", "HA", "Notes")
write.csv(prety.miss, file="Table_PreyMissSpa-Source.csv", row.names=FALSE)
prety.miss


# Number of prey captured per trip #######
# Single capture trips (n = 79)
dim(unique(prey.all[which(prey.all$Prey.Number==1),c("Trip.ID.by.date","Prey.Catch")]))[1]
kill_sing <- prey.all[which(prey.all$Prey.Number==1), c("Trip.ID.by.date","Prey.Catch","Dogs.used","Day.Night")] ; kill_sing

# Double capture trips (n = 17) Prey.Catch [Trip.ID.by.date]
length(unique(prey.all[which(prey.all$Prey.Number==2),"Trip.ID.by.date"]))
kill_doub <- prey.all[which(prey.all$Prey.Number==2), c("Trip.ID.by.date","Prey.Catch","Dogs.used","Day.Night")] ; kill_doub
# --SAME x2 PREY
#    keeh [27]
#    halee [40]
#    kiib [93]
#    wech [118], wech [119], wech [155]
#    kitam [29], kitam [39], kitam [107], kitam [130], kitam [139], kitam [142]
# --ARRAYS OF x2 PREY
#    halee-kiib [32], keeh-wech [135],  kiib-wech [141], wech-halee [172], kitam-wech [195]
# TEXT: We observed seventeen trips with double-captures: kitam (n=6), wech (n=3),
# keeh (n=1), halee (n=1), and kiib (n=1). The rest of the double-capture cases were
# for different species: halee-kiib, keeh-wech,  kiib-wech, wech-halee, kitam-wech. All
# double capture trips were diurnal, and only one of them [x2 wech, 119] was without dogs

# Triple capture trips (n = 1) Prey.Catch [Trip.ID.by.date]
length(unique(prey.all[which(prey.all$Prey.Number==3),"Trip.ID.by.date"]))
kill_trip <- prey.all[which(prey.all$Prey.Number==3), c("Trip.ID.by.date","Prey.Catch","Dogs.used","Day.Night")] ; kill_trip
# ...x3 halee [26] --- Prey.Catch [Trip.ID.by.date]
# TEXT: A unique nocturnal and without dogs trip, with a triple-capture for halee

# Quadruple capture trips (n = 0) Prey.Catch [Trip.ID.by.date]
dim(unique(prey.all[which(prey.all$Prey.Number==4),c("Trip.ID.by.date","Prey.Catch")]))[1]
kill_quad <- prey.all[which(prey.all$Prey.Number==4), c("Trip.ID.by.date","Prey.Catch","Dogs.used","Day.Night")] ; kill_quad

# Quintuple capture trips (n = 1) Prey.Catch [Trip.ID.by.date]
length(unique(prey.all[which(prey.all$Prey.Number==5),"Trip.ID.by.date"]))
kill_quin <- prey.all[which(prey.all$Prey.Number==5), c("Trip.ID.by.date","Prey.Catch","Dogs.used","Day.Night")] ; kill_quin
# TEXT: We registered a diurnal and with dogs trip with a quintuple-capture
# for wech (a whole family)


# Metrics for Figs 4 & 5 Hours~Prey + Harvest~Trip #######
# FIGURES 4 and 5 (start, bring pieces together)

# ALL CASES
# Omitting cases for which we do not know 'Hours.hunted'
hrsprey <- filter(prey.all,!is.na(Hours.hunted))
hrsprey_na <- filter(prey.all,is.na(Hours.hunted))
length(unique(hrsprey_na$Trip.ID.by.date))
pny.uhrs <- sum(xtabs(~Source.Uniform+Prey.Catch,hrsprey_na)) ; pny.uhrs
# 28 successful trips and 34 prey items for which Hours.hunted are NA
# (see 21 unsuccesful trips at the start of BEHAVIORAL METRICS OF THE HUNTS
# section; together they sum the 49 cases for which Hours.hunted are NA)	
# OMITTING the "garuba" and the "bird"
pgb.khrs <- dim(filter(hrsprey,Prey.Catch=="garuba"|Prey.Catch=="bird"))[1] ; pgb.khrs
# 2 prey items omitted (garuba and bird) with known Hours.hunted
hrsprey <- filter(hrsprey,Prey.Catch!="garuba"&Prey.Catch!="bird")
hrsprey$Prey.Catch <- droplevels(hrsprey$Prey.Catch)
# NOTE:
# The wild pigeon shot was a last chance to get wild meat on the way back home after a 12h
# unsuccessful trip with dogs which did not chase or alerted hunters about the prey.
# The iguana that hunters brought down from a tree after concluding routine agricultural
# tasks and attending to their dogs' barks, was not chased in the bush.


# FIG4a-b --- HOURS~PREY #######
# Getting the descriptive stats of trips for which we know Hours.hunted
prety.hour <- group_by(hrsprey,Prey.Catch) %>%
  summarize(count=n(), length(unique(Trip.ID.by.date)), mean(Hours.hunted,na.rm=TRUE), sd(Hours.hunted,na.rm=TRUE))
colnames(prety.hour) <- c("Prey","count","N_Trips","Mean_Trips_Hrs","SD")
prety.hour <- prety.hour[order(-prety.hour$Mean_Trips_Hrs),]
write.table(prety.hour,file="Table_Prey-HOURS_means_sd.csv", append=FALSE, quote=FALSE, sep=",", row.names = FALSE)
prety.hour
sum(prety.hour$count) # 85 prey items, in hunting trips without and with dogs

# WITHOUT DOGS CASES
hrsprey_nd <- filter(hrsprey,Dogs.used=="No")
hrsprey_nd$Prey.Catch <- droplevels(hrsprey_nd$Prey.Catch)
boxplot.medians <- boxplot(hrsprey_nd$Hours.hunted~hrsprey_nd$Prey.Catch, plot=FALSE)$stats[3,]
boxplot.names <- boxplot(hrsprey_nd$Hours.hunted~hrsprey_nd$Prey.Catch, plot=FALSE)$names
names.ordered <- boxplot.names[order(boxplot.medians)]
prey.ordered <- factor(hrsprey_nd$Prey.Catch, ordered=TRUE, levels=names.ordered) 
hpc <- boxplot(hrsprey_nd$Hours.hunted~prey.ordered,plot=0)
pnd.khrs <- sum(hpc$n[1:5]) # 11 prey items without dogs, and known Hours.hunted

# WITH DOGS CASES
hrsprey_wd <- filter(hrsprey,Dogs.used=="Yes")
hrsprey_wd$Prey.Catch <- droplevels(hrsprey_wd$Prey.Catch)
boxplot.medians <- boxplot(hrsprey_wd$Hours.hunted~hrsprey_wd$Prey.Catch, plot=FALSE)$stats[3,]
boxplot.names <- boxplot(hrsprey_wd$Hours.hunted~hrsprey_wd$Prey.Catch, plot=FALSE)$names
names.ordered <- boxplot.names[order(boxplot.medians)]
prey.ordered <- factor(hrsprey_wd$Prey.Catch, ordered=TRUE, levels=names.ordered) 
hpc <- boxplot(hrsprey_wd$Hours.hunted~prey.ordered,plot=0)
pyd.khrs <- sum(hpc$n[1:5]) # 74 prey items with dogs, and known Hours.hunted

# CHECK FOR CONSISTENCY:
# Sum of prey items holds equal to total prey number
pny.uhrs + pgb.khrs + pnd.khrs + pyd.khrs == sum(prety.stats$N)


# FIG5a-b --- HARVEST~DOGS.USED #######
# UNSUCCESSFUL AND SUCCESSFUL HUNTS

# KILLS IN TRIPS WITHOUT AND WITH DOGS
doguse_suco <- boxplot(prey.suun$PC.kg[prey.suun$Prey.Catch!="aim_PCS"] ~ prey.suun$Dogs.used[prey.suun$Prey.Catch!="aim_PCS"], plot=0)
# FAILS IN TRIPS WITHOUT AND WITH DOGS
doguse_umsk <- boxplot(prey.suun$PC.kg[prey.suun$Prey.Catch=="aim_PCS"] ~ prey.suun$Dogs.used[prey.suun$Prey.Catch=="aim_PCS"], plot=0)

# WITHOUT DOGS ALL TRIPS
doguse_suco$n[1] ; length(unique(prey.suun[which(prey.suun$PC.kg>=0 & prey.suun$Dogs.used=="No"),"Trip.ID.by.date"]))
# Kills: 17 prey items in 42 trips without dogs
doguse_umsk$n[1] ; length(unique(prey.suun[which(prey.suun$PC.kg>=0 & prey.suun$Dogs.used=="No"),"Trip.ID.by.date"]))
# Fails: 28 trips (~missed targets) in 42 trips without dogs
# WITH DOGS ALL TRIPS
doguse_suco$n[2] ; length(unique(prey.suun[which(prey.suun$PC.kg>=0 & prey.suun$Dogs.used=="Yes"),"Trip.ID.by.date"]))
# Kills: 104 prey items in 143 trips with dogs
doguse_umsk$n[2] ; length(unique(prey.suun[which(prey.suun$PC.kg>=0 & prey.suun$Dogs.used=="Yes"),"Trip.ID.by.date"]))
# Fails: 59 trips (~missed targets) in 143 trips with dogs

# WITHOUT DOGS (Fails [UNSUCCESSFUL] omitted) SUCCESSFUL TRIPS ONLY
doguse_suco$n[1] ; length(unique(prey.suun[which(prey.suun$PC.kg>0 & prey.suun$Dogs.used=="No"),"Trip.ID.by.date"]))
# 17 prey items in 14 trips without dogs
# WITH DOGS (Fails [UNSUCCESSFUL] omitted) SUCCESSFUL TRIPS ONLY
doguse_suco$n[2] ; length(unique(prey.suun[which(prey.suun$PC.kg>0 & prey.suun$Dogs.used=="Yes"),"Trip.ID.by.date"]))
# 104 prey items in 84 trips with dogs

# SUCCESSFUL HUNTS ONLY
doguse_succ <- boxplot(prey.suun$PC.kg[prey.suun$PC.kg>0] ~ prey.suun $Dogs.used[prey.suun$PC.kg>0], plot=0)

# SUM ALL KG PER TRIP INTO kg_sum AND OMIT Trip.ID.by.date REPETITIONS
kgsum <- prey.suun %>%
  group_by(Trip.ID.by.date) %>%
  mutate(kg_sum=sum(PC.kg)) %>%
  select(Trip.ID.by.date,kg_sum,Prey.Number,Dogs.used,Source.Uniform)
kgsum <- kgsum %>%
  distinct(Trip.ID.by.date,kg_sum,Prey.Number,Dogs.used,Source.Uniform)

# SUM ALL KCAL PER TRIP INTO kcal_sum AND OMIT Trip.ID.by.date REPETITIONS
kcalsum <- prey.suun %>%
  group_by(Trip.ID.by.date) %>%
  mutate(kcal_sum=sum(kcal)) %>%
  select(Trip.ID.by.date,kcal_sum,Prey.Number,Dogs.used,Source.Uniform)
kcalsum <- kcalsum %>%
  distinct(Trip.ID.by.date,kcal_sum,Prey.Number,Dogs.used,Source.Uniform)

#write.csv(kgsum,"kg_sum.csv",row.names=FALSE)
#write.csv(kcalsum,"kcal_sum.csv",row.names=FALSE)
# FIGURES 4 and 5 (end, compile postscripts)


# Figure 4: Hunting trip duration by prey type captured #######
# Raw material produced here for later BW-work on art using Adobe Illustrator
setEPS()
postscript("F4_Hrs-Prey_perDogsUsed.eps")
par(mfrow=c(1,2))
# Fig4a Without dogs
boxplot.medians <- boxplot(hrsprey_nd$Hours.hunted~hrsprey_nd$Prey.Catch, plot=FALSE)$stats[3,]
boxplot.names <- boxplot(hrsprey_nd$Hours.hunted~hrsprey_nd$Prey.Catch, plot=FALSE)$names
names.ordered <- boxplot.names[order(boxplot.medians)]
prey.ordered <- factor(hrsprey_nd$Prey.Catch, ordered=TRUE, levels=names.ordered) 
hpc <- boxplot(hrsprey_nd$Hours.hunted~prey.ordered,plot=0)
par(mar=c(5,5.5,2.5,5)+0.1,mgp=c(3,1,0))
boxplot(hrsprey_nd$Hours.hunted~prey.ordered,varwidth=TRUE,horizontal=TRUE,
        yaxt="n",xlab="Hunting (hours)\nWithout dogs",frame.plot=FALSE,cex.axis=0.9)
axis(side=2, las=2, at=c(1:5), labels = c(
  names.ordered[1],
  names.ordered[2],
  names.ordered[3],
  names.ordered[4],
  names.ordered[5]),
  tick=FALSE)
axis(side=4, las=2, at=c(1:5), labels = c(
  paste("n = ",hpc$n[1]),
  paste("n = ",hpc$n[2]),
  paste("n = ",hpc$n[3]),
  paste("n = ",hpc$n[4]),
  paste("n = ",hpc$n[5])),
  tick=FALSE)
# Fig4b With dogs
boxplot.medians <- boxplot(hrsprey_wd$Hours.hunted~hrsprey_wd$Prey.Catch, plot=FALSE)$stats[3,]
boxplot.names <- boxplot(hrsprey_wd$Hours.hunted~hrsprey_wd$Prey.Catch, plot=FALSE)$names
names.ordered <- boxplot.names[order(boxplot.medians)]
prey.ordered <- factor(hrsprey_wd$Prey.Catch, ordered=TRUE, levels=names.ordered) 
hpc <- boxplot(hrsprey_wd$Hours.hunted~prey.ordered,plot=0)
par(mar=c(5,5.5,2.5,5)+0.1,mgp=c(3,1,0))
boxplot(hrsprey_wd$Hours.hunted~prey.ordered,varwidth=TRUE,horizontal=TRUE,
        yaxt="n",xlab="Hunting (hours)\nWith dogs",frame.plot=FALSE,cex.axis=0.9)
axis(side=2, las=2, at=c(1:5), labels = c(
  names.ordered[1],
  names.ordered[2],
  names.ordered[3],
  names.ordered[4],
  names.ordered[5]),
  tick=FALSE)
axis(side=4, las=2, at=c(1:5), labels = c(
  paste("n = ",hpc$n[1]),
  paste("n = ",hpc$n[2]),
  paste("n = ",hpc$n[3]),
  paste("n = ",hpc$n[4]),
  paste("n = ",hpc$n[5])),
  tick=FALSE)
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0), mfrow=c(1,1)) # default
dev.off()

# Inferential statistics Hours~Prey when nDogs or wDogs Used
summary(lm(hrsprey_nd$Hours.hunted~hrsprey_nd$Prey.Catch))
summary(lm(hrsprey_wd$Hours.hunted~hrsprey_wd$Prey.Catch))


# Figure 5: Hunting trip harvest without and with dogs + t-Test #######
# Raw material produced here for later BW-work on art using Adobe Illustrator

# Inferential statistics Harvest~Trip per Dogs Used
# MIND: no Normal distribution found
# All hunts
huntsall <- lm(prey.suun$kcal ~ prey.suun$Dogs.used)
summary(huntsall)
# F = 4.1, df = 206, p = 0.04; Adjusted R squared = 0.015; kcal ~ 9933 + dogsY*5907
# Successful hunts only
prey.succ <- prey.suun %>% filter(kcal>0)
huntsucc <- lm(prey.succ$kcal ~ prey.succ$Dogs.used)
summary(huntsucc)
# F = 0.12, df = 119, p = 0.73; Adjusted R squared = -0.007; kcal ~ 26293 + dogsY*(-1467)

# t-tests #######
# All hunts
adpkcal <- kcalsum %>% filter(Dogs.used=="Yes")
length(adpkcal$kcal_sum) # 143
adakcal <- kcalsum %>% filter(Dogs.used=="No")
length(adakcal$kcal_sum) # 42
t.test(adpkcal$kcal_sum,adakcal$kcal_sum)
# TEXT: t=2.15, df=79.3, p-value=0.03 95% CI 567 to 14259
#       Means adpkcal = 18055 and adakcal = 10642
# Successful hunts
sdpkcal <- kcalsum %>% filter(Dogs.used=="Yes",kcal_sum>0)
length(sdpkcal$kcal_sum)
sdakcal <- kcalsum %>% filter(Dogs.used=="No",kcal_sum>0)
length(sdakcal$kcal_sum)
t.test(sdpkcal$kcal_sum,sdakcal$kcal_sum)
# TEXT: t=-0.21, df=19, p-value=0.83 95% CI -12990 to 10610
# Means sdpkcal = 30737 and sdakcal = 31927

round(mean(adpkcal$kcal_sum),1) # avg 18055.5
round(median(adpkcal$kcal_sum),1) # median 9750
round(sd(adpkcal$kcal_sum),1) # sd 22502.1
length(adpkcal$kcal_sum) # 143 trips

round(mean(adakcal$kcal_sum),1) # avg 10642.5
round(median(adakcal$kcal_sum),1) # median 0
round(sd(adakcal$kcal_sum),1) # sd 18661.4
length(adakcal$kcal_sum) # 42 trips


setEPS()
postscript("F5_Harvest-Trip_perDogsUsed.eps")
par(mfrow=c(2,1))
# Fig5a All trips (kcal>=0)
par(mar=c(6,6,4,4)+0.1,mgp=c(4,1,0))
boxplot(kcalsum$kcal_sum ~ kcalsum$Dogs.used, varwidth=TRUE, las=1, xaxt="n",
        ylab="Harvest (kcal)", frame.plot=FALSE)
axis(side = 1, at=c(1, 2), labels = c(
  paste0("Without dogs\nSample ", length(adakcal$kcal_sum), "  Succ.Tr ", length(which(adakcal$kcal_sum>0)), "\n%Succ.Rt ", round((length(which(adakcal$kcal_sum>0))*100)/length(adakcal$kcal_sum),0), "  Anim.Cap ", doguse_suco$n[1]),
  paste0("With dogs\nSample ", length(adpkcal$kcal_sum), "  Succ.Tr ", length(which(adpkcal$kcal_sum>0)), "\n%Succ.Rt ", round((length(which(adpkcal$kcal_sum>0))*100)/length(adpkcal$kcal_sum),0), "  Anim.Cap ", doguse_suco$n[2])),
  tick=FALSE, cex.axis=0.6)
# Fig5b Successful trips (kcal>0)
par(mar=c(6,6,4,4)+0.1,mgp=c(4,1,0))
boxplot(kcalsum$kcal_sum[kcalsum$kcal_sum>0] ~ kcalsum$Dogs.used[kcalsum$kcal_sum>0],
        varwidth=TRUE, las=1, xaxt="n", ylab="Harvest (kcal)",frame.plot=FALSE)
axis(side = 1, at=c(1, 2), labels = c(
  paste0("Without dogs"),
  paste0("With dogs")),
  tick=FALSE, cex.axis=0.6)
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0), mfrow=c(1,1)) # default
dev.off()
