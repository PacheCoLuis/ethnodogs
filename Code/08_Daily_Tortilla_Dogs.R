# SUBSISTENCE HUNTING TRIPS AND PARTIES #######################

source("07_Hunt_Trip_Party.R")

# Dogs' DER, Daily Tortilla (kcal), Size Peccary Group #######

# Percent of Dorgs' DER Met with Tortilla Ration
# Dogs' DER according to their activity type from:
#	Thatcher, C., Hand, M. S., & Remillard, R. L. (2010). Small animalclinical nutrition:
#	an iterative process. In C. Thatcher, M. S. Hand, & R. L. Remillard (Eds.), Smallanimal
#	clinical nutrition (pp. 3-21). Topeka: Mark Morris Institute.
# Kilocalories per gram of tortilla from:
#	USDA tortilla & dry corn kcal
#	USDA Branded Food Products Database Release October 2017 Software v.3.8.6.4 2017-10-02
#	Nutrient data for: 45135964, GONZALEZ, TORTILLAS DE MAIZ CORN TORTILLAS, UPC: 748703281103
#	according to these sources 1 tortilla (25.8 g) has 80 kcal/g

# Provide dogs' average body weight (BW), daily tortilla ration (wah), and activity type (at)
BW <- 12 # kg, body weight
wah <- 5 # tortillas (wah) fed daily
#at <- "activity type"
# pick one from the following categories
#	"Maintenance"				"Neutered adult"			"Intact adult"
#	"Inactive/obese prone"		"Weight loss"				"Critical care"
#	"Weight gain"				"Light work"					"Moderate work"
#	"Heavy work"					"Gestation first 42 days"	"Gestation last 21 days"
#	"Lactation 1 puppy"			"Lactation 2 puppies"		"Lactation 3-4 puppies"
#	"Lactation 5-6 puppies"		"Lactation 7-8 puppies"		"Lactation 9 puppies"
#	"Growth weaning-4mo age"	"Growth 4mo-adult size"

# Load function to calculate DER and percent of DER met by 'wah' (pDER_by_wah)
#	Resting Energy Requirement (RER) = (70*BW)^0.75
#	Daily Energy Requirement (DER) = RER * Activity factor
source("fun_DER_dogs.R")
#dogs_DER(BW,wah,at) # gets dogs' DER and pDER_by_wah, based on activity type
at <- "Heavy work"
round(dogs_DER(BW,wah,at)) # ...18% DER
at <- "Moderate work"
round(dogs_DER(BW,wah,at)) # ...44% DER
at <- "Intact adult"
round(dogs_DER(BW,wah,at)) # ...49% DER
at <- "Inactive/obese prone"
round(dogs_DER(BW,wah,at)) # ...74% DER
at <- "Lactation 3-4 puppies"
round(dogs_DER(BW,wah,at)) # ...22% DER
at <- "Neutered adult"
round(dogs_DER(BW,wah,at)) # ...55% DER
# Benedict and Steggerda (1936) dogs kept by Maya consumed 6 to 8 tortillas per day
BW # assuming the same body weight in 1936 and 2012
wah <- 8
at <- "Heavy work"
round(dogs_DER(BW,wah,at)) # ...28% DER
at <- "Moderate work"
round(dogs_DER(BW,wah,at)) # ...71% DER

# Total protein per capita or household anually
round(sum(prey.all$PC.kg)/SC_totpop,1) # 4.1 kg
round(sum(prey.all$kcal)/SC_totpop,1) # 7076.9 kcal
round(sum(prey.all$PC.kg)/SC_totHHs,1) # 21.3 kg
round(sum(prey.all$kcal)/SC_totHHs,1) # 36938.1 kcal

# Size range of a group of peccaries
selvar <- c("HA.file","date.hunt","date.surv",
            "dogs.used", "weapon","dura",
            "prey1", "prey2", "prey3", "prey4",
            "pnum1", "pnum2", "pnum3", "pnum4",
            "pnkill1", "pnkill2", "pnkill3", "pnkill4",
            "pweight1","pweight2","pweight3","pweight4")
HAlong <- HAsub[,selvar]
# convertimos el conjunto de datos del formato ancho al formato largo
HAlong <- reshape(HAlong,varying=list(prey=c("prey1","prey2","prey3","prey4"),
                                      weight=c("pweight1","pweight2","pweight3","pweight4"),
                                      pnum=c("pnum1","pnum2","pnum3","pnum4"),
                                      pnkill=c("pnkill1","pnkill2","pnkill3","pnkill4")),
                  direction="long")
range(HAlong$pnum1[HAlong$prey1=="PECCARY"],na.rm=TRUE)
table(HAlong$pnum1[HAlong$prey1=="PECCARY"])
# TEXT: ...a group of peccaries (n = 3-9)

write.csv(HAlong,"HA_long.csv",row.names=FALSE)

# Milpa maize that dogs protect
# see "Dogs_DER_and_Maize.xlsx" for full details
# Milpa area (sqm) or dry maize (kg) kitam can eat per visit 


# Further descriptive figures: HA (start-end, ns, schedules), GPS/HR (parties) #######
# Start and end times for HA
range(HAsub$date.surv,na.rm=TRUE) # "2012-04-17 CST" "2012-12-27 CST"

# Total interviews 221 = 137 without hunting trip + 84 with hunting trip
# as read in JAVG (2019: p. 21) thesis. That shows that (84*100)/221
# in 38% of the interviews hunters reported to have hunted in the
# previous days or did spontaneously shared their adventures
# MIND: NAs are considered within those 221 cases, see below

# Households reporting hunts. Some of the ones not reporting hunts, due
# to no longer hunting since they lost their dogs, do not appear since
# I have not coded their hh.key yet [FEED no hunt cases into HA sample]
# Dates programmed in "HA_Schedules_2012.csv"
(length(sort(unique(HAsub$hh.key))) / SC_totHHs) * 100
# TEXT: about 27% of the households represented in the HA sample
ha.sched <- read.csv("HA_Schedules_2012.csv")
(length(sort(unique(ha.sched$hh))) / SC_totHHs) * 100
# TEXT: 40% of the households represented in the HA sample

unique(ha.sched$pdate) # as sheduled
ha.sched$pdate <- as.POSIXct(ha.sched$pdate,format="%m/%d/%y")
length(ha.sched$pdate) # 225 dates [NAs included]
ha.sched <- subset(ha.sched, !is.na(ha.sched$pdate))
length(ha.sched$pdate) # 187 dates
lpc_out <-  subset(ha.sched,ha.sched$notes=="LPC out of the village")
ha.sched <- subset(ha.sched,ha.sched$notes!="LPC out of the village")
length(ha.sched$pdate) # 157 dates [after omitting days in which LPC was not in SC]
dim(ha.sched) # 157 dates, 9 variables
length(ha.sched$pdate[!is.na(ha.sched$hh)]) # 155 dates [after omitting x2 NA cases for HH]
# TEXT: 155 scheduled HA interviews
ha.sched[,c("pdate","hh")]
length(unique(ha.sched$hh[!is.na(ha.sched$hh)])) # 33 HHs scheduled
table(ha.sched$hh[!is.na(ha.sched$hh)]) # number of HA interviews shceduled per household

unique(HAsub$date.surv[!is.na(HAsub$date.surv)]) # as surveyed (expected to be != to pdate)
dim(HAsub)[1] # 84 HA records
length(unique(HAsub$hh.key)) # 22 HHs sampled
table(HAsub$hh.key) # number of HA interviews registered per household

unique(HAsub$date.surv[!is.na(HAsub$date.surv)]) %in% unique(ha.sched$pdate)

schedate <- unique(ha.sched$pdate)
survdate <- unique(HAsub$date.surv[!is.na(HAsub$date.surv)])
survdate %in% schedate
survdate[1]-schedate
survdate[6]-schedate
survdate[11]-schedate
survdate[16]-schedate

# HA plot shows (i) scheduled HAs and (ii) hunt attempts reports
plot(ha.sched$pdate[ha.sched$notes!="LPC out of the village"],
     ha.sched$hh[ha.sched$notes!="LPC out of the village"],
     las=2, ylab="Hhousehold key", xlab="Date scheduled")
points(HAsub$date.surv,HAsub$hh.key,pch=21,bg="green")
# Dates when LPC was out... no HH were scheduled
plot(lpc_out$pdate,lpc_out$hh.key,pch=21,bg="red")
unique(lpc_out$pdate)

(dim(HAsub)[1]/length(ha.sched$hh[!is.na(ha.sched$hh)])) * 100
# in 54% (N = 84) of scheduled HA interviews informants reported a hunt attempt [THUS...]
# TEXT: In 46% (n = 71) of the scheduled HA visits hunters reported having not
#       attempted to hunt in the previous days. [MIGHT NOT BE COMPLETELY CORRECT]
#       [we did not mark on the HA schedule whether or not each HHhead tried to hunt]
(length(HAsub$success[HAsub$success=="Y"])/length(ha.sched$hh[!is.na(ha.sched$hh)])) * 100

# HOW MANY OF THE 'HAsub' RECORDS MATCH THE PROGRAMMED INTERVIEWS IN 'ha.sched'?
plot(ha.sched$pdate,ha.sched$hh,las=2,ylab="Hhousehold key",xlab="Date scheduled")
points(HAsub$date.surv,HAsub$hh.key,pch=21,bg="green")
# NOT MANY FOLLOWING THE PLOT, MOST CASES MIGHT BE CLOSE TO 'ha.sched$pdate' THOUGH

(length(HAsub$success[HAsub$success=="Y"])/dim(HAsub)[1]) * 100
# 69% of HA reported hunt attempts were successful
HAsub[which(HAsub$success=="Y"),c("HA.file","hh.key","date.hunt","date.surv","dogs.used","success","dura")]

length(hsdu$Date.LPC[hsdu$Source=="HA"]) == length(HAsub$date.hunt)
length(hsdu$Date.LPC[hsdu$Source=="HA" & hsdu$Harvest>0]) # 55
length(HAsub$date.hunt[HAsub$success=="Y"]) # 58 ... definition of success?
length(HAsub$date.hunt[!is.na(HAsub$date.hunt) & HAsub$pnkillt>0]) # 54

sort(hsdu$Trip.ID.by.date[hsdu$Source=="HA" & hsdu$Harvest>0])
sort(HAsub$Trip.ID.by.date[!is.na(HAsub$date.hunt) & HAsub$pnkillt>0])
sort(HAsub$Trip.ID.by.date[HAsub$pnkillt>0])
# trip 69 instead of trip 68? and trip 132 is missed, brings differed lengths and sequence 

(length(hsdu$Trip.ID.by.date[hsdu$Source=="HA"&hsdu$Harvest>0])/length(ha.sched$hh[!is.na(ha.sched$hh)])) * 100
# TEXT: Only in 35% (37%?) of the scheduled HA interviews households
#       reported a successful hunt attempt. [FAIR COMPARISON?]


data.frame(HAsub$date.surv,HAsub$date.hunt)
(HAsub$date.surv-HAsub$date.hunt)/86400 # difference in days
table((HAsub$date.surv-HAsub$date.hunt)/86400) # difference in days
sum(table((HAsub$date.surv-HAsub$date.hunt)/86400)) # NAs present != tot n
( sum( table((HAsub$date.surv-HAsub$date.hunt)/86400)[1:7] ) / length( HAsub$date.surv ) ) * 100
( sum( table((HAsub$date.surv-HAsub$date.hunt)/86400)[8:15] ) / length( HAsub$date.surv ) ) *100
# TEXT: in the last six days (75% of reports), or between 8 to 29 days (12% of reports)
#       n = 63, or n = 10, respectively [THESE DO NOT SUM 84 AS THEY SHOULD...NAs]
(length(which(is.na(HAsub$date.surv)|is.na(HAsub$date.hunt))) / length( HAsub$date.surv )) * 100
# TEXT: 13% NA cases for date.surv | date.hunt

table(hds_hwd$hhclass)
# Active hunters represent 21% of HHs
(table(hds_hwd$hhclass)["active hunters"][[1]] / SC_totHHs) * 100
# Occasional hunters represent 30% of HHs
(table(hds_hwd$hhclass)["occasional hunters"][[1]] / SC_totHHs) * 100
# Past hunters represent 22% of HHs
(table(hds_hwd$hhclass)["past hunters"][[1]] / SC_totHHs) * 100
# Non-hunters represent 21% of HHs
(table(hds_hwd$hhclass)["non-hunters"][[1]] / SC_totHHs) * 100
# 21+30+22+21 != 100 since not all 'SC_totHHs' are included in the 'hds_hwd' sample

# Did we HA sampled active or occasional hunters' HHs?
sort(unique(HAsub$hh.key))
hds_hwd[which(hds_hwd$hhclass=="active hunters"),"hh.key"]
hds_hwd[which(hds_hwd$hhclass=="occasional hunters"),"hh.key"]
# Yes, to a good extent... though less for occasional hunters
# CHECK for additional matches among the trips' parties

# GPS/HR party members figures
# counts result from assembling the "HwD_db.R"
# according to the number of tracked hunters and dogs
# TEXT: 110 tracks for 39 dogs
#		97 tracks for 29 hunters
xtabs(~Dogs.used+Day.Night,hsdu[which(hsdu$Source=="GPS/HR"),])
# TEXT: 40 daytime trips with dogs, and 1 without dogs
#		0 nighttime trips with dogs, 7 trips without dogs


# GPS/HR trips (all members of the party, tracked ones included)
hunpty_gps <- summarise(by_trip[which(by_trip$Source=="GPS/HR"),],count=n(),
                        numh=length(ID[Hum.Dog=="Human"]),
                        numd=length(ID[Hum.Dog=="Dog"]),
                        numa=length(which(!is.na(ID)[assistant=="yes"])==TRUE)) ; hunpty_gps
# Mean and sd for dogs and hunters in the party (per day)
round(mean(hunpty_gps$numh)) ; round(sd(hunpty_gps$numh)) ; range(hunpty_gps$numh)
round(mean(hunpty_gps$numd)) ; round(sd(hunpty_gps$numd)) ; range(hunpty_gps$numd)
round(mean(hunpty_gps$numa)) ; round(sd(hunpty_gps$numa)) ; range(hunpty_gps$numa)
length(hunpty_gps$Trip.ID.by.date)
# TEXT: avg 4 sd 2 range 1 to 8 hunters
#		avg 5 sd 4 range 0 to 15 dogs
#		avg 1 sd 1 range 0 to 4 assistants
#		n = 48 trips


# FIGURE S4: Distribution of harvest per prey type (kcal) #######
# KILOGRAMS
boxplot(prey.all$PC.kg ~ prey.all$Prey.Catch, las=2)
boxplot.medians <- boxplot(prey.all$PC.kg ~ prey.all$Prey.Catch, plot=FALSE)$stats[3,]
boxplot.names <- boxplot(prey.all$PC.kg ~ prey.all$Prey.Catch, plot=FALSE)$names
names.ordered <- boxplot.names[order(boxplot.medians)]
prey.ordered <- factor(prey.all$Prey.Catch, ordered=TRUE, levels=names.ordered)
hpc <- boxplot(prey.all$PC.kg~prey.ordered,plot=0)

setEPS()
postscript("FS4_Distribution_Harvests_Prey_Type_KG.eps")
par(mar=c(5,5.5,3,4.4)+0.1)
boxplot(prey.all$PC.kg ~ prey.ordered, varwidth=TRUE, horizontal=TRUE,
        las=2, xlab="Harvest (kg)", xaxt="n", frame.plot=FALSE)
axis(side = 1)
axis(side=4, las=2, at=c(1:10), labels = c(
  paste("n = ",hpc$n[1]),
  paste("n = ",hpc$n[2]),
  paste("n = ",hpc$n[3]),
  paste("n = ",hpc$n[4]),
  paste("n = ",hpc$n[5]),
  paste("n = ",hpc$n[6]),
  paste("n = ",hpc$n[7]),
  paste("n = ",hpc$n[8]),
  paste("n = ",hpc$n[9]),
  paste("n = ",hpc$n[10])),
  tick=FALSE)
par(mar = c(5, 4, 4, 2) + 0.1) #default
dev.off()

# KILOCALORIES
boxplot(prey.all$kcal ~ prey.all$Prey.Catch, las=2)
boxplot.medians <- boxplot(prey.all$kcal ~ prey.all$Prey.Catch, plot=FALSE)$stats[3,]
boxplot.names <- boxplot(prey.all$kcal ~ prey.all$Prey.Catch, plot=FALSE)$names
names.ordered <- boxplot.names[order(boxplot.medians)]
prey.ordered <- factor(prey.all$Prey.Catch, ordered=TRUE, levels=names.ordered)
hpc <- boxplot(prey.all$kcal~prey.ordered,plot=0)	

setEPS()
postscript("FS4_Distribution_Harvests_Prey_Type_KCAL.eps")
par(mar=c(5,5.5,3,4.4)+0.1)
boxplot(prey.all$kcal ~ prey.ordered, varwidth=TRUE, horizontal=TRUE,
        las=2, xlab="Harvest (kcal)", xaxt="n", frame.plot=FALSE)
axis(side = 1)
axis(side=4, las=2, at=c(1:10), labels = c(
  paste("n = ",hpc$n[1]),
  paste("n = ",hpc$n[2]),
  paste("n = ",hpc$n[3]),
  paste("n = ",hpc$n[4]),
  paste("n = ",hpc$n[5]),
  paste("n = ",hpc$n[6]),
  paste("n = ",hpc$n[7]),
  paste("n = ",hpc$n[8]),
  paste("n = ",hpc$n[9]),
  paste("n = ",hpc$n[10])),
  tick=FALSE)
par(mar = c(5, 4, 4, 2) + 0.1) #default
dev.off()


###  THIS  IS  THE  END  ###
