# SUBSISTENCE HUNTING TRIPS AND PARTIES #######################

source("06_Hunt_Prey_Kg_to_Kcal.R")

# Prey and Trip (Successful Hunts) & Likelihood (Hours Without/With Dogs) #######
# PREY and TRIP counts for Successful Hunts
prety.trnd <- group_by(hrsprey[hrsprey$Dogs.used=="No",],Prey.Catch) %>% summarize(count=n(),  length(unique(Trip.ID.by.date)))
colnames(prety.trnd) <- c("Prey", "N", "Trips"); prety.trnd
#write.table(prety.trnd, file="Table_Prey-Trips_WithoutDogs.csv", append = FALSE, quote=FALSE, sep=", ", row.names=FALSE)
prety.trwd <- group_by(hrsprey[hrsprey$Dogs.used=="Yes",],Prey.Catch) %>% summarize(count=n(),  length(unique(Trip.ID.by.date)))
colnames(prety.trwd) <- c("Prey", "N", "Trips"); prety.trwd
#write.table(prety.trnd, file="Table_Prey-Trips_WithDogs.csv", append = FALSE, quote=FALSE, sep=", ", row.names=FALSE)

# LIKELIHOOD for Hours_WithoutDogs < Hours_WithDogs
# Breaking it down per prey type
prey.tech <- distinct( select( hsd,
                               Trip.ID.by.date, Source.Uniform , Dogs.used, Day.Night,
                               Prey.Number, Prey.Catch, Prey.Catch.2, Prey.Catch.3, Prey.Catch.4, Prey.Catch.5,
                               Hours.hunted, Technology) )
prey.tech <- filter( prey.tech, !(Trip.ID.by.date==102 & Prey.Catch==""), !(Trip.ID.by.date==180 & Prey.Catch=="") )
prey.tech$Prey.Aim <- rep("aim_PCS",dim(prey.tech)[1])
prey.tech$Prey.Catch <- as.character(prey.tech$Prey.Catch)
prey.tech$Prey.Catch[prey.tech$Prey.Catch==""] <- prey.tech$Prey.Aim[prey.tech$Prey.Catch==""]
prey.tech$Prey.Catch <- factor(prey.tech$Prey.Catch)
prey.tech <- reshape(prey.tech, varying=list(prey=c("Prey.Catch","Prey.Catch.2","Prey.Catch.3","Prey.Catch.4","Prey.Catch.5")), direction="long")
prey.tech <- filter(prey.tech,Prey.Catch!="")
prey.tech <- droplevels(prey.tech)
prey.tech <- prey.tech[order(prey.tech$Prey.Catch,prey.tech$Trip.ID.by.date),]
# ...it has to do do with the Technology
xtabs(~Prey.Catch+Technology,prey.tech)
xtabs(~Prey.Catch+Technology,prey.tech[prey.tech$Prey.Number>0,])
xtabs(~Dogs.used+Technology,prey.tech)
xtabs(~Dogs.used+Technology,prey.tech[prey.tech$Prey.Number>0,])

# Breaking it down per average trip duration
round(mean(prey.tech$Hours.hunted[prey.tech$Dogs.used=="No"&!is.na(prey.tech$Hours.hunted)]),1)
round(sd(prey.tech$Hours.hunted[prey.tech$Dogs.used=="No"&!is.na(prey.tech$Hours.hunted)]),1)
length(unique(prey.tech$Trip.ID.by.date[prey.tech$Dogs.used=="No"&!is.na(prey.tech$Hours.hunted)]))
# avg 4.5 sd 2.2 hours 31 trips
round(mean(prey.tech$Hours.hunted[prey.tech$Dogs.used=="Yes"&!is.na(prey.tech$Hours.hunted)]),1)
round(sd(prey.tech$Hours.hunted[prey.tech$Dogs.used=="Yes"&!is.na(prey.tech$Hours.hunted)]),1)
length(unique(prey.tech$Trip.ID.by.date[prey.tech$Dogs.used=="Yes"&!is.na(prey.tech$Hours.hunted)]))
# avg 5 sd 2.2 hours 105 trips


# Hunting parties composition #######
# Counting hunters, dogs, and assistants
by_trip <- hpts %>% group_by(Trip.ID.by.date,Source)

# All trips
hun_pty <- summarise(by_trip,count=n(),
                     numh=length(ID[Hum.Dog=="Human"]),
                     numd=length(ID[Hum.Dog=="Dog"]),
                     numa=length(which(!is.na(ID)[assistant=="yes"])==TRUE)) ; hun_pty       #write.csv(hun_pty,"hun_pty.csv",row.names=FALSE)

# Mean and sd for dogs and hunters in the party (per day)
round(mean(hun_pty$numh)) ; round(sd(hun_pty$numh)) ; range(hun_pty$numh)
round(mean(hun_pty$numd)) ; round(sd(hun_pty$numd)) ; range(hun_pty$numd)
round(mean(hun_pty$numa)) ; round(sd(hun_pty$numa)) ; range(hun_pty$numa)
length(hun_pty$Trip.ID.by.date)
# TEXT: avg 3 sd 2 range 1 to 8 hunters
#		avg 4 sd 3 range 0 to 15 dogs
#		avg 1 sd 1 range 1 to 4 assistants
#		n = 132 trips

# Without dogs trips
hun_pty_nd <- summarise(by_trip[which(by_trip$Dogs.used=="No"),],count=n(),
                        numh=length(ID[Hum.Dog=="Human"]),
                        numd=length(ID[Hum.Dog=="Dog"]),
                        numa=length(which(!is.na(ID)[assistant=="yes"])==TRUE)) ; hun_pty_nd
# Mean and sd for dogs and hunters in the party (per day)
round(mean(hun_pty_nd$numh)) ; round(sd(hun_pty_nd$numh)) ; range(hun_pty_nd$numh)
round(mean(hun_pty_nd$numd)) ; round(sd(hun_pty_nd$numd)) ; range(hun_pty_nd$numd)
round(mean(hun_pty_nd$numa)) ; round(sd(hun_pty_nd$numa)) ; range(hun_pty_nd$numa)
length(hun_pty_nd$Trip.ID.by.date)
# TEXT: avg 2 sd 1 range 1 to 7 hunters
#		avg 0 sd 0 range 0 to 0 dogs
#		range 0 to 2 assistants
#		n = 28 trips

# With dogs trips
hun_pty_wd <- summarise(by_trip[which(by_trip$Dogs.used=="Yes"),],count=n(),
                        numh=length(ID[Hum.Dog=="Human"]),
                        numd=length(ID[Hum.Dog=="Dog"]),
                        numa=length(which(!is.na(ID)[assistant=="yes"])==TRUE)) ; hun_pty_wd
# Mean and sd for dogs and hunters in the party (per day)
round(mean(hun_pty_wd$numh)) ; round(sd(hun_pty_wd$numh)) ; range(hun_pty_wd$numh)
round(mean(hun_pty_wd$numd)) ; round(sd(hun_pty_wd$numd)) ; range(hun_pty_wd$numd)
round(mean(hun_pty_wd$numa)) ; round(sd(hun_pty_wd$numa)) ; range(hun_pty_wd$numa)
length(hun_pty_wd$Trip.ID.by.date)
# TEXT: avg 3 sd 2 range 1 to 8 hunters
#		avg 5 sd 3 range 1 to 15 dogs
#		avg 1 sd 1 range 0 to 3 assistants
#		n = 104 trips
# NOTE: if LPC (hM0A) and Tuli (dF0A) are removed the hunting parties hold the same,
#		and the upper range values increase by 1. There is only one issue with
#		omitting Tuli: a HwD trip results with 0-dogs (which has no sense)
#		hun_pty_wd[which(hun_pty_wd$numd==0),"Trip.ID.by.date"]
#		hpts[which(hpts$Trip.ID.by.date==185),]


zhds_daydogy <- hsd %>% group_by(Trip.ID.by.date,Source.Uniform) %>% summarise(count=n(), numh=length(Hunter.ID), dayh=length(Day.Night[Day.Night=="day"]), dogy=length(Dogs.used[Dogs.used=="Yes"]))
zhds_daydogy[which(zhds_daydogy$dayh>0),] %>% group_by(Source.Uniform) %>% summarise(n=n(),avgh=mean(numh),sdh=sd(numh))

zhds_nigdogn <- hsd %>% group_by(Trip.ID.by.date,Source.Uniform) %>% summarise(count=n(),  numh=length(Hunter.ID), nigh=length(Day.Night[Day.Night=="night"]), dogn=length(Dogs.used[Dogs.used=="No"]))
zhds_nigdogn[which(zhds_nigdogn$nigh>0),] %>% group_by(Source.Uniform) %>% summarise(n=n(),avgh=mean(numh[nigh>0]),sdh=sd(numh[nigh>0]))

hsdu %>% group_by(Source) %>% tally()
hsdu %>% group_by(Source,Day.Night) %>% tally()

hpts %>% group_by(Trip.ID.by.date) %>% tally()
hpts %>% group_by(Trip.ID.by.date,Source) %>% tally()
hpts %>% group_by(Trip.ID.by.date,Source,Dogs.used) %>% tally()


# Notes trips without dogs
hptynd_notes <- group_by( hsd[which(hsd$Source.Uniform=="Notes" & hsd$Dogs.used=="No"),],
                          Trip.ID.by.date) %>% summarize(numh=length(Hunter.ID), numa=sum(length(which(
                            !is.na(Assistant.1.ID))), length(which(!is.na(Assistant.2.ID))), length(which(
                              !is.na(Assistant.3.ID)))) ) ; hptynd_notes
round(mean(hptynd_notes$numh)) ; round(sd(hptynd_notes$numh)) ; range(hptynd_notes$numh)
round(mean(hptynd_notes$numa)) ; round(sd(hptynd_notes$numa)) ; range(hptynd_notes$numa)
length(hptynd_notes$Trip.ID.by.date)
# TEXT: avg 2 sd 1 range 1 to 4 hunters
#		range 0 to 1 assistants
#		n = 14 trips

# Notes trips with dogs
hptywd_notes <- group_by( hsd[which(hsd$Source=="Notes" & hsd$Dogs.used=="Yes"),],
                          Trip.ID.by.date) %>% summarize(numh=length(Hunter.ID), numa=sum(length(which(
                            !is.na(Assistant.1.ID))), length(which(!is.na(Assistant.2.ID))), length(which(
                              !is.na(Assistant.3.ID)))) ) ; hptywd_notes
round(mean(hptywd_notes$numh)) ; round(sd(hptywd_notes$numh)) ; range(hptywd_notes$numh)
round(mean(hptywd_notes$numa)) ; round(sd(hptywd_notes$numa)) ; range(hptywd_notes$numa)
length(hptywd_notes$Trip.ID.by.date)
# TEXT: avg 2 sd 1 range 1 to 5 hunters
#		avg 0 sd 1 range 1 range 0 to 2 assistants
#		n = 39 trips

# Hunts: Unsuccessful and Successful #######
# Prey.Catch 'DayOrNight' and 'WithoutDogsOrWithDogs'
xtabs(~Prey.Catch+Day.Night,prey.all)
xtabs(~Prey.Catch+Day.Night+Dogs.used,prey.all)

# Total average harvest per successful trip
# Unsuccessful and successful hunts
# Without dogs
round(mean(prey.suun$PC.kg[prey.suun$Dogs.used=="No"]),1) # avg 7.6
round(sd(prey.suun$PC.kg[prey.suun$Dogs.used=="No"]),1) # sd 13.7
length(prey.suun$PC.kg[prey.suun$Dogs.used=="No"]) # 45 obs. [longitudinal prey catch]
length(unique(prey.suun$Trip.ID.by.date[prey.suun$Dogs.used=="No"])) # 42 trips
# With dogs
round(mean(prey.suun$PC.kg[prey.suun$Dogs.used=="Yes"]),1) # avg 8.7
round(sd(prey.suun$PC.kg[prey.suun$Dogs.used=="Yes"]),1) # sd 10.1
length(prey.suun$PC.kg[prey.suun$Dogs.used=="Yes"]) # 163 obs. [longitudinal prey catch]
length(unique(prey.suun$Trip.ID.by.date[prey.suun$Dogs.used=="Yes"])) # 143 trips
# Day
round(mean(prey.suun$PC.kg[prey.suun$Day.Night=="day"]),1) # avg 8.8
round(sd(prey.suun$PC.kg[prey.suun$Day.Night=="day"]),1) # sd 10.5
length(unique(prey.suun$Trip.ID.by.date[prey.suun$Day.Night=="day"])) # 152 trips
# Night
round(mean(prey.suun$PC.kg[prey.suun$Day.Night=="night"]),1) # avg 6.6
round(sd(prey.suun$PC.kg[prey.suun$Day.Night=="night"]),1) # sd 12.9
length(unique(prey.suun$Trip.ID.by.date[prey.suun$Day.Night=="night"])) # 33 trips

# Successful hunts
# Without dogs
round(mean(prey.all$PC.kg[prey.all$Dogs.used=="No"]),1) # avg 20
round(sd(prey.all$PC.kg[prey.all$Dogs.used=="No"]),1) # sd 15.8
length(unique(prey.all$Trip.ID.by.date[prey.all$Dogs.used=="No"])) # 14 trips
# With dogs
round(mean(prey.all$PC.kg[prey.all$Dogs.used=="Yes"]),1) # avg 13.6
round(sd(prey.all$PC.kg[prey.all$Dogs.used=="Yes"]),1) # sd 9.7
length(unique(prey.all$Trip.ID.by.date[prey.all$Dogs.used=="Yes"])) # 84 trips
# Day
round(mean(prey.all$PC.kg[prey.all$Day.Night=="day"]),1) # avg 13.9
round(sd(prey.all$PC.kg[prey.all$Day.Night=="day"]),1) # sd 10.2
length(unique(prey.all$Trip.ID.by.date[prey.all$Day.Night=="day"])) # 88 trips
# Night
round(mean(prey.all$PC.kg[prey.all$Day.Night=="night"]),1) # avg 19.2
round(sd(prey.all$PC.kg[prey.all$Day.Night=="night"]),1) # sd 15.8
length(unique(prey.all$Trip.ID.by.date[prey.all$Day.Night=="night"])) # 10 trips


# Figure S2: Total harvest per trip by methodology + Table FS2 #######
# Sources for successful trips associated with x1, x2, x3, x4, or x5 captures
# Kills
source_capt <- boxplot(prey.suun$PC.kg[prey.suun$Prey.Catch!="aim_PCS"] ~ prey.suun$Source.Uniform[prey.suun$Prey.Catch!="aim_PCS"],plot=0) # $n 16+68+37==121 captures
sum(kcalsum$Prey.Number) # 121 captures
# Fails
length(which(kcalsum$Prey.Number==0)) # 87 failures
source_unsu <- boxplot(kgsum$kg_sum[kgsum$kg_sum==0] ~ kgsum$Source.Uniform[kgsum$kg_sum==0],plot=0) # $n 35+29+23==98 unsuccessful trips

# Sources for all trips associated with x1, x2, x3, x4, or x5 captures
boxplot(prey.suun$PC.kg ~ prey.suun$Source.Uniform,plot=0) # $n 51+97+60 == 208
source_smpl <- boxplot(kcalsum$kcal_sum ~ kcalsum$Source.Uniform,plot=0) # $ 48+84+53 == 185
source_succ <- boxplot(kgsum$Prey.Number[kgsum$Prey.Number>0] ~ kgsum$Source.Uniform[kgsum$Prey.Number>0],plot=0) # $n 13+55+30==98 successful trips
kgsum %>% group_by(Source.Uniform) %>% summarise(animals_captured=sum(Prey.Number))
kcalsum %>% group_by(Source.Uniform) %>% summarise(animals_captured=sum(Prey.Number))
# Animals captured (kgsum|kcalsum):
#     GPS/HR 16 + HA 68 + Notes 37 == 121
kgsum %>% group_by(Source.Uniform) %>% summarise(total_kg=sum(kg_sum))
kcalsum %>% group_by(Source.Uniform) %>% summarise(total_kcal=sum(kcal_sum))
# kg(kcal) harvested:
#     GPS/HR 190(367419) + HA 1001(1750917) + Notes 560(910585) == 1750.6(3028922)
sum(kcalsum$kcal_sum[kcalsum$Source.Uniform=="GPS/HR"])/sum(kcalsum$Prey.Number[kcalsum$Source.Uniform=="GPS/HR"])
sum(kcalsum$kcal_sum[kcalsum$Source.Uniform=="HA"])/sum(kcalsum$Prey.Number[kcalsum$Source.Uniform=="HA"])
sum(kcalsum$kcal_sum[kcalsum$Source.Uniform=="Notes"])/sum(kcalsum$Prey.Number[kcalsum$Source.Uniform=="Notes"])

# KILOGRAMS
# All hunts
par(mar = c(6, 6, 2, 2) + 0.1, mgp=c(4,1,0))
boxplot(kgsum$kg_sum ~ kgsum$Source.Uniform, varwidth=TRUE, las=1, xaxt="n", ylab="Harvest (kg)", frame.plot=FALSE)
axis(side = 1, at=c(1:3), labels = c(
  paste0("GPS/HR\n Sample ", source_smpl$n[1], "  Succ.Tr ", source_succ$n[1], "\n %Succ.Rt ", (round((source_succ$n[1]*100)/source_smpl$n[1],0)), "  Anim.Cap ",source_capt$n[1]),
  paste0("HA\n Sample ", source_smpl$n[2], "  Succ.Tr ", source_succ$n[2], "\n %Succ.Rt ", (round((source_succ$n[2]*100)/source_smpl$n[2],0)), "  Anim.Cap ",source_capt$n[2]),
  paste0("Notes\n Sample ", source_smpl$n[3], "  Succ.Tr ", source_succ$n[3], "\n %Succ.Rt ", (round((source_succ$n[3]*100)/source_smpl$n[3],0)), "  Anim.Cap ",source_capt$n[3]) ),
  tick=FALSE, cex.axis=0.8)
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0), mfrow=c(1,1)) # default
# Successful hunts only
par(mar = c(3, 6, 2, 2) + 0.1, mgp=c(4,1,0))
boxplot(kgsum$kg_sum[kgsum$kg_sum>0] ~ kgsum$Source.Uniform[kgsum$kg_sum>0], varwidth=TRUE, las=1, xaxt="n", ylab="Harvest (kg)", frame.plot=FALSE)
axis(side = 1, at=c(1:3), labels = c( paste0("GPS/HR"), paste0("HA"), paste0("Notes") ), tick=FALSE, cex.axis=0.8)
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0), mfrow=c(1,1)) # default

# KILOCALORIES
# All hunts
setEPS()
postscript("FS2a_Harvest_by_Source.eps")
par(mar = c(6, 6, 2, 2) + 0.1, mgp=c(4,1,0))
boxplot(kcalsum$kcal_sum ~ kcalsum$Source.Uniform, varwidth=TRUE, las=1, xaxt="n", ylab="Harvest (kcal)", frame.plot=FALSE)
axis(side = 1, at=c(1:3), labels = c( 
  paste0("GPS/HR\n Sample ", source_smpl$n[1], "  Succ.Tr ", source_succ$n[1], "\n %Succ.Rt ", (round((source_succ$n[1]*100)/source_smpl$n[1],0)), "  Anim.Cap ",source_capt$n[1]),
  paste0("HA\n Sample ", source_smpl$n[2], "  Succ.Tr ", source_succ$n[2], "\n %Succ.Rt ", (round((source_succ$n[2]*100)/source_smpl$n[2],0)), "  Anim.Cap ",source_capt$n[2]),
  paste0("Notes\n Sample ", source_smpl$n[3], "  Succ.Tr ", source_succ$n[3], "\n %Succ.Rt ", (round((source_succ$n[3]*100)/source_smpl$n[3],0)), "  Anim.Cap ",source_capt$n[3]) ),
  tick=FALSE, cex.axis=0.7)
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0), mfrow=c(1,1)) # default
dev.off()
# Successful hunts only	
setEPS()
postscript("FS2b_Harvest_by_Source.eps")
par(mar = c(6, 6, 2, 2) + 0.1, mgp=c(4,1,0))
boxplot(kcalsum$kcal_sum[kcalsum$kcal_sum>0] ~ kcalsum$Source.Uniform[kcalsum$kcal_sum>0], varwidth=TRUE, las=1, xaxt="n", ylab="Harvest (kcal)", frame.plot=FALSE)
axis(side = 1, at=c(1:3), labels = c( paste0("GPS/HR"), paste0("HA"), paste0("Notes") ), tick=FALSE, cex.axis=0.7)
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0), mfrow=c(1,1)) # default
dev.off()

# median values and sample from which these come from
boxplot(kcalsum$kcal_sum[kcalsum$kcal_sum>0] ~ kcalsum$Source.Uniform[kcalsum$kcal_sum>0], varwidth=TRUE, las=1, xaxt="n", ylab="Harvest (kcal)", frame.plot=FALSE)$stats[3,]
boxplot(kcalsum$kcal_sum[kcalsum$kcal_sum>0] ~ kcalsum$Source.Uniform[kcalsum$kcal_sum>0], varwidth=TRUE, las=1, xaxt="n", ylab="Harvest (kcal)", frame.plot=FALSE)$names
# GPS/HR (17687.07 kcal), HA (30952.38 kcal), Notes (31499.57 kcal)

# Outlier point (123831 kcal) in HA boxplot corresponds to Trip.ID.by.date==139
kcalsum[which(kcalsum$kcal_sum==max(kcalsum$kcal_sum) & kcalsum$Source.Uniform=="HA"),]
hsd[which(hsd$Trip.ID.by.date==139),c("Source.Uniform","Date.LPC","Trip.ID.by.date","Harvest.kg","Prey.Number","Prey.Catch","PC.kg","PC.kg.2","PC.kg.3","Hunter.ID","Technology","Hours.hunted","Dogs.used")]
# TEXT: five hours trip where two peccaries were hunted with the aid of dogs (Fig S2 caption)


# TABLE FS2
# Parenthesis to get sample sizes, successful hunts counts, success rate, animals hunted
sampl.sourc <- group_by(unique(prey.suun[,c("Trip.ID.by.date","Source.Uniform")]),
                        Source.Uniform) %>% summarize(count=n())
colnames(sampl.sourc) <- c("Source","All_trips")
sampl.sourc

sampl.succe <- group_by(unique(prey.suun[which(prey.suun$Prey.Number>0),]),
                        Source.Uniform) %>% summarize(count=n())
colnames(sampl.succe) <- c("Source","Success_trips")
sampl.succe

succe.srate <- round(sampl.succe$Success_trips/sampl.sourc$All_trips, 2)
succe.srate <- t(data.frame(succe.srate))
colnames(succe.srate) <- c("X1","X2","X3")

sampl.anima <- group_by(unique(prey.suun[,c("Trip.ID.by.date","Source.Uniform","Prey.Number")]),
                        Source.Uniform) %>% summarize(sum(Prey.Number))
colnames(sampl.anima) <- c("Source","N_animals")
sampl.anima

combi.sharv <- data.frame(t(sampl.sourc[2]))
combi.sharv <- rbind(round(combi.sharv), data.frame(t(sampl.succe[2])),
                     succe.srate, data.frame(t(sampl.anima[2])))
colnames(combi.sharv) <- c("GPS.HR","HA","Notes")
combi.sharv["All_trips",] <- round(combi.sharv["All_trips",],0)
combi.sharv["Success_trips",] <- round(combi.sharv["Success_trips",],0)
combi.sharv["succe.srate",] <- round(combi.sharv["succe.srate",],2)
combi.sharv["N_animals",] <- round(combi.sharv["N_animals",],0)

write.table(combi.sharv, file="TableFS2_Combi_Harvest_by_Source.csv", append = FALSE, quote=FALSE, sep=" , ", row.names=TRUE, col.names=NA)
combi.sharv


# Table S1: Game captures by data methodology #######
# Game captures by data methodology
# All hunts
xtabs(~Prey.Catch+Source.Uniform,prey.all)
prey.all <- droplevels(prey.all)
xtabs(~Prey.Catch+Source.Uniform,prey.all)
# Successful hunts only
xtabs(~Prey.Catch+Source.Uniform,prey.suun[prey.suun$Prey.Catch!="aim_PCS",])
prey.suun <- droplevels(prey.suun)
prey.sour <- xtabs(~Prey.Catch+Source.Uniform,prey.suun[prey.suun$Prey.Catch!="aim_PCS",])

write.table(prey.sour, file="TableS2_Prey_by_Source.csv", append = FALSE, quote=FALSE, sep=" , ", row.names=TRUE, col.names=NA)
prey.sour


# Figure S3: Estimated weight error
# Omitting LPC from the sample
werror <- filter(werror,hum.key!="0A")
#werror$hum.key <- droplevels(werror$hum.key)
unique(werror$resource) # mabui, wech, suitcase, ik, box buul, potato, rice, kitam
# Coarsening categories
werror$wlabel <- rep(NA,dim(werror)[1])
# GAME
werror$wlabel[werror$resource=="kitam"] <- "Game"
werror$wlabel[werror$resource=="wech"] <- "Game"
# FRUITS
werror$wlabel[werror$resource=="ik"] <- "Fruits"
werror$wlabel[werror$resource=="mabui"] <- "Fruits"
# TUBERS
werror$wlabel[werror$resource=="potato"] <- "Tubers"
# GRAINS
werror$wlabel[werror$resource=="box buul"] <- "Grains"
werror$wlabel[werror$resource=="rice"] <- "Grains"
# SUITCASE
werror$wlabel[werror$resource=="suitcase"] <- "Suitcase"

#write.csv(werror,"werror.csv",row.names=FALSE)

# Getting the descriptive statistics	
max(abs(werror$wkg_real-werror$wkg_guess)) # 8.4 kg
werror[which(abs(werror$wkg_real-werror$wkg_guess)>4),]
min(abs(werror$wkg_real-werror$wkg_guess)[abs(werror$wkg_real-werror$wkg_guess)>0]) # 0.4 kg
which(werror$wkg_real==werror$wkg_guess) # two cases where estimated == real weight

round(mean(abs(werror$wkg_real-werror$wkg_guess)),1) # mean werror 2.9 kg
round(sd(abs(werror$wkg_real-werror$wkg_guess)),1) # standard deviation werror 2.1 kg
round(range(abs(werror$wkg_real-werror$wkg_guess)),1) # range werror 0 - 8.4 kg

length(unique(werror$date)) # 8 dates
length(unique(werror$wkg_real)) # 10 loads
length(unique(werror$hum.key)) # 27 hunters
length(werror$wkg_real) # 40 real weights

over.perc <- ((werror$wkg_real-werror$wkg_guess)/werror$wkg_real) * 100
# UNDER ESTIMATE
mean(over.perc[over.perc < 0]) # -33.8%
range(over.perc[over.perc < 0]) # -3.7% to -73.9%
(length(werror[which(werror$wkg_real-werror$wkg_guess<0),"resource"])/dim(werror)[1]) * 100
# TEXT: 37.5% of hunters under estimated weight
(length(werror[which((werror$wkg_real-werror$wkg_guess<0) & werror$wlabel=="Fruits"), "resource"])/dim(werror)[1]) * 100 # 2.5%
(length(werror[which((werror$wkg_real-werror$wkg_guess<0) & werror$wlabel=="Game"), "resource"])/dim(werror)[1]) * 100 # 25%
(length(werror[which((werror$wkg_real-werror$wkg_guess<0) & werror$wlabel=="Grains"), "resource"])/dim(werror)[1]) * 100 # 2.5%
# ...suitcase
(length(werror[which((werror$wkg_real-werror$wkg_guess<0) & werror$wlabel=="Tubers"), "resource"])/dim(werror)[1]) * 100 # 5%
# OVER ESTIMATE
mean(over.perc[over.perc > 0]) # 28.7%
range(over.perc[over.perc > 0]) # 7.4% to 74%
(length(werror[which(werror$wkg_real-werror$wkg_guess>0),"resource"])/dim(werror)[1]) * 100
# TEXT: 57.5% of hunters over estimated weight
(length(werror[which((werror$wkg_real-werror$wkg_guess>0) & werror$wlabel=="Fruits"), "resource"])/dim(werror)[1]) * 100 # 15%
(length(werror[which((werror$wkg_real-werror$wkg_guess>0) & werror$wlabel=="Game"), "resource"])/dim(werror)[1]) * 100 # 27.5%
(length(werror[which((werror$wkg_real-werror$wkg_guess>0) & werror$wlabel=="Grains"), "resource"])/dim(werror)[1]) * 100 # 7.5%
# ...suitcase
(length(werror[which((werror$wkg_real-werror$wkg_guess>0) & werror$wlabel=="Tubers"), "resource"])/dim(werror)[1]) * 100 # 7.5%

xtabs(~resource,werror)
xtabs(~resource+hh.key,werror)
xtabs(~resource+date,werror)


# Plotting
setEPS()
postscript("FS3_Reported_vs_Estimated_KG_jittered.eps")
plot(werror$wkg_real, werror$wkg_guess, type="n", frame.plot=FALSE, xlab="Measured weight (kg)",
     ylab="Reported weight (kg)", las=1,xlim=c(0,20), ylim=c(0,20))
# FRUITS
points(werror$wkg_real[werror$resource=="ik"], werror$wkg_guess[werror$resource=="ik"],
       pch=21, bg="red")
points(werror$wkg_real[werror$resource=="mabui"], werror$wkg_guess[werror$resource=="mabui"],
       pch=21, bg="red")
# GAME
points(werror$wkg_real[werror$resource=="kitam"],
       jitter(werror$wkg_guess[werror$resource=="kitam"],5),
       pch=23, bg="khaki")
points(werror$wkg_real[werror$resource=="wech"],
       jitter(werror$wkg_guess[werror$resource=="wech"],5),
       pch=23, bg="khaki")
# GRAINS
points(werror$wkg_real[werror$resource=="box buul"],
       werror$wkg_guess[werror$resource=="box buul"], pch=20)
points(werror$wkg_real[werror$resource=="rice"], werror$wkg_guess[werror$resource=="rice"],
       pch=20)
# SUITCASE
points(werror$wkg_real[werror$resource=="suitcase"],
       werror$wkg_guess[werror$resource=="suitcase"], pch=22, bg="ivory")
# TUBERS
points(werror$wkg_real[werror$resource=="potato"], werror$wkg_guess[werror$resource=="potato"],
       pch=24,bg="skyblue")
table(werror$wlabel)
lt <- c("Fruits (n=8)","Game (n=21)","Grains (n=5)","Suitcase (n=1)","Tubers (n=5)")
lp <- c(21,23,20,22,24)
lb <- c("red","khaki","black","ivory","skyblue")
legend(0.5,19.5,lt,cex=0.85,bty="n",pch=lp,pt.bg=lb,y.intersp=1)
lines(2:18,2:18,lty=2,lwd=1.5,col="gray33")
dev.off()

table(werror$wkg_real[werror$wlabel=="Game"],werror$wkg_guess[werror$wlabel=="Game"])

length(unique(werror$wkg_real)) == length(unique(werror$date))
# there are cases with > one resource (real weight) per date
length(unique(werror$wkg_guess))
length(unique(werror$resource))
length(unique(werror$wlabel))
table(werror$wlabel,werror$date)
# 8 dates; 10 unique real weights; 22 unique estimated weights

# are there "same weight measurements for different kills"?
table(werror$wkg_real,werror$wlabel)
table(werror$wkg_real,werror$resource)
length( which(unique(table(werror$wkg_real,werror$wlabel))>1) )
length( which(unique(table(werror$wkg_real,werror$resource))>1) )
# there are 5 cases with repeated values for real weights, same day-prey measurements
table(werror$wkg_guess,werror$wlabel)
table(werror$wkg_guess,werror$resource)
length( which(unique(table(werror$wkg_guess,werror$wlabel))>1) )
length( which(unique(table(werror$wkg_guess,werror$resource))>1) )
# there are 2 cases with repeated values for estimated weights

length( which(unique(table(werror$wkg_real,werror$wlabel))>1) ) == length( which(unique(table(werror$wkg_real,werror$date))>1) )
length( unique(table(werror$wkg_real,werror$wlabel)) ) == length( unique(table(werror$wkg_real,werror$date)) )

sort(unique(table(werror$wkg_real,werror$wlabel))) == sort(unique(table(werror$wkg_real,werror$date)))
# The number of repeated real weights is consistently associated to different kills or dates
# that is: we do not have "same weight measurements for different kills" in the sample.
