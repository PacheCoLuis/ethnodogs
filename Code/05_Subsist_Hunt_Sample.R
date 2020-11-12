# SUBSISTENCE HUNTING SAMPLE #######################

source("04_Functions_Maintenance.R")

# Table 3: Hunting sample characteristics by sampling method #######
# We omit cases were dogs' presence is uncertain in the Notes sample
# Subsetting cases by unique ID
hsdu <- unique(hsd[,c("Trip.ID.by.date", "Date.LPC", "Source.Uniform", "Dogs.used", "Day.Night", "HarvShare_NotAccounted")])
colnames(hsdu) <- c("Trip.ID.by.date", "Date.LPC", "Source", "Dogs.used", "Day.Night", "Harvest")

# ...totals
tot <- xtabs(~Source+Day.Night+Dogs.used,hsdu); tot
sum(tot["Notes",c("day","night"),c("No","Yes")]) # (53) Notes
sum(tot["HA",c("day","night"),c("No","Yes")]) # (84) HA
sum(tot["GPS/HR",c("day","night"),c("No","Yes")]) # (48) GPS/HR
sum(tot[,"day","Yes"]) # (143) with dogs day
sum(tot[,"night","Yes"]) # (0) with dogs night
sum(tot[,"day","No"]) # (9) without dogs day
sum(tot[,"night","No"]) # (33) without dogs night
# ...successful hunts
success <- xtabs(~Source[Harvest>0]+Day.Night[Harvest>0]+Dogs.used[Harvest>0],hsdu); success
sum(success["Notes",c("day","night"),c("No","Yes")]) # 30 Notes
sum(success["HA",c("day","night"),c("No","Yes")]) # 55 HA
sum(success["GPS/HR",c("day","night"),c("No","Yes")]) # 13 GPS/HR
sum(success[,"day","Yes"]) # 84 with dogs day
sum(success[,"night","Yes"]) # (0) with dogs night
sum(success[,"day","No"]) # 4 without dogs day
sum(success[,"night","No"]) # 10 without dogs night
# ...unsuccessful hunts
unsucce <- xtabs(~Source[Harvest==0]+Day.Night[Harvest==0]+Dogs.used[Harvest==0],hsdu); unsucce

# Check
unsucce == (tot-success)


# Hunting frequency: Dogs (Y|N), %HHs engaged, Dog-injury probability #######
# HUNT WITH DOGS (N | Y)
xtabs(~hhclass+hunt.with.dogs,hds_hwd)

# HUNT OR USED TO HUNT (currently | past)
xtabs(~hhclass+curr.or.past,hds_hwd)

# HUNT TPW TO TPM
xtabs(~hhclass+hunt.tpw,hds_hwd)
huntpw_DK <- which(hds_hwd$hunt.tpw=="DK")
hds_hwd[huntpw_DK,"hh.key"] # households reporting DK
hds_hwd$hunt.tpw[huntpw_DK] <- NA # converting DK to NA values
hds_hwd$hunt.tpw <- as.numeric(hds_hwd$hunt.tpw)
# multiply hunt.tpw values by four to get how many times per month (tpm)

# 'HUNT TPW TO TPM' + 'TPY' (latter from max_tpm)
hds_hwd %>%
  group_by(hhclass) %>% 
  summarise(nHH=n(),
            rHH=length(which(!is.na(hunt.tpw))),
            prHH=round((length(which(!is.na(hunt.tpw)))*100)/n(),1),
            min_tpm=round(min(hunt.tpw,na.rm=TRUE),1)*4,
            max_tpm=round(max(hunt.tpw,na.rm=TRUE),1)*4,
            tpy=(round(max(hunt.tpw,na.rm=TRUE),1)*4)*13)

# NOTES:
# active hunters: 1.6 to 16 times per month
#		the active hunters' maximum estimate of tpm hunting is over the total number of
#		hunting dates (n=185) we recorded with Notes, HA, GPS/HR, for different households
#		16 tpm * 13 months of fieldwork == 208 > 185 length(hsdu$Trip.ID.by.date)
# occasional hunters: 0 to 12 times per month
# past hunters: 2 to 28 times per month
#		the past hunters' maximum estimate of tpm hunting is way over the total number of
#		hunting dates (n=185) we recorded with Notes, HA, GPS/HR, for different households
#		28 tpm * 13 months of fieldwork == 364 > 185 length(hsdu$Trip.ID.by.date)
length(which(hds_hwd$hunt.tpw[hds_hwd$hhclass=="past hunters"]*4==28))
# past hunters: 4 out of 11 responding reported to hunt 28 tpm
# non-hunters: 4 to 4 times per month


# PERCENT OF HOUSEHOLDS THAT ENGAGE IN HUNTS
# From 17 active hunters + 24 occasional hunters
((17+24)*100)/length(unique(hds_hwd$hh.key))
# 54% of SC households hunt
# From age reported to have started hunt
xtabs(~hhclass+age.started.hunt.min,hds_hwd)
xtabs(~hhclass+age.started.hunt.max,hds_hwd)


# PROBABILITY OF INJURY TO DOGS
# Injury reported per month
table(hds_hwd$wounds.tpw)
xtabs(~hhclass+wounds.tpw,hds_hwd)
sum(xtabs(~hhclass+wounds.tpw,hds_hwd)) # 21[+5] HHs reported on wounds.tpw
dwndtpw_DK <- which(hds_hwd$wounds.tpw=="DK") # [five reported DK]
hds_hwd[dwndtpw_DK,"hh.key"] # households reporting DK
hds_hwd$wounds.tpw[dwndtpw_DK] <- NA # converting DK to NA values
hds_hwd$wounds.tpw <- as.numeric(hds_hwd$wounds.tpw)
# multiply hunt.tpw values by four to get how many times per month (tpm)
round(mean((hds_hwd$wounds.tpw * 4),na.rm=TRUE),1) # mean 2.3 wounds per month
round(sd((hds_hwd$wounds.tpw * 4),na.rm=TRUE),1) # sd 2 wounds per month
round(range((hds_hwd$wounds.tpw * 4),na.rm=TRUE),1) # range 0 to 8 wounds per month
# percent of HHs that answered this question
length(dwndtpw_DK) # 5 cases DK
(length(hds_hwd$wounds.tpw[!is.na(hds_hwd$wounds.tpw)])/
    length(hds_hwd$wounds.tpw))*100 # 28%

hds_hwd %>%
  group_by(hhclass) %>% 
  summarise(hh_rep_w=length(which(!is.na(wounds.tpw)==TRUE)),
            avg_ndog=round(mean(num.dog.male+num.dog.female,na.rm=TRUE),1),
            avg_wpm=round(mean(wounds.tpw*4,na.rm=TRUE),1),
            sd_wpm=round(sd(wounds.tpw*4,na.rm=TRUE),1),
            min_wpm=round(min(wounds.tpw*4,na.rm=TRUE),1),
            max_wpm=round(max(wounds.tpw*4,na.rm=TRUE),1),
            avg_wpy=(round(mean(wounds.tpw*4*13,na.rm=TRUE),1)))

hds_hwd %>%
  group_by(hhclass) %>% 
  summarise(hh_rep_w=length(which(!is.na(wounds.tpw)==TRUE)),
            avg_wpm=round(mean(wounds.tpw*4,na.rm=TRUE),1),
            hh_rep_h=length(which(!is.na(hunt.tpw)==TRUE)),
            avg_hpm=round(mean(hunt.tpw*4,na.rm=TRUE),1),
            hh_rep_wh=length(which(!is.na(wounds.tpw/hunt.tpw))),
            avg_wh=round(mean((wounds.tpw/hunt.tpw)*4,na.rm=TRUE),1))


round(mean(hds_hwd$wounds.tpw*4,na.rm=TRUE),1) # mean 2.3 wounds/month
round(mean(hds_hwd$hunt.tpw*4,na.rm=TRUE),1) # mean 8.6 trips/month

(sum(hds_hwd$wounds.tpw,na.rm=TRUE)/sum(hds_hwd$hunt.tpw,na.rm=TRUE)) * 8
# 0.13 * 8 == 1.1 would mean being wounded/injured once every two months
length(which(!is.na(hds_hwd$wounds.tpw/hds_hwd$hunt.tpw)))
# 18 HHs reporting wounds & hunts

length(which(!is.na(hds_hwd$wounds.tpw))) # 21 HHs
length(which(!is.na(hds_hwd$hunt.tpw))) # 40 HHs

length(which(!is.na(hds_hwd$wounds.tpw[hds_hwd$hunt.tpw!=0]))) # 18 cases
which(hds_hwd$wounds.tpw == 0) # two cases
length(which(!is.na(hds_hwd$hunt.tpw[hds_hwd$wounds.tpw!=0]))) # 16 cases
which(hds_hwd$hunt.tpw == 0) # no cases


# Hunting time (Night|Day), Harvest (kg), Success probability (Dog|EthnoSource), Other #######
round((sum(xtabs(~Source+Day.Night,hsdu)[,"night"])*100)/sum(tot),0)
# TEXT: 18% of the sample belongs to Night-time
round((sum(xtabs(~Source+Day.Night,hsdu)[,"day"])*100)/sum(tot),0)
# TEXT: 82% of the sample belongs to Day-time

# Prey-specific tactics
# Pure ethnographic accounts in this section
# Total harvest (kg per 13 months; mean, sd, n)
harv <- distinct( select( hsd, 
                          Trip.ID.by.date, Source.Uniform, HarvShare_NotAccounted, Dogs.used, Day.Night, 
                          Prey.Number, Prey.Catch, Prey.Catch.2, Prey.Catch.3, Prey.Catch.4, Prey.Catch.5,
                          PC.kg, PC.kg.2, PC.kg.3, PC.kg.4, PC.kg.5, Hours.hunted, Harvest.kg..NAs.Prey....,
                          Prey.Catch.Spa
) )
harv <- filter( harv, !(Trip.ID.by.date==102 & Prey.Catch==""), !(Trip.ID.by.date==180 & Prey.Catch=="") )
# Filtered trips (102 and 180) were the ones in which 'Prey.Catch' was unequal
# for hunters. That is, the prey was not equally diveded between members of
# the party--since it was too small or there were too many hunters to
# divide the catch into reasonable or useful portions
length(which((is.na(harv$Hours.hunted)&harv$Prey.Number==0)==TRUE))
# 21 unsuccesful trips for which Hours.hunted are NA
# (see 28 successful trips at the start of FIGURE-4 section)
# Fixing PC.kg in order to have it properly coded in the wide format,
# and to correspond with its twin variable Prey.Catch [x5 series]
# indicating unsuccessful hunts first
harv$PC.kg[harv$Prey.Number==0] <- harv$HarvShare_NotAccounted[harv$Prey.Number==0]
# followed by successful hunts with single prey captures
harv$PC.kg[harv$Prey.Number==1] <- harv$HarvShare_NotAccounted[harv$Prey.Number==1]
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# MIND: Prey.Catch.Spa contains a good idea of the prey missed or aimed in
# unssuccesful hunts. In the HA sample JAVG consistently coded the prey
# types that hunters sigthed (or aimed) during unsuccessfult hunts.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Fixing Prey.Catch when it is "" and PC.kg is 0. In such cases we will replace
# empty values "" with "aim_PCS", keeping in mind that Prey.Catch.Spa contains
# the list of game aimed by hunters. Further efforts to properly introduce these
# animals into the dataset will require to fill Prey.Catch to Prey.Catch5 with
# the correspondig values from Prey.Catch.Spa lists.
harv$Prey.Aim <- rep("aim_PCS",dim(harv)[1])
harv$Prey.Catch <- as.character(harv$Prey.Catch)
harv$Prey.Catch[harv$Prey.Catch==""] <- harv$Prey.Aim[harv$Prey.Catch==""]
harv$Prey.Catch <- factor(harv$Prey.Catch)
#write.csv(harv,"harv.csv",row.names=FALSE)

# Successful cases only (kg per 13 months; mean, sd, n)
harv_succ <- filter(harv,HarvShare_NotAccounted>0)

# kg/trip successful hunts Y|N dogs (mean, sd, n)
# Dogs Yes
round(mean(harv_succ$HarvShare_NotAccounted[harv_succ$Dogs.used=="Yes"]),1) # mean 16.8
round(sd(harv_succ$HarvShare_NotAccounted[harv_succ$Dogs.used=="Yes"]),1) # sd 14.2
length(harv_succ$HarvShare_NotAccounted[harv_succ$Dogs.used=="Yes"]) # n=84 trips
# Dogs No
round(mean(harv_succ$HarvShare_NotAccounted[harv_succ$Dogs.used=="No"]),1) # mean 24.3
round(sd(harv_succ$HarvShare_NotAccounted[harv_succ$Dogs.used=="No"]),1) # sd 15.3
length(harv_succ$HarvShare_NotAccounted[harv_succ$Dogs.used=="No"]) # n=14 trips

# Probabilities of success when dogs are present
# probability = successful cases[Dogs.used=="Yes"] / all cases[Dogs.used=="Yes"]
round(length(harv_succ$HarvShare_NotAccounted[harv_succ$Dogs.used=="Yes"]) /
        length(harv$HarvShare_NotAccounted[harv$Dogs.used=="Yes"]),2) # 0.6 WITH DOGS
# probability = successful cases[Dogs.used=="No"] / all cases[Dogs.used=="No"]
round(length(harv_succ$HarvShare_NotAccounted[harv_succ$Dogs.used=="No"]) /
        length(harv$HarvShare_NotAccounted[harv$Dogs.used=="No"]),2) # 0.3 WITHOUT DOGS

# Probabilities of hunt success by source type
# probability = successful cases / all cases
round(length(harv_succ$HarvShare_NotAccounted[harv_succ$Source.Uniform=="Notes"]) /
        length(harv$HarvShare_NotAccounted[harv$Source.Uniform=="Notes"]),2)
# 0.6 for FIELDNOTES
round(length(harv_succ$HarvShare_NotAccounted[harv_succ$Source.Uniform=="HA"]) /
        length(harv$HarvShare_NotAccounted[harv$Source.Uniform=="HA"]),2)
# 0.7 for HA
round(length(harv_succ$HarvShare_NotAccounted[harv_succ$Source.Uniform=="GPS/HR"]) /
        length(harv$HarvShare_NotAccounted[harv$Source.Uniform=="GPS/HR"]),2)
# 0.3 for GPS/HR
# MIND: these do not match with the ones in Fig S2 Edible Harvest ~ Source [since trips without duration omitted]

# Firearms and Official Regulations
xtabs(~hhclass+num.gun.own,hds_hwd)
sum(xtabs(~hhclass+num.gun.own,hds_hwd)[,"1"]) # 10 HHs report to own shotguns
hds_hwd[which(hds_hwd$hhclass=="active hunters" & hds_hwd$hunt.with.dogs=="Y" & hds_hwd$num.gun.own==1),"hh.key"]
# 4 active hunters hunt with dogs and own a shotgun: HH 9, HH 26, HH 65, HH 66
hds_hwd[which(hds_hwd$hhclass=="occasional hunters" & hds_hwd$hunt.with.dogs=="N" & hds_hwd$num.gun.own==1),"hh.key"]
# 3 occasional hunters do not hunt with dogs and own a shotgun: HH 19, HH 40, HH 70
hds_hwd[which(hds_hwd$hhclass=="occasional hunters" & hds_hwd$hunt.with.dogs=="Y" & hds_hwd$num.gun.own==1),"hh.key"]
# 1 occasional hunter hunt with dogs and own a shotgun: HH 8 
hds_hwd[which(hds_hwd$hhclass=="past hunters" & hds_hwd$num.gun.own==1),"hh.key"]
# 2 elder past hunters own a shotgun: HH 59 and HH 74

# Trading meat within SC
# BUY BUSH MEAT (Y or N)
table(hds_hwd$buy.meat)
# TEXT: only three households did not answer whether or not they buy bush meat
#		20 HHs do not buy, and 53 do buy from hunters or persons offering it
#		i.e. round((53/76)*100,0) == 70 % of survey respondents buy meat

# BUYING MEAT HABITS OF HOUSEHOLD TYPES 
xtabs(~hhclass+buy.meat,hds_hwd)
sort(table(hds_hwd$buy.meat.from))
# TEXT: consistent pattern, most active hunters do not buy meat
#		while most occasional, past and non-hunters do buy meat
# BUY BUSH MEAT FROM HOUSEHOLD
length(which(!is.na(hds_hwd$buy.meat.from)))
(length(which(!is.na(hds_hwd$buy.meat.from)))/length(hds_hwd$hh.key))*100
# 52 (68%) HHs report from whom they buy bush meat
# fixing the list of households that interviewees mentioned to sell meat
buybuch <- unlist(strsplit(as.character(hds_hwd$buy.meat.from),", "))
buybuch <- unlist(strsplit(buybuch,"A"))
buybuch <- unlist(strsplit(buybuch,"C"))
buybuch <- buybuch[-which(buybuch=="LL")]
sort(table(buybuch))
# interviewees reported to buy meat from a total of
length(unique(sort(buybuch))) # 17 HHs
# PRINT hh.key in order to match them with the lists below
hds_hwd[which(hds_hwd$hhclass=="active hunters"),"hh.key"] # ACTIVE HUNTERS
hds_hwd[which(hds_hwd$hhclass=="occasional hunters"),"hh.key"] # OCCASIONAL HUNTERS
hds_hwd[which(hds_hwd$hhclass=="past hunters"),"hh.key"] # PAST HUNTERS
hds_hwd[which(hds_hwd$hhclass=="non-hunters"),"hh.key"] # NON-HUNTERS
# respondents mentioned once
names(which(table(buybuch)==1))
# "19" "30" "40" "48" "6"  "60" "61" "73" "74"
# [3 active (6,61,73); 4 occasional (19,30,40,48); 1 past (74); 1 non-hunter (60)]
# respondents mentioned 2 to 4 times
names(which(table(buybuch)>=2 & table(buybuch)<=4))
# "4"  "49" "65" "66" [3 active hunters (49,65,66); 1 occasional hunter (4)]
# respondents mentioned 8 to 25 times
names(which(table(buybuch)>=8))
# "26" "45" "5"  "54" [3 active hunters (5,26,45), 1 occasional hunter (54)]

# TIMES PER WEEK THAT HOUSEHOLDS BUY MEAT
table(hds_hwd$buy.meat.tpw)
sort(names(table(hds_hwd$buy.meat.tpw))) # table with 25 dimnames
length(which(!is.na(hds_hwd$buy.meat.tpw)))
length(which(hds_hwd$buy.meat.tpw=="DK" | hds_hwd$buy.meat.tpw=="N"))
# TEXT: 46-5 ( (41/76)*100 ) 54% HHs reporting

# PER WEEK, multiply by 4 weeks to get times per month
buytpw <- data.frame(table(hds_hwd$buy.meat.tpw)[c("0","0.038","0.058","0.08","0.096","0.25","0.5","1","2","2.5","3","5")])
buytpw$Var1 <- as.numeric(levels(buytpw$Var1))[buytpw$Var1]
buytpw <- rep(buytpw$Var1,buytpw$Freq)
# PER MONTH, leave as it is
buytpm <- data.frame(table(hds_hwd$buy.meat.tpw)[c("1 per month","2 per month","2 to 3 per month","3 per month")])
buytpm$Var2 <- c(1,2,2.5,3)
buytpm <- rep(buytpm$Var2,buytpm$Freq)
# PER YEAR, divide by 12 months to get times per month
buytpy <- data.frame(table(hds_hwd$buy.meat.tpw)[c("1 per year","10 per year","10-15 per year","15 per year","2 times per year","2 to 3 per year","5 per year")])
buytpy$Var2 <- c(1,10,12.5,15,2,2.5,5)
buytpy <- rep(buytpy$Var2,buytpy$Freq)
# AVERAGE and SD, BUYING MEAT PER MONTH
buytpm <- c(buytpw*4, buytpm, buytpy/12)
round(mean(buytpm),1)
round(sd(buytpm),1)
# TEXT: mean 2.3 sd 3.9 times per month
