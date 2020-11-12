# FUNCTIONS AND MAINTENANCE OF DOGS #######################

source("03_Demography_of_dogs.R")

# Table 2: Origin, function, cost of keeping adults + distribution (adult | litters) #######

# Source - origin, acquisition #######
t(xtabs(~hhclass+dog.origin,hds_hwd))
# NOTE: labels correspond to
#   "Buy" is "buy"
#   "Home-born" is "grow"
#   "Both" is "buy, grow"
#   "Other" sums "exchange" and "OTHR" values
# DOG ORIGIN COUNTS PER HOUSEHOLD TYPE

# Number of HHs that did not respond to this question
hds_hwd %>% group_by(hhclass,dog.origin) %>% tally() # mind NAs: 1-AH, 8-NH, 7-OH, 4-PH
aggregate(is.na(hds_hwd$dog.origin),by=list(hds_hwd$hhclass),FUN="sum")

# active hunters
round( (xtabs(~hhclass+dog.origin,hds_hwd)["active hunters","grow"] / length(hds_hwd$hhclass[hds_hwd$hhclass=="active hunters"]))*100, 0 )
round( (xtabs(~hhclass+dog.origin,hds_hwd)["active hunters","buy"] / length(hds_hwd$hhclass[hds_hwd$hhclass=="active hunters"]))*100, 0 )
# TEXT: active hunters prefer to raise rather than buy (41% to 29%, respectively)

# occasional hunters
round( (xtabs(~hhclass+dog.origin,hds_hwd)["occasional hunters","grow"] / length(hds_hwd$hhclass[hds_hwd$hhclass=="occasional hunters"]))*100, 0 )
round( (xtabs(~hhclass+dog.origin,hds_hwd)["occasional hunters","buy"] / length(hds_hwd$hhclass[hds_hwd$hhclass=="occasional hunters"]))*100, 0 )
# TEXT: occasional hunters are more evenly divided, raise 29% to buy 33%

# past hunters
round( (xtabs(~hhclass+dog.origin,hds_hwd)["past hunters","grow"] / length(hds_hwd$hhclass[hds_hwd$hhclass=="past hunters"]))*100, 0 )
round( (xtabs(~hhclass+dog.origin,hds_hwd)["past hunters","buy"] / length(hds_hwd$hhclass[hds_hwd$hhclass=="past hunters"]))*100, 0 )

# non-hunters
round( (xtabs(~hhclass+dog.origin,hds_hwd)["non-hunters","grow"] / length(hds_hwd$hhclass[hds_hwd$hhclass=="non-hunters"]))*100, 0 )
round( (xtabs(~hhclass+dog.origin,hds_hwd)["non-hunters","buy"] / length(hds_hwd$hhclass[hds_hwd$hhclass=="non-hunters"]))*100, 0 )

# TEXT: comparable figures for past hunters (61% to 11%) and for non-hunters (35% to
#		6%) show a decided preference for raising their own pups rather than buying


# Function - dog types #######
# Number of HHs that did not respond to this question
aggregate(is.na(hds_hwd$dog.type),by=list(hds_hwd$hhclass),FUN="sum")
hds_hwd %>% group_by(hhclass,dog.type) %>% tally() # mind NAs: 1-AH, 10-NH, 12-OH, 11-PH

# NUMBER OF HHs WITH CATEGORIZED (MALE+FEMALE) DOGS
hhadty <- hds_hwd %>% filter((num.dog.male+num.dog.female)!=0) %>%
  filter(dog.type!="NONE", dog.type!="DK", !is.na(dog.type)) %>%
  select("hh.key","hhclass","dog.type","num.dog.female","num.dog.male","X2011.pup.num")
hhadty <- hhadty[order(hhadty$hhclass),]; hhadty
# within these counts we can find the details on which households had adult dogs that
# they did categorize as "guard" | "hunter" | "guard, hunter"
hhadty %>% group_by(hhclass,dog.type) %>%
  summarise(nHH=n(),adult_M=sum(num.dog.male,na.rm=TRUE),
            adult_F=sum(num.dog.female,na.rm=TRUE),pups=sum(X2011.pup.num,na.rm=TRUE))
# NOTE: I manually added one HH case 14+1=15 to 'hunter' dog.type in 'active hunters' 
#       corresponding to HH 38, which pups are counted despite not having adult dogs

# NUMBER OF HHs WITH UNCATEGORIZED (MALE+FEMALE) DOGS
hhadna <- hds_hwd %>% filter((num.dog.male+num.dog.female)!=0) %>%
  filter(dog.type=="NONE" | dog.type=="DK" | is.na(dog.type)) %>%
  select("hh.key","hhclass","dog.type","num.dog.female","num.dog.male","X2011.pup.num")
hhadna <- hhadna[order(hhadna$hhclass),]; hhadna
# within these counts we can find the details on which households had adult dogs that
# they did not categorize as guard or hunter types
hhadna %>% group_by(hhclass,dog.type) %>%
  summarise(nHH=n(),adult_M=sum(num.dog.male,na.rm=TRUE),
            adult_F=sum(num.dog.female,na.rm=TRUE),pups=sum(X2011.pup.num,na.rm=TRUE))

# counts of adult dogs and pups by household and dog function (type)
hds_hwd[which((hds_hwd$num.dog.male+hds_hwd$num.dog.female)!=0),
        c("hh.key","hhclass","dog.type","num.dog.female","num.dog.male","X2011.pup.num")]
# HHs with adult dogs
hds_hwd[which((hds_hwd$num.dog.male+hds_hwd$num.dog.female)==0),
        c("hh.key","hhclass","dog.type","num.dog.female","num.dog.male","X2011.pup.num")]
# HHs without adult dogs


# Cost - number of tortillas, Y|N medicine and vaccine #######
# FEEDING (Daily Tortilla Ration)
# Check "Dogs_DER_and_Maize.xlsx" for estimates on dogs' Daily Energetic Requirements

# fed with minimum tortilla
hds_hwd$feed.tortillas.min[hds_hwd$feed.tortillas.min=="DK"] <- NA
hds_hwd$feed.tortillas.min <- as.numeric(hds_hwd$feed.tortillas.min)

# fed with maximum tortilla
hds_hwd$feed.tortillas.max[hds_hwd$feed.tortillas.max=="DK"] <- NA
hds_hwd$feed.tortillas.max <- as.numeric(hds_hwd$feed.tortillas.max)

# global tortilla average
mean(aggregate( (hds_hwd$feed.tortillas.min+hds_hwd$feed.tortillas.max)/2, by=list(hds_hwd$hhclass), FUN="mean", na.rm=TRUE )$x)

# DAILY TORTILLA RATION per household type (mean and sd)
hds_hwd %>% group_by(hhclass) %>%
  summarise(nHH=length(which(!is.na(feed.tortillas.min+feed.tortillas.max))),
            wah_mean=round(mean((feed.tortillas.min+feed.tortillas.max)/2,na.rm=TRUE),1),
            wah_sd=round(sd((feed.tortillas.min+feed.tortillas.max)/2,na.rm=TRUE),1))
# TEXT: 15+9+17+13=54 HHs responding

# Number of HHs that did not respond to this question
aggregate(is.na(hds_hwd$feed.tortillas.min),by=list(hds_hwd$hhclass),FUN="sum")
aggregate(is.na(hds_hwd$feed.tortillas.max),by=list(hds_hwd$hhclass),FUN="sum")
# TEXT: 2+8+7+5=22 HHs not responding


# GIVE MEDICINE Y/N (MEDICAL CARE)
t(xtabs(~hhclass+give.medicine,hds_hwd))
hds_hwd %>% group_by(hhclass) %>%
  summarise(nHH=length(which(!is.na(give.medicine))), adult_M=sum(num.dog.male,na.rm=TRUE), adult_F=sum(num.dog.female,na.rm=TRUE),
            medicine_N=length(which(give.medicine=="N")), medicine_Y=length(which(give.medicine=="Y")))

hds_hwd %>% filter(hhclass=="past hunters",give.medicine=="Y") %>%
  select(hh.key,num.dog.male,num.dog.female,X2011.puppies,X2011.pup.num,give.medicine)
# MIND: HH 56 reported to 'give.medicine' when they did not have adult dogs or puppies
# ACTION: keep HH 56 within this count in Table 2

# Percents of household types giving medicine
(xtabs(~hhclass+give.medicine,hds_hwd)["active hunters","Y"]*100)/sum(xtabs(~hhclass+give.medicine,hds_hwd)["active hunters",])
# 75% of active hunters
(xtabs(~hhclass+give.medicine,hds_hwd)["occasional hunters","Y"]*100)/sum(xtabs(~hhclass+give.medicine,hds_hwd)["occasional hunters",])
# 17% of occasional hunters
(xtabs(~hhclass+give.medicine,hds_hwd)["past hunters","Y"]*100)/sum(xtabs(~hhclass+give.medicine,hds_hwd)["past hunters",])
(xtabs(~hhclass+give.medicine,hds_hwd[hds_hwd$hh.key!=56,])["past hunters","Y"]*100)/sum(xtabs(~hhclass+give.medicine,hds_hwd[hds_hwd$hh.key!=56,])["past hunters",])
# 29% of past hunters --- 23% of past hunters [after omiting HH 56, see mind note above]
(xtabs(~hhclass+give.medicine,hds_hwd)["non-hunters","Y"]*100)/sum(xtabs(~hhclass+give.medicine,hds_hwd)["non-hunters",])
# 0% of non-hunters

#   Totals
sum(xtabs(~hhclass+give.medicine,hds_hwd)[,"Y"])
sum(xtabs(~hhclass+give.medicine,hds_hwd)[,"N"])
# Y/N == 19/38 [-HH 56: Y/N == 18/38]

hds_hwd %>% group_by(hhclass,give.medicine) %>% tally() # mind NAs: 1-AH, 8-NH, 6-OH, 4-PH
# Number of HHs that did not respond to this question
aggregate(is.na(hds_hwd$give.medicine),by=list(hds_hwd$hhclass),FUN="sum")


# GIVE VACCINE Y/N
t(xtabs(~hhclass+give.vaccine,hds_hwd))
hds_hwd %>% group_by(hhclass) %>%
  summarise(nHH=length(which(!is.na(give.vaccine))), adult_M=sum(num.dog.male,na.rm=TRUE), adult_F=sum(num.dog.female,na.rm=TRUE),
            vaccine_N=length(which(give.vaccine=="N")), vaccine_Y=length(which(give.vaccine=="Y")))

hds_hwd %>% filter(hhclass=="past hunters",give.vaccine=="Y") %>%
  select(hh.key,num.dog.male,num.dog.female,X2011.puppies,X2011.pup.num,give.vaccine)
# MIND: HH 56 reported to 'give.vaccine' when they did not have adult dogs or puppies
# ACTION: keep HH 56 within this count in Table 2

# Percents of household types giving vaccine
(xtabs(~hhclass+give.vaccine,hds_hwd)["active hunters","Y"]*100)/ sum(xtabs(~hhclass+give.vaccine,hds_hwd)["active hunters",])
# 71% of active hunters
(xtabs(~hhclass+give.vaccine,hds_hwd)["occasional hunters","Y"]*100)/sum(xtabs(~hhclass+give.vaccine,hds_hwd)["occasional hunters",])
# 33% of occasional hunters
(xtabs(~hhclass+give.vaccine,hds_hwd)["past hunters","Y"]*100)/sum(xtabs(~hhclass+give.vaccine,hds_hwd)["past hunters",])
(xtabs(~hhclass+give.vaccine,hds_hwd[hds_hwd$hh.key!=56,])["past hunters","Y"]*100)/sum(xtabs(~hhclass+give.vaccine,hds_hwd[hds_hwd$hh.key!=56,])["past hunters",])
# 64% of past hunters --- 61% of past hunters [after omiting HH 56, see mind note above]
(xtabs(~hhclass+give.vaccine,hds_hwd)["non-hunters","Y"]*100)/sum(xtabs(~hhclass+give.vaccine,hds_hwd)["non-hunters",])
# 67% of non-hunters

#   Totals
sum(xtabs(~hhclass+give.vaccine,hds_hwd)[,"Y"])
sum(xtabs(~hhclass+give.vaccine,hds_hwd)[,"N"])
#   Y/N == 31/24 [-HH 56: Y/N == 30/24]

hds_hwd %>% group_by(hhclass,give.vaccine) %>% tally() # mind NAs: 3-AH, 8-NH, 6-OH, 4-PH
# Number of HHs that did not respond to this question
aggregate(is.na(hds_hwd$give.vaccine),by=list(hds_hwd$hhclass),FUN="sum")

# DK and rabies responses
table(hds_hwd$vacc.type) # 30 responding rabies vaccine


# Adults - dogs per household type #######
# abscence/presence of adult dogs reported by household type
hds_hwd %>% group_by(hhclass) %>%
  summarise(ad_present=sum((num.dog.male+num.dog.female)!=0,na.rm=TRUE), 
            ad_absent=sum((num.dog.male+num.dog.female)==0,na.rm=TRUE))
# NOTE: I manually added 1 adult-dogs-absent case to non-hunters 7+1=8 to Table 2 reported
hds_hwd %>% group_by(hhclass) %>% filter(is.na(num.dog.male+num.dog.female)) %>% summarise(n=n(),hh.key)
# MIND: the latter addition corresponds to HH 47, where 47A did not like dogs
#       nor kept any with them as LPC and EChevez can remember, their
#       adult dogs record must be 0 and not <NA>

# sex distribution of adult dogs reported by household type
hds_hwd %>% 
  group_by(hhclass) %>%
  filter((num.dog.male+num.dog.female)!=0) %>%
  summarise(nHH=n(),
            dogM=sum(num.dog.male,na.rm=TRUE),
            dogF=sum(num.dog.female,na.rm=TRUE),
            dogMF=sum(num.dog.male+num.dog.female,na.rm=TRUE),
            pups=sum((X2011.pup.num),na.rm=TRUE))


# Litters - pups per household type #######
# percent of households with pups
round( (length(which(hds_hwd$X2011.pup.num[hds_hwd$hhclass=="active hunters"]>0)) / length(which(hds_hwd$hhclass=="active hunters"))) * 100, 0)
round( (length(which(hds_hwd$X2011.pup.num[hds_hwd$hhclass=="occasional hunters"]>0)) / length(which(hds_hwd$hhclass=="occasional hunters"))) * 100, 0)
round( (length(which(hds_hwd$X2011.pup.num[hds_hwd$hhclass=="past hunters"]>0)) / length(which(hds_hwd$hhclass=="past hunters"))) * 100, 0)
round( (length(which(hds_hwd$X2011.pup.num[hds_hwd$hhclass=="non-hunters"]>0)) / length(which(hds_hwd$hhclass=="non-hunters"))) * 100, 0)

# number of births: 147 puppies were born/owned (b/o)
sum(hds_hwd$X2011.pup.num,na.rm=TRUE)

# pups: present|absent (Y|N), born/owned, died, surviving
hds_hwd %>%
  group_by(hhclass,X2011.puppies) %>% 
  summarise(nHH=n(),
            fnum=sum(num.dog.female,na.rm=TRUE),
            mnum=sum(num.dog.male,na.rm=TRUE),
            pnumb=sum(X2011.pup.num,na.rm=TRUE),
            pdied=sum((X2011.pup.num-X2011.pup.surv),na.rm=TRUE),
            psurv=sum(X2011.pup.surv,na.rm=TRUE),
            pavg=mean(X2011.pup.num,na.rm=TRUE),
            pstd=sd(X2011.pup.num,na.rm=TRUE))

# deceased pups by litter and household type
t(table(hds_hwd$X2011.pup.num-hds_hwd$X2011.pup.surv,hds_hwd$hhclass))
# deceased pups by litter and household type, omitting HH 38
t(table(hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38]-hds_hwd$X2011.pup.surv[hds_hwd$hh.key!=38],hds_hwd$hhclass[hds_hwd$hh.key!=38]))

# abscence/presence of litters reported per household type
sum(table(hds_hwd$X2011.puppies,hds_hwd$hhclass)["N",]) # 40 HH
sum(table(hds_hwd$X2011.puppies,hds_hwd$hhclass)["Y",]) # 34 HH
length(which(hds_hwd$X2011.puppies=="N" | hds_hwd$X2011.puppies=="Y")) +
  length(which(is.na(hds_hwd$X2011.puppies))) # 40 HH + 34 HH + 2 is.na(HH) = 76 HH

# number of HH without/with puppies != number of households with dogs
length(which((hds_hwd$num.dog.female+hds_hwd$num.dog.male)==0)) # 20 HH
length(which((hds_hwd$num.dog.female+hds_hwd$num.dog.male)!=0)) # 55 HH
length(which(is.na(hds_hwd$num.dog.female+hds_hwd$num.dog.male))) # 1 HH
# NOTE: 55 HHs had adult dogs but only 34 HHs had litters
#       40 HH had no litters but could have (or have not) adult dogs


# Litter sizes per dog type #######
lttsze <- hds_hwd %>%
  group_by(dog.type) %>%
  select(hh.key, dog.type, num.dog.female, X2011.puppies, X2011.pup.num, X2011.pup.surv)
# fixing labels
unique(lttsze$dog.type)
lttsze$dog.type[lttsze$dog.type=="hunter, guard"] <- "hunter"
lttsze$dog.type[lttsze$dog.type=="guard" | lttsze$dog.type=="DK" |
                  lttsze$dog.type=="NONE" | is.na(lttsze$dog.type)] <- "guard-n-uncat"
# calcuating average(sd) litter sizes
lttsze %>%
  group_by(dog.type,X2011.puppies) %>% 
  summarise(nHH=n(),
            fnum=sum(num.dog.female,na.rm=TRUE),
            pnumb=sum(X2011.pup.num,na.rm=TRUE),
            pdied=sum((X2011.pup.num-X2011.pup.surv),na.rm=TRUE),
            psurv=sum(X2011.pup.surv,na.rm=TRUE),
            pup_female_ratio=sum(X2011.pup.num,na.rm=TRUE)/sum(num.dog.female,na.rm=TRUE),
            favg=mean(num.dog.female,na.rm=TRUE),
            fstd=sd(num.dog.female,na.rm=TRUE),
            pavg=mean(X2011.pup.num,na.rm=TRUE),
            pstd=sd(X2011.pup.num,na.rm=TRUE))
# TEXT: we report the pup_female_ratio for guard-n-uncat (3.2) and hunter females (2.8)
# NOTE: case guard-n-uncat with a female and is.na(X2011.pup.num) is HH 48 an incomplete HDS
#       lttsze %>% filter(dog.type=="guard-n-uncat", num.dog.female==1, is.na(X2011.puppies))


# Other costs (BZD): antibiotics injection, antiparasite pill, rabies vaccine #######

# INJECTION
xtabs(~hhclass+cost.inje,hds_hwd)
# HHs giving a cost...
sum(xtabs(~hhclass+cost.inje,hds_hwd)[,1])
# TEXT: 2 households reported a cost equal to zero (free)
sum(xtabs(~hhclass+cost.inje,hds_hwd)[,2:5])
# TEXT: 10 households reported with a cost greater than zero
# HHs answering DK or N...
sum(xtabs(~hhclass+cost.inje,hds_hwd)[,6:7])
# TEXT: 11 households reported DK or N
sum(xtabs(~hhclass+cost.inje,hds_hwd)[,"DK"])
# TEXT: 10 households reported DK
sum(xtabs(~hhclass+cost.inje,hds_hwd)[,"N"])
# TEXT: 1 household reported N
inje_DKN <- which(hds_hwd$cost.inje=="DK"|hds_hwd$cost.inje=="N")
cinje <- as.numeric(hds_hwd$cost.inje[-inje_DKN])
range(cinje[cinje>0],na.rm=TRUE); mean(cinje[cinje>0],na.rm=TRUE)
# DOG TYPES RECEIVING INJECTION CARE ~ HOUSEHOLDS REPORTING COST
xtabs(~dog.type+cost.inje,hds_hwd)
sum(xtabs(~dog.type+cost.inje,hds_hwd)[c("DK","NONE"),])
# 3 dogs unclassified
sum(xtabs(~dog.type+cost.inje,hds_hwd)["guard",1:4])
# 1 guard dog
sum(xtabs(~dog.type+cost.inje,hds_hwd)["guard",5:6])
# 1 guard dog with unknown cost of injection
sum(xtabs(~dog.type+cost.inje,hds_hwd)[c("hunter","hunter, guard"),1:4])
# 9 hunter dogs
sum(xtabs(~dog.type+cost.inje,hds_hwd)[c("hunter","hunter, guard"),5:6])
# 5 hunter dogs with unknown cost of injection

# PILL
xtabs(~hhclass+cost.pill,hds_hwd)
# HHs giving a cost...
hds_hwd[which(hds_hwd$cost.pill==0),"hh.key"]
# TEXT: 0 households reported a cost equal to zero (free)
sum(xtabs(~hhclass+cost.pill,hds_hwd)[,1:2])
# TEXT: 4 households reported with a cost greater than zero
# HHs answering DK...
sum(xtabs(~hhclass+cost.pill,hds_hwd)[,3])
# TEXT: 9 households reported DK
pill_DK <- which(hds_hwd$cost.pill=="DK")
cpill <- as.numeric(hds_hwd$cost.pill[-pill_DK])
range(cpill[cpill>0],na.rm=TRUE); mean(cpill[cpill>0],na.rm=TRUE)
# DOG TYPES RECEIVING PILL CARE ~ HOUSEHOLDS REPORTING COST
xtabs(~dog.type+cost.pill,hds_hwd)
sum(xtabs(~dog.type+cost.pill,hds_hwd)[c("DK","NONE"),])
# 2 dogs unclassified
sum(xtabs(~dog.type+cost.pill,hds_hwd)["guard",1])
# 1 guard dog
sum(xtabs(~dog.type+cost.pill,hds_hwd)["guard",2])
# 1 guard dog with unknown cost of pill
sum(xtabs(~dog.type+cost.pill,hds_hwd)[c("hunter","hunter, guard"),1])
# 2 hunter dogs
sum(xtabs(~dog.type+cost.pill,hds_hwd)[c("hunter","hunter, guard"),2])
# 5 hunter dogs with unknown cost of pill

# VACCINE
xtabs(~hhclass+cost.vacc,hds_hwd)
# HHs giving a cost...
sum(xtabs(~hhclass+cost.vacc,hds_hwd)[,1])
# TEXT: 21 households responded a cost equal to zero (free)
sum(xtabs(~hhclass+cost.vacc,hds_hwd)[,2])
# TEXT: 1 households responded with a cost greater than zero
# HHs answering DK...
sum(xtabs(~hhclass+cost.vacc,hds_hwd)[,3])
# TEXT: 4 households responded DK
vacc_DK <- which(hds_hwd$cost.vacc=="DK")
cvacc <- as.numeric(hds_hwd$cost.vacc[-vacc_DK])
range(cvacc,na.rm=TRUE); mean(cvacc,na.rm=TRUE)
# DOG TYPES RECEIVING VACCINE CARE ~ HOUSEHOLDS REPORTING COST
xtabs(~dog.type+cost.vacc,hds_hwd)
sum(xtabs(~dog.type+cost.vacc,hds_hwd)[c("DK","NONE"),])
# 3 dogs unclassified
sum(xtabs(~dog.type+cost.vacc,hds_hwd)["guard",1])
# 3 guard dogs receiving free rabies vaccine
sum(xtabs(~dog.type+cost.vacc,hds_hwd)["guard",2])
# 0 guard dogs receiving paid rabies vaccine
sum(xtabs(~dog.type+cost.vacc,hds_hwd)["guard",3])
# 1 guard dog receiving an unknown cost of vaccine
sum(xtabs(~dog.type+cost.vacc,hds_hwd)[c("hunter","hunter, guard"),1])
# 10 hunter dogs receiving free rabies vaccine
sum(xtabs(~dog.type+cost.vacc,hds_hwd)[c("hunter","hunter, guard"),2])
# 1 hunter dog receiving paid rabies vaccine
sum(xtabs(~dog.type+cost.vacc,hds_hwd)[c("hunter","hunter, guard"),3])
# 1 hunter dogs with unknown cost of vaccine


# TRAINING #######
# DOGS' PREY PREFERENCES ARE LEARN OR TAUGHT?
hds_hwd %>% group_by(dogs.prey.pref.is) %>% tally()
# HHs report: 24 Learn, 7 Taught, 1 Learn & Taught, 44 <NA>

# TIME TO TRAIN A HUNTING DOG
xtabs(~hhclass+time.to.train,hds_hwd)
dtrain_DK <- which(hds_hwd$time.to.train =="DK") # [three reported DK]
hds_hwd[dtrain_DK,"hh.key"] # households 41, 44, 59 reporting DK
hds_hwd$time.to.train[dtrain_DK] <- NA # converting DK to NA values
hds_hwd$time.to.train <- as.numeric(hds_hwd$time.to.train)
( (length(which(!is.na(hds_hwd$time.to.train)))-length(dtrain_DK)) / length(unique(hds_hwd$hh.key)) )*100
# 38% HHs report a time to train estimate
round(mean(hds_hwd$time.to.train, na.rm=TRUE),0)
round(sd(hds_hwd$time.to.train, na.rm=TRUE),0)
# TEXT: avg 82 sd 101 days to train a [willing] dog to hunt

hds_hwd %>% group_by(hhclass) %>% filter(!is.na(time.to.train)) %>% summarise(train_mean=mean(time.to.train),train_sd=sd(time.to.train),nHH=n())
# Time to train summary (mean, sd) per household type

# Linerar regression time.to.train ~ hhclass
summary( lm( hds_hwd$time.to.train[!is.na(hds_hwd$time.to.train)] ~ hds_hwd$hhclass[!is.na(hds_hwd$time.to.train)],
             hds_hwd[which(!is.na(hds_hwd$time.to.train)),] ) )
# NOTE: no significant differences in reported time to train dogs by household types


# HUNTING DOG TYPES #######
# SANTA CRUZ --- HOFLING (2011) --- MEANING
# chabe --- ajye'-b'ej (p. 114), ajch'a'-bej (p. 540) --- guia
# alca  --- aj'alka' (p. 119) --- corredor
# alca  --- aj'alka'-pach (p. 115) --- perseguidor 
# OTHER PEK TERMS	
# ajtzakil-pek' (p. 562) --- perro cazador
# ixch'upulpel' (p. 562) --- perra

# Number of households reporting to have n chabe dogs
xtabs(~hhclass+chabe.type,hds_hwd)

hds_hwd[which(hds_hwd$hhclass=="non-hunters"&hds_hwd$chabe.type=="1"),"hh.key"]
# "non-hunter" reporting 1 chabe dog --- HH 24
hds_hwd[which(hds_hwd$hhclass=="past hunters"&hds_hwd$chabe.type=="0"),"hh.key"]
# "past hunter" reporting 0 chabe dog --- HH 14

hds_hwd %>% group_by(hhclass) %>%
  filter(chabe.type!="0",!is.na(chabe.type),chabe.type!="DK") %>% summarise(HH_with_chabe=n())

(sum(xtabs(~hhclass+chabe.type,hds_hwd)["active hunters",2:4]) / table(hds_hwd$hhclass)["active hunters"]) * 100
# 13 HHs with chabe dogs, 77% of active hunters HHs
(sum(xtabs(~hhclass+chabe.type,hds_hwd)["occasional hunters",2:4]) / table(hds_hwd$hhclass)["occasional hunters"]) * 100
# 6 HHs with chabe dogs, 25% of occasional hunters HHs
(sum(xtabs(~hhclass+chabe.type,hds_hwd)["past hunters",2:4]) / table(hds_hwd$hhclass)["past hunters"]) * 100
# 6 HHs with chabe dogs, 33% of past hunters HHs
(sum(xtabs(~hhclass+chabe.type,hds_hwd)["non-hunters",2:4]) / table(hds_hwd$hhclass)["non-hunters"]) * 100
# 1 HH with a chabe dog, 6% of non-hunters HHs

# Male and female chabe dogs
chabem_DK <- which(hds_hwd$chabe.male=="DK")
chabem <- as.numeric(hds_hwd$chabe.male[-chabem_DK])
sum(chabem,na.rm=TRUE) # 22 chabe male
chabef_DK <- which(hds_hwd$chabe.female=="DK")
chabef <- as.numeric(hds_hwd$chabe.female[-chabef_DK])
sum(chabef,na.rm=TRUE) # 17 chabe female	
# distributed in households
xtabs(~hhclass+chabe.male,hds_hwd)
# male 8+2DK active hunters; 2 occasional hunters; 5+1DK past hunters # 18 dogs
xtabs(~hhclass+chabe.female,hds_hwd)
# female 8+2DK active hunters, 4 occasional hunters, 2+1DK past hunters # 12 dogs

chabe <- hds_hwd %>%
  #filter(!is.na(chabe.type)) %>%
  filter(chabe.male!="DK",chabe.female!="DK") %>%
  select(c("hhclass","chabe.male","chabe.female"))
chabe$chabe.male <- as.numeric(chabe$chabe.male)
chabe$chabe.female <- as.numeric(chabe$chabe.female)
# NOTE: if filter(!is.na(chabe.type)) is run, one chfe is excluded and
#       chabef[!is.na(chabef)] != chabe$chabe.female when it should not
#       due to different vector lengths (Warning message)
chabef[!is.na(chabef)] == chabe$chabe.female

chabe %>% group_by(hhclass) %>% 
  summarise(nHH=n(), chma=sum(chabe.male), chfe=sum(chabe.female))
# Informants reporting:
# active hunters (n=12) hold 10+9 (M+F) chabe dogs
# occasional hunters (n=5) hold 5+6 (M+F) chabe dogs
# past hunters (n=6) hold 7+2 (M+F) chabe dogs


# Number of households reporting to have n alca dogs
xtabs(~hhclass+alca.type,hds_hwd)

hds_hwd[which(hds_hwd$hhclass=="active hunters"&hds_hwd$alca.type=="DK"),"hh.key"]
# "active hunter" reporting 1 chabe dog --- HH 72
hds_hwd[which(hds_hwd$hhclass=="past hunters"&hds_hwd$alca.type=="0"),"hh.key"]
# "past hunter" reporting 0 chabe dog --- HHs 41 and 44

hds_hwd %>% group_by(hhclass) %>%
  filter(alca.type!="0",!is.na(alca.type),alca.type!="DK") %>% summarise(HH_with_alca=n())

(sum(xtabs(~hhclass+alca.type,hds_hwd)["active hunters",2:7]) / table(hds_hwd$hhclass)["active hunters"]) * 100
# 11 HHs with alca dogs, 65% of active hunters HHs
(sum(xtabs(~hhclass+alca.type,hds_hwd)["occasional hunters",2:7]) / table(hds_hwd$hhclass)["occasional hunters"]) * 100
# 3 HHs with alca dogs, 13% of occasional hunters HHs
(sum(xtabs(~hhclass+alca.type,hds_hwd)["past hunters",2:7]) / table(hds_hwd$hhclass)["past hunters"]) * 100
# 5 HHs with alca dogs, 28% of past hunters HHs
(sum(xtabs(~hhclass+alca.type,hds_hwd)["non-hunters",2:7]) / table(hds_hwd$hhclass)["non-hunters"]) * 100
# 0 HHs with alca dogs, 0% of non-hunters HHs

hds_hwd[which(hds_hwd$alca.type=="0"),"hh.key"]
# List of HHs reporting 0 alca dogs --- 24, 32, 38, 41, 44, 62

alcam_DK <- which(hds_hwd$alca.male=="DK")
alcam <- as.numeric(hds_hwd$alca.male[-alcam_DK])
sum(alcam,na.rm=TRUE) # 27 alca male
alcaf_DK <- which(hds_hwd$alca.female=="DK")
alcaf <- as.numeric(hds_hwd$alca.female[-alcaf_DK])
sum(alcaf,na.rm=TRUE) # 16 alca female	
# distributed in households
xtabs(~hhclass+alca.male,hds_hwd)
# male 8+2DK active hunters; 3 occasional hunters; 4+1DK past hunters # 18 dogs
xtabs(~hhclass+alca.female,hds_hwd)
# female 5+2DK active hunters, 2 occasional hunters, 2+1DK past hunters # 12 dogs

alca <- hds_hwd %>% 
  #filter(!is.na(alca.type)) %>%
  filter(alca.male!="DK",alca.female!="DK") %>%
  select(c("hhclass","alca.male","alca.female"))
alca$alca.male <- as.numeric(alca$alca.male)
alca$alca.female <- as.numeric(alca$alca.female)
# NOTE: if filter(!is.na(alca.type)) is run alcaf[!is.na(alcaf)] ends
#       with a != length than alca$alca.female (Warning message)
#       no count effect on alma or alfe is observed though
alcaf[!is.na(alcaf)] == alca$alca.female

alca %>% group_by(hhclass) %>% 
  summarise(nHH=n(), alma=sum(alca.male), alfe=sum(alca.female))
# Informants reporting:
# active hunters (n=12) hold 14+6 (M+F) alca dogs
# occasional hunters (n=5) hold 7+5 (M+F) alca dogs
# past hunters (n=6) hold 6+5 (M+F) alca dogs


# PREY PREFERENCES OF DOGS AND HUNTERS #######
# Prey prefrences match between chabe dog and hunter (rough look!)
cbind(as.character(hds_hwd$chabe.prey.pref),as.character(hds_hwd$hunter.prey.pref))

hds_hwd$why.chabespp
# No interviewee reported why their chabe dogs had such preference

# Hunter
table(hds_hwd$hunter.prey.pref, hds_hwd$hhclass)
table(hds_hwd$hunter.prey.pref, hds_hwd$dog.type)
# Chabe dog
table(hds_hwd$chabe.prey.pref, hds_hwd$hhclass)
table(hds_hwd$chabe.prey.pref, hds_hwd$dog.type)
# Dog
table(hds_hwd$dogs.prey.pref, hds_hwd$dog.type)
