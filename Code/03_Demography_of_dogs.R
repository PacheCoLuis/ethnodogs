# DEMOGRAPHY OF DOGS #######################

source("02_SC_desciptive_data.R")

# Number of dogs (F|M) Deceased dogs (Adults|Pups) Litters #######

# TOTAL NUMBER OF DOGS IN SANTA CRUZ BY 2012
sum(hds_hhw$num.dog.female,na.rm=TRUE)
sum(hds_hwd$num.dog.female,na.rm=TRUE)
# TEXT: 71 female dogs
sum(hds_hhw$num.dog.male,na.rm=TRUE)
sum(hds_hwd$num.dog.male,na.rm=TRUE)
# TEXT: 77 male dogs

# TOTAL DECEASED DOGS IN THE LAST SIX MONTHS
tdd <- sum(hds_hhw$num.dog.dead,na.rm=TRUE); tdd
# TEXT: 95 dog deaths in the last six months
# this result might include adult dogs and puppies

# ROUGH ESTIMATE ADULT DEATHS substract number of deceased puppies to total deceased dogs
tdp <- sum(hds_hwd$X2011.pup.num - hds_hwd$X2011.pup.surv, na.rm=TRUE)
tdp - tdd
# TEXT: ...95 dog deaths, of these 23 were adults...
#		though it may not be so straight forward, the counts were requested
#		for the last year (section VI) and the last six months (section IV)

# ANNUAL ADULT SURVIVAL RATE
# total adult dogs (tad)
tad <- sum(hds_hhw$num.dog.female,na.rm=TRUE) + sum(hds_hhw$num.dog.male,na.rm=TRUE)
# 23 adult dogs died in the previous six months
# would turn into 46 adult dogs die per year
# surviving adults vs adults at the beginning
# note we assume tad as the starting figure
round(((tad-46)*100)/tad,0)
# TEXT: adult dogs survival rate of 69%

# NUMBER OF DEATHS AND SURVIVALS (omitting HH 38, see within TABLE-2 for reconciliation details)
sum((hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38]-hds_hwd$X2011.pup.surv[hds_hwd$hh.key!=38]),na.rm=TRUE)
# TEXT: 114 puppies died
sum(hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38],na.rm=TRUE)-sum((hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38]-hds_hwd$X2011.pup.surv[hds_hwd$hh.key!=38]),na.rm=TRUE)
# TEXT: 29 puppies survived
round((29*100)/sum(hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38],na.rm=TRUE),0)
# TEXT: puppies survival rate of only 20%

hds_hwd %>%
  group_by(X2011.puppies) %>% 
  summarise(fnum=sum(num.dog.female,na.rm=TRUE),
            pnumb=sum(X2011.pup.num,na.rm=TRUE),
            pdied=sum((X2011.pup.num-X2011.pup.surv),na.rm=TRUE),
            psurv=sum(X2011.pup.surv,na.rm=TRUE),
            pavg=mean(X2011.pup.num,na.rm=TRUE),
            pstd=sd(X2011.pup.num,na.rm=TRUE))

# PERCENT OF HHs WITH LITTERS IN WHICH ALL PUPPIES DIED
(length(which(hds_hwd$X2011.pup.surv[hds_hwd$hh.key!=38]==0))*100) / length(which(hds_hwd$X2011.puppies[hds_hwd$hh.key!=38]=="Y"))
# TEXT: 58% of the households reporting that their litters had no surviving puppies

# CHECK FOR CONSISTENCY:
length(which(hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38]>0)) == length(which(hds_hwd$X2011.puppies[hds_hwd$hh.key!=38]=="Y"))


# Figure S1: Frequency distribution of the number of dogs #######

# Need to create two additional factor levesl 7 & 9 adjusting the general code example below
# object$variable <- factor(object$variable, levels = c(levels(object$variable), "level"))
# but data come from barplot(table), a start is shown below
#frqdis <- data.frame(table(hds_hhw$num.dog.female+hds_hhw$num.dog.male))
#plot(frqdis, xlab="Number of dogs", ylab="Number of households", las=1)

setEPS()
postscript("FS1_Frequency_Distribution_HH_Dogs.eps")
barplot(table(hds_hhw$num.dog.female+hds_hhw$num.dog.male), xlab="Number of dogs", ylab="Number of households", las=1)
dev.off()


# Table 1: Household distribution of dogs by sex and age #######

# SEX ___ [from HDS_IV_DogsGuns.R]
# HHs with FEMALE dogs only
idx_hhfdogs <- which(hds_hhw$num.dog.female!=0 & hds_hhw$num.dog.male==0)
hds_hhw[idx_hhfdogs,c("hh.key","num.dog.female","num.dog.male")]
sum(hds_hhw[idx_hhfdogs,"num.dog.female"])
dim(hds_hhw[idx_hhfdogs,c("hh.key","num.dog.female")])[1]
# TABLE-1: 24 female dogs in 14 HHs
sum(hds_hwd[idx_hhfdogs,c("hh.key","X2011.pup.num")][2],na.rm=TRUE)
# TABLE-1: 58 puppies were born or owned in those HHs female-adult-dogs only

# HHs with MALE dogs only
idx_hhmdogs <- which(hds_hhw$num.dog.male!=0 & hds_hhw$num.dog.female==0)
hds_hhw[idx_hhmdogs,c("hh.key","num.dog.female","num.dog.male")]
sum(hds_hhw[idx_hhmdogs,"num.dog.male"])
dim(hds_hhw[idx_hhmdogs,c("hh.key","num.dog.male")])[1]
# TABLE-1: 24 male dogs in 15 HHs
sum(hds_hwd[idx_hhmdogs,c("hh.key","X2011.pup.num")][2],na.rm=TRUE)
# TABLE-1: 17 puppies were born or owned in those HHs with male-adult-dogs only

# HHs with FEMALE and MALE dogs
idx_hhwdogs <- which(hds_hhw$num.dog.female!=0 & hds_hhw$num.dog.male!=0)
hds_hhw[idx_hhwdogs,c("hh.key","num.dog.female","num.dog.male")]
sum(hds_hhw[idx_hhwdogs,c("num.dog.female","num.dog.male")])
dim(hds_hhw[idx_hhwdogs,c("hh.key","num.dog.female","num.dog.male")])[1]
# TABLE-1: 100 dogs in 26 HHs
sum(hds_hhw[idx_hhwdogs,c("num.dog.female")])
# TABLE-1: 47 females
sum(hds_hhw[idx_hhwdogs,c("num.dog.male")])
# TABLE-1: 53 males
sum(hds_hwd[idx_hhwdogs,c("hh.key","X2011.pup.num")][2],na.rm=TRUE)
# TABLE-1: 68 puppies were born or owned in those HHs with both-sexes-adult-dogs

# CHECK FOR CONSISTENCY: 58+17+68+(4) == sum(hds_hwd$X2011.pup.num,na.rm=TRUE)
#                        the (4) comes from HH 38 which had puppies only (see 'pnad' below)
# Total number of houses with (female + male + both sexes) dogs
length(idx_hhfdogs)+length(idx_hhmdogs)+length(idx_hhwdogs)
( (length(idx_hhfdogs)+length(idx_hhmdogs)+length(idx_hhwdogs))*100 ) / length(hds_hhw$hh.key)
# TEXT: 55 (72.4%) out of 76 surveyed households had dogs in 2012

# AGE
# HHs with adult dogs but without puppies (adult dogs, no puppies)
adnp <- hds_hwd[which((hds_hwd$num.dog.female + hds_hwd$num.dog.male != 0) & (hds_hwd$X2011.puppies == "N")), c("hh.key", "num.dog.female", "num.dog.male", "X2011.puppies", "X2011.pup.surv", "X2011.pup.num")]
dim(adnp)[1]
# TABLE-1: 21 HHs with adult dogs reported not to have puppies in the last year|12mo
# SEE the sections "CHECK... or FIX... FOR CONSISTENCY" below to
# reveal and overcome the inconsistency in the total counts of dogs
sum(adnp$num.dog.female,na.rm=TRUE)+sum(adnp$num.dog.male,na.rm=TRUE)+sum(adnp$X2011.pup.surv,na.rm=TRUE)
# [female + male + live puppies] 21+38+0 == 59 [dogs]

# HHs with puppies but without adult dogs (puppies, no adult dogs)
pnad <- hds_hwd[which((hds_hwd$num.dog.female + hds_hwd$num.dog.male == 0) & (hds_hwd$X2011.puppies == "Y")), c("hh.key", "num.dog.female", "num.dog.male", "X2011.puppies", "X2011.pup.surv", "X2011.pup.num")]
dim(pnad)[1]
# TABLE-1: 1 HH reported to have puppies but not adult dogs in the last year|12mo
sum(pnad$num.dog.female,na.rm=TRUE)+sum(pnad$num.dog.male,na.rm=TRUE)+sum(pnad$X2011.pup.surv,na.rm=TRUE)
# [female + male + live puppies] 0+0+0 == 0 [dogs]

# HHs with adult dogs and with puppies (adult dogs and puppies)
adap <- hds_hwd[which((hds_hwd$num.dog.female + hds_hwd$num.dog.male != 0) & (hds_hwd$X2011.puppies == "Y")), c("hh.key", "num.dog.female", "num.dog.male", "X2011.puppies", "X2011.pup.surv", "X2011.pup.num")]
dim(adap)[1]
# TABLE-1: 33 HHs with adult dogs reported to have puppies in the last year|12mo
(dim(adap)[1]*100)/sum(length(idx_hhfdogs)+length(idx_hhmdogs)+length(idx_hhwdogs))
# TEXT: 60% of HHs (adult dogs and puppies) had a litter
sum(adap$num.dog.female,na.rm=TRUE)+sum(adap$num.dog.male,na.rm=TRUE)+sum(adap$X2011.pup.surv,na.rm=TRUE)
# [female + male + live puppies] 49 + 39 + 29 == 117 [dogs]

# CHECK FOR CONSISTENCY
#   list of HHs with adult dogs (female, male, or both sexes)
sort(c(hds_hhw[idx_hhfdogs,"hh.key"], hds_hhw[idx_hhmdogs,"hh.key"], hds_hhw[idx_hhwdogs,"hh.key"]))
#   list of HHs with dogs (adults only, puppies only, or both ages)
sort(c(adnp$hh.key,pnad$hh.key,adap$hh.key))
#   some HHs did report dogs in Section IV but not in Section VI, and viceversa
cbind( sort(c(hds_hhw[idx_hhfdogs,"hh.key"], hds_hhw[idx_hhmdogs,"hh.key"], hds_hhw[idx_hhwdogs,"hh.key"])), sort(c(adnp$hh.key, pnad$hh.key, adap$hh.key)) )
#   HH 48 is subsetted Section IV but not in Section VI
#   HH 38 is subsetted Section VI but not in Section IV
# NOTE: when inquiring about puppies
#		HH 48 is missed since data on Section VI is == NA
hds_hwd[which(hds_hwd$hh.key==48), c("hh.key", "num.dog.female", "num.dog.male", "X2011.puppies", "X2011.pup.surv", "X2011.pup.num")]
#		HH 38 is included since data on Section VI is != NA
hds_hwd[which(hds_hwd$hh.key==38), c("hh.key", "num.dog.female", "num.dog.male", "X2011.puppies", "X2011.pup.surv", "X2011.pup.num")]

# FIX FOR CONSISTENCY
# HHs with adult dogs but without puppies (adult dogs, no puppies)
# count as "N" the <NA> value for 'hds_hwd$X2011.puppies' in HH 48
adnp <- hds_hwd[which( (hds_hwd$num.dog.female + hds_hwd$num.dog.male != 0) & ((hds_hwd$X2011.puppies == "N")|is.na(hds_hwd$X2011.puppies)) ), c("hh.key", "num.dog.female", "num.dog.male", "X2011.puppies", "X2011.pup.surv", "X2011.pup.num")]
dim(adnp)[1]
# TABLE-1: 22 HHs with adult dogs reported not to have puppies in the last year|12mo
(dim(adnp)[1]*100)/sum(length(idx_hhfdogs)+length(idx_hhmdogs)+length(idx_hhwdogs))
# TEXT: 40% of HHs (adult dogs, no puppies) had not a litter
sum(adnp$num.dog.female,na.rm=TRUE)+sum(adnp$num.dog.male,na.rm=TRUE)+sum(adnp$X2011.pup.surv,na.rm=TRUE)
# [female + male + live puppies] 22+38+0 == 60 [dogs]

# OCCUPATIONAL
# HUNTING DOGS
length(which(hds_hwd$dog.type=="hunter" | hds_hwd$dog.type=="hunter, guard"))
sum( hds_hwd[which(hds_hwd$dog.type=="hunter" | hds_hwd$dog.type=="hunter, guard"), "num.dog.male"] ) + sum( hds_hwd[which(hds_hwd$dog.type=="hunter" | hds_hwd$dog.type=="hunter, guard"), "num.dog.female"] )
# TEXT: 23 HHs having (46 male + 40 female) 86 HUNTER dogs
sum(hds_hwd[which(hds_hwd$dog.type=="hunter" | hds_hwd$dog.type=="hunter, guard"),"X2011.pup.num"],na.rm=TRUE)
# TEXT: 67 pups are associated to such HUNTER dogs

# GUARD DOGS
length(which(hds_hwd$dog.type=="guard"))
sum(hds_hwd[which(hds_hwd$dog.type=="guard"),"num.dog.male"]) +
  sum(hds_hwd[which(hds_hwd$dog.type=="guard"),"num.dog.female"])
# TEXT: 13 HHs with (13 male + 15 female) 28 GUARD dogs
sum(hds_hwd[which(hds_hwd$dog.type=="guard"),"X2011.pup.num"],na.rm=TRUE)
# TEXT: 29 pups are associated to such GUARD dogs

# UNCATEGORIZED DOGS
uncateg <- which( hds_hwd$dog.type=="NONE" | hds_hwd$dog.type=="DK" | (hds_hwd$num.dog.female+hds_hwd$num.dog.male!=0)&(is.na(hds_hwd$dog.type)) )
length(hds_hwd[uncateg,"hh.key"])
sum(hds_hwd[uncateg,"num.dog.male"]) + sum(hds_hwd[uncateg,"num.dog.female"])
# TEXT: 19 HHs with (18 male + 16 female) 34 UNCATEGORIZED dogs
sum(hds_hwd[uncateg,"X2011.pup.num"],na.rm=TRUE)
# TEXT: 47 pups are associated to such UNCATEGORIZED dogs

# CHECK FOR CONSISTENCY:
67+29+47+(4) == sum(hds_hwd$X2011.pup.num,na.rm=TRUE)
# the (4) is from the "Puppies only" HH

# LITTER SIZE ___ (omitting HH 38)
length(subset(hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38],hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38]!=0))
# TEXT: 33 HHs
round(mean(subset(hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38],hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38]!=0)),1)
round(sd(subset(hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38],hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38]!=0)),1)
# TEXT: (mean +- sd) 4.3 +- 2.7
aggregate(hds_hwd$X2011.pup.num,list(hds_hwd$X2011.puppies),FUN="mean",na.rm=TRUE)
aggregate(hds_hwd$X2011.pup.num,list(hds_hwd$X2011.puppies),FUN="sd",na.rm=TRUE)

# PUPS THAT DIE PER LITTER ___ (omitting HH 38)
round(mean((hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38]-hds_hwd$X2011.pup.surv[hds_hwd$hh.key!=38]),na.rm=TRUE),1)
round(sd((hds_hwd$X2011.pup.num[hds_hwd$hh.key!=38]-hds_hwd$X2011.pup.surv[hds_hwd$hh.key!=38]),na.rm=TRUE),1)
# TEXT: (mean +- sd) 3.5 + 2.6
aggregate(hds_hwd$X2011.pup.num-hds_hwd$X2011.pup.surv,list(hds_hwd$X2011.puppies),FUN="mean",na.rm=TRUE)
aggregate(hds_hwd$X2011.pup.num-hds_hwd$X2011.pup.surv,list(hds_hwd$X2011.puppies),FUN="sd",na.rm=TRUE)

# SEE "Table 2" SECTION "DISTRIBUTION OF LITTERS (PUPS) PER HOUSEHOLD TYPE"
# for details on litters average(sd) sizes per 'dog.type' and 'hhclass'


# Life expectancy: Descriptive Stats #######

# ___ GENERIC DOG
dlen_DK <- which(hds_hwd$dog.lifexp.min=="DK")
hds_hwd[dlen_DK,"hh.key"] # households reporting DK
hds_hwd$dog.lifexp.min[dlen_DK] <- NA # converting DK to NA values
hds_hwd$dog.lifexp.min <- as.numeric(hds_hwd$dog.lifexp.min)
table(hds_hwd[which(hds_hwd$dog.lifexp.min > 12),"dog.lifexp.min"]) # 14 cases

dlex_DK <- which(hds_hwd$dog.lifexp.max=="DK")
hds_hwd[dlex_DK,"hh.key"] # households reporting DK
hds_hwd$dog.lifexp.max[dlex_DK] <- NA # converting DK to NA values
hds_hwd$dog.lifexp.max <- as.numeric(hds_hwd$dog.lifexp.max)
table(hds_hwd[which(hds_hwd$dog.lifexp.max > 12),"dog.lifexp.max"]) # 19 cases

# calculating the mean life expectancy from min and max values
hds_hwd$dlife <- (hds_hwd$dog.lifexp.min+hds_hwd$dog.lifexp.max)/2

# ___ GUARD DOG
dgrdn_DK <- which(hds_hwd$lifexp.guard.min=="DK")
hds_hwd[dgrdn_DK,"hh.key"] # households reporting DK
hds_hwd$lifexp.guard.min[dgrdn_DK] <- NA # converting DK to NA values
hds_hwd$lifexp.guard.min <- as.numeric(hds_hwd$lifexp.guard.min)
table(hds_hwd[which(hds_hwd$lifexp.guard.min > 12),"lifexp.guard.min"]) # 6 cases

dgrdx_DK <- which(hds_hwd$lifexp.guard.max=="DK")
hds_hwd[dgrdx_DK,"hh.key"] # households reporting DK
hds_hwd$lifexp.guard.max[dgrdx_DK] <- NA # converting DK to NA values
hds_hwd$lifexp.guard.max <- as.numeric(hds_hwd$lifexp.guard.max)
table(hds_hwd[which(hds_hwd$lifexp.guard.max > 12),"lifexp.guard.max"]) # 7 cases

# calculating the mean life expectancy from min and max values
hds_hwd$dlife.grd <- (hds_hwd$lifexp.guard.min+hds_hwd$lifexp.guard.max)/2

# ___ HUNTER DOG
dhntn_DK <- which(hds_hwd$lifexp.hunt.min=="DK")
hds_hwd[dhntn_DK,"hh.key"] # households reporting DK
hds_hwd$lifexp.hunt.min[dhntn_DK] <- NA # converting DK to NA values
hds_hwd$lifexp.hunt.min <- as.numeric(hds_hwd$lifexp.hunt.min)
table(hds_hwd[which(hds_hwd$lifexp.hunt.min > 12),"lifexp.hunt.min"]) # 3 cases

dhntx_DK <- which(hds_hwd$lifexp.hunt.max=="DK")
hds_hwd[dhntx_DK,"hh.key"] # households reporting DK
hds_hwd$lifexp.hunt.max[dhntx_DK] <- NA # converting DK to NA values
hds_hwd$lifexp.hunt.max <- as.numeric(hds_hwd$lifexp.hunt.max)
table(hds_hwd[which(hds_hwd$lifexp.hunt.max > 12),"lifexp.hunt.max"]) # 3 cases

# calculating the mean life expectancy from min and max values
hds_hwd$dlife.hnt <- (hds_hwd$lifexp.hunt.max+hds_hwd$lifexp.hunt.max)/2

# AVG SD FOR CASES >12-YEARS-OLD ONLY
# GUARD TYPE
  length(which(hds_hwd$dlife.grd>12))
  # 7 HHs reported guard dog to live >12y
  length(which(hds_hwd$dlife.grd<=12))
  # 23 HHs reported guard dog to live <=12y
  mean(hds_hwd$dlife.grd[hds_hwd$dlife.grd<=12],na.rm=TRUE)
  sd(hds_hwd$dlife.grd[hds_hwd$dlife.grd<=12],na.rm=TRUE)
  # 6.6 +- 2.3 y for guard dogs
# HUNTER TYPE
  length(which(hds_hwd$dlife.hnt>12))
  # 3 HHs reported hunter dog to live >12y
  length(which(hds_hwd$dlife.hnt<=12))
  # 44 HHs reported hunter dog to live <=12y
  mean(hds_hwd$dlife.hnt[hds_hwd$dlife.hnt<=12],na.rm=TRUE)
  sd(hds_hwd$dlife.hnt[hds_hwd$dlife.hnt<=12],na.rm=TRUE)
  # 3.8 +- 2.1 y for hunter dogs


# Figure 3: Life expectancy + t-Test #######

# SELECTING <=12y as a cut point, given that dogs rarely live
# up to 25y e.g. max(hds_hwd$lifexp.guard.max,na.rm=TRUE)

setEPS()
postscript("F3_Dog_Life_Expectancy_According_to_Type.eps")
dog_agest <- boxplot(hds_hwd$dlife.grd[hds_hwd$dlife.grd<=12], hds_hwd$dlife.hnt[hds_hwd$dlife.hnt<=12],plot=0)
boxplot(hds_hwd$dlife.grd[hds_hwd$dlife.grd<=12], hds_hwd$dlife.hnt[hds_hwd$dlife.hnt<=12], varwidth=TRUE, frame.plot=FALSE, las=1, xaxt="n", ylab="Age estimate (years)", xlab="Dog type")
axis(side = 1, at=c(1, 2), labels =
       c(paste("Guard\n n = ",dog_agest$n[1]),
         paste("Hunter\n n = ",dog_agest$n[2])),
     tick=FALSE)
dev.off()

#	t-test
dlg <- hds_hwd$dlife.grd[hds_hwd$dlife.grd<=12 & !is.na(hds_hwd$dlife.grd)]
dlh <- hds_hwd$dlife.hnt[hds_hwd$dlife.hnt<=12 & !is.na(hds_hwd$dlife.hnt)]
t.test(dlg,dlh)
# TEXT: t=4.86, df=43.5, p-value<0.001 [1.548e-05] 95% CI 1.7 - 4.0 Means dlg=6.6 dlh=3.8

# actual median values for dogs <12y
median(hds_hwd$dlife.grd[hds_hwd$dlife.grd<=12],na.rm=TRUE)/median(hds_hwd$dlife.hnt[hds_hwd$dlife.hnt<=12],na.rm=TRUE)
# TEXT: the median lifespan for hunting dogs fits twice in the median for guard dogs
mean(hds_hwd$dlife.grd[hds_hwd$dlife.grd<=12],na.rm=TRUE)/mean(hds_hwd$dlife.hnt[hds_hwd$dlife.hnt<=12],na.rm=TRUE)
# TEXT: the average lifespan for hunting dogs fits 1.7 in the average for guard dogs

# CHECK that the difference in means holds if all age estimates are included 
dog_ageall <- boxplot(hds_hwd$dlife.grd, hds_hwd$dlife.hnt,plot=0)
boxplot(hds_hwd$dlife.grd, hds_hwd$dlife.hnt, varwidth=TRUE, frame.plot=FALSE, las=1, xaxt="n", ylab="Age estimate (years)", xlab="Dog type")
axis(side = 1, at=c(1, 2), labels =
       c(paste("Guard\n n = ",dog_ageall$n[1]),
         paste("Hunter\n n = ",dog_ageall$n[2])),
     tick=FALSE)
# actual median values for dogs up to 25y
mean(hds_hwd$dlife.grd,na.rm=TRUE)/mean(hds_hwd$dlife.hnt,na.rm=TRUE)
# TEXT: the median for hunting dogs keeps fitting twice in the median for guard dogs
