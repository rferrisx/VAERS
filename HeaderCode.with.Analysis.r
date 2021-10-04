
library(data.table)
setDTthreads(0)
library(lattice)
library(lubridate)
# Needs unzipped VAERS library from:
# https://vaers.hhs.gov/data/datasets.html
# Below is full 2020 data and 2021 data through September

# File folder should contain like this:
# 2020VAERSDATA.csv
# 2020VAERSSYMPTOMS.csv
# 2020VAERSVAX.csv 

# merge routines:
# All 2020 data
setwd("D:\\Politics\\VAERS\\2020VAERSData.2020_07.19.2021")
Data_Vax <- merge(fread("2020VAERSDATA.csv"),fread("2020VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2020VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")[
order(VAERS_ID,DIED,L_THREAT)]
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...
Data_Vax_SYMP_2020 <- Data_Vax_SYMP[order(-VAERS_ID)]
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2020 <- mergeDVS

# through September 10 2021 data
setwd("D:\\Politics\\VAERS\\2021VAERSData.10.01.2021")
Data_Vax <- merge(fread("2021VAERSDATA.csv"),fread("2021VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2021VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")[
order(VAERS_ID,DIED,L_THREAT)]
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...
Data_Vax_SYMP_2021 <- Data_Vax_SYMP[order(-VAERS_ID)]
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2021 <- mergeDVS
mergeDVS <-rbind(mergeDVS2020,mergeDVS2021)[
order(VAERS_ID,DIED,L_THREAT)]
mergeDVS[,All_symptoms:= (cbind(paste0(SYMPTOM1," ",SYMPTOM2," ",SYMPTOM3," ",SYMPTOM4," ",SYMPTOM5)))]
fsum <- function(x) {base::sum(x,na.rm=TRUE)}


print("Reports,Deaths,Life Threats by Age")
# CAGE_YR
print("Non duplicated VAERSID where DIED == 'Y' irrespective of CAGE_YR")
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.N,]
print("Gross died CAGE_YR < 12")
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y"  & CAGE_YR < 12,.N]
print("Gross died CAGE_YR > 12")
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y"  & CAGE_YR >= 12,.N,]
print("Gross injuries between CAGE_YR >= 12 and <= 30")
mergeDVS[VAX_TYPE == "COVID19" & between(CAGE_YR,12,30),.N]
print("Gross deaths between CAGE_YR >= 12 and <= 30")
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y"  & between(CAGE_YR,12,30),.N]

#AGE_YRS
print("Non duplicated VAERSID here DIED == "Y" irrespective of AGE_YRS")
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.N,]
print("Gross died AGE_YRS < 12")
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y"  & AGE_YRS < 12,.N]
print("Gross died AGE_YRS > 12")
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y"  & AGE_YRS >= 12,.N,]
print("Gross injuries between AGE_YRS >= 12 and <= 30")
mergeDVS[VAX_TYPE == "COVID19" & between(AGE_YRS,12,30),.N]
print("Gross deaths between AGE_YRS >= 12 and <= 30")
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y"  & between(AGE_YRS,12,30),.N]

library(lubridate);
merge(
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y"  & between(AGE_YRS,12,30),],
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y"  & between(CAGE_YR,12,30),],by=c("VAERS_ID","SEX","AGE_YRS","VAX_DATE","DATEDIED"))[,
.(VAERS_ID,SEX,AGE_YRS,VAX_DATE,DATEDIED,NUMDAYS=mdy(DATEDIED) - mdy(VAX_DATE),PartialSymptomText=substr(SYMPTOM_TEXT.x,0,225))]

# Covid VAERS Reports
fsum <- function(x) {base::sum(x,na.rm=TRUE)}
dev.new()
Events <- mergeDVS[VAX_TYPE == "COVID19" & !is.na(AGE_YRS) & AGE_YRS >= 12,.N,.(AGE_YRS)][order(AGE_YRS)];
Events[,barplot(N,names.arg=AGE_YRS,col=rainbow(nrow(.SD)))]
Count <- fsum(Events$N)
mtext(paste0("All Covid19 VAERS Report where AGE_YRS exists and >= 12 years. Count=",Count),cex=1.15,side=3)

# Covid VAERS Reported Deaths (e.g. DIED == "Y")
fsum <- function(x) {base::sum(x,na.rm=TRUE)}
dev.new()
Deaths <- mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y" & !is.na(AGE_YRS) & AGE_YRS >= 12,.N,.(AGE_YRS)][order(AGE_YRS)];
Deaths[,barplot(N,names.arg=AGE_YRS,col=rainbow(nrow(.SD)))]
Count <- fsum(Deaths$N)
mtext(paste0("All Covid19 VAERS Reported Deaths (DIED == 'Y')where AGE_YRS exists and >= 12 years. Count=",Count),cex=1.15,side=3)
#


# Covid VAERS Reported Life Threatening Events (e.g. L_THREAT == "Y")
fsum <- function(x) {base::sum(x,na.rm=TRUE)}
dev.new()
L_THREAT <- mergeDVS[VAX_TYPE == "COVID19" & L_THREAT == "Y" & !is.na(AGE_YRS) & AGE_YRS >= 12,.N,.(AGE_YRS)][order(AGE_YRS)];
L_THREAT[,barplot(N,names.arg=AGE_YRS,col=rainbow(nrow(.SD)))]
Count <- fsum(L_THREAT$N)
mtext(paste0("All Covid19 VAERS Reported L_THREAT == 'Y' where AGE_YRS exists and >= 12 years. Count=",Count),cex=1.15,side=3)
#


# P <- rbind(Data_Vax_SYMP_2020,Data_Vax_SYMP_2021)
# Write out Covid deaths
# mergeDVS[,All_symptoms:= (cbind(paste0(SYMPTOM1," ",SYMPTOM2," ",SYMPTOM3," ",SYMPTOM4," ",SYMPTOM5)))]
fwrite(mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.(VAERS_ID,VAX_DATE,ONSET_DATE,RECVDATE,CAGE_YR,SEX,L_THREAT,DIED,All_symptoms,SYMPTOM_TEXT,LAB_DATA,HISTORY)],"CovidVAXDeaths.some.date.csv")
CovidVAXDIED <- mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.(VAERS_ID,VAX_DATE,ONSET_DATE,RECVDATE,CAGE_YR,SEX,L_THREAT,DIED,All_symptoms,SYMPTOM_TEXT,LAB_DATA,HISTORY)] 

setnames(merge(mergeDVS[DIED == "Y",.N,.(VAX_TYPE)],mergeDVS[DIED != "Y" ,.N,.(VAX_TYPE)],by="VAX_TYPE"),c("VAX_TYPE","DIED","Other.VAERS.LOG"))[order(-DIED)]
mergeDVS[DIED == "Y",.N,.(LAB_DATA,VAX_TYPE,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]

mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.(VAX_TYPE,VAX_MANU,LAB_DATA,VAERS_ID,AGE_YRS,CUR_ILL,VAX_DATE,ONSET_DATE,NUMDAYS,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.(VAX_TYPE,VAX_MANU,VAERS_ID,AGE_YRS,VAX_DATE,ONSET_DATE,NUMDAYS,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
# mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.(VAX_TYPE,VAX_MANU,VAERS_ID,AGE_YRS,VAX_DATE,ONSET_DATE,NUMDAYS,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][order(-VAX_DATE,VAERS_ID)][,as.data.frame(.SD)]


# Covid VAERS Reports
fsum <- function(x) {base::sum(x,na.rm=TRUE)}
dev.new()
Events <- mergeDVS[VAX_TYPE == "COVID19" & !is.na(CAGE_YR) & CAGE_YR >= 12,.N,.(CAGE_YR)][order(CAGE_YR)];
Events[,barplot(N,names.arg=CAGE_YR,col=rainbow(nrow(.SD)))]
VAERS_Event_Count <- fsum(Events$N)
mtext(paste0("All Covid19 VAERS Reports & CAGE_YR >= 12. VAERS_Event_Count=",VAERS_Event_Count),cex=1.5,side=3)
#

# Covid VAERS Reported Deaths (e.g. DIED == "Y")
fsum <- function(x) {base::sum(x,na.rm=TRUE)}
dev.new()
Deaths <- mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y" & !is.na(CAGE_YR) & CAGE_YR >= 12,.N,.(CAGE_YR)][order(CAGE_YR)];
Deaths[,barplot(N,names.arg=CAGE_YR,col=rainbow(nrow(.SD)))]
Death_Count <- fsum(Deaths$N)
mtext(paste0("All Covid19 VAERS Reported Deaths (DIED == 'Y') & CAGE_YR >= 12. Death_Count=",Death_Count),cex=1.5,side=3)
#

# Covid VAERS Reported Life Threatening Events (e.g. L_THREAT == "Y")
fsum <- function(x) {base::sum(x,na.rm=TRUE)}

dev.new()
L_THREAT <- mergeDVS[VAX_TYPE == "COVID19" & L_THREAT == "Y" & !is.na(CAGE_YR) & CAGE_YR >= 12,.N,.(CAGE_YR)][order(CAGE_YR)];
L_THREAT[,barplot(N,names.arg=CAGE_YR,col=rainbow(nrow(.SD)))]
L_THREAT_count <- fsum(L_THREAT$N)
mtext(paste0("All Covid19 VAERS Reported L_THREAT == 'Y' & CAGE_YR >= 12. ","L_THREAT=",L_THREAT_count),cex=1.5,side=3)
#


L_THREAT <- mergeDVS[VAX_TYPE == "COVID19" & L_THREAT == "Y",.N,.(RECVDATE=mdy(RECVDATE))];
L_THREAT_count <- fsum(L_THREAT$N)
dev.new()
L_THREAT[,
xyplot(N ~ RECVDATE,
pch=19,
cex=1.5,
col=rainbow(nrow(.SD)),
main=paste0("All Covid19 VAERS Reported L_THREAT == 'Y' ","L_THREAT=",L_THREAT_count))]
#


mtext(paste0("All Covid19 VAERS Reported L_THREAT == 'Y'","L_THREAT=",L_THREAT_count),cex=1.5,side=3)
#


# COVID.LifeThreatening Corpus
rm(Life.Threatening)
Life.Threatening <- mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM1)]
Life.Threatening <- rbind(Life.Threatening,mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM2)])
Life.Threatening <- rbind(Life.Threatening,mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM3)])
Life.Threatening <- rbind(Life.Threatening,mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM4)])
Life.Threatening <- rbind(Life.Threatening,mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM5)])

mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.N,.(CAGE_YR)][order(CAGE_YR)][
!is.na(CAGE_YR) & CAGE_YR >= 12,barplot(N,names.arg=CAGE_YR,
main="VAX_TYPE == 'COVID19' & L_THREAT == 'Y' & !is.na(CAGE_YR) & CAGE_YR >= 12",col=rainbow(nrow(.SD)))]

rm(stroke)
stroke <- mergeDVS[grepl("stroke",ignore.case=TRUE,SYMPTOM1),]
stroke <- rbind(stroke,mergeDVS[grepl("stroke",ignore.case=TRUE,SYMPTOM2),])
stroke <- rbind(stroke,mergeDVS[grepl("stroke",ignore.case=TRUE,SYMPTOM3),])
stroke <- rbind(stroke,mergeDVS[grepl("stroke",ignore.case=TRUE,SYMPTOM4),])
stroke <- rbind(stroke,mergeDVS[grepl("stroke",ignore.case=TRUE,SYMPTOM5),])
stroke <- stroke[!duplicated(VAERS_ID),]

rm(infarction)
infarction <- mergeDVS[grepl("infarction",ignore.case=TRUE,SYMPTOM1),]
infarction <- rbind(infarction,mergeDVS[grepl("infarction",ignore.case=TRUE,SYMPTOM2),])
infarction <- rbind(infarction,mergeDVS[grepl("infarction",ignore.case=TRUE,SYMPTOM3),])
infarction <- rbind(infarction,mergeDVS[grepl("infarction",ignore.case=TRUE,SYMPTOM4),])
infarction <- rbind(infarction,mergeDVS[grepl("infarction",ignore.case=TRUE,SYMPTOM5),])
infarction <- infarction[!duplicated(VAERS_ID),]

rm(anaph)
anaph <- mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM1),]
anaph <- rbind(anaph,mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM2),])
anaph <- rbind(anaph,mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM3),])
anaph <- rbind(anaph,mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM4),])
anaph <- rbind(anaph,mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM5),])
anaph <- anaph[!duplicated(VAERS_ID),]

anaph.COVID19 <- anaph[VAX_TYPE == "COVID19",
.(VAX_TYPE,VAX_MANU,VAERS_ID,AGE_YRS,VAX_DATE,ONSET_DATE,NUMDAYS,DIED,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][order(-VAX_DATE,VAERS_ID)]

rm(thromb)
thromb <-  mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM1),]
thromb <- rbind(thromb,mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM2),])
thromb <- rbind(thromb,mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM3),])
thromb <- rbind(thromb,mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM4),])
thromb <- rbind(thromb,mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM5),])
thromb <- thromb[!duplicated(VAERS_ID),]

thromb.COVID19 <- thromb[VAX_TYPE == "COVID19",
.(VAX_TYPE,VAX_MANU,VAERS_ID,AGE_YRS,DIED,VAX_DATE,ONSET_DATE,NUMDAYS,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][order(-VAX_DATE,VAERS_ID)]

# rbind(anaph,thromb)[,.N,.(VAX_TYPE,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
anaph.thromb <- setnames(merge(anaph[,.N,.(VAX_TYPE)],thromb[,.N,.(VAX_TYPE)],by="VAX_TYPE"),
c("VAX_TYPE","anaph","thromb"))[order(-(anaph + thromb))]

rm(card)
card <-  mergeDVS[grepl("card",ignore.case=TRUE,SYMPTOM1),]
card <- rbind(card,mergeDVS[grepl("card",ignore.case=TRUE,SYMPTOM2),])
card <- rbind(card,mergeDVS[grepl("card",ignore.case=TRUE,SYMPTOM3),])
card <- rbind(card,mergeDVS[grepl("card",ignore.case=TRUE,SYMPTOM4),])
card <- rbind(card,mergeDVS[grepl("card",ignore.case=TRUE,SYMPTOM5),])
card <- card[!duplicated(VAERS_ID),]

rm(lymph)
lymph <-  mergeDVS[grepl("lymph",ignore.case=TRUE,SYMPTOM1),]
lymph <- rbind(lymph,mergeDVS[grepl("lymph",ignore.case=TRUE,SYMPTOM2),])
lymph <- rbind(lymph,mergeDVS[grepl("lymph",ignore.case=TRUE,SYMPTOM3),])
lymph <- rbind(lymph,mergeDVS[grepl("lymph",ignore.case=TRUE,SYMPTOM4),])
lymph <- rbind(lymph,mergeDVS[grepl("lymph",ignore.case=TRUE,SYMPTOM5),])
lymph <- lymph[!duplicated(VAERS_ID),]

# rbind(card,lymph)[,.N,.(VAX_TYPE,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
# card.lymph <- setnames(merge(card[,.N,.(VAX_TYPE)],lymph[,.N,.(VAX_TYPE)],by="VAX_TYPE"),c("VAX_TYPE","card","lymph"))[order(-(card+lymph))]
# death.other <- setnames(merge(mergeDVS[DIED == "Y",.N,.(VAX_TYPE)],mergeDVS[DIED != "Y" ,.N,.(VAX_TYPE)],by="VAX_TYPE"),c("VAX_TYPE","DIED","Other.VAERS.LOG"))[order(-DIED)]
# merge(merge(death.other,card.lymph,all=TRUE,by="VAX_TYPE"),anaph.thromb,all=TRUE,by="VAX_TYPE") [order(-(card+lymph+anaph+thromb))]


# Blood beta-D-glucan positive
rm(glucan)
glucan <-  mergeDVS[grepl("glucan",ignore.case=TRUE,SYMPTOM1),]
glucan <- rbind(glucan,mergeDVS[grepl("glucan",ignore.case=TRUE,SYMPTOM2),])
glucan <- rbind(glucan,mergeDVS[grepl("glucan",ignore.case=TRUE,SYMPTOM3),])
glucan <- rbind(glucan,mergeDVS[grepl("glucan",ignore.case=TRUE,SYMPTOM4),])
glucan <- rbind(glucan,mergeDVS[grepl("glucan",ignore.case=TRUE,SYMPTOM5),])
glucan <- glucan[!duplicated(VAERS_ID),]

rm(blood)
blood <-  mergeDVS[grepl("blood",ignore.case=TRUE,SYMPTOM1),]
blood <- rbind(blood,mergeDVS[grepl("blood",ignore.case=TRUE,SYMPTOM2),])
blood <- rbind(blood,mergeDVS[grepl("blood",ignore.case=TRUE,SYMPTOM3),])
blood <- rbind(blood,mergeDVS[grepl("blood",ignore.case=TRUE,SYMPTOM4),])
blood <- rbind(blood,mergeDVS[grepl("blood",ignore.case=TRUE,SYMPTOM5),])
blood <- blood[!duplicated(VAERS_ID),]

rm(fung)
fung <-  mergeDVS[grepl("fung",ignore.case=TRUE,SYMPTOM1),]
fung <- rbind(fung,mergeDVS[grepl("fung",ignore.case=TRUE,SYMPTOM2),])
fung <- rbind(fung,mergeDVS[grepl("fung",ignore.case=TRUE,SYMPTOM3),])
fung <- rbind(fung,mergeDVS[grepl("fung",ignore.case=TRUE,SYMPTOM4),])
fung <- rbind(fung,mergeDVS[grepl("fung",ignore.case=TRUE,SYMPTOM5),])
fung <- fung[!duplicated(VAERS_ID),]

rm(throat)
throat <-  mergeDVS[grepl("throat",ignore.case=TRUE,SYMPTOM1),]
throat <- rbind(throat,mergeDVS[grepl("throat",ignore.case=TRUE,SYMPTOM2),])
throat <- rbind(throat,mergeDVS[grepl("throat",ignore.case=TRUE,SYMPTOM3),])
throat <- rbind(throat,mergeDVS[grepl("throat",ignore.case=TRUE,SYMPTOM4),])
throat <- rbind(throat,mergeDVS[grepl("throat",ignore.case=TRUE,SYMPTOM5),])
throat <- throat[!duplicated(VAERS_ID),]

# head
rm(head)
head <- mergeDVS[grepl("head",ignore.case=TRUE,SYMPTOM1),]
head <- rbind(head,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM2),])
head <- rbind(head,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM3),])
head <- rbind(head,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM4),])
head <- rbind(head,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM5),])
head <- head[!duplicated(VAERS_ID)]

#chill
rm(chill)
chill <-  mergeDVS[grepl("chill",ignore.case=TRUE,SYMPTOM1),]
chill <- rbind(chill,mergeDVS[grepl("chill",ignore.case=TRUE,SYMPTOM2),])
chill <- rbind(chill,mergeDVS[grepl("chill",ignore.case=TRUE,SYMPTOM3),])
chill <- rbind(chill,mergeDVS[grepl("chill",ignore.case=TRUE,SYMPTOM4),])
chill <- rbind(chill,mergeDVS[grepl("chill",ignore.case=TRUE,SYMPTOM5),])
chill <- chill[!duplicated(VAERS_ID),]

#HIV
rm(HIV)
HIV <- mergeDVS[grepl("HIV",ignore.case=TRUE,SYMPTOM1),]
HIV <- rbind(HIV,mergeDVS[grepl("HIV",ignore.case=TRUE,SYMPTOM2),])
HIV <- rbind(HIV,mergeDVS[grepl("HIV",ignore.case=TRUE,SYMPTOM3),])
HIV <- rbind(HIV,mergeDVS[grepl("HIV",ignore.case=TRUE,SYMPTOM4),])
HIV <- rbind(HIV,mergeDVS[grepl("HIV",ignore.case=TRUE,SYMPTOM5),])
HIV <- HIV  [!duplicated(VAERS_ID),]


# Guillain-Barre
rm(Guillain.Barre)
Guillain.Barre <- mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM1),]
Guillain.Barre <- rbind(Guillain.Barre,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM2),])
Guillain.Barre <- rbind(Guillain.Barre,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM3),])
Guillain.Barre <- rbind(Guillain.Barre,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM4),])
Guillain.Barre <- rbind(Guillain.Barre,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM5),])
Guillain.Barre <- Guillain.Barre[!duplicated(VAERS_ID)]

dev.new()
Guillain.Barre[VAX_TYPE == "COVID19",.N,.(VAX_TYPE,CAGE_YR)][order(CAGE_YR)][,barplot(N,names.arg=CAGE_YR,col=rainbow(nrow(.SD)))]
Guillain.Barre_count <- fsum(L_THREAT$N)
mtext(paste0("All Covid19 VAERS Reported L_THREAT == 'Y' & CAGE_YR >= 12. L_THREAT=",L_THREAT_count),cex=1.5,side=3)
#

rm(infection)
infection <-  mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM1),]
infection <- rbind(infection,mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM2),])
infection <- rbind(infection,mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM3),])
infection <- rbind(infection,mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM4),])
infection <- rbind(infection,mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM5),])
infection <- infection[!duplicated(VAERS_ID),]

# neuro
rm(neuro)
neuro <- mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM1),]
neuro <- rbind(neuro,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM2),])
neuro <- rbind(neuro,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM3),])
neuro <- rbind(neuro,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM4),])
neuro <- rbind(neuro,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM5),])
neuro <- neuro[!duplicated(VAERS_ID),]

# ceph
rm(ceph)
ceph <-  mergeDVS[grepl("ceph",ignore.case=TRUE,SYMPTOM1),]
ceph <- rbind(ceph,mergeDVS[grepl("ceph",ignore.case=TRUE,SYMPTOM2),])
ceph <- rbind(ceph,mergeDVS[grepl("ceph",ignore.case=TRUE,SYMPTOM3),])
ceph <- rbind(ceph,mergeDVS[grepl("ceph",ignore.case=TRUE,SYMPTOM4),])
ceph <- rbind(ceph,mergeDVS[grepl("ceph",ignore.case=TRUE,SYMPTOM5),])
ceph <- ceph[!duplicated(VAERS_ID),]

rm(abort)
abort <-  mergeDVS[grepl("abort",ignore.case=TRUE,SYMPTOM1),]
abort <- rbind(abort,mergeDVS[grepl("abort",ignore.case=TRUE,SYMPTOM2),])
abort <- rbind(abort,mergeDVS[grepl("abort",ignore.case=TRUE,SYMPTOM3),])
abort <- rbind(abort,mergeDVS[grepl("abort",ignore.case=TRUE,SYMPTOM4),])
abort <- rbind(abort,mergeDVS[grepl("abort",ignore.case=TRUE,SYMPTOM5),])
abort <- abort[!duplicated(VAERS_ID),]

rm(myocard)
myocard <-  mergeDVS[grepl("myocard",ignore.case=TRUE,SYMPTOM1),]
myocard <- rbind(myocard,mergeDVS[grepl("myocard",ignore.case=TRUE,SYMPTOM2),])
myocard <- rbind(myocard,mergeDVS[grepl("myocard",ignore.case=TRUE,SYMPTOM3),])
myocard <- rbind(myocard,mergeDVS[grepl("myocard",ignore.case=TRUE,SYMPTOM4),])
myocard <- rbind(myocard,mergeDVS[grepl("myocard",ignore.case=TRUE,SYMPTOM5),])
myocard <- myocard[!duplicated(VAERS_ID),]

rm(pericard)
pericard <-  mergeDVS[grepl("pericard",ignore.case=TRUE,SYMPTOM1),]
pericard <- rbind(pericard,mergeDVS[grepl("pericard",ignore.case=TRUE,SYMPTOM2),])
pericard <- rbind(pericard,mergeDVS[grepl("pericard",ignore.case=TRUE,SYMPTOM3),])
pericard <- rbind(pericard,mergeDVS[grepl("pericard",ignore.case=TRUE,SYMPTOM4),])
pericard <- rbind(pericard,mergeDVS[grepl("pericard",ignore.case=TRUE,SYMPTOM5),])
pericard <- pericard[!duplicated(VAERS_ID),]

# herpes
rm(herpes)
herpes <-  mergeDVS[grepl("herpes",ignore.case=TRUE,SYMPTOM1),]
herpes <- rbind(herpes,mergeDVS[grepl("herpes",ignore.case=TRUE,SYMPTOM2),])
herpes <- rbind(herpes,mergeDVS[grepl("herpes",ignore.case=TRUE,SYMPTOM3),])
herpes <- rbind(herpes,mergeDVS[grepl("herpes",ignore.case=TRUE,SYMPTOM4),])
herpes <- rbind(herpes,mergeDVS[grepl("herpes",ignore.case=TRUE,SYMPTOM5),])
herpes <- herpes[!duplicated(VAERS_ID),]

# zoster
rm(zoster)
zoster <-  mergeDVS[grepl("zoster",ignore.case=TRUE,SYMPTOM1),]
zoster <- rbind(zoster,mergeDVS[grepl("zoster",ignore.case=TRUE,SYMPTOM2),])
zoster <- rbind(zoster,mergeDVS[grepl("zoster",ignore.case=TRUE,SYMPTOM3),])
zoster <- rbind(zoster,mergeDVS[grepl("zoster",ignore.case=TRUE,SYMPTOM4),])
zoster <- rbind(zoster,mergeDVS[grepl("zoster",ignore.case=TRUE,SYMPTOM5),])
zoster <- zoster[!duplicated(VAERS_ID),]
 
rm(study)
study <-  mergeDVS[grepl("study",ignore.case=TRUE,SYMPTOM1),]
study <- rbind(study,mergeDVS[grepl("study",ignore.case=TRUE,SYMPTOM2),])
study <- rbind(study,mergeDVS[grepl("study",ignore.case=TRUE,SYMPTOM3),])
study <- rbind(study,mergeDVS[grepl("study",ignore.case=TRUE,SYMPTOM4),])
study <- rbind(study,mergeDVS[grepl("study",ignore.case=TRUE,SYMPTOM5),])
study <- study[!duplicated(VAERS_ID),]

rm(breakthrough)
breakthrough <-  mergeDVS[grepl("breakthrough",ignore.case=TRUE,SYMPTOM1),]
breakthrough <- rbind(breakthrough,mergeDVS[grepl("breakthrough",ignore.case=TRUE,SYMPTOM2),])
breakthrough <- rbind(breakthrough,mergeDVS[grepl("breakthrough",ignore.case=TRUE,SYMPTOM3),])
breakthrough <- rbind(breakthrough,mergeDVS[grepl("breakthrough",ignore.case=TRUE,SYMPTOM4),])
breakthrough <- rbind(breakthrough,mergeDVS[grepl("breakthrough",ignore.case=TRUE,SYMPTOM5),])
breakthrough <- breakthrough[!duplicated(VAERS_ID),]

# combinations
death.other <- setnames(merge(mergeDVS[DIED == "Y",.N,.(VAX_TYPE)],mergeDVS[DIED != "Y" ,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),c("VAX_TYPE","DIEDeqYES","Other.VAERS.LOG"))[order(-DIEDeqYES)]
life.threat <- setnames(merge(mergeDVS[L_THREAT == "Y",.N,.(VAX_TYPE)],mergeDVS[DIED == "Y" & L_THREAT == "Y" ,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),c("VAX_TYPE","L_THREATeqYES","DIEDandL_THREATeqYES"))[order(-L_THREATeqYES)]
blood.card <- setnames(merge(blood[,.N,.(VAX_TYPE)],card[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),c("VAX_TYPE","blood","card"))[order(-(blood+card))]
lymph.throat <- setnames(merge(lymph[,.N,.(VAX_TYPE)],throat[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","lymph","throat"))[order(-(lymph + throat))]
anaph.thromb <- setnames(merge(anaph[,.N,.(VAX_TYPE)],thromb[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","anaph","thromb"))[order(-(anaph + thromb))]
herpes.zoster <- setnames(merge(herpes[,.N,.(VAX_TYPE)],zoster[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","herpes","zoster"))[order(-(herpes + zoster))]
head.chill <- setnames(merge(head[,.N,.(VAX_TYPE)],chill[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","head","chill"))[order(-(head + chill))]
ceph.neuro <- setnames(merge(ceph[,.N,.(VAX_TYPE)],neuro[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","ceph","neuro"))[order(-(ceph + neuro))]
HIV.abort <- setnames(merge(HIV[,.N,.(VAX_TYPE)],abort[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","HIV","abort"))[order(-(HIV + abort))]
myocard.pericard <- setnames(merge(myocard[,.N,.(VAX_TYPE)],pericard[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","myocard","pericard"))[order(-(myocard + pericard))]
GBS.infection <- setnames(merge(infection[,.N,.(VAX_TYPE)],Guillain.Barre[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","infection","Guillain.Barre"))[order(-(infection + Guillain.Barre))]
fung.glucan <- setnames(merge(fung[,.N,.(VAX_TYPE)],glucan[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","fung","glucan"))[order(-(fung + glucan))]
study.breakthrough <- setnames(merge(study[,.N,.(VAX_TYPE)],breakthrough[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","study","breakthrough"))[order(-(study + breakthrough))]
stroke.infarction <- setnames(merge(stroke[,.N,.(VAX_TYPE)],infarction[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","stroke","infarction"))[order(-(stroke + infarction))]

mergeDVS[,.N,.(VAX_TYPE)][order(-N)]

m1 <- merge(death.other,life.threat,all=TRUE,by="VAX_TYPE")
m2 <- merge(head.chill,herpes.zoster,all=TRUE,by="VAX_TYPE")
m3 <- merge(blood.card,lymph.throat,all=TRUE,by="VAX_TYPE")
m4 <- merge(anaph.thromb,ceph.neuro,all=TRUE,by="VAX_TYPE")
m5 <- merge(myocard.pericard,stroke.infarction,all=TRUE,by="VAX_TYPE")
m6 <- merge(GBS.infection,HIV.abort,all=TRUE,by="VAX_TYPE")
m7 <- merge(fung.glucan,study.breakthrough,all=TRUE,by="VAX_TYPE")

m8 <- merge(m1,m2,all=TRUE,by="VAX_TYPE")
m9 <- merge(m3,m4,all=TRUE,by="VAX_TYPE")
m10 <- merge(m5,m6,all=TRUE,by="VAX_TYPE")
m11 <- m7

m12 <- merge(m8,m9,all=TRUE,by="VAX_TYPE")
m13 <- merge(m10,m11,all=TRUE,by="VAX_TYPE")
m14 <- merge(m12,m13,by="VAX_TYPE")

m15 <- m14[order(-Other.VAERS.LOG)]
m15[is.na(m15)] <-0
m_all <- m15[order(-Other.VAERS.LOG)]
m_all[]

print("Code that generates possible list of integrity errors in the data")
mergeDVS[VAX_TYPE == "COVID19" & mdy(ONSET_DATE) < mdy(VAX_DATE) ,.N,.(VAX_DATE,ONSET_DATE,NUMDAYS)][order(-N)][1:40]
mergeDVS[VAX_TYPE == "COVID19",.N,.(VAX_DATE,ONSET_DATE,NUMDAYS)][order(-NUMDAYS)][1:40]
mergeDVS[VAX_TYPE == "COVID19" & AGE_YRS != CAGE_YR & AGE_YRS < 12,.N,.(AGE_YRS,CAGE_YR)][order(-N)][1:40]
dupVAERS <- Data_Vax_SYMP_2021[, as.data.table(.SD[duplicated(VAERS_ID),.(VAERS_ID)])];Data_Vax_SYMP_2021[VAERS_ID %in% dupVAERS$VAERS_ID,][,.(VAERS_ID,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
dupVAERS

