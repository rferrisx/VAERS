# Grokking VAER data
library(data.table)
library(lattice) # for charts
# Needs unzipped VAERS library from:
# https://vaers.hhs.gov/data/datasets.html
# Bellow is December 2020 data and (paritial)January 2021 data

# File folder should contain like this:
# 2020VAERSDATA.csv
# 2020VAERSSYMPTOMS.csv
# 2020VAERSVAX.csv

# merge routines:
# December 2020 data
setwd("D:\\Politics\\VAERS\\2020VAERSData12.28.2020")
Data_Vax <- merge(fread("2020VAERSDATA.csv"),fread("2020VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2020VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...still a little hazy on the what/why of dual VAERS_ID entries:
Data_Vax_SYMP_2020 <- Data_Vax_SYMP
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2020 <- mergeDVS

# up to March 12 2021 data
setwd("D:\\Politics\\VAERS\\2021VAERSData.03.12.2021")
Data_Vax <- merge(fread("2021VAERSDATA.csv"),fread("2021VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2021VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...still a little hazy on the what/why of dual VAERS_ID entries:
Data_Vax_SYMP_2021 <- Data_Vax_SYMP
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2021 <- mergeDVS
mergeDVS <-rbind(mergeDVS2020,mergeDVS2021)
mergeData_Vax_SYMP <- rbind(Data_Vax_SYMP_2020,Data_Vax_SYMP_2021)

setnames(merge(mergeDVS[DIED == "Y",.N,.(VAX_TYPE)],mergeDVS[DIED != "Y" ,.N,.(VAX_TYPE)],by="VAX_TYPE"),c("VAX_TYPE","DIED","Other.VAERS.LOG"))[order(-DIED)]
mergeDVS[DIED == "Y",.N,.(LAB_DATA,VAX_TYPE,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.(VAX_TYPE,VAX_MANU,LAB_DATA,VAERS_ID,AGE_YRS,CUR_ILL,VAX_DATE,ONSET_DATE,NUMDAYS,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.(VAX_TYPE,VAX_MANU,VAERS_ID,AGE_YRS,VAX_DATE,ONSET_DATE,NUMDAYS,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y",.(VAX_TYPE,VAX_MANU,VAERS_ID,AGE_YRS,VAX_DATE,ONSET_DATE,NUMDAYS,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][order(-VAX_DATE,VAERS_ID)][,as.data.frame(.SD)]

dev.new()
mergeDVS[VAX_TYPE == "COVID19" & !is.na(CAGE_YR) & CAGE_YR > 17,.N,.(CAGE_YR)][order(CAGE_YR)][,barplot(N,names.arg=CAGE_YR,col=rainbow(nrow(.SD)))]
dev.new()
mergeDVS[VAX_TYPE == "COVID19" & DIED == "Y" & !is.na(CAGE_YR) & CAGE_YR > 17,.N,.(CAGE_YR)][order(CAGE_YR)][,barplot(N,names.arg=CAGE_YR,col=rainbow(nrow(.SD)))]

#mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
# COVID.LifeThreatening Corpus
rm(Life.Threatening)
Life.Threatening <- mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM1)]
Life.Threatening <- rbind(Life.Threatening,mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM2)])
Life.Threatening <- rbind(Life.Threatening,mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM3)])
Life.Threatening <- rbind(Life.Threatening,mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM4)])
Life.Threatening <- rbind(Life.Threatening,mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.(SYMPTOMS=SYMPTOM5)])
mergeDVS[VAX_TYPE == "COVID19"& L_THREAT == "Y",.N,.(CAGE_YR)][order(CAGE_YR)][!is.na(CAGE_YR) & CAGE_YR > 17,barplot(N,names.arg=CAGE_YR,col=rainbow(nrow(.SD)))]

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
rbind(anaph,thromb)[,.N,.(VAX_TYPE,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
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

rbind(card,lymph)[,.N,.(VAX_TYPE,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)]
card.lymph <- setnames(merge(card[,.N,.(VAX_TYPE)],lymph[,.N,.(VAX_TYPE)],by="VAX_TYPE"),c("VAX_TYPE","card","lymph"))[order(-(card+lymph))]
death.other <- setnames(merge(mergeDVS[DIED == "Y",.N,.(VAX_TYPE)],mergeDVS[DIED != "Y" ,.N,.(VAX_TYPE)],by="VAX_TYPE"),c("VAX_TYPE","DIED","Other.VAERS.LOG"))[order(-DIED)]
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


# Guillain-Barre
rm(Guillain.Barre)
Guillain.Barre <- mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM1),]
Guillain.Barre <- rbind(Guillain.Barre,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM2),])
Guillain.Barre <- rbind(Guillain.Barre,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM3),])
Guillain.Barre <- rbind(Guillain.Barre,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM4),])
Guillain.Barre <- rbind(Guillain.Barre,mergeDVS[grepl("Guillain-Barre",ignore.case=TRUE,SYMPTOM5),])
Guillain.Barre <- Guillain.Barre[!duplicated(VAERS_ID)

rm(infection)
infection <-  mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM1),]
infection <- rbind(infection,mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM2),])
infection <- rbind(infection,mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM3),])
infection <- rbind(infection,mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM4),])
infection <- rbind(infection,mergeDVS[grepl("infection",ignore.case=TRUE,SYMPTOM5),])
infection <- infection[!duplicated(VAERS_ID),]

death.other <- setnames(merge(mergeDVS[DIED == "Y",.N,.(VAX_TYPE)],mergeDVS[DIED != "Y" ,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),c("VAX_TYPE","DIEDeqYES","Other.VAERS.LOG"))[order(-DIEDeqYES)]
blood.card <- setnames(merge(blood[,.N,.(VAX_TYPE)],card[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),c("VAX_TYPE","blood","card"))[order(-(blood+card))]
lymph.throat <- setnames(merge(lymph[,.N,.(VAX_TYPE)],throat[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","lymph","throat"))[order(-(lymph + throat))]
anaph.thromb <- setnames(merge(anaph[,.N,.(VAX_TYPE)],thromb[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","anaph","thromb"))[order(-(anaph + thromb))]
GBS.infection <- setnames(merge(infection[,.N,.(VAX_TYPE)],Guillain.Barre[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","Guillain.Barre","infection"))[order(-(Guillain.Barre + infection))]
fung.glucan <- setnames(merge(fung[,.N,.(VAX_TYPE)],glucan[,.N,.(VAX_TYPE)],all=TRUE,by="VAX_TYPE"),
c("VAX_TYPE","fung","glucan"))[order(-(fung + glucan))]

m1 <- merge(death.other,blood.card,all=TRUE,by="VAX_TYPE")
m2 <- merge(lymph.throat,anaph.thromb,all=TRUE,by="VAX_TYPE")
m3 <- merge(GBS.infection,fung.glucan,all=TRUE,by="VAX_TYPE")
m4 <- merge(m1,merge(m2,m3,all=TRUE,by="VAX_TYPE"),all=TRUE,by="VAX_TYPE")[order(-DIEDeqYES)]
m4 <- m4[is.na(m4)] <-0
