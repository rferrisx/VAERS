
library(data.table)
# Needs unzipped VAERS library from:
# https://vaers.hhs.gov/data/datasets.html
# Bellow is December 2020 data and (paritial)January 2021 data

# File folder should contain like this:
# 2020VAERSDATA.csv
# 2020VAERSSYMPTOMS.csv
# 2020VAERSVAX.csv

# merge routines:
# December 2020 data
setwd("D:\\Politics\\VAERS\\2020VAERSData.01.22.2021")
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

# January 2021 data
setwd("D:\\Politics\\VAERS\\2021VAERSData.01.22.2021")
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

rm(anaph)
anaph <- mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM1),]
anaph <- rbind(anaph,mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM2),])
anaph <- rbind(anaph,mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM3),])
anaph <- rbind(anaph,mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM4),])
anaph <- rbind(anaph,mergeDVS[grepl("anaph",ignore.case=TRUE,SYMPTOM5),])
anaph <- anaph[!duplicated(VAERS_ID),]

anaph <- anaph[VAX_TYPE == "COVID19",
.(VAX_TYPE,VAX_MANU,VAERS_ID,AGE_YRS,VAX_DATE,ONSET_DATE,NUMDAYS,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][order(-VAX_DATE,VAERS_ID)]

rm(thromb)
thromb <-  mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM1),]
thromb <- rbind(thromb,mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM2),])
thromb <- rbind(thromb,mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM3),])
thromb <- rbind(thromb,mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM4),])
thromb <- rbind(thromb,mergeDVS[grepl("thromb",ignore.case=TRUE,SYMPTOM5),])
thromb <- thromb[!duplicated(VAERS_ID),]


thromb[VAX_TYPE == "COVID19",
.(VAX_TYPE,VAX_MANU,VAERS_ID,AGE_YRS,DIED,VAX_DATE,ONSET_DATE,NUMDAYS,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][order(-VAX_DATE,VAERS_ID)]


print("list of errors in the data")
cat('
Errors in the Data (Data discrepancies)
mergeDVS[VAX_TYPE == "COVID19",.N,.(VAX_DATE,ONSET_DATE,NUMDAYS)][order(-NUMDAYS)][1:100]
       VAX_DATE ONSET_DATE NUMDAYS N
  1: 09/16/1966 12/23/2020   19822 1
  2: 06/06/1969 12/22/2020   18827 1
  3: 12/18/1969 12/18/2020   18628 1
  4: 07/23/1970 12/18/2020   18411 1
  5: 04/22/2019 12/23/2020     611 1
  6: 01/02/2020 01/04/2021     368 1
  7: 01/04/2020 01/05/2021     367 1
  8: 01/04/2020 01/04/2021     366 1
  9: 01/04/2020 12/30/2020     361 1
 10: 02/18/2020 12/18/2020     304 1

')



