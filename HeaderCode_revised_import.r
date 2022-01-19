# Updated 2:08 PM 12/18/2021
# Includes new db 'all.vaers.data' and 'all.data'
# Includes memory management routines

library(data.table)
setDTthreads(0)
library(lubridate)
# Needs unzipped VAERS library from:
# https://vaers.hhs.gov/data/datasets.html
# Below is full 2020 data and 2021 data through November

# File folder should contain like this:
# 2020VAERSDATA.csv
# 2020VAERSSYMPTOMS.csv
# 2020VAERSVAX.csv 

fsum <- function(x) {base::sum(x,na.rm=TRUE)}

## DATA IMPORT routines
# merge routines:
# All 2020 data Can pull this year explicitly:  https://vaers.hhs.gov/data/datasets.html
setwd("D:\\Politics\\VAERS\\2020VAERSData.2020_07.19.2021")
Data_Vax <- merge(fread("2020VAERSDATA.csv"),fread("2020VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2020VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")[
order(VAERS_ID,-DIED,VAX_DOSE_SERIES)]
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...
Data_Vax_SYMP_2020 <- Data_Vax_SYMP[order(VAERS_ID,-DIED,-VAX_DOSE_SERIES)]
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2020 <- mergeDVS

# 2021 data # Can pull this year explicitly:  https://vaers.hhs.gov/data/datasets.html 
setwd("D:\\Politics\\VAERS\\2021VAERSData.01.07.2022")
Data_Vax <- merge(fread("2021VAERSDATA.csv"),fread("2021VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2021VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")[
order(VAERS_ID,-DIED,VAX_DOSE_SERIES)]
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...
Data_Vax_SYMP_2021 <- Data_Vax_SYMP[order(-VAERS_ID)]
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2021 <- mergeDVS


# 2022 data Use latest from https://vaers.hhs.gov/data/datasets.html
setwd("D:\\Politics\\VAERS\\2022VAERSData.01.14.2022")
Data_Vax <- merge(fread("2022VAERSDATA.csv"),fread("2022VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2022VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")[
order(VAERS_ID,-DIED,VAX_DOSE_SERIES)]
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...
Data_Vax_SYMP_2022 <- Data_Vax_SYMP[order(-VAERS_ID)]
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2022 <- mergeDVS

mergeDVS <-rbind(mergeDVS2020,mergeDVS2021,mergeDVS2022)[
order(VAERS_ID,-DIED,VAX_DOSE_SERIES)]
mergeDVS[,All_symptoms:= (cbind(paste0(SYMPTOM1," ",SYMPTOM2," ",SYMPTOM3," ",SYMPTOM4," ",SYMPTOM5)))]

# data:
# mergeDVS is de-duplicated by VAERS_ID only
# all.vaers.data is de-duplicated by VAERS_ID_enhanced_vaers:= paste0(VAERS_ID,".",VAX_DOSE_SERIES,".",num.RECV)
# all.data is de-duplicated by VAERS_ID_enhanced:= paste0(VAERS_ID,".",VAX_DOSE_SERIES)

# 1:49 PM 11/30/2021 : Do not use setkey(); Working on why that changes data
all.vaers.data <- rbind(Data_Vax_SYMP_2020,Data_Vax_SYMP_2021,Data_Vax_SYMP_2022)
all.vaers.data[,VAERS_ID_enhanced:= paste0(VAERS_ID,".",VAX_DOSE_SERIES)]
all.vaers.data[,num.RECV := as.numeric(mdy(RECVDATE))]
all.vaers.data[,VAERS_ID_enhanced_vaers:= paste0(VAERS_ID,".",VAX_DOSE_SERIES,".",num.RECV)]
all.vaers.data[,.(VAERS_ID,VAERS_ID_enhanced,VAERS_ID_enhanced_vaers)]
all.vaers.data[,All_symptoms:= (cbind(paste0(SYMPTOM1," ",SYMPTOM2," ",SYMPTOM3," ",SYMPTOM4," ",SYMPTOM5)))]
all.vaers.data <- all.vaers.data[order(VAERS_ID,VAX_DOSE_SERIES,num.RECV)][!duplicated(VAERS_ID_enhanced_vaers),]
all.vaers.data[VAX_MANU %in% c("MODERNA","PFIZER\\BIONTECH","JANSSEN","UNKNOWN MANUFACTURER") &
 VAX_TYPE == "COVID19",][!duplicated(VAERS_ID_enhanced_vaers),.N]


all.data <- rbind(Data_Vax_SYMP_2020,Data_Vax_SYMP_2021,Data_Vax_SYMP_2022)
all.data[,VAERS_ID_enhanced:= paste0(VAERS_ID,".",VAX_DOSE_SERIES)]
all.data[,.(VAERS_ID,VAERS_ID_enhanced)]
all.data[,All_symptoms:= (cbind(paste0(SYMPTOM1," ",SYMPTOM2," ",SYMPTOM3," ",SYMPTOM4," ",SYMPTOM5)))]
all.data <- all.data[order(VAERS_ID,VAX_DOSE_SERIES)][!duplicated(VAERS_ID_enhanced),]
all.data[VAX_MANU %in% c("MODERNA","PFIZER\\BIONTECH","JANSSEN","UNKNOWN MANUFACTURER") &
 VAX_TYPE == "COVID19",][!duplicated(VAERS_ID_enhanced),.N]

# some queries
nrow(all.data)
all.data[VAX_TYPE=="COVID19",.N,.(VAX_MANU,VAX_DOSE_SERIES)][order(-N)][1:10]
all.data[VAX_TYPE=="COVID19" & !duplicated(VAERS_ID_enhanced),.N,.(VAX_MANU,VAX_DOSE_SERIES)][order(VAX_MANU,VAX_DOSE_SERIES)]
all.data[duplicated(VAERS_ID),.N]
all.data[duplicated(VAERS_ID_enhanced),.N]
all.data[!duplicated(VAERS_ID),.N]
all.data[!duplicated(VAERS_ID_enhanced),.N]
all.data[VAX_TYPE == "COVID19",.N]
all.data[VAX_TYPE == "COVID19" & !duplicated(VAERS_ID_enhanced),.N]
all.data[VAX_TYPE == "COVID19" & !duplicated(VAERS_ID_enhanced),
.(Death=fsum(DIED == "Y"),L_THREAT=fsum(L_THREAT == "Y"),HOSPITAL=fsum(HOSPITAL =="Y"),HOSPITAL.DAYS=fsum(HOSPDAYS))]
#END DATA IMPORT routines

# memory management
# deletes intermediate files
# runs gc()

memory.size()
# to free up memory
rm( list=c("Data_Vax",
"Data_Vax_SYMP",
"Data_Vax_SYMP_2020",
"Data_Vax_SYMP_2021",
"Data_Vax_SYMP_2022",
"mergeDVS2020",
"mergeDVS2021",
"mergeDVS2022"))
gc();gc();gc();
memory.size()

# list memory
as.data.table(sapply(ls(),
function(x){format(object.size(get(x)),
nsmall=3,digits=3,unit="Mb")}),keep.rownames=TRUE)[,
c("mem","unit") := tstrsplit(V2, " ", fixed=TRUE)][,
setnames(.SD,"V1","obj")][,.(obj,mem=as.numeric(mem),unit)][order(-mem)]

