# Updated 10:36 AM 7/29/2022
# Includes new db 'all.vaers.data','all.data', all.covid.data
# Includes memory management routines
# Includes symptom and lexical summary analysis tables

library(data.table)
setDTthreads(restore_after_fork = TRUE, percent = 80, throttle = 4096)
library(lubridate)
library(stringi)
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
# All 2020 data
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

# 2021 data
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


# 2022 data
setwd("D:\\Politics\\VAERS\\2022VAERSData.08.16.2022")
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
all.covid.data <- all.vaers.data[VAX_MANU %in% c("MODERNA","PFIZER\\BIONTECH","JANSSEN","UNKNOWN MANUFACTURER") &
 VAX_TYPE == "COVID19",][!duplicated(VAERS_ID_enhanced_vaers),]
saveRDS(all.covid.data, file="all.covid.data.RDS")


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
dupVAERS <- all.covid.data[DIED == "Y",][duplicated(VAERS_ID),.(VAERS_ID)]
all.covid.data[DIED == "Y",][VAERS_ID %in% dupVAERS$VAERS_ID,
.(VAERS_ID,RECVDATE=mdy(RECVDATE),All_symptoms,VAERS_ID_enhanced)][order(RECVDATE)][1:100]
#END DATA IMPORT routines

# memory management
# deletes intermediate files
# runs gc()
rver <- as.numeric(substr(paste0(as.matrix(version)[7],".",as.matrix(version)[8]),0,3))
if(rver < 4.2) {memory.size()}
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
if(rver < 4.2) {memory.size()}

# list memory
as.data.table(sapply(ls(),
function(x){format(object.size(get(x)),
nsmall=3,digits=3,unit="Mb")}),keep.rownames=TRUE)[,
c("mem","unit") := tstrsplit(V2, " ", fixed=TRUE)][,
setnames(.SD,"V1","obj")][,.(obj,mem=as.numeric(mem),unit)][order(-mem)]

# All Covid Analysis

# START FUNCTION
CritMarks <- function() {
# critical markers
AE.report <- rbind(
 x[VAX_TYPE == "COVID19",lapply(.SD[
	,.(
	!is.null(VAERS_ID),
	DIED == "Y",
	L_THREAT == "Y",
	DISABLE == "Y",
	HOSPITAL == "Y",
	HOSPDAYS, # < 720, # less than 22 mo * 30 days: TO AVOID HOSPDAYS entries of '99999'
	ER_ED_VISIT == "Y")],fsum)][,
  t(setnames(.SD,c(
	"CovidAE.Reported.Total",
	"DIED",
	"L_THREAT",
	"DISABLE",
	"HOSPITAL",
	"HOSPITAL.DAYS",
	"ER_ED_VISIT")))],


# critical demographics
	t(x[
	VAX_TYPE == "COVID19",lapply(.SD[
	,.(
	All.AE.Reports=!is.null(VAERS_ID),
	SEX.Female=SEX == "F",
	SEX.Male=SEX == "M",
	SEX.UNK=SEX == "U",
	Age.0.30=between(AGE_YRS,0,30),
	Age.31.55=between(AGE_YRS,31,55),
	Age.56.120=between(AGE_YRS,56,120))],fsum)])

)

# critical markers
Died <- x[DIED == "Y",]
AE.Died.report <- rbind(
  Died[VAX_TYPE == "COVID19",lapply(.SD[
	,.(
	!is.null(VAERS_ID),
	DIED == "Y",
	L_THREAT == "Y",
	DISABLE == "Y",
	HOSPITAL == "Y",
	HOSPDAYS, # < 720,  less than 24 mo * 30 days: TO AVOID HOSPDAYS entries of '99999'
	ER_ED_VISIT == "Y")],fsum)][,
  t(setnames(.SD,c(
	"CovidAE.Reported.Total",
	"DIED",
	"L_THREAT",
	"DISABLE",
	"HOSPITAL",
	"HOSPITAL.DAYS",
	"ER_ED_VISIT")))],


# critical demographics
 t(Died[VAX_TYPE == "COVID19",lapply(.SD[
	,.(
	All.AE.Reports=!is.null(VAERS_ID),
	SEX.Female=SEX == "F",
	SEX.Male=SEX == "M",
	SEX.UNK=SEX == "U",
	Age.0.30=between(AGE_YRS,0,30),
	Age.31.55=between(AGE_YRS,31,55),
	Age.56.120=between(AGE_YRS,56,120))],fsum)])

)

final <- merge(setnames(as.data.table(AE.report,keep.rownames=TRUE),
	c("Factor","All.Reported.Covid.AE")),
       setnames(as.data.table(AE.Died.report,keep.rownames=TRUE),
	c("Factor","All.Reported.Covid.AE.Deaths")),by="Factor")
#final$All.Reported.Covid.AE <- final[,as.integer(All.Reported.Covid.AE)]
#final$All.Reported.Covid.Deaths <- final[,as.integer(All.Reported.Covid.Deaths)]
final[c(4:5,6:14,1:3)][c(2:8,12:14,9:11)][]
}

# END FUNCTION

# Aggregations: 
# Aggregated Measures by NUMDAYS
x <- all.covid.data;CritMarks()
x <- all.covid.data[is.na(NUMDAYS) | NUMDAYS == "" | NUMDAYS == "NA" ,];CritMarks()
x <- all.covid.data[NUMDAYS <= 2,];CritMarks()
x <- all.covid.data[between(NUMDAYS,0,7)];CritMarks()
x <- all.covid.data[between(NUMDAYS,8,14)];CritMarks()
x <- all.covid.data[between(NUMDAYS,15,21)];CritMarks()
x <- all.covid.data[between(NUMDAYS,22,28)];CritMarks()
x <- all.covid.data[NUMDAYS > 28,];CritMarks()

# Aggregated Measures by AGE_YRS
x <- all.covid.data[between(AGE_YRS,10,19),];CritMarks()
x <- all.covid.data[between(AGE_YRS,20,29),];CritMarks()
x <- all.covid.data[between(AGE_YRS,30,39),];CritMarks()
x <- all.covid.data[between(AGE_YRS,40,49),];CritMarks()
x <- all.covid.data[between(AGE_YRS,50,59),];CritMarks()
x <- all.covid.data[between(AGE_YRS,60,69),];CritMarks()
x <- all.covid.data[between(AGE_YRS,70,79),];CritMarks()
x <- all.covid.data[between(AGE_YRS,80,89),];CritMarks()
x <- all.covid.data[between(AGE_YRS,90,99),];CritMarks()
x <- all.covid.data[between(AGE_YRS,100,109),];CritMarks()

# Aggregated Measures by GENDER
x <- all.covid.data[SEX == "M",];CritMarks()
x <- all.covid.data[SEX == "F",];CritMarks()
x <- all.covid.data[SEX == "U",];CritMarks()

# Aggregated Measures by VAX_MANU

x <- all.covid.data[VAX_MANU == "MODERNA",];CritMarks()
x <- all.covid.data[VAX_MANU == "PFIZER\\BIONTECH",];CritMarks()
x <- all.covid.data[VAX_MANU == "JANSSEN",];CritMarks()

# Top Symptom* Strings

# stack symptoms function. All terms lower case.
stack.symptoms <- function(x)
{
m <- x[,tstrsplit(All_symptoms, " ",fill="")]
l <- {}; m[,for(i in names(.SD)) {l <<- cbind(append(l,get(i)))}]
l <- as.data.table(l)[!stringi::stri_isempty(V1),.(terms=as.character(V1))]
l <- l[,setnames(as.data.table(stringi::stri_trans_tolower(terms)),"V1","terms")]
stack.symptom.terms <<- l
}

# stack symptoms function. All terms lower case.
stack.symptom.text <- function(x)
{
m <- x[,tstrsplit(SYMPTOM_TEXT, " ",fill="")]
l <- {}; m[,for(i in names(.SD)) {l <<- cbind(append(l,get(i)))}]
l <- as.data.table(l)[!stringi::stri_isempty(V1),.(terms=as.character(V1))]
l <- l[,setnames(as.data.table(stringi::stri_trans_tolower(terms)),"V1","terms")]
stack.symptom.terms <<- l
}


# run "All_symptoms" string functions

print("All_symptoms for all injuries")
stack.symptoms(all.covid.data)
# list top 30 terms nchar < 10
stack.symptom.terms[nchar(terms) < 10,.N,.(terms)][order(-N)][1:30]
# list top 30 terms nchar >= 10
stack.symptom.terms[nchar(terms) > 10,.N,.(terms)][order(-N)][1:30]

# list top 30 terms nchar < 13
stack.symptom.terms[nchar(terms) < 13,.N,.(terms)][order(-N)][1:30]
# list top 30 terms nchar >= 13
stack.symptom.terms[nchar(terms) > 13,.N,.(terms)][order(-N)][1:30]

print("DIED == 'Y'")
stack.symptoms(all.covid.data[DIED == "Y",])
# list top 30 terms nchar < 10
stack.symptom.terms[nchar(terms) < 10,.N,.(terms)][order(-N)][1:30]
# list top 30 terms nchar >= 10
stack.symptom.terms[nchar(terms) > 10,.N,.(terms)][order(-N)][1:30]

# list top 30 terms nchar < 13
stack.symptom.terms[nchar(terms) < 13,.N,.(terms)][order(-N)][1:30]
# list top 30 terms nchar >= 13
stack.symptom.terms[nchar(terms) > 13,.N,.(terms)][order(-N)][1:30]




cat('
stack.symptom.text(all.covid.data)
# list top 30 terms nchar < 10
stack.symptom.terms[nchar(terms) < 10,.N,.(terms)][order(-N)][1:30]
# list top 30 terms nchar >= 10
stack.symptom.terms[nchar(terms) > 10,.N,.(terms)][order(-N)][1:30]

stack.symptom.text(all.covid.data[DIED == "Y",])
# list top 30 terms nchar < 10
stack.symptom.terms[nchar(terms) < 10,.N,.(terms)][order(-N)][1:30]
# list top 30 terms nchar >= 10
stack.symptom.terms[nchar(terms) > 10,.N,.(terms)][order(-N)][1:30]
')

all.covid.data[,.N,.(All_symptoms)][order(-N)][1:40]
all.covid.data[DIED == "Y",.N,.(All_symptoms)][order(-N)][1:40]



all.covid.data[!duplicated(VAERS_ID) &
 stri_detect_regex(All_symptoms,"COVID-19 Drug",case_insensitive=TRUE),.(substr(SYMPTOM_TEXT,0,220))]










