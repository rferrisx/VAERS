# File folder should contain like this:
# 2020VAERSDATA.csv
# 2020VAERSSYMPTOMS.csv
# 2020VAERSVAX.csv 

# merge routines:
# All 2020 data
setwd("D:\\Politics\\VAERS\\2020VAERSData.2020_07.19.2021")
Data_Vax <- merge(fread("2020VAERSDATA.csv"),fread("2020VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2020VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...
Data_Vax_SYMP_2020 <- Data_Vax_SYMP[order(-VAERS_ID)]
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2020 <- mergeDVS

# through September 24 2021 data
setwd("D:\\Politics\\VAERS\\2021VAERSData.09.24.2021")
Data_Vax <- merge(fread("2021VAERSDATA.csv"),fread("2021VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2021VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...
Data_Vax_SYMP_2021 <- Data_Vax_SYMP[order(-VAERS_ID)]
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2021 <- mergeDVS
mergeDVS <-rbind(mergeDVS2020,mergeDVS2021)
mergeDVS[,All_symptoms:= (cbind(paste0(SYMPTOM1," ",SYMPTOM2," ",SYMPTOM3," ",SYMPTOM4," ",SYMPTOM5)))]
fsum <- function(x) {base::sum(x,na.rm=TRUE)}


# Making explicit some problems with VAERS db
# Improper VAX_DATE
mergeDVS[
VAX_TYPE == "COVID19" &
NUMDAYS > (9 * 30),
.(VAERS_ID,VAX_MANU,VAX_DATE,ONSET_DATE,ONSETminusVAX=mdy(ONSET_DATE) - mdy(VAX_DATE),NUMDAYS)]


# Improper AGE_YRS
mergeDVS[
VAX_TYPE == "COVID19" &
AGE_YRS <= 10,
.(VAERS_ID,VAX_MANU,VAX_DATE,AGE_YRS,CAGE_YR)]

# Explicating top 20 VAX_MANU x VAX_TYPE
mergeDVS[,.N,.(VAX_MANU,VAX_TYPE)][order(-N)][1:20]
nrow(mergeDVS[VAX_TYPE == "COVID19",])

fsum <- function(x) {base::sum(x,na.rm=TRUE)}
#fmean <- function(x) {base::mean(as.integer(x),na.rm=TRUE)}


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
	HOSPDAYS < 540, # less than 18 mo * 30 days: TO AVOID HOSPDAYS entries of '99999'
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
	Age.10.30=between(AGE_YRS,10,30),
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
	HOSPDAYS < 540, # less than 18 mo * 30 days: TO AVOID HOSPDAYS entries of '99999'
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
	Age.10.30=between(AGE_YRS,10,30),
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
x <- mergeDVS;CritMarks()
x <- mergeDVS[is.na(NUMDAYS) | NUMDAYS == "" | NUMDAYS == "NA" ,];CritMarks()
x <- mergeDVS[NUMDAYS <= 2,];CritMarks()
x <- mergeDVS[between(NUMDAYS,0,7)];CritMarks()
x <- mergeDVS[between(NUMDAYS,8,14)];CritMarks()
x <- mergeDVS[between(NUMDAYS,15,21)];CritMarks()
x <- mergeDVS[between(NUMDAYS,22,28)];CritMarks()
x <- mergeDVS[NUMDAYS > 28,];CritMarks()

# Aggregated Measures by AGE_YRS
x <- mergeDVS[between(AGE_YRS,10,19),];CritMarks()
x <- mergeDVS[between(AGE_YRS,20,29),];CritMarks()
x <- mergeDVS[between(AGE_YRS,30,39),];CritMarks()
x <- mergeDVS[between(AGE_YRS,40,49),];CritMarks()
x <- mergeDVS[between(AGE_YRS,50,59),];CritMarks()
x <- mergeDVS[between(AGE_YRS,60,69),];CritMarks()
x <- mergeDVS[between(AGE_YRS,70,79),];CritMarks()
x <- mergeDVS[between(AGE_YRS,80,89),];CritMarks()
x <- mergeDVS[between(AGE_YRS,90,99),];CritMarks()
x <- mergeDVS[between(AGE_YRS,100,109),];CritMarks()

# Aggregated Measures by GENDER
x <- mergeDVS[SEX == "M",];CritMarks()
x <- mergeDVS[SEX == "F",];CritMarks()
x <- mergeDVS[SEX == "U",];CritMarks()