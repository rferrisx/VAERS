# VAERS db code to pull out 'pediatric' AEs 
# 2:00 PM 11/30/2021 -RMF
# R 4.0.2 "Microsoft Open' or R 4.1.2 plus latest data.table

library(data.table)
library(lubridate)
library(stringi)

# Can use Header_Revised.r code for data import. See
# Header_code_revised_import.r : https://github.com/rferrisx/VAERS/blob/main/HeaderCode_revised_import.r


# Functions
`%nilike%` <- Negate(`%ilike%`) # e.g. 'not like'
fsum <- function(x) {base::sum(x,na.rm=TRUE)}

# function to split,stack,remove empty fields,decapitalize all terms,then sort by count 
stack.symptoms <- function(x)
{
m <- x[,tstrsplit(All_symptoms, " ",fill="")]
l <- {}; m[,for(i in names(.SD)) {l <<- cbind(append(l,get(i)))}]
l <- as.data.table(l)[!stringi::stri_isempty(V1),.(terms=as.character(V1))]
stack.symptom.terms <<- l
stack.symptom.terms <<- stack.symptom.terms[,
	setnames(as.data.table(stringi::stri_trans_tolower(terms)),"V1","terms")] 
}

# This query gets the age group but also picks some newborns suffering injury from 
# being nursed vaccinated mothers. Some misplaced older (age) injuries. It does eliminate
# adverse events with phrase 'inappropriate age' in the SYMPTOM* fields:

# setkey(all.data,"VAERS_ID_enhanced")
all.data[VAX_TYPE == "COVID19" &
 (AGE_YRS < 12 & CAGE_YR < 12) &
 # mdy(VAX_DATE) > mdy("10/30/2021") &
 All_symptoms %nilike% 'inappropriate age',
.(VAERS_ID_enhanced,
TODAYS_DATE=mdy(TODAYS_DATE),
RECVDATE,
VAX_DATE,
SEX,
AGE_YRS,
CAGE_YR,
diff=AGE_YRS - CAGE_YR,
VAX_MANU,
All_symptoms,
abbrevST = substr(SYMPTOM_TEXT,0,225))][
!duplicated(VAERS_ID_enhanced)][order(-TODAYS_DATE)]


# 'Under12 is limited COVID 19 by age, vax_date only.
# This attempts to get all pediatric immunizations

# setkey(all.data,"VAERS_ID_enhanced")
Under12 <- all.data[VAX_TYPE == "COVID19" &
 (AGE_YRS < 12 & CAGE_YR < 12) &
 mdy(VAX_DATE) > mdy("10/30/2021"),]

Under12[,.(VAERS_ID_enhanced,
TODAYS_DATE=mdy(TODAYS_DATE),
RECVDATE,
VAX_DATE,
SEX,
AGE_YRS,
CAGE_YR,
VAX_MANU,
DIED,
All_symptoms,
SYMPTOM_TEXT,
LAB_DATA,
HISTORY)][!duplicated(VAERS_ID_enhanced)][order(-TODAYS_DATE)]

# write to CSV: 'Under12.csv'
# setkey(all.data,"VAERS_ID_enhanced")
fwrite(all.data[VAX_TYPE == "COVID19" &
 (AGE_YRS < 12 & CAGE_YR < 12) &
 mdy(VAX_DATE) > mdy('10/30/2021') &
 All_symptoms %nilike% 'inappropriate age',
.(VAERS_ID_enhanced,
TODAYS_DATE=mdy(TODAYS_DATE),
RECVDATE,
VAX_DATE,
SEX,
AGE_YRS,
CAGE_YR,
VAX_MANU,
DIED,
All_symptoms,
SYMPTOM_TEXT,
LAB_DATA,
HISTORY)][!duplicated(VAERS_ID_enhanced)][order(-TODAYS_DATE)],
"Under12.csv")

# Analysis
# Word counts
Under12[,.N,.(All_symptoms)][order(-N)][1:40]

stack.symptoms(Under12)
stack.symptom.terms[,.N,.(terms)][order(-N)][1:40]
stack.symptom.terms[nchar(terms) > 10,.N,.(terms)][order(-N)][1:40]
Under12[VAX_TYPE == "COVID19" & !duplicated(VAERS_ID_enhanced),
.(Death=fsum(DIED == "Y"),
L_THREAT=fsum(L_THREAT == "Y"),
HOSPITAL=fsum(HOSPITAL =="Y"),
HOSPITAL.DAYS=fsum(HOSPDAYS))]


# Demographics/Health markers Function

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
	HOSPDAYS < 600, # less than 20 mo * 30 days: TO AVOID HOSPDAYS entries of '99999'
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
	Age.0.12=between(AGE_YRS,0,12),
	Age.13.55=between(AGE_YRS,13,55),
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
	HOSPDAYS < 600, # less than 20 mo * 30 days: TO AVOID HOSPDAYS entries of '99999'
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
	Age.0.12=between(AGE_YRS,0,12),
	Age.13.55=between(AGE_YRS,13,55),
	Age.56.120=between(AGE_YRS,56,120))],fsum)])

)

final <- merge(setnames(as.data.table(AE.report,keep.rownames=TRUE),
	c("Factor","All.Reported.Covid.AE")),
       setnames(as.data.table(AE.Died.report,keep.rownames=TRUE),
	c("Factor","All.Reported.Covid.AE.Deaths")),by="Factor")
#final$All.Reported.Covid.AE <- final[,as.integer(All.Reported.Covid.AE)]
#final$All.Reported.Covid.Deaths <- final[,as.integer(All.Reported.Covid.Deaths)]
final[c(4:5,6:14,1:3)][c(2:8,12:14,9:11)][]
#final[]
}
# END FUNCTION

x <- Under12;CritMarks()
x <- all.data[VAX_TYPE == "COVID19" & !duplicated(VAERS_ID_enhanced),];CritMarks()
