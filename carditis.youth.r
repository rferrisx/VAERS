library(data.table)
library(lubridate)


a1 <- mergeDVS[VAX_TYPE == "COVID19" &
grepl("carditis",All_symptoms) &
AGE_YRS <= 20,
.(VAERS_ID,
VAX_TYPE,
VAX_MANU,
AGE_YRS,
VAX_DATE,
ONSET_DATE,
ONSETminusVAX=mdy(ONSET_DATE) - mdy(VAX_DATE),
All_symptoms,
abbrevSYMPTOM_TEXT=substr(SYMPTOM_TEXT,0,225))][
VAX_DATE !="" & mdy(VAX_DATE) > mdy("11/01/2020"),][order(mdy(VAX_DATE),mdy(ONSET_DATE))]


a2 <- mergeDVS[VAX_TYPE == "COVID19" &
grepl("carditis",SYMPTOM_TEXT) &
AGE_YRS <= 20,
.(VAERS_ID,
VAX_TYPE,
VAX_MANU,
AGE_YRS,
VAX_DATE,
ONSET_DATE,
ONSETminusVAX=mdy(ONSET_DATE) - mdy(VAX_DATE),
All_symptoms,
abbrevSYMPTOM_TEXT=substr(SYMPTOM_TEXT,0,225))][
VAX_DATE !="" & mdy(VAX_DATE) > mdy("11/01/2020"),][order(mdy(VAX_DATE),mdy(ONSET_DATE))]

# binding by row a1 & a2
rbind(a1,a2)[!duplicated(VAERS_ID),]

#searching for all 'carditis' in fields relevant then removing duplicates
# this returns widest possible range 

all.carditis <- mergeDVS[VAX_TYPE == "COVID19" &
(grepl("carditis",All_symptoms) |
grepl("carditis",SYMPTOM_TEXT) |
grepl("carditis",LAB_DATA)),]

all.carditis[!duplicated(VAERS_ID)][between(AGE_YRS,10,30),.N]








