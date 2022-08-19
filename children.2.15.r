library(data.table)
library(lubridate)

child.2.15 <-
all.covid.data[between(AGE_YRS,2,15),
.(
VAERS_ID,
AGE_YRS,
CAGE_YR,
SEX,
DIED,
L_THREAT,
HOSPDAYS,
ER_ED_VISIT,
ER_VISIT,
VAX_DATE=mdy(VAX_DATE),
RECVDATE=mdy(RECVDATE),
All_symptoms,
SYMPTOM_TEXT,
LAB_DATA,
HISTORY)][!duplicated(VAERS_ID),][order(VAERS_ID,RECVDATE)]
fwrite(child.2.15,"child.2.15.csv")

child.2.15[,.N,.(AGE_YRS)][order(AGE_YRS)]
child.2.15[DIED == "Y",.N,.(AGE_YRS)][order(AGE_YRS)]


child.2.15.serious <-
all.covid.data[
between(AGE_YRS,2,15) &
(
DIED == "Y"     |
L_THREAT == "Y" |
HOSPDAYS > 0    |
ER_ED_VISIT == "Y" |
ER_VISIT == "Y"),

.(
VAERS_ID,
AGE_YRS,
CAGE_YR,
SEX,
DIED,
L_THREAT,
HOSPDAYS,
ER_ED_VISIT,
ER_VISIT,
VAX_DATE=mdy(VAX_DATE),
RECVDATE=mdy(RECVDATE),
All_symptoms,
SYMPTOM_TEXT,
LAB_DATA,
HISTORY)][!duplicated(VAERS_ID),][order(VAERS_ID,RECVDATE)]
fwrite(child.2.15.serious,"child.2.15.serious.csv")


child.2.15.serious[,.N,.(AGE_YRS)][order(AGE_YRS)]
child.2.15.serious[DIED == "Y",.N,.(AGE_YRS)][order(AGE_YRS)]