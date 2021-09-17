library(data.table)
setDTthreads(0)
library(stringi)

# Needs unzipped VAERS library from:
# https://vaers.hhs.gov/data/datasets.html
# Below is full 2020 data and September 2021 data

# File folder should contain like this:
# 2020VAERSDATA.csv
# 2020VAERSSYMPTOMS.csv
# 2020VAERSVAX.csv 

# merge routines:
# All 2020 data: rename this file as needed
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

# through September 10 for 2021 data
setwd("D:\\Politics\\VAERS\\2021VAERSData.09.10.2021")
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


# Long form gives you DT for each search. Short form is single function automation far below:
#Search terms
cat('
Search terms:
erectile dysfunction
swollen testicles
impoten # picks up following two terms:
#impotence
#impotent
testicle
testicul # picks up following five terms:
#testicular
#Testicular swelling
#testicular edema
#Testicular disorder
#Testicular pain
Prostatomegaly
Libido decreased
enlarged prostate
Orchitis
Groin pain
testes abnormal
Varicocele
Prostatic
Pollakiuria
penile
penis
erection')

d1 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"impoten",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"impoten",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"impoten",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d2 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"swollen testicles",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"swollen testicles",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"swollen testicles",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d3 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"erectile dysfunction",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"erectile dysfunction",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"erectile dysfunction",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d4 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"testicul",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"testicul",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"testicul",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d5 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"Prostatomegaly",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"Prostatomegaly",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"Prostatomegaly",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d6 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"Libido decreased",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"Libido decreased",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"Libido decreased",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d7 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"enlarged prostate",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"enlarged prostate",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"enlarged prostate",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d8 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"testicle",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"testicle",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"testicle",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d9 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"Orchitis",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"Orchitis",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"Orchitis",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d10 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"Groin pain",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"Groin pain",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"Groin pain",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d11 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"testes abnormal",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"testes abnormal",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"testes abnormal",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d12 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"Varicocele",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"Varicocele",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"Varicocele",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d13 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"Prostatic",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"Prostatic",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"Prostatic",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d14 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"Prostatitis",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"Prostatitis",case_insensitive=TRUE) |
  stri_detect_regex(LAB_DATA,"Prostatitis",case_insensitive=TRUE)),
 .(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d15 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"Pollakiuria",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"Pollakiuria",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"Pollakiuria",case_insensitive=TRUE)),
 .(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d16 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"penile",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"penile",case_insensitive=TRUE) |
  stri_detect_regex(LAB_DATA,"penile",case_insensitive=TRUE)),
 .(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

#Penis disorder
d17 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"penis",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"penis",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"penis",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

d18 <- mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,"erection",case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,"erection",case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,"erection",case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]

male.problems <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18)[SEX == "M" & !duplicated(VAERS_ID),]
male.problems <- male.problems[order(VAERS_ID)]
fwrite(male.problems,"male.problems_f1.csv")
mp1 <- male.problems


# Short form is single function automation:

search <- c(
"erectile dysfunction",
"swollen testicles",
"impoten",
"testicle",
"testicul", 
"Prostatomegaly",
"Libido decreased",
"enlarged prostate",
"Orchitis",
"Groin pain",
"testes abnormal",
"Varicocele",
"Prostatic",
"Pollakiuria",
"penile",
"penis",
"erection")

FUN.search <- function() {mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,i,case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,i,case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,i,case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]}

l <- {};for(i in search) {l <- rbind(l,FUN.search())}
male.problems <- l[SEX == "M" & !duplicated(VAERS_ID),]
male.problems <- male.problems[order(VAERS_ID)]
fwrite(male.problems,"male.problems_f2.csv")
mp2 <- male.problems

# optional search function. Run as 'searchFUN("erection")'
searchFUN <- function(x) {mergeDVS[VAX_TYPE == "COVID19" &
 (stri_detect_regex(All_symptoms,x,case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,x,case_insensitive=TRUE) |
 stri_detect_regex(LAB_DATA,x,case_insensitive=TRUE)),
.(VAERS_ID,DIED,AGE_YRS,SEX,HOSPITAL,L_THREAT,ER_ED_VISIT,All_symptoms,SYMPTOM_TEXT,LAB_DATA)]}
