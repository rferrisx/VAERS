# Advanced string functions I am working on for grepping 'All_symptoms' and SYMPTOM_TEXT fields in VAERS
# Under construction9:50 AM 8/17/2022 -RMF
# Notes:
# stringi functions don't always seem to see .SD[] inside data.table brackets especially when encapsulated in a function().
# Explicit dt$field reference always required? Or some other scope voodoo...


# library(data.table)
# library(stringi)
# setDTthreads(restore_after_fork = TRUE, percent = 80, throttle = 4096)

# dt[stri_detect_regex(search_field,"search_term",case_insensitive=TRUE),]
# dt[Field == "some_field_qualifier" & stri_detect_regex(search_field,"search_term",case_insensitive=TRUE)]

# simple examples

# dt <- all.covid.data[stri_detect_regex(SYMPTOM_TEXT,"pain",case_insensitive=TRUE),];nrow(dt)
# dt.c19 <- all.vaers.data[VAX_TYPE == "COVID19" & stri_detect_regex(SYMPTOM_TEXT,"pain",case_insensitive=TRUE)];nrow(dt.c19)


library(data.table)
library(stringi)
setDTthreads(restore_after_fork = TRUE, percent = 80, throttle = 4096)

fsum <- function(x) {base::sum(x, na.rm=TRUE)}
all.covid.data <- readRDS("all.covid.data.RDS")
setalloccol("all.covid.data")
setkey(all.covid.data,VAERS_ID_enhanced)

QueryCovid <- function(x)
all.covid.data[VAX_TYPE == "COVID19" &
(stri_detect_regex(All_symptoms,x,case_insensitive=TRUE) |
 stri_detect_regex(SYMPTOM_TEXT,x,case_insensitive=TRUE))][
!duplicated(VAERS_ID),.(All_symptoms,Abbrev_ST=substr(SYMPTOM_TEXT,0,100))]


# 'All_symptoms' field created by:
# all.covid.data[,All_symptoms:= (cbind(paste0(SYMPTOM1," ",SYMPTOM2," ",SYMPTOM3," ",SYMPTOM4," ",SYMPTOM5)))]
setkey(all.covid.data,"VAERS_ID_enhanced")

three.top.terms <- function(t1,t2,t3){
stri_All <- function(x){stri_detect_regex(all.covid.data$All_symptoms,x,case_insensitive=TRUE)}
tlist <- list(t1,t2,t3)
# all.covid.data[,lapply(tlist,stri_All)]
x1 <- all.covid.data[,lapply(tlist,stri_All)]
colnames(x1) <- c(as.character(tlist))
print(x1)
x1 <- x1[get(unlist(tlist[1])) == TRUE |
	 get(unlist(tlist[2])) == TRUE |
	 get(unlist(tlist[3])) == TRUE,]
x2 <- x1[,colSums(.SD)]
print(x2)
x3 <- rbind(x1,as.data.table(t(x2)))
print(x3)
x4 <- cbind(x3,x3[,setnames(as.data.table(rowSums(.SD)),"rowsums")])
print(x4)
x4[,.N,.(rowsums)][order(-N)]
}

# three.top.terms("lymphadenopathy","oropharyngeal","temperature")
three.top.terms("thrombocytopenia","haemoglobin","immunoglobulin")


three.top.terms <- function(t1,t2,t3){
setkey(all.covid.data,"VAERS_ID_enhanced")
tlist <- list(t1,t2,t3)
print(tlist)
x1 <- as.data.table(cbind(
all.covid.data[,stri_detect_regex(all.covid.data$All_symptoms,t1,case_insensitive=TRUE),],
all.covid.data[,stri_detect_regex(all.covid.data$All_symptoms,t2,case_insensitive=TRUE),],
all.covid.data[,stri_detect_regex(all.covid.data$All_symptoms,t3,case_insensitive=TRUE),]))
colnames(x1) <- c(t1,t2,t3)
print(x1)
x1 <- x1[get(unlist(tlist[1])) == TRUE |
	 get(unlist(tlist[2])) == TRUE |
	 get(unlist(tlist[3])) == TRUE,]
x2 <- x1[,colSums(.SD)]
print(x2)
x3 <- rbind(x1,as.data.table(t(x2)))
print(x3)
x4 <- cbind(x3,x3[,setnames(as.data.table(rowSums(.SD)),"rowsums")])
print(x4)
x4[,.N,.(rowsums)][order(-N)]
}


three.top.terms("lymphadenopathy","oropharyngeal","temperature")
three.top.terms("thrombocytopenia","haemoglobin","immunoglobulin")






#.... scratch.....#

#x1 <- x1[eval(sustitute(df1 == TRUE | df2 == TRUE | df3 == TRUE)),]
#x1[str2lang(paste(sprintf(as.character(x)), collapse = '== TRUE | ')),]


str2lang(paste(sprintf(x), collapse = ' | ')


[names(x1)[1] == "TRUE" |
 names(x1)[2] == "TRUE" |
 names(x1)[3] == "TRUE",]

x1 <- x1[names(x1)[1] == "TRUE"|
 	names(x1)[2] == "TRUE"  |
 	names(x1)[3] == "TRUE",]


x2 <- x1[,colSums(.SD)]
print(x2)
x3 <- rbind(x1,as.data.table(t(x2)))
print(x3)
x4 <- cbind(x3,x3[,setnames(as.data.table(rowSums(.SD)),"colsums")])
print(x4)
x4[,.N,.(colsums)][order(-N)]
}
 



