# (More) Advanced string functions I am working on for grepping 'All_symptoms' and SYMPTOM_TEXT fields in VAERS
# Under construction 9:50 AM 8/17/2022 -RMF

library(data.table)
library(stringi)

# stringi functions don't always seem to see .SD[] inside data.table brackets especially when encapsulated in a function().
# Explicit dt$field reference always required? Or some other scope voodoo...


# all.covid.data <- readRDS("all.covid.data.RDS")
# setalloccol("all.covid.data.RDS")

list <- list("pain","injection","temperature")
striAll.sy <- function(x) {as.data.table(stri_detect_regex(all.covid.data$All_symptoms,x,case_insensitive=TRUE))}
sy <- all.covid.data[,lapply(list,striAll.sy)]
colnames(sy) <- c(as.character(list))
sy

x1 <-  sy[get(list[[1]])|
	  get(list[[2]]) |
	  get(list[[3]]),]
print(x1)
x2 <- x1[,colSums(.SD)]
print(x2)
x3 <- rbind(x1,as.data.table(t(x2)))
print(x3)
x4 <- cbind(x3,x3[,setnames(as.data.table(rowSums(.SD)),"rowsums")])
print(x4)
x4[,.N,.(rowsums)][order(-N)]




striAll.st <- function(x) {as.data.table(stri_detect_regex(all.covid.data$SYMPTOM_TEXT,x,case_insensitive=TRUE))}
list <- list("pain","injection","temperature")
st <- all.covid.data[,lapply(list,striAll.st)]
colnames(st) <- c(as.character(list))
st

stri_All.sy <- function(x){stri_detect_regex(all.covid.data$All_symptoms,x,case_insensitive=TRUE)}
stri_All.st <- function(x){stri_detect_regex(all.covid.data$SYMPTOM_TEXT,x,case_insensitive=TRUE)}

rbind(as.data.table(stri_All.sy("tinnitus")),as.data.table(stri_All.st("tinnitus")))

stri_sy.st.x <- function(x) setnames(
	cbind(as.data.table(stri_All.sy(x)),
	as.data.table(stri_All.st(x))),
	c(paste0(x,".All_symptoms"),paste0(x,".SYMPTOM_TEXT")))[]


stri_sy.st.xy <- function(x,y) setnames(
	cbind(as.data.table(stri_All.sy("x")),
	as.data.table(stri_All.st("x")),
 	as.data.table(stri_All.sy("y")),
	as.data.table(stri_All.st("y"))),
	c(paste0(x,".All_symptoms"),
	paste0(x,".SYMPTOM_TEXT"),
	paste0(y,".All_symptoms"),
	paste0(y,".SYMPTOM_TEXT")))[]


)