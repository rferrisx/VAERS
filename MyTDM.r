# 7:16 PM 1/15/2021
# This code dumps five columns (1 - 5) of Covid19 VAERS Symptom entries into a corpus for Word Cloud graphics

# install data.table
library(data.table)
# Needs unzipped VAERS library from:
# https://vaers.hhs.gov/data/datasets.html
# Bellow is December 2020 data and (paritial)January 2021 data

# File folder should contain like this:
# 2020VAERSDATA.csv
# 2020VAERSSYMPTOMS.csv
# 2020VAERSVAX.csv

# merge routines:
setwd("D:\\Politics\\VAERS\\2020VAERSData01.08.2021")
Data_Vax <- merge(fread("2020VAERSDATA.csv"),fread("2020VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2020VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...still a little hazy on the what/why of dual VAERS_ID entries:
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2020 <- mergeDVS

setwd("D:\\Politics\\VAERS\\2021VAERSData01.15.2021")
Data_Vax <- merge(fread("2021VAERSDATA.csv"),fread("2021VAERSVAX.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax,VAERS_ID)
Data_Vax_SYMP <- merge(Data_Vax,fread("2021VAERSSYMPTOMS.csv"),all.x=TRUE,by="VAERS_ID")
setkey(Data_Vax_SYMP,VAERS_ID)
print("All merged db entry count");nrow(Data_Vax_SYMP)
print("Duplicated VAERS_ID count");Data_Vax_SYMP[duplicated(VAERS_ID),.N]
print("Not duplicated VAERS_ID count");Data_Vax_SYMP[!duplicated(VAERS_ID),.N]
# remove duplicated VAERS_ID...still a little hazy on the what/why of dual VAERS_ID entries:
mergeDVS <- Data_Vax_SYMP[!duplicated(VAERS_ID),]
mergeDVS2021 <- mergeDVS

mergeDVS <-rbind(mergeDVS2020,mergeDVS2021)

print("..."
print("Merge is Done. Ctrl+C to cancel rest of script. Sleeping 10 seconds");Sys.sleep(10)
print("Continuing ...")


# tm (corpus) and wordcloud routines
library(tm) # Install from CRAN if neccessary
# Install the rest from Bio Conductor
# https://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html
# if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
# BiocManager::install("Rgraphviz")
# This *should* install:
library(Rgraphviz)
library(wordcloud)
library(graph)

# Create 'corpus' of all Symptom* for 'tm' library
rm(corpus)
corpus <- mergeDVS[VAX_TYPE == "COVID19",.(SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][,rbind(sapply(.SD[,1],as.character))]
corpus <- mergeDVS[VAX_TYPE == "COVID19",.(SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][,rbind(corpus,sapply(.SD[,2],as.character))]
corpus <- mergeDVS[VAX_TYPE == "COVID19",.(SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][,rbind(corpus,sapply(.SD[,3],as.character))]
corpus <- mergeDVS[VAX_TYPE == "COVID19",.(SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][,rbind(corpus,sapply(.SD[,4],as.character))]
corpus <- mergeDVS[VAX_TYPE == "COVID19",.(SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][,rbind(corpus,sapply(.SD[,5],as.character))]

Symptom_corpus <- setnames(as.data.table(corpus),c("symptoms"))[]
myCorpus <- Corpus(VectorSource(Symptom_corpus$symptoms))
myTdm <- TermDocumentMatrix(myCorpus,control=list(wordLengths=c(1,Inf)))
(freq.terms <- findFreqTerms(myTdm, lowfreq=20))
m <- as.matrix(myTdm)
freq <- sort(rowSums(m), decreasing=T)

dev.new()
# Word Cloud charts
par(mfrow=c(1,2))
wordcloud(words=names(freq), freq=freq, min.freq=128,scale=c(4,1),colors=TRUE, random.order=F)
mtext("Covid19 VAERS Symptoms Word Cloud with min word freq =128",col="red",cex=1.5)
wordcloud(words=names(freq), freq=freq, min.freq=64,scale=c(4,1),colors=TRUE, random.order=F)
mtext("Covid19 VAERS Symptoms Word Cloud with min word freq =64",col="red",cex=1.5)
par(mfrow=c(1,1))
mtext("12/2020 and 01/2021 VAERS Symptom data for Covid19",col="blue",cex=1.75,side=1,line=-1)
mtext("from https://vaers.hhs.gov/data/datasets.html",col="blue",cex=1.75,side=1,line=1)
#not run

dev.new()
par(mfrow=c(1,2))
wordcloud(words=names(freq), freq=freq, min.freq=32,scale=c(4,1),colors=TRUE,random.order=F)
mtext("Covid19 VAERS Symptoms Word Cloud with min word freq =32",col="red",cex=1.5)
wordcloud(words=names(freq), freq=freq, min.freq=16,scale=c(4,1),colors=TRUE, random.order=F)
mtext("Covid19 VAERS Symptoms Word Cloud with min word freq =16",col="red",cex=1.5)
par(mfrow=c(1,1))
mtext("12/2020 and 01/2021 VAERS Symptom data for Covid19",col="blue",cex=1.75,side=1,line=-1)
mtext("from https://vaers.hhs.gov/data/datasets.html",col="blue",cex=1.75,side=1,line=1)
#not run

dev.new()
par(mfrow=c(1,2))
wordcloud(words=names(freq), freq=freq, min.freq=8,scale=c(4,1),colors=TRUE,random.order=F)
mtext("Covid19 VAERS Symptoms Word Cloud with min word freq =8",col="red",cex=1.5)
wordcloud(words=names(freq), freq=freq, min.freq=4,scale=c(4,1),colors=TRUE, random.order=F)
mtext("Covid19 VAERS Symptoms Word Cloud with min word freq =4",col="red",cex=1.5)
par(mfrow=c(1,1))
mtext("12/2020 and 01/2021 VAERS Symptom data for Covid19",col="blue",cex=1.75,side=1,line=-1)
mtext("from https://vaers.hhs.gov/data/datasets.html",col="blue",cex=1.75,side=1,line=1)
#not run
