 # IMPORTANT: *Open in Notepad or other text editor. In notepad from the format menu unclick wordwrap to view this document*
 # Compare US Vaccine (AE) entries in the VAERS DB.
 # ADHOC comparision of US VAX VAERS AE from SYMPTOM* fields. This code does exactly what is written.
 # Whether this output is useful ... for comparing MODERNA, PFIZER , JANSSEN ... is unknown.
 # This output can't possibly begin to pick up all the terrible categories of Vaccine Injuries for COVID-19.
 # See  
 # VAERS data is poorly curated and fields inconsistent or inaccurate. For example,binary fields (e.g. DIED == "Y") do not pick up all deaths among many other errors...
 # Header Code is here: https://github.com/rferrisx/VAERS/blob/main/HeaderCode.with.Analysis.r
 
 library(data.table)
 library(stringi)
# Run Header Code to get mergeDVS for all 2020 and 2021 Adverse Events: https://github.com/rferrisx/VAERS/blob/main/HeaderCode.with.Analysis.r

# Total number of Adverse Events (AE) or VACCINE injuries:
 mergeDVS[VAX_TYPE == "COVID19",.N,.(VAX_MANU)][order(-N)]

# Critical Marks
# Run  critical markers function
# See Critical Markers code here: https://github.com/rferrisx/VAERS/blob/main/CriticalMarkers.r
# First, Critical Markers Code for all AE data combined. Then MODERNA, PFIZER, JANSSEN respectively

x <- mergeDVS;CritMarks() # All COVID VAX_TYPES
x <- mergeDVS[VAX_MANU == "MODERNA",];CritMarks()
x <- mergeDVS[VAX_MANU == "PFIZER\\BIONTECH",];CritMarks()
x <- mergeDVS[VAX_MANU == "JANSSEN",];CritMarks()

 # column bind and count the 'aggregated SYMPTOM* fields' for each VAERS_ID AE
 # These are relative SYMPTOM* field phrase for term counts.
 # Sample SYMPTOM fields (turn word wrap off):

cat('
mergeDVS[VAX_TYPE == "COVID19",.(VAX_MANU,All_symptoms,SYMPTOM1,SYMPTOM2,SYMPTOM3,SYMPTOM4,SYMPTOM5)][sample(.N), c(.SD[1:2], .N),keyby="VAX_MANU"]
               VAX_MANU                                                                          All_symptoms                          SYMPTOM1                        SYMPTOM2      SYMPTOM3                 SYMPTOM4                 SYMPTOM5       
1:              JANSSEN                                    Immediate post-injection reaction Nausea Syncope   Immediate post-injection reaction                          Nausea       Syncope                                                    54236
2:              JANSSEN                               Chills Fatigue Headache Hypoaesthesia Pain in extremity                            Chills                         Fatigue      Headache            Hypoaesthesia        Pain in extremity  54236
3:              MODERNA                                                      Abdominal pain upper Headache                 Abdominal pain upper                        Headache                                                                 281846
4:              MODERNA                                             Feeling hot Inflammation Pain Tenderness                        Feeling hot                    Inflammation          Pain               Tenderness                          281846
5:     PFIZER\\BIONTECH         Arthralgia Joint range of motion decreased Periarthritis X-ray limb abnormal                         Arthralgia Joint range of motion decreased Periarthritis      X-ray limb abnormal                          256337
6:     PFIZER\\BIONTECH Angioedema Chest discomfort Dry eye Electrocardiogram normal Irritable bowel syndrome                        Angioedema                Chest discomfort       Dry eye Electrocardiogram normal Irritable bowel syndrome 256337
7: UNKNOWN MANUFACTURER                                             Body temperature Chills Pyrexia Vomiting                   Body temperature                          Chills       Pyrexia                 Vomiting                            1266
8: UNKNOWN MANUFACTURER                       Arthralgia Ear pain Headache Herpes zoster Herpes zoster oticus                        Arthralgia                        Ear pain      Headache            Herpes zoster     Herpes zoster oticus   1266
')

# Like the cbind above but merge and sort by top MODERNA N
merge(
merge(
mergeDVS[VAX_MANU == "MODERNA",.N,.(All_symptoms)][order(-N)][1:30000],
mergeDVS[VAX_MANU == "PFIZER\\BIONTECH",.N,.(All_symptoms)][order(-N)][1:30000],by="All_symptoms"),
mergeDVS[VAX_MANU == "JANSSEN",.N,.(All_symptoms)][order(-N)][1:30000],by="All_symptoms")[,
setnames(.SD,c("All_symptoms","MODERNA","PFIZER","JANSSEN"))][order(-MODERNA)][1:99]


# split,stack,remove empty fields,decapitalize all terms,sort by count 
stack.symptoms <- function(x)
{
m <- x[,tstrsplit(All_symptoms, " ",fill="")]
l <- {}; m[,for(i in names(.SD)) {l <<- cbind(append(l,get(i)))}]
l <- as.data.table(l)[!stringi::stri_isempty(V1),.(terms=as.character(V1))]
stack.symptom.terms <<- l 
}

l <- as.data.table({})
for(i in c("MODERNA","PFIZER\\BIONTECH","JANSSEN")) {
stack.symptoms(mergeDVS[VAX_TYPE == "COVID19" & VAX_MANU == i,])
stack.symptom.terms <- stack.symptom.terms[,
	setnames(as.data.table(stringi::stri_trans_tolower(terms)),"V1","terms")]
nrow(stack.symptom.terms)
stack.symptom.terms[,.N,.(terms)][order(-N)]
l <<- cbind(l,stack.symptom.terms[,.N,.(terms)][order(-N)][1:100])}
l

# # split,stack,remove empty fields,decapitalize terms,sort by count 
# Same as above but filter by 'nchar(terms) >= 10' ... to pick up medical terms
l <- as.data.table({})
for(i in c("MODERNA","PFIZER\\BIONTECH","JANSSEN")) {
stack.symptoms(mergeDVS[VAX_TYPE == "COVID19" & VAX_MANU == i,])
stack.symptom.terms <- stack.symptom.terms[,
	setnames(as.data.table(stringi::stri_trans_tolower(terms)),"V1","terms")]
nrow(stack.symptom.terms)
stack.symptom.terms[,.N,.(terms)][order(-N)]
l <<- cbind(l,stack.symptom.terms[nchar(terms) >= 10,.N,.(terms)][order(-N)][1:100])}
l





