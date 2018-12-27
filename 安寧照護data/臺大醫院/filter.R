
library(NLP)
library(readr)
library(textreadr)
library(stringr)


Hdir.out <- "C:/Users/user/Desktop/NTU_COOL/ntu_hospital/NTU_Hospital"

Patient.file <- list.files(pattern = '*.doc', full.names = F)
print(Patient.file)
length(Patient.file)

PList <- list()
for(i in 1:length(Patient.file))
{
  Patient <- Patient.file[i] %>% as.character()
  PList <- rbind(PList, Patient)
}


Ptotal.doc <- list()
text1 = Ptotal.doc
for(Patient in PList[1:147])
{
  print(Patient)
  Info <- read_doc(Patient, skip = 0, remove.empty = TRUE, trim = TRUE,format = FALSE)
  texts = ""
  for (text in Info)
  {
    texts = paste(texts, text)
  }
  Ptotal.doc <- rbind( Ptotal.doc, as.matrix(texts, sep=''))
}

totaldoc <- Ptotal.doc
AnalyzText = c()
for (t in totaldoc)
{
  AnalyzText <- append(AnalyzText,t)
}
AnalyzText$language = NULL

AIDTexts = c()
for (count in str_count(AnalyzText, "Hospice"))
{
  AIDTexts = append(AIDTexts, count != 0)
}
l = str_count(AnalyzText, "Hospice")



try <- data.frame(PList,AIDTexts,l)
try

