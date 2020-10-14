#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")

#MM/DD/YYYY or MM/DD/YYYY HH24:MI


#Alene Onion
#Oct 2020
#Script to generate a clean lci downstream tables
#2014-2018

####################################################################################################
#Reading in Data tables
rm(list=setdiff(ls(), "data"))

head(data)
####################################################################################################
####################################################################################################
#LCI results
library(dplyr)
library(tidyr)
rec<-data
rec<-rec %>% 
  filter(SAMPLE_DATE>'2019-01-01',
         DATA_PROVIDER=="LCI") %>% 
  distinct()

sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE) %>% 
  rename(RSLT_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         RSLT_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         RSLT_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         RSLT_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         RSLT_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         RSLT_SAMPLE_TYPE=SEI_SAMPLE_TYPE) %>% 
  distinct()

results<-rec %>% 
  filter(INFO_TYPE %in% c('OW','BS','SD','DP')) %>% 
  rename(RSLT_LOCATION_HISTORY_ID=LOCATION_ID,
         RSLT_EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         RSLT_INFORMATION_TYPE=INFO_TYPE,
         RSLT_CHARACTERISTIC_NAME=Characteristic.Name,
         RSLT_RESULT_VALUE=Result.Value,
         RSLT_PROFILE_DEPTH=Depth,
         RSLT_RESULT_UNIT=Result.Unit,
         RSLT_RESULT_SAMPLE_FRACTION=Result.Sample.Fraction,
         RSLT_VALIDATOR_QUALIFIER=VALIDATOR_QUALIFIERS,
         RSLT_INTERPRETED_QUALIFIER=INTERPRETED_QUALIFIERS,
         RSLT_LABORATORY_NAME_CDE=LAB_NAME_CODE,
         RSLT_LABORATORY_QUALIFIER=LAB_QUALIFIERS,
         RSLT_QUANTITATION_LIMIT=QUANTITATION_LIMIT,
         RSLT_METHOD_DETECTION_LIMIT=METHOD_DETECTION_LIMIT,
         RSLT_DETECTION_QUANT_LMT_UNIT=Result.Detection.Quantitation.Limit.Unit,
         RSLT_ANALYTICAL_METHOD_ID=Result.Analytical.Method.ID,
         RSLT_COMMENT=Result.Comment,
         RSLT_FLAG=Flags,
         RSLT_DETECTION_CONDITION=Result.Detection.Condition,
         RSLT_LABORATORY_SAMPLE_NAME=Lab_Sample_Name,
         RSLT_DETECT_QUANT_LMT_MEASURE=Result.Detection.Quantitation.Limit.Measure,
         RSLT_VALUE_TYPE=Result.Value.Type,
         RSLT_METHOD_SPECIATION=Method.Speciation,
         RSLT_SERIAL_NUMBER=Serial,
         RSLT_DATE_ADDED=Results_DATE_ADDED)%>% 
  select(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_INFORMATION_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,RSLT_PROFILE_DEPTH,
         RSLT_RESULT_SAMPLE_FRACTION,RSLT_VALIDATOR_QUALIFIER,RSLT_INTERPRETED_QUALIFIER,RSLT_LABORATORY_NAME_CDE,
         RSLT_LABORATORY_QUALIFIER,RSLT_QUANTITATION_LIMIT,RSLT_METHOD_DETECTION_LIMIT,RSLT_DETECTION_QUANT_LMT_UNIT,
         RSLT_ANALYTICAL_METHOD_ID,RSLT_COMMENT,RSLT_FLAG,RSLT_DETECTION_CONDITION,RSLT_LABORATORY_SAMPLE_NAME,
         RSLT_DETECT_QUANT_LMT_MEASURE,RSLT_VALUE_TYPE,RSLT_METHOD_SPECIATION,RSLT_SERIAL_NUMBER,RSLT_DATE_ADDED) %>% 
  mutate(RSLT_LOCATION_HISTORY_ID=trimws(RSLT_LOCATION_HISTORY_ID),
         RSLT_EVENT_LMAS_SAMPLE_DATE=trimws(RSLT_EVENT_LMAS_SAMPLE_DATE),
         RSLT_INFORMATION_TYPE=trimws(RSLT_INFORMATION_TYPE),
         RSLT_CHARACTERISTIC_NAME=trimws(RSLT_CHARACTERISTIC_NAME),
         RSLT_RESULT_VALUE=trimws(RSLT_RESULT_VALUE),
         RSLT_RESULT_UNIT=trimws(RSLT_RESULT_UNIT),
         RSLT_PROFILE_DEPTH=trimws(RSLT_PROFILE_DEPTH),
         RSLT_RESULT_SAMPLE_FRACTION=trimws(RSLT_RESULT_SAMPLE_FRACTION),
         RSLT_VALIDATOR_QUALIFIER=trimws(RSLT_VALIDATOR_QUALIFIER),
         RSLT_INTERPRETED_QUALIFIER=trimws(RSLT_INTERPRETED_QUALIFIER),
         RSLT_LABORATORY_NAME_CDE=trimws(RSLT_LABORATORY_NAME_CDE),
         RSLT_LABORATORY_QUALIFIER=trimws(RSLT_LABORATORY_QUALIFIER),
         RSLT_QUANTITATION_LIMIT=trimws(RSLT_QUANTITATION_LIMIT),
         RSLT_METHOD_DETECTION_LIMIT=trimws(RSLT_METHOD_DETECTION_LIMIT),
         RSLT_DETECTION_QUANT_LMT_UNIT=trimws(RSLT_DETECTION_QUANT_LMT_UNIT),
         RSLT_ANALYTICAL_METHOD_ID=trimws(RSLT_ANALYTICAL_METHOD_ID),
         RSLT_COMMENT=trimws(RSLT_COMMENT),
         RSLT_FLAG=trimws(RSLT_FLAG),
         RSLT_DETECTION_CONDITION=trimws(RSLT_DETECTION_CONDITION),
         RSLT_LABORATORY_SAMPLE_NAME=trimws(RSLT_LABORATORY_SAMPLE_NAME),
         RSLT_DETECT_QUANT_LMT_MEASURE=trimws(RSLT_DETECT_QUANT_LMT_MEASURE),
         RSLT_VALUE_TYPE=trimws(RSLT_VALUE_TYPE),
         RSLT_METHOD_SPECIATION=trimws(RSLT_METHOD_SPECIATION),
         RSLT_SERIAL_NUMBER=trimws(RSLT_SERIAL_NUMBER),
         RSLT_DATE_ADDED=trimws(RSLT_DATE_ADDED)) %>% 
  mutate(RSLT_DATE_ADDED="2020-06-04") %>% 
  mutate(RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  distinct()
results<-merge(results,sei,by=c('RSLT_LOCATION_HISTORY_ID','RSLT_EVENT_LMAS_SAMPLE_DATE','RSLT_INFORMATION_TYPE'),all.x = TRUE)

write.csv(results, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.only.tables/results.csv", na = "", quote = TRUE, row.names = FALSE)
write.csv(results, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/results.csv", na = "", quote = TRUE, row.names = FALSE)

rm(list=setdiff(ls(), "data"))

####################################################################################################
####################################################################################################
#adding CSLAP results
results<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.only.tables/results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
results<-results %>% mutate(RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
#habs<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.only.tables/results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
#habs<-habs %>% mutate(RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
#results<-merge(results,habs,all=TRUE)
cslap<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/CSLAP/CSL2019_testresults.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

cslap<-cslap %>% 
  rename(SEI_SAMPLE_NAME=RSLT_SAMPLE_NAME,
         RSLT_EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         RSLT_SAMPLE_TYPE=SAMPLE_TYPE,
         RSLT_INFORMATION_TYPE=RSLT_INFORMATION_TYPE) %>% 
  select(SEI_SAMPLE_NAME,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_INFORMATION_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,
         RSLT_RESULT_SAMPLE_FRACTION,RSLT_VALIDATOR_QUALIFIER,RSLT_INTERPRETED_QUALIFIER,RSLT_LABORATORY_NAME_CDE,RSLT_LABORATORY_QUALIFIER,RSLT_QUANTITATION_LIMIT,RSLT_METHOD_DETECTION_LIMIT,
         RSLT_DETECTION_QUANT_LMT_UNIT,RSLT_ANALYTICAL_METHOD_ID,RSLT_COMMENT,RSLT_FLAG,
         RSLT_SAMPLE_TYPE) %>% 
  mutate(RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"))
cslap<-cslap %>% 
  mutate(RSLT_DETECTION_CONDITION=ifelse(RSLT_VALIDATOR_QUALIFIER=="U","Not Detected",NA),
         RSLT_DATE_ADDED="5/11/2020",
         RSLT_LABORATORY_SAMPLE_NAME=SEI_SAMPLE_NAME)

#add time
sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  select(SEI_SAMPLE_NAME,SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE) %>% 
  rename(RSLT_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         RSLT_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         RSLT_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         RSLT_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         RSLT_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         RSLT_SAMPLE_TYPE=SEI_SAMPLE_TYPE) %>% 
  distinct()
cslap<-merge(cslap,sei,by=c('SEI_SAMPLE_NAME','RSLT_EVENT_LMAS_SAMPLE_DATE','RSLT_INFORMATION_TYPE','RSLT_SAMPLE_TYPE'),all.x = TRUE)
cslap<-cslap %>% 
  select(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_INFORMATION_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,
         RSLT_RESULT_SAMPLE_FRACTION,RSLT_VALIDATOR_QUALIFIER,RSLT_INTERPRETED_QUALIFIER,RSLT_LABORATORY_NAME_CDE,RSLT_LABORATORY_QUALIFIER,RSLT_QUANTITATION_LIMIT,RSLT_METHOD_DETECTION_LIMIT,
         RSLT_DETECTION_QUANT_LMT_UNIT,RSLT_ANALYTICAL_METHOD_ID,RSLT_COMMENT,RSLT_FLAG,RSLT_DETECTION_CONDITION,RSLT_LABORATORY_SAMPLE_NAME,
         RSLT_DATE_ADDED,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_SAMPLE_TYPE) %>% 
  mutate(RSLT_PROFILE_DEPTH=NA,
         RSLT_SERIAL_NUMBER=NA,
         RSLT_METHOD_SPECIATION=NA,
         RSLT_VALUE_TYPE=NA,
         RSLT_DETECT_QUANT_LMT_MEASURE=NA) %>% 
  select(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_INFORMATION_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,RSLT_PROFILE_DEPTH,
         RSLT_RESULT_SAMPLE_FRACTION,RSLT_VALIDATOR_QUALIFIER,RSLT_INTERPRETED_QUALIFIER,RSLT_LABORATORY_NAME_CDE,RSLT_LABORATORY_QUALIFIER,RSLT_QUANTITATION_LIMIT,RSLT_METHOD_DETECTION_LIMIT,
         RSLT_DETECTION_QUANT_LMT_UNIT,RSLT_ANALYTICAL_METHOD_ID,RSLT_COMMENT,RSLT_FLAG,RSLT_DETECTION_CONDITION,RSLT_LABORATORY_SAMPLE_NAME,RSLT_DETECT_QUANT_LMT_MEASURE,
         RSLT_VALUE_TYPE,RSLT_METHOD_SPECIATION,RSLT_SERIAL_NUMBER,RSLT_DATE_ADDED,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_SAMPLE_TYPE)

#set fields to as character so they match:
cslap<-cslap %>% 
  mutate(RSLT_VALIDATOR_QUALIFIER=as.character(RSLT_VALIDATOR_QUALIFIER),
         RSLT_DATE_ADDED=as.character(RSLT_DATE_ADDED),
         RSLT_INTERPRETED_QUALIFIER=as.character(RSLT_INTERPRETED_QUALIFIER),
         RSLT_DETECTION_CONDITION=as.character(RSLT_DETECTION_CONDITION),
         RSLT_QUANTITATION_LIMIT=as.numeric(RSLT_QUANTITATION_LIMIT),
         RSLT_RESULT_SAMPLE_FRACTION=as.character(RSLT_RESULT_SAMPLE_FRACTION),
         RSLT_ANALYTICAL_METHOD_ID=as.character(RSLT_ANALYTICAL_METHOD_ID),
         RSLT_FLAG=as.logical(RSLT_FLAG),
         RSLT_COMMENT=as.logical(RSLT_COMMENT),
         RSLT_PROFILE_DEPTH=as.numeric(RSLT_PROFILE_DEPTH))

sapply(results,class)
sapply(cslap,class)
colnames(cslap)[colSums(is.na(cslap)) > 0]
#if time is one of the missing na columns then that means those records weren't in the SEI table
junk<-cslap %>% filter(is.na(RSLT_EVENT_LMAS_SAMPLE_TIME)) %>% distinct()
write.csv(junk,file="junk.stephs.results.for.missing.events.csv",row.names=FALSE)
junk<-cslap %>% filter(is.na(RSLT_EVENT_LMAS_SAMPLE_TIME)) %>% select(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_SAMPLE_TYPE,RSLT_INFORMATION_TYPE,RSLT_LABORATORY_SAMPLE_NAME) %>% distinct()
write.csv(junk,file="junk.stephs.missing.events.csv",row.names=FALSE)

results<-merge(results,cslap,all=TRUE)

write.csv(cslap, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.only.tables/results.csv", na = "", quote = TRUE, row.names = FALSE)
write.csv(results, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/results.csv", na = "", quote = TRUE, row.names = FALSE)

rm(list=setdiff(ls(), "data"))

####################################################################################################
####################################################################################################
#adding HABs results
results<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.only.tables/results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
results<-results %>% mutate(RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
cslap<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.only.tables/results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
cslap<-cslap %>% mutate(RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
results<-merge(results,cslap,all=TRUE)
rm(cslap)
habs<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/habs.results.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

habs<-habs %>% 
  rename(RSLT_LOCATION_HISTORY_ID=LOCATION_HISTORY_ID,
         RSLT_EVENT_LMAS_SAMPLE_DATE=LMAS_SAMPLE_DATE,
         RSLT_SAMPLE_TYPE=SAMPLE_TYPE,
         RSLT_INFORMATION_TYPE=RSLT_INFORMATION_TYPE,
         RSLT_EVENT_LMAS_DATA_PROVIDER=LMAS_DATA_PROVIDER) %>% 
  select(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_INFORMATION_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,RSLT_PROFILE_DEPTH,
         RSLT_RESULT_SAMPLE_FRACTION,RSLT_VALIDATOR_QUALIFIER,RSLT_INTERPRETED_QUALIFIER,RSLT_LABORATORY_NAME_CDE,RSLT_LABORATORY_QUALIFIER,RSLT_QUANTITATION_LIMIT,RSLT_METHOD_DETECTION_LIMIT,
         RSLT_DETECTION_QUANT_LMT_UNIT,RSLT_ANALYTICAL_METHOD_ID,RSLT_COMMENT,RSLT_FLAG,RSLT_DETECTION_CONDITION,RSLT_LABORATORY_SAMPLE_NAME,RSLT_DETECT_QUANT_LMT_MEASURE,
         RSLT_VALUE_TYPE,RSLT_METHOD_SPECIATION,RSLT_SERIAL_NUMBER,RSLT_DATE_ADDED,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_SAMPLE_TYPE) %>% 
  mutate(RSLT_DATE_ADDED="2020-06-04") %>% 
  mutate(RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
#add time
sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE) %>% 
  rename(RSLT_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         RSLT_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         RSLT_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         RSLT_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         RSLT_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         RSLT_SAMPLE_TYPE=SEI_SAMPLE_TYPE) %>% 
  distinct()
habs<-merge(habs,sei,by=c('RSLT_LOCATION_HISTORY_ID','RSLT_EVENT_LMAS_SAMPLE_DATE','RSLT_INFORMATION_TYPE','RSLT_EVENT_LMAS_DATA_PROVIDER','RSLT_SAMPLE_TYPE'),all.x = TRUE)
habs<-habs %>% 
  select(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_INFORMATION_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,RSLT_PROFILE_DEPTH,
         RSLT_RESULT_SAMPLE_FRACTION,RSLT_VALIDATOR_QUALIFIER,RSLT_INTERPRETED_QUALIFIER,RSLT_LABORATORY_NAME_CDE,RSLT_LABORATORY_QUALIFIER,RSLT_QUANTITATION_LIMIT,RSLT_METHOD_DETECTION_LIMIT,
         RSLT_DETECTION_QUANT_LMT_UNIT,RSLT_ANALYTICAL_METHOD_ID,RSLT_COMMENT,RSLT_FLAG,RSLT_DETECTION_CONDITION,RSLT_LABORATORY_SAMPLE_NAME,RSLT_DETECT_QUANT_LMT_MEASURE,
         RSLT_VALUE_TYPE,RSLT_METHOD_SPECIATION,RSLT_SERIAL_NUMBER,RSLT_DATE_ADDED,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_SAMPLE_TYPE)

#set fields to as character so they match:
habs<-habs %>% 
  mutate(RSLT_VALIDATOR_QUALIFIER=as.character(RSLT_VALIDATOR_QUALIFIER),
         RSLT_DATE_ADDED=as.character(RSLT_DATE_ADDED),
         RSLT_INTERPRETED_QUALIFIER=as.character(RSLT_INTERPRETED_QUALIFIER),
         RSLT_DETECTION_CONDITION=as.character(RSLT_DETECTION_CONDITION),
         RSLT_QUANTITATION_LIMIT=as.numeric(RSLT_QUANTITATION_LIMIT),
         RSLT_PROFILE_DEPTH=as.numeric(RSLT_PROFILE_DEPTH),
         RSLT_RESULT_SAMPLE_FRACTION=as.character(RSLT_RESULT_SAMPLE_FRACTION),
         RSLT_ANALYTICAL_METHOD_ID=as.character(RSLT_ANALYTICAL_METHOD_ID),
         RSLT_FLAG=as.logical(RSLT_FLAG))

sapply(results,class)
sapply(habs,class)
colnames(habs)[colSums(is.na(habs)) > 0]
#if time is one of the missing na columns then that means those records weren't in the SEI table
#if time is one of the missing na columns then that means those records weren't in the SEI table
junk<-habs %>% filter(is.na(RSLT_EVENT_LMAS_SAMPLE_TIME)) %>% distinct()
write.csv(junk,file="junk.stephs.results.for.missing.events.csv",row.names=FALSE)
junk<-habs %>% filter(is.na(RSLT_EVENT_LMAS_SAMPLE_TIME)) %>% select(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_SAMPLE_TYPE,RSLT_INFORMATION_TYPE,RSLT_LABORATORY_SAMPLE_NAME) %>% distinct()
write.csv(junk,file="junk.stephs.missing.events.csv",row.names=FALSE)

write.csv(habs, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.only.tables/results.csv", na = "", quote = TRUE, row.names = FALSE)
results<-merge(results,habs,all=TRUE)
write.csv(results, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/results.csv", na = "", quote = TRUE, row.names = FALSE)

rm(list=setdiff(ls(), "data"))


#adding pcode table
####################################################################################################
####################################################################################################
####################################################################################################
results<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
backupresults<-results

#identify in situ parameters based on presence of profile depth
results<-backupresults
results<-results %>% 
  mutate(RESULT_TYPE=ifelse(is.na(RSLT_PROFILE_DEPTH),"lab","in-situ" ),
         RESULT_TYPE=ifelse(RSLT_CHARACTERISTIC_NAME=="DEPTH, SECCHI DISK DEPTH","in-situ",RESULT_TYPE),
         RSLT_RESULT_UNIT=tolower(RSLT_RESULT_UNIT),
         RSLT_RESULT_UNIT=ifelse(RSLT_CHARACTERISTIC_NAME=="CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)","rfu",RSLT_RESULT_UNIT),
         RSLT_RESULT_UNIT=ifelse(RSLT_CHARACTERISTIC_NAME=="PHYCOCYANIN (PROBE RELATIVE FLUORESCENCE)","rfu",RSLT_RESULT_UNIT),
         RSLT_CHARACTERISTIC_NAME=ifelse(RSLT_CHARACTERISTIC_NAME=="CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)","CHLOROPHYLL A (PROBE)",RSLT_CHARACTERISTIC_NAME),
         RSLT_CHARACTERISTIC_NAME=ifelse(RSLT_CHARACTERISTIC_NAME=="PHYCOCYANIN (PROBE RELATIVE FLUORESCENCE)","PHYCOCYANIN (PROBE)",RSLT_CHARACTERISTIC_NAME),
         RSLT_RESULT_SAMPLE_FRACTION=ifelse(RSLT_CHARACTERISTIC_NAME=="CHLOROPHYLL A (PROBE)",NA,RSLT_RESULT_SAMPLE_FRACTION),
         RSLT_CHARACTERISTIC_NAME=ifelse(RSLT_CHARACTERISTIC_NAME=="CHLOROPHYLL A (PROBE) CONCENTRATION","CHLOROPHYLL A (PROBE)",RSLT_CHARACTERISTIC_NAME),
         RSLT_CHARACTERISTIC_NAME=ifelse(RSLT_CHARACTERISTIC_NAME=="BMAA","BMAA (BETA-METHYL-AMINO-(L)-ALANINE)",RSLT_CHARACTERISTIC_NAME),
         RSLT_CHARACTERISTIC_NAME=ifelse(RSLT_CHARACTERISTIC_NAME=="ANATOXIN","ANATOXIN-A",RSLT_CHARACTERISTIC_NAME),
         RSLT_RESULT_UNIT=ifelse(RSLT_RESULT_UNIT=="us/cm","uS/cm",RSLT_RESULT_UNIT))


#converting streams files to lakes names     
#pcode<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/dec_pcode_join_2020-08-24.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
#pcode<-pcode %>% 
#  rename(RSLT_RESULT_SAMPLE_FRACTION=fraction,
#         RSLT_RESULT_UNIT=result_unit,
#         RSLT_CHARACTERISTIC_NAME=chemical_name,
#         DEC_pcode=DEC_pcode_FINAL,
#         RSLT_RESULT_TYPE=RESULT_TYPE) %>% 
#  mutate(RSLT_CHARACTERISTIC_NAME=toupper(RSLT_CHARACTERISTIC_NAME),
#         RSLT_CHARACTERISTIC_NAME=ifelse(RSLT_CHARACTERISTIC_NAME=="CHLOROPHYLL A (PROBE) CONCENTRATION, TOTAL","CHLOROPHYLL A (PROBE)",RSLT_CHARACTERISTIC_NAME),
#         DEC_pcode = sprintf("%03d", DEC_pcode),
#         DEC_pcode = as.character(DEC_pcode),
#         RSLT_RESULT_UNIT=ifelse(RSLT_RESULT_UNIT=="us/cm","uS/cm",RSLT_RESULT_UNIT)) %>% 
#  bind_rows(c(RSLT_RESULT_TYPE="lab",RSLT_CHARACTERISTIC_NAME="SPECIFIC CONDUCTANCE",RSLT_RESULT_SAMPLE_FRACTION=NA,RSLT_RESULT_UNIT="uS/cm",DEC_pcode="139")) %>% 
#  bind_rows(c(RSLT_RESULT_TYPE="lab",RSLT_CHARACTERISTIC_NAME="PH",RSLT_RESULT_SAMPLE_FRACTION=NA,RSLT_RESULT_UNIT="ph units",DEC_pcode="110")) 
#write.csv(pcode,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/dec_pcode.csv", na = "", quote = TRUE, row.names = FALSE)

pcode<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/dec_pcode.csv",na.strings=c("","NA"), stringsAsFactors=FALSE,)
pcode<-pcode %>% 
  select(RSLT_RESULT_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_SAMPLE_FRACTION,RSLT_RESULT_UNIT,DEC_pcode) %>% 
  rename(RESULT_TYPE=RSLT_RESULT_TYPE)

results<-merge(results,pcode,by=c("RESULT_TYPE","RSLT_CHARACTERISTIC_NAME","RSLT_RESULT_SAMPLE_FRACTION","RSLT_RESULT_UNIT"),all.x=TRUE)


#check those that didn't merge
results %>% filter(is.na(DEC_pcode)) %>% select(RESULT_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_SAMPLE_FRACTION,RSLT_RESULT_UNIT) %>% distinct()

write.csv(results, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/results.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=setdiff(ls(), "data"))


####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#check tables merge with upstream tables
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
results<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
SEI<-SEI %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(RSLT_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         RSLT_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         RSLT_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         RSLT_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         RSLT_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_NAME=SEI_SAMPLE_NAME,
         RSLT_SAMPLE_TYPE=SEI_SAMPLE_TYPE)
results<-results %>% 
  mutate(RSLT_LOCATION_HISTORY_ID=as.character(RSLT_LOCATION_HISTORY_ID),
         RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         RSLT_EVENT_LMAS_DATA_PROVIDER=as.character(RSLT_EVENT_LMAS_DATA_PROVIDER),
         RSLT_EVENT_LMAS_SAMPLE_TIME=as.character(RSLT_EVENT_LMAS_SAMPLE_TIME),
         RSLT_SAMPLE_TYPE=as.character(RSLT_SAMPLE_TYPE),
         RSLT_INFORMATION_TYPE=as.character(RSLT_INFORMATION_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_SAMPLE_TYPE,RSLT_INFORMATION_TYPE) %>% 
  mutate(event="this")
results<-results %>% 
  arrange(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_SAMPLE_TYPE,RSLT_INFORMATION_TYPE)


junk<-merge(results,SEI,by=c('RSLT_LOCATION_HISTORY_ID','RSLT_EVENT_LMAS_SAMPLE_DATE','RSLT_EVENT_LMAS_DATA_PROVIDER','RSLT_EVENT_LMAS_SAMPLE_TIME','RSLT_SAMPLE_TYPE','RSLT_INFORMATION_TYPE'),all=TRUE)
junk<-junk %>% filter(is.na(event))

#check for fields with NA records
#only these fields should have NA: 
#               "RSLT_RESULT_VALUE"             "RSLT_RESULT_UNIT"              "RSLT_PROFILE_DEPTH"            "RSLT_RESULT_SAMPLE_FRACTION"   "RSLT_VALIDATOR_QUALIFIER,RSLT_INTERPRETED_QUALIFIER"   
#[7] "RSLT_LABORATORY_NAME_CDE"      "RSLT_LABORATORY_QUALIFIER"     "RSLT_QUANTITATION_LIMIT"       "RSLT_METHOD_DETECTION_LIMIT"   "RSLT_DETECTION_QUANT_LMT_UNIT" "RSLT_ANALYTICAL_METHOD_ID"    
#[13] "RSLT_COMMENT"                  "RSLT_FLAG"                     "RSLT_DETECTION_CONDITION"      "RSLT_LABORATORY_SAMPLE_NAME"   "RSLT_DETECT_QUANT_LMT_MEASURE" "RSLT_VALUE_TYPE"              
#[19] "RSLT_METHOD_SPECIATION"        "RSLT_SERIAL_NUMBER"            "RSLT_DATE_ADDED" 
colnames(results)[colSums(is.na(results)) > 0]

rm(list=setdiff(ls(), "data"))




####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#checking ITS files
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
rm(list=setdiff(ls(), "data"))

results<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
results<-results %>% 
  mutate(RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  mutate(RSLT_RESULT_VALUE=as.numeric(RSLT_RESULT_VALUE)) %>% 
  rename(RSLT_DEC_PCODE=DEC_pcode,
         RSLT_RESULT_TYPE=RESULT_TYPE) %>% 
  select(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_INFORMATION_TYPE,
         RSLT_SAMPLE_TYPE,RSLT_METHOD_SPECIATION,RSLT_RESULT_VALUE,RSLT_CHARACTERISTIC_NAME,
         RSLT_RESULT_UNIT,RSLT_RESULT_SAMPLE_FRACTION,RSLT_INTERPRETED_QUALIFIER,RSLT_VALIDATOR_QUALIFIER,
         RSLT_LABORATORY_NAME_CDE,RSLT_LABORATORY_SAMPLE_NAME,RSLT_QUANTITATION_LIMIT,RSLT_LABORATORY_QUALIFIER,RSLT_METHOD_DETECTION_LIMIT,
         RSLT_DETECTION_QUANT_LMT_UNIT,RSLT_ANALYTICAL_METHOD_ID,RSLT_DETECTION_CONDITION,RSLT_DETECT_QUANT_LMT_MEASURE,RSLT_VALUE_TYPE,RSLT_PROFILE_DEPTH,
         RSLT_SERIAL_NUMBER,RSLT_FLAG,RSLT_DEC_PCODE,RSLT_RESULT_TYPE,RSLT_DATE_ADDED,RSLT_COMMENT) %>% 
  arrange(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_INFORMATION_TYPE,
          RSLT_SAMPLE_TYPE,RSLT_METHOD_SPECIATION,RSLT_RESULT_VALUE,RSLT_CHARACTERISTIC_NAME,
          RSLT_RESULT_UNIT,RSLT_RESULT_SAMPLE_FRACTION,RSLT_INTERPRETED_QUALIFIER,RSLT_VALIDATOR_QUALIFIER,
          RSLT_LABORATORY_NAME_CDE,RSLT_LABORATORY_SAMPLE_NAME,RSLT_QUANTITATION_LIMIT,RSLT_LABORATORY_QUALIFIER,RSLT_METHOD_DETECTION_LIMIT,
          RSLT_DETECTION_QUANT_LMT_UNIT,RSLT_ANALYTICAL_METHOD_ID,RSLT_DETECTION_CONDITION,RSLT_DETECT_QUANT_LMT_MEASURE,RSLT_VALUE_TYPE,RSLT_PROFILE_DEPTH,
          RSLT_SERIAL_NUMBER,RSLT_FLAG,RSLT_DEC_PCODE,RSLT_RESULT_TYPE,RSLT_DATE_ADDED,RSLT_COMMENT)



ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_RESULT.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS <-ITS %>% 
  select(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_INFORMATION_TYPE,
         RSLT_SAMPLE_TYPE,RSLT_METHOD_SPECIATION,RSLT_RESULT_VALUE,RSLT_CHARACTERISTIC_NAME,
         RSLT_RESULT_UNIT,RSLT_RESULT_SAMPLE_FRACTION,RSLT_INTERPRETED_QUALIFIER,RSLT_VALIDATOR_QUALIFIER,
         RSLT_LABORATORY_NAME_CDE,RSLT_LABORATORY_SAMPLE_NAME,RSLT_QUANTITATION_LIMIT,RSLT_LABORATORY_QUALIFIER,RSLT_METHOD_DETECTION_LIMIT,
         RSLT_DETECTION_QUANT_LMT_UNIT,RSLT_ANALYTICAL_METHOD_ID,RSLT_DETECTION_CONDITION,RSLT_DETECT_QUANT_LMT_MEASURE,RSLT_VALUE_TYPE,RSLT_PROFILE_DEPTH,
         RSLT_SERIAL_NUMBER,RSLT_FLAG,RSLT_DEC_PCODE,RSLT_RESULT_TYPE,RSLT_DATE_ADDED,RSLT_COMMENT) %>%  
  mutate(RSLT_EVENT_LMAS_SAMPLE_DATE=as.Date(RSLT_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y")) %>% 
  arrange(RSLT_LOCATION_HISTORY_ID,RSLT_EVENT_LMAS_DATA_PROVIDER,RSLT_EVENT_LMAS_SAMPLE_DATE,RSLT_EVENT_LMAS_SAMPLE_TIME,RSLT_INFORMATION_TYPE,
          RSLT_SAMPLE_TYPE,RSLT_METHOD_SPECIATION,RSLT_RESULT_VALUE,RSLT_CHARACTERISTIC_NAME,
          RSLT_RESULT_UNIT,RSLT_RESULT_SAMPLE_FRACTION,RSLT_INTERPRETED_QUALIFIER,RSLT_VALIDATOR_QUALIFIER,
          RSLT_LABORATORY_NAME_CDE,RSLT_LABORATORY_SAMPLE_NAME,RSLT_QUANTITATION_LIMIT,RSLT_LABORATORY_QUALIFIER,RSLT_METHOD_DETECTION_LIMIT,
          RSLT_DETECTION_QUANT_LMT_UNIT,RSLT_ANALYTICAL_METHOD_ID,RSLT_DETECTION_CONDITION,RSLT_DETECT_QUANT_LMT_MEASURE,RSLT_VALUE_TYPE,RSLT_PROFILE_DEPTH,
          RSLT_SERIAL_NUMBER,RSLT_FLAG,RSLT_DEC_PCODE,RSLT_RESULT_TYPE,RSLT_DATE_ADDED,RSLT_COMMENT)

#test pcode merge
pcode<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/coded.tables/L_PCODE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
pcode<-pcode %>% 
  select(PCODE_RSLT_CHAR_NAME,PCODE_RSLT_SAMPLE_FRACTION,PCODE_RSLT_UNIT,PCODE_RSLT_TYPE,PCODE_DEC) %>% 
  rename(RSLT_CHARACTERISTIC_NAME=PCODE_RSLT_CHAR_NAME,
         RSLT_RESULT_SAMPLE_FRACTION=PCODE_RSLT_SAMPLE_FRACTION,
         RSLT_RESULT_UNIT=PCODE_RSLT_UNIT,
         RSLT_RESULT_TYPE=PCODE_RSLT_TYPE,
         RSLT_DEC_PCODE=PCODE_DEC) 
pcode$test<-"junk"
junk<-merge(ITS,pcode,by=c('RSLT_DEC_PCODE','RSLT_CHARACTERISTIC_NAME','RSLT_RESULT_SAMPLE_FRACTION','RSLT_RESULT_UNIT','RSLT_RESULT_TYPE'),all.x = TRUE)
unique(junk$test)


#check class
sapply(results,class)
sapply(ITS,class)

#Convert to only 4 sig dig since a diff number are preserved in Cindy's database
results <-results %>% 
  mutate(RSLT_RESULT_VALUE=signif(RSLT_RESULT_VALUE,digits=4))
ITS <-ITS %>% 
           mutate(RSLT_RESULT_VALUE=signif(RSLT_RESULT_VALUE,digits=4))
                  
#check if identical
identical(results,ITS)

#check for differences between specific columns
test <- lapply(names(results), function(name.i){
  anti_join(results, ITS, by = name.i)
})
names(test) <- names(results)
#test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS,results, by = name.i)
})
names(test2) <- names(ITS)
#test

#NOTES
#The result values are rounded to 5 sig digs where as our data can be more than that
#

rm(list=setdiff(ls(), "data"))
