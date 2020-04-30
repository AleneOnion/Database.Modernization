#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")


#Alene Onion
#Feb 2020
#Script to generate a clean lci downstream tables

####################################################################################################
#Reading in Data tables
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
  mutate(EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE) %>% 
  rename(RSLT_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         RSLT_INFORMATION_TYPE=SEI_INFORMATION_TYPE) %>% 
  distinct()

results<-rec %>% 
  filter(INFO_TYPE %in% c('OW','BS','SD','DP')) %>% 
  rename(LOCATION_HISTORY_ID=LOCATION_ID,
         EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
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
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,RSLT_INFORMATION_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,RSLT_PROFILE_DEPTH,
         RSLT_RESULT_SAMPLE_FRACTION,RSLT_VALIDATOR_QUALIFIER,RSLT_INTERPRETED_QUALIFIER,RSLT_LABORATORY_NAME_CDE,
         RSLT_LABORATORY_QUALIFIER,RSLT_QUANTITATION_LIMIT,RSLT_METHOD_DETECTION_LIMIT,RSLT_DETECTION_QUANT_LMT_UNIT,
         RSLT_ANALYTICAL_METHOD_ID,RSLT_COMMENT,RSLT_FLAG,RSLT_DETECTION_CONDITION,RSLT_LABORATORY_SAMPLE_NAME,
         RSLT_DETECT_QUANT_LMT_MEASURE,RSLT_VALUE_TYPE,RSLT_METHOD_SPECIATION,RSLT_SERIAL_NUMBER,RSLT_DATE_ADDED) %>% 
  mutate(LOCATION_HISTORY_ID=trimws(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=trimws(EVENT_LMAS_SAMPLE_DATE),
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
  mutate(EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  distinct()
results<-merge(results,sei,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE','RSLT_INFORMATION_TYPE'),all.x = TRUE)

write.csv(results, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/results.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=c('field','junk','macro','rec','results','sei','SEI','wc'))

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
  mutate(LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER),
         EVENT_LMAS_TIME=as.character(EVENT_LMAS_TIME),
         SEI_SAMPLE_TYPE=as.character(SEI_SAMPLE_TYPE),
         SEI_INFORMATION_TYPE=as.character(SEI_INFORMATION_TYPE)) %>% 
  rename(RSLT_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         RSLT_INFORMATION_TYPE=SEI_INFORMATION_TYPE)
results<-results %>% 
  mutate(LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER),
         EVENT_LMAS_TIME=as.character(EVENT_LMAS_TIME),
         RSLT_SAMPLE_TYPE=as.character(RSLT_SAMPLE_TYPE),
         RSLT_INFORMATION_TYPE=as.character(RSLT_INFORMATION_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_TIME,RSLT_SAMPLE_TYPE,RSLT_INFORMATION_TYPE) %>% 
  mutate(event="this")
results<-results %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_TIME,RSLT_SAMPLE_TYPE,RSLT_INFORMATION_TYPE)


junk<-merge(results,SEI,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE','EVENT_LMAS_PROJECT_CDE','EVENT_LMAS_DATA_PROVIDER','EVENT_LMAS_TIME','RSLT_SAMPLE_TYPE','RSLT_INFORMATION_TYPE'),all=TRUE)
junk<-junk %>% filter(is.na(event))

#check for fields with NA records
#only these fields should have NA: 
#               "EVENT_LMAS_PROJECT_CDE" 
#[1] "RSLT_RESULT_VALUE"             "RSLT_RESULT_UNIT"              "RSLT_PROFILE_DEPTH"            "RSLT_RESULT_SAMPLE_FRACTION"   "RSLT_VALIDATOR_QUALIFIER"      "RSLT_INTERPRETED_QUALIFIER"   
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
