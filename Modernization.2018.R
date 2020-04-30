#Alene Onion
#Feb 2020
#Script to generate a clean lci downstream tables

####################################################################################################
#Reading in Data tables
#read data tables
lake<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/Lake.Master.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
profiles<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/DEPTH.PROFILE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
location<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/Location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
results<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/Test.Results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sample<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/Sample.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
habs<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/HABstatus.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#Merge data tables
data<-merge(sample,results,by=c('SAMPLE_ID','SAMPLE_NAME','INFO_TYPE'),all.x=TRUE)
data<-data %>% filter(!is.na(Characteristic.Name))
data2<-merge(sample,profiles,by=c("SAMPLE_ID",'INFO_TYPE'),all.x=TRUE)
data2<-data2 %>% filter(!is.na(Characteristic.Name))
data<-merge(data,data2,by=c("LAKE_ID","LOCATION_ID","SAMPLE_ID","SAMPLE_NAME","DATA_PROVIDER","INFO_TYPE","SAMPLE_DATE",
                            "TIME","START_DEPTH","END_DEPTH","ESF.","UFI.","REMARK","BLOOM_LOC",
                            "WIND_DIR","WIND_INT","EXTENT","BLOOM_DESC","EQUIPMENT_DESC","EQUIPMENT_TYPE","LAKE_PROJ_CODE",
                            "SAMPLE_METHOD","PERCENT","DEPTH_UNIT","Characteristic.Name","Result.Value","Result.Unit"),all=TRUE)
rm(data2)
data<-merge(data,sample,by=c("LAKE_ID","LOCATION_ID","SAMPLE_ID","SAMPLE_NAME","DATA_PROVIDER","INFO_TYPE","SAMPLE_DATE",
                             "TIME","START_DEPTH","END_DEPTH","ESF.","UFI.","REMARK","BLOOM_LOC",
                             "WIND_DIR","WIND_INT","EXTENT","BLOOM_DESC","EQUIPMENT_DESC","EQUIPMENT_TYPE","LAKE_PROJ_CODE",
                             "SAMPLE_METHOD","PERCENT","DEPTH_UNIT"),all=TRUE)
data<-merge(data,lake,by=c('LAKE_ID'),all=TRUE)
data<-merge(data,location,by=c('LOCATION_ID','LAKE_ID'),all = TRUE)
data<-merge(data,habs, by=c('SAMPLE_ID'),all.x = TRUE)

#remove individual data tables in working environment
rm(list = c('lake','profiles','location','results','sample','habs'))
#Make sure records are distinct
library(dplyr)
data<-distinct(data)
####################################################################################################
####################################################################################################
library(dplyr)
library(tidyr)
rec<-data
#rec$SAMPLE_DATE<-as.Date(rec$SAMPLE_DATE,format="%m/%d/%Y")
rec<-rec %>% 
  filter(SAMPLE_DATE>'2018-01-01',
         SAMPLE_DATE<'2019-01-01',
         Characteristic.Name %in% c('ALKALINITY, TOTAL (AS CACO3)','AMMONIA','ARSENIC','CHLOROPHYLL A','CHLOROPHYLL A (PROBE)','DEPTH, SECCHI DISK DEPTH','DISSOLVED ORGANIC CARBON','DISSOLVED OXYGEN (DO)','DISSOLVED OXYGEN SATURATION','IRON','MANGANESE','NITRITE','NITROGEN, KJELDAHL, TOTAL','NITROGEN, NITRATE (AS N)','NITROGEN, NITRATE-NITRITE','OXIDATION REDUCTION POTENTIAL (ORP)','PH','PH FOR COLOR ANALYSIS','PHOSPHORUS','SPECIFIC CONDUCTANCE','SULFATE (AS SO4)','TEMPERATURE, WATER','TOTAL ORGANIC CARBON','TRUE COLOR','UV 254'),
         DATA_PROVIDER=="LCI") %>% 
  distinct()

####################################################################################################
####################################################################################################
#LCI EVENT TABLE
#location
#date
#project_code
#data_provider

#removed:
EVENT_LMAS<-rec %>%
  filter(INFO_TYPE %in% c("OW","BS","SD","DP")) %>% 
  mutate(INFO_TYPE=factor(INFO_TYPE, levels = c("OW","BS","SD","DP"))) %>% 
  arrange(INFO_TYPE) %>% 
  distinct(LOCATION_ID,SAMPLE_DATE,.keep_all = TRUE) %>% 
  select(LOCATION_ID,SAMPLE_DATE,LAKE_PROJ_CODE,DATA_PROVIDER,TIME) %>% 
  distinct()
#check, there shouldn't be multiple samples per location/date
check<-EVENT_LMAS %>% 
  group_by(LOCATION_ID,SAMPLE_DATE) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  filter(n>2) %>% 
  distinct()

EVENT_LMAS<-EVENT_LMAS %>% 
  rename(EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         EVENT_LMAS_TIME=TIME,
         EVENT_LMAS_PROJECT_CDE=LAKE_PROJ_CODE,
         EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER) %>% 
  distinct()
write.csv(EVENT_LMAS, file="ITS.fixed.tables/event2018.csv", na = "", quote = TRUE, row.names = FALSE)
#rm(list=c('rec','check'))

####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#SAMPLE_EVENT_INFORMATION 
#EVENT_LMAS_ID - for this table it's LOCATION_ID,SAMPLE_DATE,LAKE_PROJ_CODE,DATA_PROVIDER
#SAMPLE_TYPE:
#Water Column
#Macrophyte
#HAB

#INFO_TYPE:
#OW
#BS
#SD
#DP
#MP
#SB
#SR
#RT

#######################
#LCI
his_lci<-rec %>% 
  filter(INFO_TYPE %in% c("OW","BS","SD","DP")) %>% 
  select(LOCATION_ID,SAMPLE_DATE,LAKE_PROJ_CODE,DATA_PROVIDER,INFO_TYPE,SAMPLE_ID,Characteristic.Name,SAMPLE_NAME) %>% 
  distinct() 
#add sample type
his_lci<-his_lci %>% 
  select(LOCATION_ID,SAMPLE_DATE,LAKE_PROJ_CODE,DATA_PROVIDER,INFO_TYPE,SAMPLE_NAME,SAMPLE_ID) %>% 
  distinct() %>% 
  mutate(SAMPLE_TYPE=NA,
         SAMPLE_TYPE=ifelse(INFO_TYPE=='OW','WATER COLUMN',SAMPLE_TYPE),
         SAMPLE_TYPE=ifelse(INFO_TYPE=='SD','WATER COLUMN',SAMPLE_TYPE),
         SAMPLE_TYPE=ifelse(INFO_TYPE=='DP','WATER COLUMN',SAMPLE_TYPE),
         SAMPLE_TYPE=ifelse(INFO_TYPE=='BS','WATER COLUMN',SAMPLE_TYPE),
         SAMPLE_TYPE=ifelse(INFO_TYPE=='MP','MACROPHYTE',SAMPLE_TYPE)) %>% 
  select(LOCATION_ID,SAMPLE_DATE,LAKE_PROJ_CODE,DATA_PROVIDER,SAMPLE_TYPE,INFO_TYPE,SAMPLE_NAME,SAMPLE_ID) %>% 
  distinct()
#his_lci<-merge(his_lci,habs,by=c('SAMPLE_ID'),all.x=TRUE)
his_lci<-his_lci %>% 
  #  mutate(SAMPLE_TYPE=ifelse(habs="habs","HAB",SAMPLE_TYPE)) %>% 
  select(LOCATION_ID,SAMPLE_DATE,LAKE_PROJ_CODE,DATA_PROVIDER,SAMPLE_TYPE,INFO_TYPE,SAMPLE_NAME) %>%
  distinct()

#add purpose
purpose<-read.csv("Event.Purpose.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
purpose<-purpose %>% 
  select(LOCATION_ID,PURPOSE) %>% 
  distinct()
his_lci<-merge(his_lci,purpose,by=c('LOCATION_ID'),all.x = TRUE)


#check, there should be 4 sample types per 
check<-his_lci %>% 
  select(LOCATION_ID,SAMPLE_DATE,LAKE_PROJ_CODE,DATA_PROVIDER,SAMPLE_TYPE,INFO_TYPE) %>%
  mutate(types=paste(SAMPLE_TYPE,INFO_TYPE,sep=" - ")) %>% 
  select(LOCATION_ID,SAMPLE_DATE,LAKE_PROJ_CODE,DATA_PROVIDER,types) %>%
  group_by(LOCATION_ID,SAMPLE_DATE) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  #  filter(n<3) %>% 
  distinct() %>% 
  arrange(LOCATION_ID,SAMPLE_DATE,types) %>% 
  spread(types,n)
nms<-his_lci %>% 
  filter(INFO_TYPE=="DP") %>% 
  select(SAMPLE_NAME,LOCATION_ID,SAMPLE_DATE) %>% 
  distinct()
check<-merge(check,nms,by=c("LOCATION_ID","SAMPLE_DATE"),all.x = TRUE)
checkmerge<-his_lci %>% 
  select(LOCATION_ID,SAMPLE_DATE,INFO_TYPE,SAMPLE_NAME) %>% 
  filter(INFO_TYPE=="OW") %>% 
  distinct()
write.csv(check,file="junk.check2.csv",row.names=FALSE)
write.csv(checkmerge,file="junk.checknames.csv",row.names=FALSE)

#write.file
his_lci_ITS<-his_lci %>% 
  rename(LOCATION_HISTORY_ID=LOCATION_ID,
         EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         EVENT_LMAS_PROJECT_CODE=LAKE_PROJ_CODE,
         EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         SAMPLE_TYPE=SAMPLE_TYPE,
         INFO_TYPE=INFO_TYPE,
         SEI_SAMPLE_NAME=SAMPLE_NAME) %>% 
  distinct()
write.csv(his_lci_ITS, file="ITS.fixed.tables/sample_event_information.2018.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=c('check','checkmerge','his_lci','his_lci_ITS'))






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
#Results table
results<-rec %>% 
  filter(INFO_TYPE %in% c('OW','BS','SD','DP')) %>% 
  rename(LOCATION_HISTORY_ID=LOCATION_ID,
         EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         EVENT_LMAS_TIME=TIME,
         RSLT_INFORMATION_TYPE=INFO_TYPE,
         RSLT_SAMPLE_NAME=SAMPLE_NAME,
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
  mutate(SAMPLE_TYPE="WATER COLUMN",
         EVENT_LMAS_DATA_PROVIDER="LCI",
         EVENT_LMAS_PROJECT_CDE=NA) %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_TIME,SAMPLE_TYPE,EVENT_LMAS_DATA_PROVIDER,
         EVENT_LMAS_PROJECT_CDE,RSLT_INFORMATION_TYPE,RSLT_CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,RSLT_PROFILE_DEPTH,
         RSLT_RESULT_SAMPLE_FRACTION,RSLT_VALIDATOR_QUALIFIER,RSLT_INTERPRETED_QUALIFIER,RSLT_LABORATORY_NAME_CDE,
         RSLT_LABORATORY_QUALIFIER,RSLT_QUANTITATION_LIMIT,RSLT_METHOD_DETECTION_LIMIT,RSLT_DETECTION_QUANT_LMT_UNIT,
         RSLT_ANALYTICAL_METHOD_ID,RSLT_COMMENT,RSLT_FLAG,RSLT_DETECTION_CONDITION,RSLT_LABORATORY_SAMPLE_NAME,
         RSLT_DETECT_QUANT_LMT_MEASURE,RSLT_VALUE_TYPE,RSLT_METHOD_SPECIATION,RSLT_SERIAL_NUMBER,RSLT_DATE_ADDED) %>% 
  mutate(LOCATION_HISTORY_ID=trimws(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=trimws(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_TIME=trimws(EVENT_LMAS_TIME),
         SAMPLE_TYPE=trimws(SAMPLE_TYPE),
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
  distinct()

write.csv(results, file="ITS.fixed.tables/results.csv", na = "", quote = TRUE, row.names = FALSE)

#generate profile data table
profile<-rec %>% 
  rename(LOCATION_HISTORY_ID=LOCATION_ID,
         EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         EVENT_LMAS_TIME=TIME,
         PROFILE_INFORMATION_TYPE=INFO_TYPE,
         PROFILE_DATE_ADDED=Results_DATE_ADDED,
         PROFILE_EQUIPMENT_TYPE=EQUIPMENT_TYPE,
         PROFILE_EQUIPMENT_DESC=EQUIPMENT_DESC)%>% 
  filter(PROFILE_INFORMATION_TYPE=="DP") %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER,PROFILE_INFORMATION_TYPE,
         PROFILE_EQUIPMENT_TYPE,PROFILE_EQUIPMENT_DESC,PROFILE_DATE_ADDED) %>% 
  distinct() %>% 
  mutate(LOCATION_HISTORY_ID=trimws(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=trimws(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_DATA_PROVIDER=trimws(EVENT_LMAS_DATA_PROVIDER),
         PROFILE_INFORMATION_TYPE=trimws(PROFILE_INFORMATION_TYPE),
         PROFILE_EQUIPMENT_TYPE=trimws(PROFILE_EQUIPMENT_TYPE),
         PROFILE_EQUIPMENT_DESC=trimws(PROFILE_EQUIPMENT_DESC),
         PROFILE_DATE_ADDED=trimws(PROFILE_DATE_ADDED)) %>% 
  mutate(SAMPLE_TYPE="WATER COLUMN",
         EVENT_LMAS_PROJECT_CDE=NA)  
write.csv(profile, file="ITS.fixed.tables/profile.csv", na = "", quote = TRUE, row.names = FALSE)

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
#check ITS returned table
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#check the records she said didn't match her location files wer actually sent to her
#notinlocation<-read.csv("ITS.fixed.tables/event.ITS.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
#notinlocation<-notinlocation %>% 
#  mutate(EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y")) %>% 
#  distinct()
#check<-EVENT_LMAS %>% 
#  select(LOCATION_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER) %>% 
#  distinct() %>% 
#  rename(LOCATION_HISTORY_ID=LOCATION_ID) %>% 
#  mutate(filprovided="in") %>% 
#  distinct()
#notinlocation<-merge(notinlocation,check,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE','EVENT_LMAS_DATA_PROVIDER'),all.x = TRUE)

#check that they are truly not in the location table 
#location<-read.csv("ITS.fixed.tables/location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
#location<-location %>% 
#  select(LAKE_ID,LOCATION_ID) %>% 
##  rename(LOCATION_HISTORY_ID=LOCATION_ID) %>% 
#  mutate(loctable="in") %>% 
#  distinct()
#notinlocation<-merge(notinlocation,location,by=c('LOCATION_HISTORY_ID'),all.x = TRUE)

#inlocation<-notinlocation %>% 
#  filter(loctable=="in") %>% 
#  distinct()
#notinlocation<-notinlocation %>% 
##  filter(is.na(loctable)) %>% 
#  distinct()

ITS<-read.csv("ITS.fixed.tables/ITS/event.ITS.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-ITS %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER)
event<-read.csv("ITS.fixed.tables/event.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event<-event %>% 
  rename(LOCATION_HISTORY_ID=LOCATION_ID) %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER)


#change to character so can check for diff
ITS<-ITS %>% 
  mutate(LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"),
         EVENT_LMAS_SAMPLE_DATE=format(EVENT_LMAS_SAMPLE_DATE,"%m/%d/%Y"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER))
         #EVENT_LMAS_TIME=as.character(EVENT_LMAS_TIME))
event<-event %>% 
  mutate(LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=format(EVENT_LMAS_SAMPLE_DATE,"%m/%d/%Y"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER))
         #EVENT_LMAS_TIME=as.character(EVENT_LMAS_TIME))


#check class
sapply(event,class)
sapply(ITS,class)

#make sure samples sorted the same
ITS<-ITS %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER)
rownames(ITS)<-NULL
event<-event %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER)
rownames(event)<-NULL

head(event)
head(ITS)

#check if identical
identical(event,ITS)

#check for differences between specific columns
test <- lapply(names(event), function(name.i){
  anti_join(event, ITS, by = name.i)
})
names(test) <- names(event)
test


test <- lapply(names(ITS), function(name.i){
  anti_join(ITS, event, by = name.i)
})
names(test) <- names(ITS)
test
