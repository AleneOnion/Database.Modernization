#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")

rm(list=setdiff(ls(), "data"))

#######################################################################################################################################################################################
#######################################################################################################################################################################################
#######################################################################################################################################################################################
#additing CSLAP and HABS data
#creating file for upload to the databse
library(dplyr)
library(tidyr)
sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
habfield<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
cslapfield<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
status<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.status.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
resultshabs<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.only.tables/results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
resultscslap<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.only.tables/results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
location<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
location<-location %>% select(LAKE_ID,LOCATION_ID) %>% distinct()

sei<-sei %>% 
  rename(LOCATION_ID=SEI_LOCATION_HISTORY_ID,
         SAMPLE_NAME=SEI_SAMPLE_NAME,
         DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         INFO_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         SAMPLE_TYPE=SEI_SAMPLE_TYPE) %>% 
  select(LOCATION_ID,SAMPLE_NAME,DATA_PROVIDER,INFO_TYPE,SAMPLE_DATE,TIME,SAMPLE_TYPE) %>% 
  mutate(SAMPLE_DATE=as.Date(SAMPLE_DATE,format="%Y-%m-%d"))
habfield<-habfield %>% 
  rename(LOCATION_ID=HFD_LOCATION_HISTORY_ID,
         SAMPLE_DATE=HFD_EVENT_LMAS_SAMPLE_DATE,
         TIME=HFD_EVENT_LMAS_SAMPLE_TIME,
         DATA_PROVIDER=HFD_EVENT_LMAS_DATA_PROVIDER,
         INFO_TYPE=HFD_INFORMATION_TYPE,
         REMARK=HFD_REMARK,
         EXTENT=HFD_EXTENT,
         BLOOM_DESC=HFD_BLOOM_DESC,
         EQUIPMENT_DESC=HFD_EQUIPMENT_DESC,
         EQUIPMENT_TYPE=HFD_EQUIPMENT_TYPE,
         SAMPLE_METHOD=HFD_SAMPLE_METHOD,
         PERCENT=HFD_PERCENT,
         SAMPLE_TYPE=HFD_SAMPLE_TYPE) %>% 
  select(LOCATION_ID,SAMPLE_DATE,TIME,DATA_PROVIDER,INFO_TYPE,SAMPLE_TYPE,REMARK,EXTENT,BLOOM_DESC,EQUIPMENT_DESC,EQUIPMENT_TYPE,SAMPLE_METHOD,PERCENT) %>% 
  mutate(SAMPLE_DATE=as.Date(SAMPLE_DATE,format="%Y-%m-%d"))
cslapfield<-cslapfield %>% 
  rename(LOCATION_ID=CSLAPFD_LOCATION_HISTORY_ID,
         SAMPLE_DATE=CSLAPFD_EVENT_LMAS_SAMPLE_DATE,
         TIME=CSLAPFD_EVENT_LMAS_SAMPLE_TIME,
         DATA_PROVIDER=CSLAPFD_EVENT_LMAS_DATA_PROVIDER,
         SAMPLE_TYPE=CSLAPFD_SAMPLE_TYPE) %>% 
  select(LOCATION_ID,SAMPLE_DATE,TIME,DATA_PROVIDER,SAMPLE_TYPE) %>% 
  mutate(SAMPLE_DATE=as.Date(SAMPLE_DATE,format="%Y-%m-%d"))
status<-status %>% 
  rename(LOCATION_ID=HAB_LOCATION_HISTORY_ID,
         SAMPLE_DATE=HAB_EVENT_LMAS_SAMPLE_DATE,
         DATA_PROVIDER=HAB_EVENT_LMAS_DATA_PROVIDER,
         INFO_TYPE=HAB_INFORMATION_TYPE,
         STATUS=HAB_STATUS,
         STATUS_DATE=HAB_STATUS_DATE,
         status_REMARK=HAB_STATUS_REMARK,
         TIME=HAB_EVENT_LMAS_SAMPLE_TIME,
         SAMPLE_TYPE=HAB_SAMPLE_TYPE) %>% 
  select(LOCATION_ID,SAMPLE_DATE,DATA_PROVIDER,SAMPLE_TYPE,INFO_TYPE,STATUS,STATUS_DATE,TIME,status_REMARK) %>% 
  mutate(SAMPLE_DATE=as.Date(SAMPLE_DATE,format="%Y-%m-%d"))
results<-merge(resultshabs,resultscslap,all=TRUE)
results<-results %>% 
  rename(LOCATION_ID=RSLT_LOCATION_HISTORY_ID,
         SAMPLE_DATE=RSLT_EVENT_LMAS_SAMPLE_DATE,
         DATA_PROVIDER=RSLT_EVENT_LMAS_DATA_PROVIDER,
         INFO_TYPE=RSLT_INFORMATION_TYPE,
         TIME=RSLT_EVENT_LMAS_SAMPLE_TIME,
         SAMPLE_TYPE=RSLT_SAMPLE_TYPE,
         Characteristic.Name=RSLT_CHARACTERISTIC_NAME,
         Result.Value=RSLT_RESULT_VALUE,
         Result.Unit=RSLT_RESULT_UNIT,
         Result.Sample.Fraction=RSLT_RESULT_SAMPLE_FRACTION,
         VALIDATOR_QUALIFIERS=RSLT_VALIDATOR_QUALIFIER,
         INTERPRETED_QUALIFIERS=RSLT_INTERPRETED_QUALIFIER,
         LAB_NAME_CODE=RSLT_LABORATORY_NAME_CDE,
         LAB_QUALIFIERS=RSLT_LABORATORY_QUALIFIER,
         QUANTITATION_LIMIT=RSLT_QUANTITATION_LIMIT,
         METHOD_DETECTION_LIMIT=RSLT_METHOD_DETECTION_LIMIT,
         Result.Detection.Quantitation.Limit.Unit=RSLT_DETECTION_QUANT_LMT_UNIT,
         Result.Analytical.Method.ID=RSLT_ANALYTICAL_METHOD_ID,
         Result.Comment=RSLT_COMMENT,
         Flags=RSLT_FLAG,
         Result.Detection.Condition=RSLT_DETECTION_CONDITION,
         Lab_Sample_Name=RSLT_LABORATORY_SAMPLE_NAME,
         Result.Detection.Quantitation.Limit.Measure=RSLT_DETECT_QUANT_LMT_MEASURE,
         Result.Value.Type=RSLT_VALUE_TYPE,
         Method.Speciation=RSLT_METHOD_SPECIATION,
         Serial=RSLT_SERIAL_NUMBER,
         Results_DATE_ADDED=RSLT_DATE_ADDED) %>% 
  mutate(SAMPLE_DATE=as.Date(SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  select(LOCATION_ID,SAMPLE_DATE,DATA_PROVIDER,INFO_TYPE,TIME,SAMPLE_TYPE,Characteristic.Name,Result.Value,Result.Unit,
         Result.Sample.Fraction,VALIDATOR_QUALIFIERS,INTERPRETED_QUALIFIERS,LAB_NAME_CODE,LAB_QUALIFIERS,QUANTITATION_LIMIT,
         METHOD_DETECTION_LIMIT,Result.Detection.Quantitation.Limit.Unit,Result.Analytical.Method.ID,Result.Comment,Flags,
         Result.Detection.Condition,Lab_Sample_Name,Result.Detection.Quantitation.Limit.Measure,Result.Value.Type,Method.Speciation,
         Serial,Results_DATE_ADDED)

status<-merge(status,sei,by=c('LOCATION_ID','DATA_PROVIDER','SAMPLE_DATE','TIME','SAMPLE_TYPE','INFO_TYPE'),all.x = TRUE)
habfield<-merge(habfield,sei,by=c('LOCATION_ID','DATA_PROVIDER','SAMPLE_DATE','TIME','SAMPLE_TYPE','INFO_TYPE'),all.x = TRUE)
cslapfield<-merge(cslapfield,sei,by=c('LOCATION_ID','DATA_PROVIDER','SAMPLE_DATE','TIME','SAMPLE_TYPE'),all.x = TRUE)
results<-merge(results,sei,by=c('LOCATION_ID','DATA_PROVIDER','SAMPLE_DATE','TIME','SAMPLE_TYPE','INFO_TYPE'),all.x = TRUE)

samples<-merge(status,habfield,by=c('LOCATION_ID','DATA_PROVIDER','SAMPLE_DATE','TIME','SAMPLE_TYPE','INFO_TYPE','SAMPLE_NAME'),all=TRUE)
samples<-merge(samples,cslapfield,by=c('LOCATION_ID','DATA_PROVIDER','SAMPLE_DATE','TIME','SAMPLE_TYPE','SAMPLE_NAME','INFO_TYPE'),all=TRUE)
samples<-merge(samples,results,by=c('LOCATION_ID','DATA_PROVIDER','SAMPLE_DATE','TIME','SAMPLE_TYPE','SAMPLE_NAME','INFO_TYPE'),all=TRUE)
samples<-merge(samples,location,by=c('LOCATION_ID'),all.x = TRUE)
samples<-samples %>% distinct()
rm(list=setdiff(ls(), c("data","samples")))

samples<-samples %>% 
  mutate(SAMPLE_ID=64500:(64499+nrow(samples)),
         START_DEPTH=0,
         END_DEPTH=0,
         LAKE_PROJ_CODE=NA,
         BLOOM_LOC=NA,
         ESF.=NA,
         UFI.=NA,
         DEPTH_UNIT="m",
         WIND_DIR=NA,
         WIND_INT=NA)
newstatus<-samples %>% 
  select(SAMPLE_ID,STATUS,STATUS_DATE,REMARK) %>% 
  distinct() %>% 
  mutate(STATUS_DATE=as.Date(STATUS_DATE,format="%Y-%m-%d"),
         STATUS_DATE=format(STATUS_DATE,"%m/%d/%Y"),
         REMARK=as.character(REMARK)) %>% 
  rename(status_REMARK=REMARK)
newsamples<-samples %>% 
  select(LAKE_ID,LOCATION_ID,SAMPLE_ID,SAMPLE_NAME,DATA_PROVIDER,INFO_TYPE,SAMPLE_DATE,TIME,REMARK,EXTENT,BLOOM_DESC,EQUIPMENT_DESC,EQUIPMENT_TYPE,SAMPLE_METHOD,PERCENT) %>% 
  distinct()
newresults<-samples %>% 
  select(SAMPLE_ID,SAMPLE_NAME,INFO_TYPE,Characteristic.Name,Result.Value,Result.Unit,
         Result.Sample.Fraction,VALIDATOR_QUALIFIERS,INTERPRETED_QUALIFIERS,LAB_NAME_CODE,LAB_QUALIFIERS,QUANTITATION_LIMIT,
         METHOD_DETECTION_LIMIT,Result.Detection.Quantitation.Limit.Unit,Result.Analytical.Method.ID,Result.Comment,Flags,
         Result.Detection.Condition,Lab_Sample_Name,Result.Detection.Quantitation.Limit.Measure,Result.Value.Type,Method.Speciation,
         Serial,Results_DATE_ADDED) %>% 
  distinct()
write.csv(newsamples,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/samples_to_add_to_sample_table_habs.csv", na = "", quote = TRUE, row.names = FALSE)
write.csv(newstatus,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/habs_to_add_to_habs_table.csv", na = "", quote = TRUE, row.names = FALSE)  
write.csv(newresults,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/results_to_add_to_results_table_habscslap.csv", na = "", quote = TRUE, row.names = FALSE)

rm(list=setdiff(ls(), "data"))










