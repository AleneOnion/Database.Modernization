#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")

#MM/DD/YYYY or MM/DD/YYYY HH24:MI


#Alene Onion
#Feb 2020
#Script to generate a clean lci downstream tables

####################################################################################################
####################################################################################################
#CSLAP field table

#I need to delete 12 duplicate bottom samples
#at some point we'll have to fix this.

rm(list=setdiff(ls(), "data"))

sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(CSLAPFD_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         CSLAPFD_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         CSLAPFD_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         CSLAPFD_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         CSLAPFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_NAME=SEI_SAMPLE_NAME,
         CSLAPFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE)

field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/CSLAP/fielddata_CSLAP.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
field<-field %>% 
  select(SAMPLE_NAME,CSLAPFD_WATER_SAMPLER,CSLAPFD_SECCHI_SAMPLER,CSLAPFD_OTHER_SAMPLER,CSLAPFD_SITE_SOUND_DEPTH,
         CSLAPFD_SECCHI_BOTTOM,CSLAPFD_LAKE_LEVEL,CSLAPFD_AIR_TEMP,CSLAPFD_WATER_TEMP_EPI,CSLAPFD_WATER_TEMP_HYPO,
         CSLAPFD_HYPO_SULFUR_IND,CSLAPFD_HABS_CSLAP_SITE,CSLAPFD_CURRENT_WIND,CSLAPFD_CURRENT_WEATHER,CSLAPFD_WEEK_WIND,
         CSLAPFD_WEEK_WEATHER,CSLAPFD_CURRENT_WIND_DIRECTION,CSLAPFD_WEATHER_COMMENTS,CSLAPFD_SAMPLING_RECORD_COMM,CSLAPFD_QA,
         CSLAPFD_QB,CSLAPFD_QC,CSLAPFD_QD,CSLAPFD_QF,CSLAPFD_QG,CSLAPFD_PERCEPTION_COMMENTS) %>% 
  mutate(CSLAPFD_SAMPLE_TYPE="WATER COLUMN")

field<-merge(field,sei,by=c('SAMPLE_NAME','CSLAPFD_SAMPLE_TYPE'),all.x=TRUE)
field<-field %>% 
  select(CSLAPFD_LOCATION_HISTORY_ID,CSLAPFD_EVENT_LMAS_SAMPLE_DATE,CSLAPFD_EVENT_LMAS_DATA_PROVIDER,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,
         CSLAPFD_SAMPLE_TYPE,CSLAPFD_WATER_SAMPLER,CSLAPFD_SECCHI_SAMPLER,CSLAPFD_OTHER_SAMPLER,CSLAPFD_SITE_SOUND_DEPTH,
         CSLAPFD_SECCHI_BOTTOM,CSLAPFD_LAKE_LEVEL,CSLAPFD_AIR_TEMP,CSLAPFD_WATER_TEMP_EPI,CSLAPFD_WATER_TEMP_HYPO,
         CSLAPFD_HYPO_SULFUR_IND,CSLAPFD_HABS_CSLAP_SITE,CSLAPFD_CURRENT_WIND,CSLAPFD_CURRENT_WEATHER,CSLAPFD_WEEK_WIND,
         CSLAPFD_WEEK_WEATHER,CSLAPFD_CURRENT_WIND_DIRECTION,CSLAPFD_WEATHER_COMMENTS,CSLAPFD_SAMPLING_RECORD_COMM,CSLAPFD_QA,
         CSLAPFD_QB,CSLAPFD_QC,CSLAPFD_QD,CSLAPFD_QF,CSLAPFD_QG,CSLAPFD_PERCEPTION_COMMENTS) %>% 
  filter(!is.na(CSLAPFD_LOCATION_HISTORY_ID))
 
write.csv(field,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.field.csv", na = "", quote = TRUE, row.names = FALSE)



#check tables merge with upstream tables
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
rm(list=setdiff(ls(), "data"))

field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
SEI<-SEI %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(CSLAPFD_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         CSLAPFD_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         CSLAPFD_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         CSLAPFD_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         CSLAPFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE)
field<-field %>% 
  mutate(CSLAPFD_LOCATION_HISTORY_ID=as.character(CSLAPFD_LOCATION_HISTORY_ID),
         CSLAPFD_EVENT_LMAS_SAMPLE_DATE=as.Date(CSLAPFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         CSLAPFD_EVENT_LMAS_DATA_PROVIDER=as.character(CSLAPFD_EVENT_LMAS_DATA_PROVIDER),
         CSLAPFD_EVENT_LMAS_SAMPLE_TIME=as.character(CSLAPFD_EVENT_LMAS_SAMPLE_TIME),
         CSLAPFD_SAMPLE_TYPE=as.character(CSLAPFD_SAMPLE_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(CSLAPFD_LOCATION_HISTORY_ID,CSLAPFD_EVENT_LMAS_SAMPLE_DATE,CSLAPFD_EVENT_LMAS_DATA_PROVIDER,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,CSLAPFD_SAMPLE_TYPE) %>% 
  mutate(event="this")
field<-field %>% 
  arrange(CSLAPFD_LOCATION_HISTORY_ID,CSLAPFD_EVENT_LMAS_SAMPLE_DATE,CSLAPFD_EVENT_LMAS_DATA_PROVIDER,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,CSLAPFD_SAMPLE_TYPE) 

sapply(SEI,class)
sapply(field,class)

junk<-merge(field,SEI,by=c('CSLAPFD_LOCATION_HISTORY_ID','CSLAPFD_EVENT_LMAS_SAMPLE_DATE','CSLAPFD_EVENT_LMAS_DATA_PROVIDER','CSLAPFD_EVENT_LMAS_SAMPLE_TIME','CSLAPFD_SAMPLE_TYPE'),all=TRUE)
junk1<-junk %>% filter(is.na(event))
junk<-junk %>% filter(is.na(CSLAPFD_SAMPLE_TYPE))

#check for fields with NA records
#only these fields should have NA: 
#[1] "CSLAPFD_OTHER_SAMPLER"          "CSLAPFD_SITE_SOUND_DEPTH"       "CSLAPFD_SECCHI_BOTTOM"          "CSLAPFD_LAKE_LEVEL"             "CSLAPFD_AIR_TEMP"              
#[6] "CSLAPFD_WATER_TEMP_EPI"         "CSLAPFD_WATER_TEMP_HYPO"        "CSLAPFD_HYPO_SULFUR_IND"        "CSLAPFD_HABS_CSLAP_SITE"        "CSLAPFD_CURRENT_WIND"          
#[11] "CSLAPFD_CURRENT_WEATHER"        "CSLAPFD_WEEK_WIND"              "CSLAPFD_WEEK_WEATHER"           "CSLAPFD_CURRENT_WIND_DIRECTION" "CSLAPFD_WEATHER_COMMENTS"      
#[16] "CSLAPFD_SAMPLING_RECORD_COMM"   "CSLAPFD_QA"                     "CSLAPFD_QB"                     "CSLAPFD_QC"                     "CSLAPFD_QD"                    
#[21] "CSLAPFD_QF"                     "CSLAPFD_QG"                     "CSLAPFD_PERCEPTION_COMMENTS"
colnames(field)[colSums(is.na(field)) > 0]

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
#checking ITS files
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#field data
#Check that file returned from ITS matches
#check that lakes table matches its
rm(list=setdiff(ls(), "data"))
field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_CSLAP_FIELD_DATA.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(CSLAPFD_LOCATION_HISTORY_ID,CSLAPFD_EVENT_LMAS_SAMPLE_DATE,CSLAPFD_EVENT_LMAS_DATA_PROVID,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,
         CSLAPFD_SAMPLE_TYPE,CSLAPFD_WATER_SAMPLER,CSLAPFD_SECCHI_SAMPLER,CSLAPFD_OTHER_SAMPLER,CSLAPFD_SITE_SOUND_DEPTH,
         CSLAPFD_SECCHI_BOTTOM,CSLAPFD_LAKE_LEVEL,CSLAPFD_AIR_TEMP,CSLAPFD_WATER_TEMP_EPI,CSLAPFD_WATER_TEMP_HYPO,CSLAPFD_HYPO_SULFUR,
         CSLAPFD_HABS_CSLAP_SITE,CSLAPFD_CURRENT_WIND,CSLAPFD_CURRENT_WEATHER,CSLAPFD_WEEK_WIND,CSLAPFD_WEEK_WEATHER,CSLAPFD_CURRENT_WIND_DIRECTION,
         CSLAPFD_WEATHER_COMMENT,CSLAPFD_SAMP_RECORD_COMMENT,CSLAPFD_QA,CSLAPFD_QB,CSLAPFD_QC,CSLAPFD_QD,CSLAPFD_QF,CSLAPFD_QG,CSLAPFD_PERCEPTION_COMMENT) %>% 
  rename(CSLAPFD_EVENT_LMAS_DATA_PROVIDER=CSLAPFD_EVENT_LMAS_DATA_PROVID,
         CSLAPFD_HYPO_SULFUR_IND=CSLAPFD_HYPO_SULFUR,
         CSLAPFD_WEATHER_COMMENTS=CSLAPFD_WEATHER_COMMENT,
         CSLAPFD_SAMPLING_RECORD_COMM=CSLAPFD_SAMP_RECORD_COMMENT,
         CSLAPFD_HYPO_SULFUR_IND=CSLAPFD_HYPO_SULFUR,
         CSLAPFD_PERCEPTION_COMMENTS=CSLAPFD_PERCEPTION_COMMENT)

field<-field %>% 
  select(CSLAPFD_LOCATION_HISTORY_ID,CSLAPFD_EVENT_LMAS_SAMPLE_DATE,CSLAPFD_EVENT_LMAS_DATA_PROVIDER,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,
         CSLAPFD_SAMPLE_TYPE,CSLAPFD_WATER_SAMPLER,CSLAPFD_SECCHI_SAMPLER,CSLAPFD_OTHER_SAMPLER,CSLAPFD_SITE_SOUND_DEPTH,
         CSLAPFD_SECCHI_BOTTOM,CSLAPFD_LAKE_LEVEL,CSLAPFD_AIR_TEMP,CSLAPFD_WATER_TEMP_EPI,CSLAPFD_WATER_TEMP_HYPO,CSLAPFD_HYPO_SULFUR_IND,
         CSLAPFD_HABS_CSLAP_SITE,CSLAPFD_CURRENT_WIND,CSLAPFD_CURRENT_WEATHER,CSLAPFD_WEEK_WIND,CSLAPFD_WEEK_WEATHER,CSLAPFD_CURRENT_WIND_DIRECTION,
         CSLAPFD_WEATHER_COMMENTS,CSLAPFD_SAMPLING_RECORD_COMM,CSLAPFD_QA,CSLAPFD_QB,CSLAPFD_QC,CSLAPFD_QD,CSLAPFD_QF,CSLAPFD_QG,CSLAPFD_PERCEPTION_COMMENTS)
#no formatting required

#format as dates so can compare
field<-field %>% 
  mutate(CSLAPFD_EVENT_LMAS_SAMPLE_DATE=as.Date(CSLAPFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  mutate(CSLAPFD_EVENT_LMAS_SAMPLE_TIME=as.character(CSLAPFD_EVENT_LMAS_SAMPLE_TIME),
         CSLAPFD_EVENT_LMAS_SAMPLE_TIME=sub(".*? ", "", CSLAPFD_EVENT_LMAS_SAMPLE_TIME),
         junk=substr(CSLAPFD_EVENT_LMAS_SAMPLE_TIME,1,2),
         junk=as.numeric(junk),
         pm=ifelse(junk>11,"PM","AM"),
         junk=ifelse(junk>12,junk-12,junk),
         CSLAPFD_EVENT_LMAS_SAMPLE_TIME=substr(CSLAPFD_EVENT_LMAS_SAMPLE_TIME,3,8),
         CSLAPFD_EVENT_LMAS_SAMPLE_TIME=paste(junk,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,sep=""),
         CSLAPFD_EVENT_LMAS_SAMPLE_DATE2=format(CSLAPFD_EVENT_LMAS_SAMPLE_DATE,"%m/%d/%Y"),
         CSLAPFD_EVENT_LMAS_SAMPLE_DATE2=as.character(CSLAPFD_EVENT_LMAS_SAMPLE_DATE2),
         CSLAPFD_EVENT_LMAS_SAMPLE_DATE2=sub("^0", "", CSLAPFD_EVENT_LMAS_SAMPLE_DATE2),
         CSLAPFD_EVENT_LMAS_SAMPLE_DATE2=sub("/0","/",CSLAPFD_EVENT_LMAS_SAMPLE_DATE2),
         CSLAPFD_EVENT_LMAS_SAMPLE_TIME=paste(CSLAPFD_EVENT_LMAS_SAMPLE_DATE2,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,pm,sep=" ")) %>% 
  select(CSLAPFD_LOCATION_HISTORY_ID,CSLAPFD_EVENT_LMAS_SAMPLE_DATE,CSLAPFD_EVENT_LMAS_DATA_PROVIDER,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,
         CSLAPFD_SAMPLE_TYPE,CSLAPFD_WATER_SAMPLER,CSLAPFD_SECCHI_SAMPLER,CSLAPFD_OTHER_SAMPLER,CSLAPFD_SITE_SOUND_DEPTH,
         CSLAPFD_SECCHI_BOTTOM,CSLAPFD_LAKE_LEVEL,CSLAPFD_AIR_TEMP,CSLAPFD_WATER_TEMP_EPI,CSLAPFD_WATER_TEMP_HYPO,CSLAPFD_HYPO_SULFUR_IND,
         CSLAPFD_HABS_CSLAP_SITE,CSLAPFD_CURRENT_WIND,CSLAPFD_CURRENT_WEATHER,CSLAPFD_WEEK_WIND,CSLAPFD_WEEK_WEATHER,CSLAPFD_CURRENT_WIND_DIRECTION,
         CSLAPFD_WEATHER_COMMENTS,CSLAPFD_SAMPLING_RECORD_COMM,CSLAPFD_QA,CSLAPFD_QB,CSLAPFD_QC,CSLAPFD_QD,CSLAPFD_QF,CSLAPFD_QG,CSLAPFD_PERCEPTION_COMMENTS)
ITS<-ITS %>% 
  mutate(CSLAPFD_EVENT_LMAS_SAMPLE_DATE=as.Date(CSLAPFD_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"))


#check class
sapply(field,class)
sapply(ITS,class)

#make sure samples sorted the same
ITS<-ITS %>% 
  arrange(CSLAPFD_LOCATION_HISTORY_ID,CSLAPFD_EVENT_LMAS_SAMPLE_DATE,CSLAPFD_EVENT_LMAS_DATA_PROVIDER,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,
          CSLAPFD_SAMPLE_TYPE,CSLAPFD_WATER_SAMPLER,CSLAPFD_SECCHI_SAMPLER,CSLAPFD_OTHER_SAMPLER,CSLAPFD_SITE_SOUND_DEPTH,
          CSLAPFD_SECCHI_BOTTOM,CSLAPFD_LAKE_LEVEL,CSLAPFD_AIR_TEMP,CSLAPFD_WATER_TEMP_EPI,CSLAPFD_WATER_TEMP_HYPO,CSLAPFD_HYPO_SULFUR_IND,
          CSLAPFD_HABS_CSLAP_SITE,CSLAPFD_CURRENT_WIND,CSLAPFD_CURRENT_WEATHER,CSLAPFD_WEEK_WIND,CSLAPFD_WEEK_WEATHER,CSLAPFD_CURRENT_WIND_DIRECTION,
          CSLAPFD_WEATHER_COMMENTS,CSLAPFD_SAMPLING_RECORD_COMM,CSLAPFD_QA,CSLAPFD_QB,CSLAPFD_QC,CSLAPFD_QD,CSLAPFD_QF,CSLAPFD_QG,CSLAPFD_PERCEPTION_COMMENTS)
rownames(ITS)<-NULL
field<-field %>% 
  arrange(CSLAPFD_LOCATION_HISTORY_ID,CSLAPFD_EVENT_LMAS_SAMPLE_DATE,CSLAPFD_EVENT_LMAS_DATA_PROVIDER,CSLAPFD_EVENT_LMAS_SAMPLE_TIME,
          CSLAPFD_SAMPLE_TYPE,CSLAPFD_WATER_SAMPLER,CSLAPFD_SECCHI_SAMPLER,CSLAPFD_OTHER_SAMPLER,CSLAPFD_SITE_SOUND_DEPTH,
          CSLAPFD_SECCHI_BOTTOM,CSLAPFD_LAKE_LEVEL,CSLAPFD_AIR_TEMP,CSLAPFD_WATER_TEMP_EPI,CSLAPFD_WATER_TEMP_HYPO,CSLAPFD_HYPO_SULFUR_IND,
          CSLAPFD_HABS_CSLAP_SITE,CSLAPFD_CURRENT_WIND,CSLAPFD_CURRENT_WEATHER,CSLAPFD_WEEK_WIND,CSLAPFD_WEEK_WEATHER,CSLAPFD_CURRENT_WIND_DIRECTION,
          CSLAPFD_WEATHER_COMMENTS,CSLAPFD_SAMPLING_RECORD_COMM,CSLAPFD_QA,CSLAPFD_QB,CSLAPFD_QC,CSLAPFD_QD,CSLAPFD_QF,CSLAPFD_QG,CSLAPFD_PERCEPTION_COMMENTS) 
rownames(field)<-NULL


#check if identical
identical(field,ITS)

#check for differences between specific columns
test <- lapply(names(field), function(name.i){
  anti_join(field, ITS, by = name.i)
})
names(test) <- names(field)
#test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, field, by = name.i)
})
names(test2) <- names(ITS)
#test2

rm(list=c('field','ITS','test','test2'))
