#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")


#Alene Onion
#Feb 2020
#Script to generate a clean lci downstream tables

####################################################################################################
####################################################################################################
#CSLAP field table

sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  rename(CSLAPFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         CSLAPFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_NAME=SEI_SAMPLE_NAME)

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
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_TIME,
         CSLAPFD_SAMPLE_TYPE,CSLAPFD_WATER_SAMPLER,CSLAPFD_SECCHI_SAMPLER,CSLAPFD_OTHER_SAMPLER,CSLAPFD_SITE_SOUND_DEPTH,
         CSLAPFD_SECCHI_BOTTOM,CSLAPFD_LAKE_LEVEL,CSLAPFD_AIR_TEMP,CSLAPFD_WATER_TEMP_EPI,CSLAPFD_WATER_TEMP_HYPO,
         CSLAPFD_HYPO_SULFUR_IND,CSLAPFD_HABS_CSLAP_SITE,CSLAPFD_CURRENT_WIND,CSLAPFD_CURRENT_WEATHER,CSLAPFD_WEEK_WIND,
         CSLAPFD_WEEK_WEATHER,CSLAPFD_CURRENT_WIND_DIRECTION,CSLAPFD_WEATHER_COMMENTS,CSLAPFD_SAMPLING_RECORD_COMM,CSLAPFD_QA,
         CSLAPFD_QB,CSLAPFD_QC,CSLAPFD_QD,CSLAPFD_QF,CSLAPFD_QG,CSLAPFD_PERCEPTION_COMMENTS)
 
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
field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
SEI<-SEI %>% 
  mutate(LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER),
         EVENT_LMAS_TIME=as.character(EVENT_LMAS_TIME),
         SEI_SAMPLE_TYPE=as.character(SEI_SAMPLE_TYPE)) %>% 
  rename(CSLAPFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE)
field<-field %>% 
  mutate(LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER),
         EVENT_LMAS_TIME=as.character(EVENT_LMAS_TIME),
         CSLAPFD_SAMPLE_TYPE=as.character(CSLAPFD_SAMPLE_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_TIME,CSLAPFD_SAMPLE_TYPE) %>% 
  mutate(event="this")
field<-field %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_TIME,CSLAPFD_SAMPLE_TYPE) 


junk<-merge(field,SEI,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE','EVENT_LMAS_PROJECT_CDE','EVENT_LMAS_DATA_PROVIDER','EVENT_LMAS_TIME','CSLAPFD_SAMPLE_TYPE'),all=TRUE)
junk<-junk %>% filter(is.na(event))

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
field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_LCI_FIELD_DATA.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,LCIFD_EVENT_LMAS_PROJECT_CDE,
         LCIFD_SAMPLE_TYPE,LCIFD_INFORMATION_TYPE,
         LCIFD_GLOBAL_ID,LCIFD_WEATHER_48HR_PRECIP,LCIFD_WETLAND,LCIFD_LOW_ELEV_FLIGHT_HAZ_IND,LCIFD_SAMPLE_LONG,
         LCIFD_OTHER_SAMPLER,LCIFD_COMMENT,LCIFD_BEACH,LCIFD_USER_PERCEPT_RECREATION,LCIFD_SAMPLE_LAT,
         LCIFD_WEATHER_CURRENT_COND,LCIFD_MOTORBOAT_DENSITY,LCIFD_AGRICULTURE,LCIFD_USER_PERCEPT_PHYS_COND,
         LCIFD_LMAS_SAMPLER,LCIFD_WEATHER_CURRENT_PRECIP,LCIFD_FOREST,LCIFD_BARE_GROUND,LCIFD_HAB_PRESENCE_IND,
         LCIFD_WEATHER_WIND,LCIFD_GRASS,LCIFD_SHORELINE_MODS,LCIFD_MAX_SOUND_DEPTH,LCIFD_SHRUB,LCIFD_DEVELOPMENT,LCIFD_SITE_SOUND_DEPTH)
field<-field %>% 
  rename(LCIFD_LOCATION_HISTORY_ID=LOCATION_HISTORY_ID,
         LCIFD_EVENT_LMAS_SAMPLE_DATE=EVENT_LMAS_SAMPLE_DATE,
         LCIFD_EVENT_LMAS_DATA_PROVIDER=EVENT_LMAS_DATA_PROVIDER,
         LCIFD_EVENT_LMAS_PROJECT_CDE=EVENT_LMAS_PROJECT_CDE,
         LCIFD_HAB_PRESENCE_IND=LCIFD_HAB_PRESENCE,
         LCIFD_WEATHER_CURRENT_COND=LCIFD_WEATHER_CURRENT_CONDITION,
         LCIFD_COMMENT=LCIFD_COMMENTS,
         LCIFD_LOW_ELEV_FLIGHT_HAZ_IND=LCIFD_LOW_ELEV_FLIGHT_HAZ,
         LCIFD_GLOBAL_ID=LCIFD_GLOBALID) %>% 
  select(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,LCIFD_EVENT_LMAS_PROJECT_CDE,
       LCIFD_SAMPLE_TYPE,LCIFD_INFORMATION_TYPE,
       LCIFD_GLOBAL_ID,LCIFD_WEATHER_48HR_PRECIP,LCIFD_WETLAND,LCIFD_LOW_ELEV_FLIGHT_HAZ_IND,LCIFD_SAMPLE_LONG,
       LCIFD_OTHER_SAMPLER,LCIFD_COMMENT,LCIFD_BEACH,LCIFD_USER_PERCEPT_RECREATION,LCIFD_SAMPLE_LAT,
       LCIFD_WEATHER_CURRENT_COND,LCIFD_MOTORBOAT_DENSITY,LCIFD_AGRICULTURE,LCIFD_USER_PERCEPT_PHYS_COND,
       LCIFD_LMAS_SAMPLER,LCIFD_WEATHER_CURRENT_PRECIP,LCIFD_FOREST,LCIFD_BARE_GROUND,LCIFD_HAB_PRESENCE_IND,
       LCIFD_WEATHER_WIND,LCIFD_GRASS,LCIFD_SHORELINE_MODS,LCIFD_MAX_SOUND_DEPTH,LCIFD_SHRUB,LCIFD_DEVELOPMENT,LCIFD_SITE_SOUND_DEPTH)
#format as dates so can compare
field<-field %>% 
  mutate(LCIFD_EVENT_LMAS_SAMPLE_DATE=as.Date(LCIFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
ITS<-ITS %>% 
  mutate(LCIFD_EVENT_LMAS_SAMPLE_DATE=as.Date(LCIFD_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"))

#change yes to Y and no to N in field table
field<-field %>% 
  mutate(LCIFD_LOW_ELEV_FLIGHT_HAZ_IND=ifelse(LCIFD_LOW_ELEV_FLIGHT_HAZ_IND=="YES","Y",LCIFD_LOW_ELEV_FLIGHT_HAZ_IND),
         LCIFD_LOW_ELEV_FLIGHT_HAZ_IND=ifelse(LCIFD_LOW_ELEV_FLIGHT_HAZ_IND=="NO","N",LCIFD_LOW_ELEV_FLIGHT_HAZ_IND),
         LCIFD_HAB_PRESENCE_IND=ifelse(LCIFD_HAB_PRESENCE_IND=="YES","Y",LCIFD_HAB_PRESENCE_IND),
         LCIFD_HAB_PRESENCE_IND=ifelse(LCIFD_HAB_PRESENCE_IND=="NO","N",LCIFD_HAB_PRESENCE_IND))

#check class
sapply(field,class)
sapply(ITS,class)

#make sure samples sorted the same
ITS<-ITS %>% 
  arrange(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,LCIFD_EVENT_LMAS_PROJECT_CDE,
          LCIFD_SAMPLE_TYPE,LCIFD_INFORMATION_TYPE,
          LCIFD_GLOBAL_ID,LCIFD_WEATHER_48HR_PRECIP,LCIFD_WETLAND,LCIFD_LOW_ELEV_FLIGHT_HAZ_IND,LCIFD_SAMPLE_LONG,
          LCIFD_OTHER_SAMPLER,LCIFD_COMMENT,LCIFD_BEACH,LCIFD_USER_PERCEPT_RECREATION,LCIFD_SAMPLE_LAT,
          LCIFD_WEATHER_CURRENT_COND,LCIFD_MOTORBOAT_DENSITY,LCIFD_AGRICULTURE,LCIFD_USER_PERCEPT_PHYS_COND,
          LCIFD_LMAS_SAMPLER,LCIFD_WEATHER_CURRENT_PRECIP,LCIFD_FOREST,LCIFD_BARE_GROUND,LCIFD_HAB_PRESENCE_IND,
          LCIFD_WEATHER_WIND,LCIFD_GRASS,LCIFD_SHORELINE_MODS,LCIFD_MAX_SOUND_DEPTH,LCIFD_SHRUB,LCIFD_DEVELOPMENT,LCIFD_SITE_SOUND_DEPTH)
rownames(ITS)<-NULL
field<-field %>% 
  arrange(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,LCIFD_EVENT_LMAS_PROJECT_CDE,
          LCIFD_SAMPLE_TYPE,LCIFD_INFORMATION_TYPE,
          LCIFD_GLOBAL_ID,LCIFD_WEATHER_48HR_PRECIP,LCIFD_WETLAND,LCIFD_LOW_ELEV_FLIGHT_HAZ_IND,LCIFD_SAMPLE_LONG,
          LCIFD_OTHER_SAMPLER,LCIFD_COMMENT,LCIFD_BEACH,LCIFD_USER_PERCEPT_RECREATION,LCIFD_SAMPLE_LAT,
          LCIFD_WEATHER_CURRENT_COND,LCIFD_MOTORBOAT_DENSITY,LCIFD_AGRICULTURE,LCIFD_USER_PERCEPT_PHYS_COND,
          LCIFD_LMAS_SAMPLER,LCIFD_WEATHER_CURRENT_PRECIP,LCIFD_FOREST,LCIFD_BARE_GROUND,LCIFD_HAB_PRESENCE_IND,
          LCIFD_WEATHER_WIND,LCIFD_GRASS,LCIFD_SHORELINE_MODS,LCIFD_MAX_SOUND_DEPTH,LCIFD_SHRUB,LCIFD_DEVELOPMENT,LCIFD_SITE_SOUND_DEPTH)
rownames(field)<-NULL


#check if identical
identical(field,ITS)

#check for differences between specific columns
test <- lapply(names(field), function(name.i){
  anti_join(field, ITS, by = name.i)
})
names(test) <- names(field)
test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, field, by = name.i)
})
names(test2) <- names(ITS)
test2

rm(list=c('field','ITS','test','test2'))
