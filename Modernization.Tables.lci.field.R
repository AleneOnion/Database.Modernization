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
#LCI field table
rm(list=setdiff(ls(), "data"))

sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(LCIFD_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         LCIFD_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         LCIFD_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         LCIFD_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         LCIFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_NAME=SEI_SAMPLE_NAME,
         LCIFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE)

field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/field.data/2019.field.data.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
field$SAMPLE_DATE<-NULL
field$SAMPLE_TYPE<-"WATER COLUMN"
field<-field %>% 
  mutate(COMMENTS=gsub("\n"," ",COMMENTS),
         COMMENTS=ifelse(COMMENTS=="DO mg/L is turbitity FNU  Hit the bottom a 16 m","DO mg/L is turbitity FNU Hit the bottom a 16 m",COMMENTS),
         COMMENTS=ifelse(COMMENTS=="Deep hole was not deepest spot; windy conditions,Do calibration was not getting to 100. Calibrated to 93,Logged two profiles since I wasn<U+FFFD>t sure if it was saving ","Deep hole was not deepest spot; windy conditions,Do calibration was not getting to 100. Calibrated to 93,Logged two profiles since I wasn<U+FFFD>t sure if it was saving",COMMENTS),
         COMMENTS=ifelse(COMMENTS=="DO mg/L is FNU.  Hit bottom- see turbidity (FNU)","DO mg/L is FNU. Hit bottom- see turbidity (FNU)",COMMENTS),
         COMMENTS=ifelse(COMMENTS=="FIXED Hypo sample ID 19SUS112 ","FIXED Hypo sample ID 19SUS112",COMMENTS)) %>% 
  rename(LCIFD_SAMPLE_TYPE=SAMPLE_TYPE,
         LCIFD_GLOBAL_ID=GLOBALID,
         LCIFD_WEATHER_48HR_PRECIP=WEATHER_48HR_PRECIP,
         LCIFD_WETLAND=WETLAND,
         LCIFD_LOW_ELEV_FLIGHT_HAZ_IND=LOW_ELEV_FLIGHT_HAZ,
         LCIFD_SAMPLE_LONG=SAMPLE_LONG,
         LCIFD_OTHER_SAMPLER=OTHER_SAMPLER,
         LCIFD_COMMENT=COMMENTS,
         LCIFD_BEACH=BEACH,
         LCIFD_USER_PERCEPT_RECREATION=USER_PERCEPT_RECREATION,
         LCIFD_SAMPLE_LAT=SAMPLE_LAT,
         LCIFD_WEATHER_CURRENT_COND=WEATHER_CURRENT_CONDITION,
         LCIFD_MOTORBOAT_DENSITY=MOTORBOAT_DENSITY,
         LCIFD_AGRICULTURE=AGRICULTURE,
         LCIFD_USER_PERCEPT_PHYS_COND=USER_PERCEPT_PHYS_COND,
         LCIFD_LMAS_SAMPLER=LMAS_SAMPLER,
         LCIFD_WEATHER_CURRENT_PRECIP=WEATHER_CURRENT_PRECIP,
         LCIFD_FOREST=FOREST,
         LCIFD_BARE_GROUND=BARE_GROUND,
         LCIFD_HAB_PRESENCE_IND=HAB_PRESENCE,
         LCIFD_WEATHER_WIND=WEATHER_WIND,
         LCIFD_GRASS=GRASS,
         LCIFD_SHORELINE_MODS=SHORELINE_MODS,
         LCIFD_MAX_SOUND_DEPTH=MAX_SOUND_DEPTH,
         LCIFD_SHRUB=SHRUB,
         LCIFD_DEVELOPMENT=DEVELOPMENT,
         LCIFD_SITE_SOUND_DEPTH=SITE_SOUND_DEPTH)
field<-merge(field,sei,by=c('SAMPLE_NAME','LCIFD_SAMPLE_TYPE'),all.x = TRUE)
field<-field %>% 
  select(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,LCIFD_EVENT_LMAS_SAMPLE_TIME,
         LCIFD_SAMPLE_TYPE,LCIFD_GLOBAL_ID,LCIFD_WEATHER_48HR_PRECIP,LCIFD_WETLAND,LCIFD_LOW_ELEV_FLIGHT_HAZ_IND,LCIFD_SAMPLE_LONG,
         LCIFD_OTHER_SAMPLER,LCIFD_COMMENT,LCIFD_BEACH,LCIFD_USER_PERCEPT_RECREATION,LCIFD_SAMPLE_LAT,
         LCIFD_WEATHER_CURRENT_COND,LCIFD_MOTORBOAT_DENSITY,LCIFD_AGRICULTURE,LCIFD_USER_PERCEPT_PHYS_COND,
         LCIFD_LMAS_SAMPLER,LCIFD_WEATHER_CURRENT_PRECIP,LCIFD_FOREST,LCIFD_BARE_GROUND,LCIFD_HAB_PRESENCE_IND,
         LCIFD_WEATHER_WIND,LCIFD_GRASS,LCIFD_SHORELINE_MODS,LCIFD_MAX_SOUND_DEPTH,LCIFD_SHRUB,LCIFD_DEVELOPMENT,LCIFD_SITE_SOUND_DEPTH) %>% 
  distinct()
field<-field %>% 
  mutate(LCIFD_USER_PERCEPT_PHYS_COND=ifelse((LCIFD_LOCATION_HISTORY_ID=="1702SHE1087_DH")&(LCIFD_EVENT_LMAS_SAMPLE_DATE=="2019-09-10"),5,LCIFD_USER_PERCEPT_PHYS_COND))

#add fields for which we do not yet have data
field<-field %>% 
  mutate(LCIFD_BOTTOM_WATER_ODOR_IND=NA,
         LCIFD_SECCHI_BOTTOM_IND=NA,
         LCIFD_USER_PERCPT_PHYS_CND_CDE=NA,
         LCIFD_EMERGENT_FLOATING_COVERG=NA,
         LCIFD_SUBMERGENT_COVERAGE=NA,
         LCIFD_MACROPHTYE_DENSITY=NA,
         LCIFD_PRISTINE=NA,
         LCIFD_APPEALING=NA,
         LCIFD_ECOLOGICAL_INTEGRITY=NA,
         LCIFD_QAUL_TROPHIC_STATUS=NA,
         LCIFD_RECREATIONAL_VALUE=NA,
         LCIFD_RESIDENCES=NA,
         LCIFD_MAINTAINED_LAWNS=NA,
         LCIFD_CONSTRUCTION=NA,
         LCIFD_PIPES_DRAINS=NA,
         LCIFD_DUMPING=NA,
         LCIFD_ROADS=NA,
         LCIFD_BRIDGES_CAUSEWAYS=NA,
         LCIFD_SEWAGE_TREATMENT=NA,
         LCIFD_HIKING_TRAILS=NA,
         LCIFD_PARKS_CAMPGROUNDS=NA,
         LCIFD_PRIMITIVE_PARKS_CAMPING=NA,
         LCIFD_RESORTS=NA,
         LCIFD_MARINAS=NA,
         LCIFD_TRASH_LITTER=NA,
         LCIFD_SURFACE_FILM_SCUM_SLICK=NA,
         LCIFD_CROPLAND=NA,
         LCIFD_PASTURE=NA,
         LCIFD_LIVESTOCK=NA,
         LCIFD_ORCHARD=NA,
         LCIFD_POULTRY=NA,
         LCIFD_FEEDLOT=NA,
         LCIFD_WATER_WITHDRAWL=NA,
         LCIFD_INDUSTRIAL_PLANTS=NA,
         LCIFD_MINE_QUARRY=NA,
         LCIFD_OIL_GAS_EXT=NA,
         LCIFD_POWER_PLANT=NA,
         LCIFD_LOGGING=NA,
         LCIFD_FIRE=NA,
         LCIFD_ODORS=NA,
         LCIFD_COMMERCIAL=NA,
         LCIFD_LIMING=NA,
         LCIFD_CHEMICAL_TREAT=NA,
         LCIFD_DRINKING_WATER_TREAT=NA,
         LCIFD_ANGLING_PRESS=NA,
         LCIFD_MACROPHTYE_CONTROL=NA,
         LCIFD_WATER_LVL_FLUCTUATION=NA,
         LCIFD_FISH_STOCK=NA,
         LCIFD_HYDRO_LAKE_TYPE=NA,
         LCIFD_OUTLET_DAMS=NA,
         LCIFD_SWIMABILITY=NA,
         LCIFD_LAKE_LVL_CHANGES=NA)


write.csv(field,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.field.csv", na = "", quote = TRUE, row.names = FALSE)

#check tables merge with upstream tables
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
SEI<-SEI %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(LCIFD_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         LCIFD_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         LCIFD_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         LCIFD_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         LCIFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_NAME=SEI_SAMPLE_NAME,
         LCIFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE)
field<-field %>% 
  mutate(LCIFD_LOCATION_HISTORY_ID=as.character(LCIFD_LOCATION_HISTORY_ID),
         LCIFD_EVENT_LMAS_SAMPLE_DATE=as.Date(LCIFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         LCIFD_EVENT_LMAS_DATA_PROVIDER=as.character(LCIFD_EVENT_LMAS_DATA_PROVIDER),
         LCIFD_EVENT_LMAS_SAMPLE_TIME=as.character(LCIFD_EVENT_LMAS_SAMPLE_TIME),
         LCIFD_SAMPLE_TYPE=as.character(LCIFD_SAMPLE_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,LCIFD_EVENT_LMAS_SAMPLE_TIME,LCIFD_SAMPLE_TYPE) %>% 
  mutate(event="this")
field<-field %>% 
  arrange(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,LCIFD_EVENT_LMAS_SAMPLE_TIME,LCIFD_SAMPLE_TYPE) 


junk<-merge(field,SEI,by=c('LCIFD_LOCATION_HISTORY_ID','LCIFD_EVENT_LMAS_SAMPLE_DATE','LCIFD_EVENT_LMAS_DATA_PROVIDER','LCIFD_EVENT_LMAS_SAMPLE_TIME','LCIFD_SAMPLE_TYPE'),all=TRUE)
junk1<-junk %>% filter(is.na(event))
junk<-junk %>% filter(is.na(LCIFD_SAMPLE_TYPE))

#check for fields with NA records
#only these fields should have NA: "LCIFD_COMMENT"               "LCIFD_MOTORBOAT_DENSITY"   
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
field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_LCI_FIELD_DATA.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,
         LCIFD_SAMPLE_TYPE,
         LCIFD_GLOBAL_ID,LCIFD_WEATHER_48HR_PRECIP,LCIFD_WETLAND,LCIFD_LOW_ELEV_FLIGHT_HAZ_IND,LCIFD_SAMPLE_LONG,
         LCIFD_OTHER_SAMPLER,LCIFD_COMMENT,LCIFD_BEACH,LCIFD_USER_PERCEPT_RECREATION,LCIFD_SAMPLE_LAT,
         LCIFD_WEATHER_CURRENT_COND,LCIFD_MOTORBOAT_DENSITY,LCIFD_AGRICULTURE,LCIFD_USER_PERCEPT_PHYS_COND,
         LCIFD_LMAS_SAMPLER,LCIFD_WEATHER_CURRENT_PRECIP,LCIFD_FOREST,LCIFD_BARE_GROUND,LCIFD_HAB_PRESENCE_IND,
         LCIFD_WEATHER_WIND,LCIFD_GRASS,LCIFD_SHORELINE_MODS,LCIFD_MAX_SOUND_DEPTH,LCIFD_SHRUB,LCIFD_DEVELOPMENT,LCIFD_SITE_SOUND_DEPTH)
field<-field %>% 
  select(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,
       LCIFD_SAMPLE_TYPE,LCIFD_GLOBAL_ID,LCIFD_WEATHER_48HR_PRECIP,LCIFD_WETLAND,LCIFD_LOW_ELEV_FLIGHT_HAZ_IND,LCIFD_SAMPLE_LONG,
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
  arrange(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,
          LCIFD_SAMPLE_TYPE,
          LCIFD_GLOBAL_ID,LCIFD_WEATHER_48HR_PRECIP,LCIFD_WETLAND,LCIFD_LOW_ELEV_FLIGHT_HAZ_IND,LCIFD_SAMPLE_LONG,
          LCIFD_OTHER_SAMPLER,LCIFD_COMMENT,LCIFD_BEACH,LCIFD_USER_PERCEPT_RECREATION,LCIFD_SAMPLE_LAT,
          LCIFD_WEATHER_CURRENT_COND,LCIFD_MOTORBOAT_DENSITY,LCIFD_AGRICULTURE,LCIFD_USER_PERCEPT_PHYS_COND,
          LCIFD_LMAS_SAMPLER,LCIFD_WEATHER_CURRENT_PRECIP,LCIFD_FOREST,LCIFD_BARE_GROUND,LCIFD_HAB_PRESENCE_IND,
          LCIFD_WEATHER_WIND,LCIFD_GRASS,LCIFD_SHORELINE_MODS,LCIFD_MAX_SOUND_DEPTH,LCIFD_SHRUB,LCIFD_DEVELOPMENT,LCIFD_SITE_SOUND_DEPTH)
rownames(ITS)<-NULL
field<-field %>% 
  arrange(LCIFD_LOCATION_HISTORY_ID,LCIFD_EVENT_LMAS_SAMPLE_DATE,LCIFD_EVENT_LMAS_DATA_PROVIDER,
          LCIFD_SAMPLE_TYPE,
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
#test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, field, by = name.i)
})
names(test2) <- names(ITS)
#test2

rm(list=c('field','ITS','test','test2'))
