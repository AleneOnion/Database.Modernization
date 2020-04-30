#Alene Onion
#Feb 2020
#Script to generate a clean lci downstream tables

####################################################################################################
####################################################################################################
#Read in Data tables first
head(data)
####################################################################################################
####################################################################################################


####################################################################################################
####################################################################################################
#LCI EVENT TABLE
#location
#date
#project_code
#data_provider
library(dplyr)
library(tidyr)
rec<-data
rec<-rec %>% 
  filter(SAMPLE_DATE>'2019-01-01',
         DATA_PROVIDER=="LCI") %>% 
  distinct()


#removed:
#TIME - because this will be distinct for each sample type
EVENT_LMAS<-rec %>%
  filter(INFO_TYPE=="OW") %>% 
  select(LOCATION_ID,SAMPLE_DATE,LAKE_PROJ_CODE,DATA_PROVIDER,TIME) %>% 
  mutate(LAKE_PROJ_CODE=NA) %>% 
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
         EVENT_LMAS_SAMPLE_TIME=TIME,
         EVENT_LMAS_PROJECT_CDE=LAKE_PROJ_CODE,
         EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         EVENT_LMAS_LOCATION_HISTORY_ID=LOCATION_ID) %>% 
  distinct() 

#fix time field
EVENT_LMAS<-EVENT_LMAS %>% 
  mutate(EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("\\d:\\d\\d:00",EVENT_LMAS_SAMPLE_TIME),gsub(":00","",EVENT_LMAS_SAMPLE_TIME),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("^\\d:",EVENT_LMAS_SAMPLE_TIME),paste("0",EVENT_LMAS_SAMPLE_TIME,sep=""),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=paste(EVENT_LMAS_SAMPLE_DATE," ",EVENT_LMAS_SAMPLE_TIME,":00",sep=""))

#sort for clear viewing
EVENT_LMAS<-EVENT_LMAS %>% 
  arrange(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE)

#check that there are expected samples
check2<-EVENT_LMAS %>% 
  group_by(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_DATA_PROVIDER) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  distinct()
colnames(EVENT_LMAS)[colSums(is.na(EVENT_LMAS)) > 0]
write.csv(EVENT_LMAS, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/event.csv", na = "", quote = TRUE, row.names = FALSE)
write.csv(EVENT_LMAS, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.only.tables/event.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=c('rec','check','check2','EVENT_LMAS'))
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#adding CSLAP
#MAKE SURE TO DO TOUPPER!!
cslap<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/CSLAP/2019.CSLAP.Sample.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
cslap<-cslap %>% select(SAMPLE_NAME,LAKE_NAME,SAMPLE_DATE,INFO_TYPE,SAMPLE_TYPE,LOCATION_ID,LAKE_PROJ_CODE,DATA_PROVIDER,LakeID,TIME) %>% distinct()
event<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.only.tables/event.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#reformat 
cslap<-cslap %>% 
  rename(EVENT_LMAS_LOCATION_HISTORY_ID=LOCATION_ID,
         EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         EVENT_LMAS_PROJECT_CDE=LAKE_PROJ_CODE,
         EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         EVENT_LMAS_SAMPLE_TIME=TIME) %>% 
  mutate(EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"),
         EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("PM",EVENT_LMAS_SAMPLE_TIME)&grepl("^\\d:",EVENT_LMAS_SAMPLE_TIME),gsub("^1:","13:",EVENT_LMAS_SAMPLE_TIME),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("PM",EVENT_LMAS_SAMPLE_TIME)&grepl("^\\d:",EVENT_LMAS_SAMPLE_TIME),gsub("^2:","14:",EVENT_LMAS_SAMPLE_TIME),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("PM",EVENT_LMAS_SAMPLE_TIME)&grepl("^\\d:",EVENT_LMAS_SAMPLE_TIME),gsub("^3:","15:",EVENT_LMAS_SAMPLE_TIME),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("PM",EVENT_LMAS_SAMPLE_TIME)&grepl("^\\d:",EVENT_LMAS_SAMPLE_TIME),gsub("^4:","16:",EVENT_LMAS_SAMPLE_TIME),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("PM",EVENT_LMAS_SAMPLE_TIME)&grepl("^\\d:",EVENT_LMAS_SAMPLE_TIME),gsub("^5:","17:",EVENT_LMAS_SAMPLE_TIME),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("PM",EVENT_LMAS_SAMPLE_TIME)&grepl("^\\d:",EVENT_LMAS_SAMPLE_TIME),gsub("^6:","18:",EVENT_LMAS_SAMPLE_TIME),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("PM",EVENT_LMAS_SAMPLE_TIME)&grepl("^\\d:",EVENT_LMAS_SAMPLE_TIME),gsub("^7:","19:",EVENT_LMAS_SAMPLE_TIME),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=gsub("\\sAM","",EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=gsub("\\sPM","",EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=ifelse(grepl("^\\d:",EVENT_LMAS_SAMPLE_TIME),paste("0",EVENT_LMAS_SAMPLE_TIME,sep=""),EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=ifelse(is.na(EVENT_LMAS_SAMPLE_TIME),"00:00:00",EVENT_LMAS_SAMPLE_TIME),
         EVENT_LMAS_SAMPLE_TIME=paste(EVENT_LMAS_SAMPLE_DATE," ",EVENT_LMAS_SAMPLE_TIME,sep=""),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_LOCATION_HISTORY_ID=toupper(EVENT_LMAS_LOCATION_HISTORY_ID),
         EVENT_LMAS_PROJECT_CDE=toupper(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=toupper(EVENT_LMAS_DATA_PROVIDER))%>% 
  filter(!SAMPLE_NAME %in% c('19-182.3-11','19-182.3-12','19-182.3-14'),
         !is.na(EVENT_LMAS_LOCATION_HISTORY_ID)) %>% 
  select(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_SAMPLE_TIME) %>% 
  distinct()
event<-event %>% 
  mutate(EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE))
sapply(cslap,class)
sapply(event,class)

event<-merge(event,cslap,all=TRUE)
event<-event %>% distinct()
write.csv(event, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/event.csv", na = "", quote = TRUE, row.names = FALSE)
write.csv(cslap, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.only.tables/event.csv", na = "", quote = TRUE, row.names = FALSE)


####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#adding HABs table
#MAKE SURE TO DO TOUPPER!!
habs<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/habs.event.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.only.tables/event.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event2<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.only.tables/event.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event<-merge(event,event2,all=TRUE)
rm(event2)

habs<-habs %>% 
  rename(EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         EVENT_LMAS_PROJECT_CDE=PROJECT_CDE,
         EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         EVENT_LMAS_SAMPLE_TIME=TIME,
         EVENT_LMAS_LOCATION_HISTORY_ID=LOCATION_HISTORY_ID) %>% 
  mutate(EVENT_LMAS_SAMPLE_TIME=paste(EVENT_LMAS_SAMPLE_DATE,"00:00:00",sep=" "),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_LOCATION_HISTORY_ID=ifelse(EVENT_LMAS_LOCATION_HISTORY_ID=="1306WALXXX1_C","1306WALXXX1_ESOPUS",EVENT_LMAS_LOCATION_HISTORY_ID),
         EVENT_LMAS_LOCATION_HISTORY_ID=toupper(EVENT_LMAS_LOCATION_HISTORY_ID),
         EVENT_LMAS_PROJECT_CDE=toupper(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=toupper(EVENT_LMAS_DATA_PROVIDER)) %>%
  select(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_SAMPLE_TIME) %>% 
  distinct()
  
event<-event %>% 
  mutate(EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE)) %>% 
  select(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_SAMPLE_TIME) %>% 
  distinct()

sapply(event,class)
sapply(habs,class)
write.csv(habs,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.only.tables/event.csv",na = "", quote = TRUE, row.names = FALSE)
event<-merge(event,habs,all=TRUE)
event<-event %>% distinct()
write.csv(event,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/event.csv",na = "", quote = TRUE, row.names = FALSE)

rm(list=setdiff(ls(), "data"))


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
event<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/event.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
location<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
location<-location %>% 
  rename(LAKE_HISTORY_ID=LAKE_ID,
         EVENT_LMAS_LOCATION_HISTORY_ID=LOCATION_ID,
         LOCATION_NAME=LocationName,
         LOCATION_TYPE=Type,
         LOCATION_X_COORDINATE=X_Coordinate,
         LOCATION_Y_COORDINATE=Y_Coordinate,
         LOCATION_HORIZONTAL_METHOD=Horz_Method,
         LOCATION_HORIZONTAL_DATUM=Horz_Datum)

#change to character so can check for diff
location<-location %>% 
  mutate(LAKE_HISTORY_ID=as.character(LAKE_HISTORY_ID),
         EVENT_LMAS_LOCATION_HISTORY_ID=as.character(EVENT_LMAS_LOCATION_HISTORY_ID),
         LOCATION_NAME=as.character(LOCATION_NAME),
         LOCATION_TYPE=as.character(LOCATION_TYPE),
         LOCATION_X_COORDINATE=as.character(LOCATION_X_COORDINATE),
         LOCATION_Y_COORDINATE=as.character(LOCATION_Y_COORDINATE),
         LOCATION_HORIZONTAL_METHOD=as.character(LOCATION_HORIZONTAL_METHOD),
         LOCATION_HORIZONTAL_DATUM=as.character(LOCATION_HORIZONTAL_DATUM))
event<-event %>% 
  mutate(EVENT_LMAS_LOCATION_HISTORY_ID=as.character(EVENT_LMAS_LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=format(EVENT_LMAS_SAMPLE_DATE,"%m/%d/%Y"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER))

#make sure samples sorted the same
event<-event %>% 
  arrange(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER)
rownames(event)<-NULL
location<-location %>% 
  arrange(LAKE_HISTORY_ID,EVENT_LMAS_LOCATION_HISTORY_ID,LOCATION_NAME,LOCATION_TYPE,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE,
          LOCATION_HORIZONTAL_METHOD,LOCATION_HORIZONTAL_DATUM)
rownames(location)<-NULL

junk<-merge(event,location,by=c('EVENT_LMAS_LOCATION_HISTORY_ID'),all=TRUE)
junk<-junk %>% filter(is.na(LAKE_HISTORY_ID))
#THIS SHOULD HAVE 0 RECORDS

#check for fields with NA records
#only these fields should have NA: EVENT_LMAS_PROJECT_CDE
colnames(event)[colSums(is.na(event)) > 0]

rm(list=setdiff(ls(), "data"))

###NOTE####
#you have to do this before sending to Cindy:
#•    Open your spreadsheet 
#•    Create a new column 
#•    Format it as text 
#•    Copy and paste the data into the new column 
#•    Delete the old column 
#•    Save the file as Excel  


#check ITS returned table
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_EVENT_LMAS.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-ITS %>% 
  select(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_SAMPLE_TIME)
event<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/event.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event<-event %>% 
  select(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_SAMPLE_TIME)

#change to character so can check for diff
ITS<-ITS %>% 
  mutate(EVENT_LMAS_LOCATION_HISTORY_ID=as.character(EVENT_LMAS_LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"),
         EVENT_LMAS_SAMPLE_DATE=format(EVENT_LMAS_SAMPLE_DATE,"%m/%d/%Y"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER),
         EVENT_LMAS_SAMPLE_TIME=as.character(EVENT_LMAS_SAMPLE_TIME))
event<-event %>% 
  mutate(EVENT_LMAS_LOCATION_HISTORY_ID=as.character(EVENT_LMAS_LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=format(EVENT_LMAS_SAMPLE_DATE,"%m/%d/%Y"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER),
         EVENT_LMAS_SAMPLE_TIME=as.character(EVENT_LMAS_SAMPLE_TIME))


#check class
sapply(event,class)
sapply(ITS,class)

#make sure samples sorted the same
ITS<-ITS %>% 
  arrange(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER)
rownames(ITS)<-NULL
event<-event %>% 
  arrange(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER)
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


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, event, by = name.i)
})
names(test2) <- names(ITS)
test
