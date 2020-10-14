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
#Read in Data tables first
rm(list=setdiff(ls(), "data"))
head(data)

####################################################################################################
####################################################################################################
#LCI SAMPLE_EVENT_INFORMATION 

library(dplyr)
library(tidyr)
rec<-data
rec<-rec %>% 
  filter(SAMPLE_DATE>'2019-01-01',
         DATA_PROVIDER=="LCI") %>% 
  distinct()

sei_lci<-rec %>% 
  select(LOCATION_ID,SAMPLE_DATE,DATA_PROVIDER,INFO_TYPE,SAMPLE_ID,Characteristic.Name,SAMPLE_NAME) %>% 
  distinct() 
sei_lci<-sei_lci %>% 
  select(LOCATION_ID,SAMPLE_DATE,DATA_PROVIDER,INFO_TYPE,SAMPLE_NAME,SAMPLE_ID) %>% 
  distinct() %>% 
  mutate(SAMPLE_TYPE=NA,
         SAMPLE_TYPE=ifelse(INFO_TYPE=='OW','WATER COLUMN',SAMPLE_TYPE),
         SAMPLE_TYPE=ifelse(INFO_TYPE=='SD','WATER COLUMN',SAMPLE_TYPE),
         SAMPLE_TYPE=ifelse(INFO_TYPE=='DP','WATER COLUMN',SAMPLE_TYPE),
         SAMPLE_TYPE=ifelse(INFO_TYPE=='BS','WATER COLUMN',SAMPLE_TYPE),
         SAMPLE_TYPE=ifelse(INFO_TYPE=='MP','MACROPHYTE',SAMPLE_TYPE)) %>% 
  select(LOCATION_ID,SAMPLE_DATE,DATA_PROVIDER,SAMPLE_TYPE,INFO_TYPE,SAMPLE_NAME,SAMPLE_ID) %>% 
  distinct()

#add purpose
purpose<-read.csv("Event.Purpose.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
purpose<-purpose %>% 
  select(LOCATION_ID,PURPOSE) %>% 
  distinct()
sei_lci<-merge(sei_lci,purpose,by=c('LOCATION_ID'),all.x = TRUE)
  
  
#check, there should be 4 sample types per 
check<-sei_lci %>% 
  select(LOCATION_ID,SAMPLE_DATE,DATA_PROVIDER,SAMPLE_TYPE,INFO_TYPE) %>%
  mutate(types=paste(SAMPLE_TYPE,INFO_TYPE,sep=" - ")) %>% 
  select(LOCATION_ID,SAMPLE_DATE,DATA_PROVIDER,types) %>%
  group_by(LOCATION_ID,SAMPLE_DATE) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  #  filter(n<3) %>% 
  distinct() %>% 
  arrange(LOCATION_ID,SAMPLE_DATE,types) %>% 
  spread(types,n)

#rename fields
sei_lci<-sei_lci %>% 
  rename(SEI_LOCATION_HISTORY_ID=LOCATION_ID,
         SEI_EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         SEI_EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         SEI_SAMPLE_TYPE=SAMPLE_TYPE,
         SEI_INFORMATION_TYPE=INFO_TYPE,
         SEI_SAMPLE_NAME=SAMPLE_NAME,
         SEI_PURPOSE=PURPOSE) %>% 
  distinct()
#merge with event table
event<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/event.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event<-event %>% rename(SEI_LOCATION_HISTORY_ID=EVENT_LMAS_LOCATION_HISTORY_ID,
                        SEI_EVENT_LMAS_SAMPLE_DATE=EVENT_LMAS_SAMPLE_DATE,
                        SEI_EVENT_LMAS_DATA_PROVIDER=EVENT_LMAS_DATA_PROVIDER) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
sei_lci<-merge(sei_lci,event,by=c('SEI_LOCATION_HISTORY_ID','SEI_EVENT_LMAS_SAMPLE_DATE','SEI_EVENT_LMAS_DATA_PROVIDER'),all.x=TRUE)
sei_lci<-sei_lci %>% 
  rename(SEI_SAMPLE_ID=SAMPLE_ID,
         SEI_EVENT_LMAS_SAMPLE_TIME=EVENT_LMAS_SAMPLE_TIME)
#necessary step to make sure there are no duplicates
sei_lci<-sei_lci %>% 
  arrange(SEI_PURPOSE,SEI_SAMPLE_NAME,SEI_SAMPLE_ID) %>% 
  distinct(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,.keep_all = TRUE)
write.csv(sei_lci, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv", na = "", quote = TRUE, row.names = FALSE)
write.csv(sei_lci, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.only.tables/sample_event_information.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=setdiff(ls(), "data"))
####################################################################################################
####################################################################################################
####################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#adding CSLAP
#MAKE SURE TO DO TOUPPER!!
cslap<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/CSLAP/2019.CSLAP.Sample.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
cslap<-cslap %>% select(SAMPLE_NAME,LAKE_NAME,SAMPLE_DATE,INFO_TYPE,SAMPLE_TYPE,LOCATION_ID,LAKE_PROJ_CODE,DATA_PROVIDER,LakeID,TIME) %>% distinct()
cslap2<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/CSLAP/2019.CSLAP.EventTable.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
cslap2<-cslap2 %>% select(SAMPLE_NAME,LAKE_NAME,SAMPLE_DATE,INFO_TYPE,SAMPLE_TYPE,LOCATION_ID,LAKE_PROJ_CODE,DATA_PROVIDER,LakeID,PURPOSE) %>% distinct()
cslap<-merge(cslap,cslap2,by=c('SAMPLE_NAME','LAKE_NAME','SAMPLE_DATE','INFO_TYPE','SAMPLE_TYPE','LOCATION_ID','LAKE_PROJ_CODE','DATA_PROVIDER','LakeID'),all=TRUE)
rm(cslap2)
#fix incorrect info_types
cslap<-cslap %>% 
  mutate(INFO_TYPE=ifelse(SAMPLE_NAME %in% c('19-115.1-01','19-115.1-02','19-115.1-03','19-115.1-04','19-115.1-05','19-115.1-06','19-115.1-07','19-115.1-08')&is.na(TIME),"SD",INFO_TYPE))
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.only.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/event.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event<-event %>% rename(SEI_LOCATION_HISTORY_ID=EVENT_LMAS_LOCATION_HISTORY_ID,
                        SEI_EVENT_LMAS_SAMPLE_DATE=EVENT_LMAS_SAMPLE_DATE,
                        SEI_EVENT_LMAS_DATA_PROVIDER=EVENT_LMAS_DATA_PROVIDER,
                        SEI_EVENT_LMAS_SAMPLE_TIME=EVENT_LMAS_SAMPLE_TIME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))

#reformat 
cslap<-cslap %>% 
  rename(SEI_LOCATION_HISTORY_ID=LOCATION_ID,
         SEI_EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         SEI_EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         SEI_SAMPLE_TYPE=SAMPLE_TYPE,
         SEI_INFORMATION_TYPE=INFO_TYPE,
         SEI_SAMPLE_NAME=SAMPLE_NAME,
         SEI_PURPOSE=PURPOSE) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"),
         SEI_SAMPLE_ID=NA,
         SEI_LOCATION_HISTORY_ID=toupper(SEI_LOCATION_HISTORY_ID),
         SEI_EVENT_LMAS_DATA_PROVIDER=toupper(SEI_EVENT_LMAS_DATA_PROVIDER),
         SEI_SAMPLE_TYPE=toupper(SEI_SAMPLE_TYPE),
         SEI_INFORMATION_TYPE=toupper(SEI_INFORMATION_TYPE),
         SEI_SAMPLE_NAME=toupper(SEI_SAMPLE_NAME),
         SEI_PURPOSE=toupper(SEI_PURPOSE))%>% 
  filter(!SEI_SAMPLE_NAME %in% c('19-182.3-11','19-182.3-12','19-182.3-14'),
         !is.na(SEI_LOCATION_HISTORY_ID)) %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME,SEI_SAMPLE_ID,SEI_PURPOSE) %>% 
  distinct()
cslap<-merge(cslap,event,all.x=TRUE)
#now make sure only one time is associated with these sample: 19-254.1-B2, 19-254.1-B3
#This is the only record that has two times for one sample date which is why the merge doesn't work correctly
cslap<-cslap %>% 
  filter(!(SEI_SAMPLE_NAME=="19-254.1-B2"&SEI_EVENT_LMAS_SAMPLE_TIME=="2019-08-21 00:01:00"),
         !(SEI_SAMPLE_NAME=="19-254.1-B3"&SEI_EVENT_LMAS_SAMPLE_TIME=="2019-08-21 00:00:00"))
SEI<-SEI %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME,SEI_SAMPLE_ID,SEI_PURPOSE) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
SEI<-merge(SEI,cslap,all=TRUE)
SEI<-SEI %>% distinct()
#necessary step to make sure there are no duplicates
SEI<-SEI %>% 
  arrange(SEI_PURPOSE,SEI_SAMPLE_NAME,SEI_SAMPLE_ID) %>% 
  distinct(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,.keep_all = TRUE)

write.csv(SEI, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv", na = "", quote = TRUE, row.names = FALSE)
write.csv(cslap, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.only.tables/sample_event_information.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=setdiff(ls(), "data"))

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#HABs SAMPLE_EVENT_INFORMATION 
habs<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/habs.event.information.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lci.only.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei2<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.only.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-merge(sei,sei2,all=TRUE)
rm(sei2)
event<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/event.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event<-event %>% rename(SEI_LOCATION_HISTORY_ID=EVENT_LMAS_LOCATION_HISTORY_ID,
                        SEI_EVENT_LMAS_SAMPLE_DATE=EVENT_LMAS_SAMPLE_DATE,
                        SEI_EVENT_LMAS_DATA_PROVIDER=EVENT_LMAS_DATA_PROVIDER,
                        SEI_EVENT_LMAS_SAMPLE_TIME=EVENT_LMAS_SAMPLE_TIME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))

habs<-habs %>% 
  rename(SEI_LOCATION_HISTORY_ID=LOCATION_HISTORY_ID,
         SEI_EVENT_LMAS_SAMPLE_DATE=LMAS_SAMPLE_DATE,
         SEI_EVENT_LMAS_DATA_PROVIDER=LMAS_DATA_PROVIDER,
         SEI_SAMPLE_TYPE=SAMPLE_TYPE,
         SEI_SAMPLE_NAME=HFD_SAMPLE_NAME,
         SEI_PURPOSE=PURPOSE,
         SEI_INFORMATION_TYPE=HFD_INFORMATION_TYPE) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         SEI_LOCATION_HISTORY_ID=as.character(SEI_LOCATION_HISTORY_ID),
         SEI_EVENT_LMAS_DATA_PROVIDER=as.character(SEI_EVENT_LMAS_DATA_PROVIDER),
         SEI_SAMPLE_TYPE=as.character(SEI_SAMPLE_TYPE),
         SEI_INFORMATION_TYPE=as.character(SEI_INFORMATION_TYPE),
         SEI_SAMPLE_NAME=as.character(SEI_SAMPLE_NAME),
         SEI_LOCATION_HISTORY_ID=ifelse(SEI_LOCATION_HISTORY_ID=="1306WALXXX1_C","1306WALXXX1_ESOPUS",SEI_LOCATION_HISTORY_ID),         
         SEI_PURPOSE=as.character(SEI_PURPOSE),
         SEI_LOCATION_HISTORY_ID=toupper(SEI_LOCATION_HISTORY_ID),
         SEI_EVENT_LMAS_DATA_PROVIDER=toupper(SEI_EVENT_LMAS_DATA_PROVIDER),
         SEI_SAMPLE_TYPE=toupper(SEI_SAMPLE_TYPE),
         SEI_INFORMATION_TYPE=toupper(SEI_INFORMATION_TYPE),
         SEI_SAMPLE_NAME=toupper(SEI_SAMPLE_NAME),
         SEI_PURPOSE=toupper(SEI_PURPOSE),
         SEI_SAMPLE_ID=NA) %>% 
#remove time and project code so can draw these from the event table
    select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME,SEI_SAMPLE_ID,SEI_PURPOSE) %>% 
  distinct()
habs<-merge(habs,event,by=c('SEI_LOCATION_HISTORY_ID','SEI_EVENT_LMAS_SAMPLE_DATE','SEI_EVENT_LMAS_DATA_PROVIDER'),all.x=TRUE)
habs<-habs %>%   
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME,SEI_SAMPLE_ID,SEI_PURPOSE) %>% 
  distinct()
sei<-sei %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME,SEI_SAMPLE_ID,SEI_PURPOSE) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  distinct()
sapply(sei,class)
sapply(habs,class)
sei<-merge(sei,habs,all=TRUE)
#necessary step to make sure there are no duplicates
sei<-sei %>% 
  arrange(SEI_PURPOSE,SEI_SAMPLE_NAME,SEI_SAMPLE_ID) %>% 
  distinct(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,.keep_all = TRUE)


write.csv(sei, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv", na = "", quote = TRUE, row.names = FALSE)
write.csv(habs, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.only.tables/sample_event_information.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=setdiff(ls(), "data"))




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
event<-event %>% rename(SEI_LOCATION_HISTORY_ID=EVENT_LMAS_LOCATION_HISTORY_ID,
                        SEI_EVENT_LMAS_SAMPLE_DATE=EVENT_LMAS_SAMPLE_DATE,
                        SEI_EVENT_LMAS_DATA_PROVIDER=EVENT_LMAS_DATA_PROVIDER,
                        SEI_EVENT_LMAS_SAMPLE_TIME=EVENT_LMAS_SAMPLE_TIME)
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
event<-event %>% 
  mutate(SEI_LOCATION_HISTORY_ID=as.character(SEI_LOCATION_HISTORY_ID),
         SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         SEI_EVENT_LMAS_SAMPLE_DATE=as.character(SEI_EVENT_LMAS_SAMPLE_DATE),
         SEI_EVENT_LMAS_DATA_PROVIDER=as.character(SEI_EVENT_LMAS_DATA_PROVIDER),
         SEI_EVENT_LMAS_SAMPLE_TIME=as.character(SEI_EVENT_LMAS_SAMPLE_TIME))
SEI<-SEI %>% 
  mutate(SEI_LOCATION_HISTORY_ID=as.character(SEI_LOCATION_HISTORY_ID),
         SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         SEI_EVENT_LMAS_SAMPLE_DATE=as.character(SEI_EVENT_LMAS_SAMPLE_DATE),
         SEI_EVENT_LMAS_DATA_PROVIDER=as.character(SEI_EVENT_LMAS_DATA_PROVIDER),
         SEI_EVENT_LMAS_SAMPLE_TIME=as.character(SEI_EVENT_LMAS_SAMPLE_TIME))

#make sure samples sorted the same
event<-event %>% 
  arrange(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME) %>% 
  mutate(event="this")
SEI<-SEI %>% 
  arrange(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME)


junk<-merge(event,SEI,by=c('SEI_LOCATION_HISTORY_ID','SEI_EVENT_LMAS_SAMPLE_DATE','SEI_EVENT_LMAS_DATA_PROVIDER','SEI_EVENT_LMAS_SAMPLE_TIME'),all=TRUE)
junk1<-junk %>% filter(is.na(event))
junk<-junk %>% filter(is.na(SEI_SAMPLE_TYPE))
#SHOULD HAVE 0 RECORDS

#check for fields with NA records
#only these fields should have NA: "SEI_SAMPLE_ID"     "SEI_SAMPLE_NAME"     "SEI_PURPOSE"
colnames(SEI)[colSums(is.na(SEI)) > 0]
#check, there shouldn't be multiple samples per location/date
check<-SEI %>% 
  group_by(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  filter(n>1) %>% 
  distinct()


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
#Sample Event Information data
#Check that file returned from ITS matches
#check that lakes table matches its
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_SAMPLE_EVENT_INFORMATION.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(SEI_LOCATION_HISTORY_ID,
         SEI_EVENT_LMAS_SAMPLE_DATE,
         SEI_EVENT_LMAS_DATA_PROVIDER,
         SEI_SAMPLE_TYPE,
         SEI_INFORMATION_TYPE,
         SEI_SAMPLE_NAME,
         SEI_PURPOSE)
SEI<-SEI %>% 
  select(SEI_LOCATION_HISTORY_ID,
         SEI_EVENT_LMAS_SAMPLE_DATE,
         SEI_EVENT_LMAS_DATA_PROVIDER,
         SEI_SAMPLE_TYPE,
         SEI_INFORMATION_TYPE,
         SEI_SAMPLE_NAME,
         SEI_PURPOSE)

#format as dates so can compare
SEI<-SEI %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
ITS<-ITS %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"))

#check class
sapply(SEI,class)
sapply(ITS,class)

#make sure samples sorted the same
ITS<-ITS %>% 
  arrange(SEI_LOCATION_HISTORY_ID,
          SEI_EVENT_LMAS_SAMPLE_DATE,
          SEI_EVENT_LMAS_DATA_PROVIDER,
          SEI_SAMPLE_TYPE,
          SEI_INFORMATION_TYPE,
          SEI_SAMPLE_NAME,
          SEI_PURPOSE)
rownames(ITS)<-NULL
SEI<-SEI %>% 
  arrange(SEI_LOCATION_HISTORY_ID,
          SEI_EVENT_LMAS_SAMPLE_DATE,
          SEI_EVENT_LMAS_DATA_PROVIDER,
          SEI_SAMPLE_TYPE,
          SEI_INFORMATION_TYPE,
          SEI_SAMPLE_NAME,
          SEI_PURPOSE)
rownames(SEI)<-NULL


#check if identical
identical(SEI,ITS)

#check for differences between specific columns
test <- lapply(names(SEI), function(name.i){
  anti_join(SEI, ITS, by = name.i)
})
names(test) <- names(SEI)
#test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, SEI, by = name.i)
})
names(test2) <- names(ITS)
#test2