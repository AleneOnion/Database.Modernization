#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")

#MM/DD/YYYY or MM/DD/YYYY HH24:MI


####################################################################################################
####################################################################################################
#habs status table

sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  rename(HAB_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         HAB_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         HAB_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         HAB_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         HAB_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         HAB_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         HAB_SAMPLE_NAME=SEI_SAMPLE_NAME)

status<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/habs.status.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
status<-status %>% 
  rename(HAB_LOCATION_HISTORY_ID=LOCATION_HISTORY_ID,
         HAB_EVENT_LMAS_SAMPLE_DATE=LMAS_SAMPLE_DATE,
         HAB_INFORMATION_TYPE=HFD_INFORMATION_TYPE,
         HAB_EVENT_LMAS_DATA_PROVIDER=LMAS_DATA_PROVIDER,
         HAB_SAMPLE_TYPE=SAMPLE_TYPE) %>% 
  select(HAB_LOCATION_HISTORY_ID,HAB_EVENT_LMAS_SAMPLE_DATE,HAB_EVENT_LMAS_DATA_PROVIDER,
         HAB_SAMPLE_TYPE,HAB_INFORMATION_TYPE,HAB_STATUS,HAB_STATUS_DATE,HAB_STATUS_REMARK)
#get from sei table
#HAB_EVENT_LMAS_SAMPLE_TIME
#sample name
status<-merge(status,sei,all.x = TRUE)  
  
status<-status %>% 
  select(HAB_LOCATION_HISTORY_ID,HAB_EVENT_LMAS_SAMPLE_DATE,HAB_EVENT_LMAS_SAMPLE_TIME,HAB_EVENT_LMAS_DATA_PROVIDER,
         HAB_SAMPLE_TYPE,HAB_INFORMATION_TYPE,HAB_STATUS,HAB_STATUS_DATE,HAB_STATUS_REMARK)
 
write.csv(status,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.status.csv", na = "", quote = TRUE, row.names = FALSE)

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
status<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.status.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
SEI<-SEI %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  rename(HAB_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         HAB_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         HAB_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         HAB_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         HAB_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         HAB_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         HAB_SAMPLE_NAME=SEI_SAMPLE_NAME) %>%
  mutate(HAB_LOCATION_HISTORY_ID=as.character(HAB_LOCATION_HISTORY_ID),
         HAB_EVENT_LMAS_SAMPLE_DATE=as.Date(HAB_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         HAB_EVENT_LMAS_SAMPLE_DATE=as.character(HAB_EVENT_LMAS_SAMPLE_DATE),
         HAB_EVENT_LMAS_DATA_PROVIDER=as.character(HAB_EVENT_LMAS_DATA_PROVIDER),
         HAB_EVENT_LMAS_SAMPLE_TIME=as.character(HAB_EVENT_LMAS_SAMPLE_TIME),
         HAB_SAMPLE_TYPE=as.character(HAB_SAMPLE_TYPE))
status<-status %>% 
  mutate(HAB_LOCATION_HISTORY_ID=as.character(HAB_LOCATION_HISTORY_ID),
         HAB_EVENT_LMAS_SAMPLE_DATE=as.Date(HAB_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         HAB_EVENT_LMAS_SAMPLE_DATE=as.character(HAB_EVENT_LMAS_SAMPLE_DATE),
         HAB_EVENT_LMAS_DATA_PROVIDER=as.character(HAB_EVENT_LMAS_DATA_PROVIDER),
         HAB_EVENT_LMAS_SAMPLE_TIME=as.character(HAB_EVENT_LMAS_SAMPLE_TIME),
         HAB_SAMPLE_TYPE=as.character(HAB_SAMPLE_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(HAB_LOCATION_HISTORY_ID,HAB_EVENT_LMAS_SAMPLE_DATE,HAB_EVENT_LMAS_DATA_PROVIDER,HAB_EVENT_LMAS_SAMPLE_TIME,HAB_SAMPLE_TYPE) %>% 
  mutate(event="this")
status<-status %>% 
  arrange(HAB_LOCATION_HISTORY_ID,HAB_EVENT_LMAS_SAMPLE_DATE,HAB_EVENT_LMAS_DATA_PROVIDER,HAB_EVENT_LMAS_SAMPLE_TIME,HAB_SAMPLE_TYPE) 


junk<-merge(status,SEI,by=c('HAB_LOCATION_HISTORY_ID','HAB_EVENT_LMAS_SAMPLE_DATE','HAB_EVENT_LMAS_DATA_PROVIDER','HAB_EVENT_LMAS_SAMPLE_TIME','HAB_SAMPLE_TYPE','HAB_INFORMATION_TYPE'),all=TRUE)
junk<-junk %>% filter(is.na(event))

#check for statuss with NA records
#only these statuss should have NA: "WCFD_GLOBALID"          "WCFD_START_DEPTH"(b/c of missing surveys)       "WCFD_END_DEPTH"         "WCFD_EQUIPMENT_DESC" 
colnames(status)[colSums(is.na(status)) > 0]

#remove these for sending to cindy
#status<-anti_join(status,junk)
#write.csv(status,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.status.csv", na = "", quote = TRUE, row.names = FALSE)


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
field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.status.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_HAB_STATUS.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(HS_LOCATION_HISTORY_ID,HS_EVENT_LMAS_SAMPLE_DATE,HS_EVENT_LMAS_DATA_PROVIDER,
         HS_EVENT_LMAS_SAMPLE_TIME,HS_INFORMATION_TYPE,HS_SAMPLE_TYPE,HS_HAB_STATUS,HS_HAB_STATUS_DATE,HS_HAB_STATUS_REMARK)
field<-field %>% 
  rename(HS_LOCATION_HISTORY_ID=HAB_LOCATION_HISTORY_ID,
         HS_EVENT_LMAS_SAMPLE_DATE=HAB_EVENT_LMAS_SAMPLE_DATE,
         HS_EVENT_LMAS_SAMPLE_TIME=HAB_EVENT_LMAS_SAMPLE_TIME,
         HS_EVENT_LMAS_DATA_PROVIDER=HAB_EVENT_LMAS_DATA_PROVIDER,
         HS_SAMPLE_TYPE=HAB_SAMPLE_TYPE,
         HS_INFORMATION_TYPE=HAB_INFORMATION_TYPE,
         HS_HAB_STATUS=HAB_STATUS,
         HS_HAB_STATUS_DATE=HAB_STATUS_DATE,
         HS_HAB_STATUS_REMARK=HAB_STATUS_REMARK) %>% 
  select(HS_LOCATION_HISTORY_ID,HS_EVENT_LMAS_SAMPLE_DATE,HS_EVENT_LMAS_DATA_PROVIDER,
         HS_EVENT_LMAS_SAMPLE_TIME,HS_INFORMATION_TYPE,HS_SAMPLE_TYPE,HS_HAB_STATUS,HS_HAB_STATUS_DATE,HS_HAB_STATUS_REMARK)
#format as dates so can compare
field<-field %>% 
  mutate(HS_EVENT_LMAS_SAMPLE_DATE=as.Date(HS_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
ITS<-ITS %>% 
  mutate(HS_EVENT_LMAS_SAMPLE_DATE=as.Date(HS_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"))

#check class
sapply(field,class)
sapply(ITS,class)

#make sure samples sorted the same
ITS<-ITS %>% 
  arrange(HS_LOCATION_HISTORY_ID,HS_EVENT_LMAS_SAMPLE_DATE,HS_EVENT_LMAS_DATA_PROVIDER,
          HS_EVENT_LMAS_SAMPLE_TIME,HS_INFORMATION_TYPE,HS_SAMPLE_TYPE,HS_HAB_STATUS,HS_HAB_STATUS_DATE,HS_HAB_STATUS_REMARK)
rownames(ITS)<-NULL
field<-field %>% 
  arrange(HS_LOCATION_HISTORY_ID,HS_EVENT_LMAS_SAMPLE_DATE,HS_EVENT_LMAS_DATA_PROVIDER,
          HS_EVENT_LMAS_SAMPLE_TIME,HS_INFORMATION_TYPE,HS_SAMPLE_TYPE,HS_HAB_STATUS,HS_HAB_STATUS_DATE,HS_HAB_STATUS_REMARK)
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
