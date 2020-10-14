#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")

#MM/DD/YYYY or MM/DD/YYYY HH24:MI


####################################################################################################
####################################################################################################
#habs field table

rm(list=setdiff(ls(), "data"))

sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  rename(HFD_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         HFD_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         HFD_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         HFD_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         HFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         HFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         HFD_SAMPLE_NAME=SEI_SAMPLE_NAME)

field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/habs.field.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
field<-field %>% 
  rename(HFD_EVENT_LMAS_SAMPLE_DATE=LMAS_SAMPLE_DATE,
         HFD_EVENT_LMAS_DATA_PROVIDER=LMAS_DATA_PROVIDER,
         HFD_LOCATION_HISTORY_ID=LOCATION_HISTORY_ID,
         HFD_SAMPLE_TYPE=SAMPLE_TYPE,
         HFD_SAMPLE_LAT=SAMPLE_LAT,
         HFD_SAMPLE_LONG=SAMPLE_LONG) %>% 
  select(HFD_LOCATION_HISTORY_ID,HFD_EVENT_LMAS_SAMPLE_DATE,HFD_EVENT_LMAS_DATA_PROVIDER,
         HFD_SAMPLE_TYPE,HFD_INFORMATION_TYPE,HFD_SAMPLE_METHOD,HFD_EXTENT,HFD_PERCENT,HFD_BLOOM_DESC,HFD_EQUIPMENT_TYPE,HFD_EQUIPMENT_DESC,HFD_REMARK,HFD_SAMPLE_LAT,HFD_SAMPLE_LONG)
#get from sei table
#HFD_EVENT_LMAS_SAMPLE_TIME
#sample name
field<-merge(field,sei,all.x = TRUE)  
  
field<-field %>% 
  select(HFD_LOCATION_HISTORY_ID,HFD_EVENT_LMAS_SAMPLE_DATE,HFD_EVENT_LMAS_SAMPLE_TIME,HFD_EVENT_LMAS_DATA_PROVIDER,
         HFD_SAMPLE_TYPE,HFD_INFORMATION_TYPE,HFD_SAMPLE_NAME,HFD_SAMPLE_METHOD,HFD_EXTENT,HFD_PERCENT,HFD_BLOOM_DESC,
         HFD_EQUIPMENT_TYPE,HFD_EQUIPMENT_DESC,HFD_REMARK,HFD_SAMPLE_LAT,HFD_SAMPLE_LONG)
 
write.csv(field,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.field.csv", na = "", quote = TRUE, row.names = FALSE)


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
field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
SEI<-SEI %>% 
  rename(HFD_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         HFD_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         HFD_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         HFD_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         HFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         HFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         HFD_SAMPLE_NAME=SEI_SAMPLE_NAME) %>% 
  mutate(HFD_LOCATION_HISTORY_ID=as.character(HFD_LOCATION_HISTORY_ID),
         HFD_EVENT_LMAS_SAMPLE_DATE=as.Date(HFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         HFD_EVENT_LMAS_SAMPLE_DATE=as.character(HFD_EVENT_LMAS_SAMPLE_DATE),
         HFD_EVENT_LMAS_DATA_PROVIDER=as.character(HFD_EVENT_LMAS_DATA_PROVIDER),
         HFD_EVENT_LMAS_SAMPLE_TIME=as.character(HFD_EVENT_LMAS_SAMPLE_TIME),
         HFD_SAMPLE_TYPE=as.character(HFD_SAMPLE_TYPE))
field<-field %>% 
  mutate(HFD_LOCATION_HISTORY_ID=as.character(HFD_LOCATION_HISTORY_ID),
         HFD_EVENT_LMAS_SAMPLE_DATE=as.Date(HFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         HFD_EVENT_LMAS_SAMPLE_DATE=as.character(HFD_EVENT_LMAS_SAMPLE_DATE),
         HFD_EVENT_LMAS_DATA_PROVIDER=as.character(HFD_EVENT_LMAS_DATA_PROVIDER),
         HFD_EVENT_LMAS_SAMPLE_TIME=as.character(HFD_EVENT_LMAS_SAMPLE_TIME),
         HFD_SAMPLE_TYPE=as.character(HFD_SAMPLE_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(HFD_LOCATION_HISTORY_ID,HFD_EVENT_LMAS_SAMPLE_DATE,HFD_EVENT_LMAS_DATA_PROVIDER,HFD_EVENT_LMAS_SAMPLE_TIME,HFD_SAMPLE_TYPE) %>% 
  mutate(event="this")
field<-field %>% 
  arrange(HFD_LOCATION_HISTORY_ID,HFD_EVENT_LMAS_SAMPLE_DATE,HFD_EVENT_LMAS_DATA_PROVIDER,HFD_EVENT_LMAS_SAMPLE_TIME,HFD_SAMPLE_TYPE) 


junk<-merge(field,SEI,by=c('HFD_LOCATION_HISTORY_ID','HFD_EVENT_LMAS_SAMPLE_DATE','HFD_EVENT_LMAS_DATA_PROVIDER','HFD_EVENT_LMAS_SAMPLE_TIME','HFD_SAMPLE_TYPE','HFD_INFORMATION_TYPE','HFD_SAMPLE_NAME'),all=TRUE)
junk<-junk %>% filter(is.na(event))
#check for fields with NA records
#only these fields should have NA: "HFD_SAMPLE_NAME"    "HFD_EXTENT"         "HFD_BLOOM_DESC"     "HFD_EQUIPMENT_TYPE" "HFD_EQUIPMENT_DESC" "HFD_REMARK"         "HFD_SAMPLE_LAT"     "HFD_SAMPLE_LONG"   
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
field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/habs.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_HAB_FIELD_DATA.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(HFD_EVENT_LMAS_DATA_PROVIDER,HFD_EVENT_LMAS_SAMPLE_DATE,HFD_EVENT_LMAS_SAMPLE_TIME,HFD_INFORMATION_TYPE,HFD_SAMPLE_NAME,HFD_SAMPLE_TYPE,HFD_SAMPLE_METHOD,
         HFD_BLOOM_DESC,HFD_EXTENT,HFD_PERCENT,HFD_EQUIPMENT_TYPE,HFD_EQUIPMENT_DESC,HFD_SAMPLE_LAT,HFD_SAMPLE_LONG)
field<-field %>% 
  select(HFD_EVENT_LMAS_DATA_PROVIDER,HFD_EVENT_LMAS_SAMPLE_DATE,HFD_EVENT_LMAS_SAMPLE_TIME,HFD_INFORMATION_TYPE,HFD_SAMPLE_NAME,HFD_SAMPLE_TYPE,HFD_SAMPLE_METHOD,
         HFD_BLOOM_DESC,HFD_EXTENT,HFD_PERCENT,HFD_EQUIPMENT_TYPE,HFD_EQUIPMENT_DESC,HFD_SAMPLE_LAT,HFD_SAMPLE_LONG)

#missing fields in field versus ITS file
#HFD_ESF,HFD_UFI,HFD_BLOOM_LOCATION,HFD_WIND_DIRECTION,HFD_WIND_INTENSITY,HFD_COMMENT

#format as dates so can compare
field<-field %>% 
  mutate(HFD_EVENT_LMAS_SAMPLE_DATE=as.Date(HFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
ITS<-ITS %>% 
  mutate(HFD_EVENT_LMAS_SAMPLE_DATE=as.Date(HFD_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"))


#check class
sapply(field,class)
sapply(ITS,class)

#make sure samples sorted the same
ITS<-ITS %>% 
  arrange(HFD_EVENT_LMAS_DATA_PROVIDER,HFD_EVENT_LMAS_SAMPLE_DATE,HFD_EVENT_LMAS_SAMPLE_TIME,HFD_INFORMATION_TYPE,HFD_SAMPLE_NAME,HFD_SAMPLE_TYPE,HFD_SAMPLE_METHOD,
          HFD_BLOOM_DESC,HFD_EXTENT,HFD_PERCENT,HFD_EQUIPMENT_TYPE,HFD_EQUIPMENT_DESC,HFD_SAMPLE_LAT,HFD_SAMPLE_LONG)
rownames(ITS)<-NULL
field<-field %>% 
  arrange(HFD_EVENT_LMAS_DATA_PROVIDER,HFD_EVENT_LMAS_SAMPLE_DATE,HFD_EVENT_LMAS_SAMPLE_TIME,HFD_INFORMATION_TYPE,HFD_SAMPLE_NAME,HFD_SAMPLE_TYPE,HFD_SAMPLE_METHOD,
          HFD_BLOOM_DESC,HFD_EXTENT,HFD_PERCENT,HFD_EQUIPMENT_TYPE,HFD_EQUIPMENT_DESC,HFD_SAMPLE_LAT,HFD_SAMPLE_LONG)
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
