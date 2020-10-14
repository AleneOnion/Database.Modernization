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
#LCI Water Column Table
library(dplyr)
library(tidyr)
rec<-data
rec<-rec %>% 
  filter(SAMPLE_DATE>'2019-01-01',
         DATA_PROVIDER=="LCI") %>% 
  distinct()

#load sample event information table
sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  rename(WCFD_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         WCFD_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         WCFD_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         WCFD_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         WCFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         WCFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_NAME=SEI_SAMPLE_NAME) %>% 
  select(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,SAMPLE_NAME) 

#OW and BS samples
wc<-rec %>% 
  filter(INFO_TYPE %in% c('OW','BS')) %>% 
  select(LOCATION_ID,SAMPLE_DATE,INFO_TYPE,SAMPLE_NAME) %>% 
  rename(WCFD_LOCATION_HISTORY_ID=LOCATION_ID,
         WCFD_EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         WCFD_INFORMATION_TYPE=INFO_TYPE) %>% 
  distinct()

#water column table
wct<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/field.data/water_column_table_2019.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
wct<-wct %>% 
  select(GLOBALID,SAMPLE_NAME,LOCATION_HISTORY_ID,START_DEPTH,END_DEPTH,WCFD_EQUIPMENT_TYPE,WCFD_EQUIPMENT_DESC,INFORMATION_TYPE,WCFD_SAMPLE_METHOD) %>% 
  rename(WCFD_LOCATION_HISTORY_ID=LOCATION_HISTORY_ID,
         WCFD_INFORMATION_TYPE=INFORMATION_TYPE,
         WCFD_START_DEPTH=START_DEPTH,
         WCFD_END_DEPTH=END_DEPTH)
wcn<-wct %>% 
  select(GLOBALID,SAMPLE_NAME,WCFD_LOCATION_HISTORY_ID)
wct<-wct %>% 
  select(SAMPLE_NAME,WCFD_LOCATION_HISTORY_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,WCFD_EQUIPMENT_DESC,WCFD_INFORMATION_TYPE,WCFD_SAMPLE_METHOD)
wc<-merge(wc,wcn,by=c('SAMPLE_NAME','WCFD_LOCATION_HISTORY_ID'),all=TRUE)
wc<-merge(wc,wct,by=c('SAMPLE_NAME','WCFD_LOCATION_HISTORY_ID','WCFD_INFORMATION_TYPE'),all=TRUE)
rm(list=c('wct','wcn'))
wc<-merge(wc,sei,by=c('WCFD_LOCATION_HISTORY_ID','SAMPLE_NAME','WCFD_INFORMATION_TYPE','WCFD_EVENT_LMAS_SAMPLE_DATE'),all.x=TRUE)
wc<-wc %>% 
  rename(WCFD_GLOBAL_ID=GLOBALID) %>% 
  select(SAMPLE_NAME,WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
  mutate(WCFD_EVENT_LMAS_SAMPLE_DATE=as.Date(WCFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))

#add in missing equipment information
missing<-wc %>% 
  filter(is.na(WCFD_GLOBAL_ID))
wc<-anti_join(wc,missing)
#remove missing fields
missing<-missing %>% 
  select(-WCFD_START_DEPTH,-WCFD_END_DEPTH,-WCFD_EQUIPMENT_TYPE,-WCFD_EQUIPMENT_DESC,-WCFD_SAMPLE_METHOD)
add<-rec %>% 
  filter(INFO_TYPE %in% c('OW','BS')) %>% 
  select(LOCATION_ID,SAMPLE_DATE,INFO_TYPE,SAMPLE_NAME,START_DEPTH,END_DEPTH,EQUIPMENT_TYPE,EQUIPMENT_DESC,SAMPLE_METHOD) %>% 
  rename(WCFD_LOCATION_HISTORY_ID=LOCATION_ID,
         WCFD_EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         WCFD_INFORMATION_TYPE=INFO_TYPE,
         WCFD_START_DEPTH=START_DEPTH,
         WCFD_END_DEPTH=END_DEPTH,
         WCFD_EQUIPMENT_TYPE=EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC=EQUIPMENT_DESC,
         WCFD_SAMPLE_METHOD=SAMPLE_METHOD) %>% 
  distinct()
missing<-merge(missing,add,by=c('WCFD_LOCATION_HISTORY_ID','WCFD_EVENT_LMAS_SAMPLE_DATE','WCFD_INFORMATION_TYPE','SAMPLE_NAME'),all.x=TRUE)
missing<-missing %>% 
  mutate(WCFD_EQUIPMENT_TYPE=ifelse(is.na(WCFD_EQUIPMENT_TYPE)&WCFD_INFORMATION_TYPE=="BS","VAN DORN",WCFD_EQUIPMENT_TYPE),
         WCFD_EQUIPMENT_TYPE=ifelse(is.na(WCFD_EQUIPMENT_TYPE)&WCFD_INFORMATION_TYPE=="OW","VINYL TUBE",WCFD_EQUIPMENT_TYPE)) %>% 
  distinct()
wc<-merge(wc,missing,all=TRUE)
wc<-wc %>% 
  mutate(WCFD_EQUIPMENT_DESC=ifelse(WCFD_EQUIPMENT_TYPE=="VINYL TUBE","INTEGRATED SAMPLER (MPCA DESIGN)",WCFD_EQUIPMENT_DESC)) %>% 
  distinct()




#merge profile with WC
#generate profile data table
profile<-rec %>% 
  rename(WCFD_LOCATION_HISTORY_ID=LOCATION_ID,
         WCFD_EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         WCFD_EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         WCFD_EVENT_LMAS_SAMPLE_TIME=TIME,
         WCFD_INFORMATION_TYPE=INFO_TYPE,
         WCFD_EQUIPMENT_TYPE=EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC=EQUIPMENT_DESC)%>% 
  filter(WCFD_INFORMATION_TYPE=="DP") %>% 
  select(SAMPLE_NAME,WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_INFORMATION_TYPE,
         WCFD_EQUIPMENT_TYPE,WCFD_EQUIPMENT_DESC) %>% 
  distinct() %>% 
  mutate(WCFD_LOCATION_HISTORY_ID=trimws(WCFD_LOCATION_HISTORY_ID),
         WCFD_EVENT_LMAS_SAMPLE_DATE=trimws(WCFD_EVENT_LMAS_SAMPLE_DATE),
         WCFD_INFORMATION_TYPE=trimws(WCFD_INFORMATION_TYPE),
         WCFD_EQUIPMENT_TYPE=trimws(WCFD_EQUIPMENT_TYPE),
         WCFD_EQUIPMENT_DESC=trimws(WCFD_EQUIPMENT_DESC)) %>% 
  mutate(SAMPLE_TYPE="WATER COLUMN",
         WCFD_EVENT_LMAS_SAMPLE_DATE=as.Date(WCFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))  
#getting end depth
enddepth<-rec %>% 
  filter(Characteristic.Name=="DISSOLVED OXYGEN SATURATION") %>% 
  select(LOCATION_ID,SAMPLE_DATE,Depth) %>% 
  distinct() %>% 
  arrange(LOCATION_ID,SAMPLE_DATE,-Depth) %>% 
  distinct(LOCATION_ID,SAMPLE_DATE,.keep_all = TRUE) %>% 
  distinct() %>% 
  rename(WCFD_END_DEPTH=Depth,
         WCFD_LOCATION_HISTORY_ID=LOCATION_ID,
         WCFD_EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE)
#getting global id
global<-wc %>% 
  select(WCFD_GLOBAL_ID,WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER) %>% 
  distinct()
#adding all of these
profile<-merge(profile,global,by=c('WCFD_LOCATION_HISTORY_ID','WCFD_EVENT_LMAS_SAMPLE_DATE'),all.x=TRUE)
profile<-merge(profile,enddepth,by=c('WCFD_LOCATION_HISTORY_ID','WCFD_EVENT_LMAS_SAMPLE_DATE'),all.x=TRUE)
profile<-profile %>% 
  mutate(WCFD_START_DEPTH=0,
         WCFD_SAMPLE_TYPE="WATER COLUMN",
         WCFD_SAMPLE_METHOD="SOP 203",) %>% 
  select(SAMPLE_NAME,WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD)
wc<-merge(wc,profile,all=TRUE)
wc<-wc %>% 
  select(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
  filter(!is.na(WCFD_LOCATION_HISTORY_ID)) %>% 
  distinct()

write.csv(wc,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/water_column.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=setdiff(ls(), "data"))

####################################################################################################
####################################################################################################
####################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#adding CSLAP
cs<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/CSLAP/watercolumn_CSLAP.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
cs<-cs %>% 
  mutate(WCFD_INFORMATION_TYPE=ifelse(WCFD_SAMPLE_NAME %in% c('19-115.1-11','19-115.1-12','19-115.1-13','19-115.1-14','19-115.1-15','19-115.1-16','19-115.1-17','19-115.1-18'),"SD",WCFD_INFORMATION_TYPE))
         
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
         
#reformat 
cs<-cs %>% 
  select(WCFD_SAMPLE_NAME,WCFD_START_DEPTH,WCFD_INFORMATION_TYPE,WCFD_END_DEPTH,WCFD_EQUIPMENT_DESC,WCFD_EQUIPMENT_TYPE,WCFD_SAMPLE_METHOD,WCFD_DEPTH_UNIT)
SEI<-SEI %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(WCFD_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         WCFD_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         WCFD_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         WCFD_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         WCFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         WCFD_SAMPLE_NAME=SEI_SAMPLE_NAME,
         WCFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE)
cs<-merge(cs,SEI,by=c('WCFD_SAMPLE_NAME','WCFD_INFORMATION_TYPE'),all.x=TRUE)
cs<-cs %>% 
  mutate(WCFD_GLOBAL_ID=NA,
         WCFD_GLOBAL_ID=as.character(WCFD_GLOBAL_ID),
         WCFD_EQUIPMENT_DESC=as.character(WCFD_EQUIPMENT_DESC)) %>% 
  select(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
  filter(!is.na(WCFD_LOCATION_HISTORY_ID)) %>% 
  distinct() 
rm(SEI)
wc<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/water_column.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
wc<-wc %>% 
  mutate(WCFD_EVENT_LMAS_SAMPLE_DATE=as.Date(WCFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))

sapply(cs,class)
sapply(wc,class)
wc<-merge(wc,cs,all=TRUE)

write.csv(wc, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/water_column.csv", na = "", quote = TRUE, row.names = FALSE)
write.csv(cs, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/cslap.only.tables/water_column.csv", na = "", quote = TRUE, row.names = FALSE)
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
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
wc<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/water_column.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
SEI<-SEI %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(WCFD_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         WCFD_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         WCFD_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         WCFD_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         WCFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         WCFD_SAMPLE_NAME=SEI_SAMPLE_NAME,
         WCFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE)
wc<-wc %>% 
  mutate(WCFD_LOCATION_HISTORY_ID=as.character(WCFD_LOCATION_HISTORY_ID),
         WCFD_EVENT_LMAS_SAMPLE_DATE=as.Date(WCFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         WCFD_EVENT_LMAS_DATA_PROVIDER=as.character(WCFD_EVENT_LMAS_DATA_PROVIDER),
         WCFD_EVENT_LMAS_SAMPLE_TIME=as.character(WCFD_EVENT_LMAS_SAMPLE_TIME),
         WCFD_SAMPLE_TYPE=as.character(WCFD_SAMPLE_TYPE),
         WCFD_INFORMATION_TYPE=as.character(WCFD_INFORMATION_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE) %>% 
  mutate(event="this")
wc<-wc %>% 
  arrange(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE)

sapply(wc,class)
sapply(SEI,class)

junk<-merge(wc,SEI,by=c('WCFD_LOCATION_HISTORY_ID','WCFD_EVENT_LMAS_SAMPLE_DATE','WCFD_EVENT_LMAS_DATA_PROVIDER','WCFD_EVENT_LMAS_SAMPLE_TIME','WCFD_SAMPLE_TYPE','WCFD_INFORMATION_TYPE'),all=TRUE)
junk1<-junk %>% filter(is.na(event))
junk<-junk %>% filter(is.na(WCFD_SAMPLE_TYPE))
#check for fields with NA records
#only these fields should have NA: "WCFD_GLOBAL_ID"          "WCFD_START_DEPTH"(b/c of missing surveys)       "WCFD_END_DEPTH"         "WCFD_EQUIPMENT_DESC" 
colnames(wc)[colSums(is.na(wc)) > 0]

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
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#Water Column Field data
#Check that file returned from ITS matches
#check that lakes table matches its
wc<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/water_column.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_WATER_COLUMN_FIELD_DATA.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
  arrange(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
          WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
          WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD)
wc<-wc %>%
  select(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
                  WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
                  WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
  arrange(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
          WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
          WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD)

#change to character so can check for diff
#wc<-wc %>% 
#  mutate(LAKE_FIN=as.numeric(LAKE_FIN))
ITS<-ITS %>% 
  mutate(WCFD_START_DEPTH=as.numeric(WCFD_START_DEPTH),
         WCFD_END_DEPTH=as.numeric(WCFD_END_DEPTH))

#format as dates so can compare
wc<-wc %>% 
  mutate(WCFD_EVENT_LMAS_SAMPLE_DATE=as.Date(WCFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  mutate(WCFD_EVENT_LMAS_SAMPLE_TIME=as.character(WCFD_EVENT_LMAS_SAMPLE_TIME),
         WCFD_EVENT_LMAS_SAMPLE_TIME=sub(".*? ", "", WCFD_EVENT_LMAS_SAMPLE_TIME),
         junk=substr(WCFD_EVENT_LMAS_SAMPLE_TIME,1,2),
         junk=as.numeric(junk),
         pm=ifelse(junk>11,"PM","AM"),
         junk=ifelse(junk>12,junk-12,junk),
         WCFD_EVENT_LMAS_SAMPLE_TIME=substr(WCFD_EVENT_LMAS_SAMPLE_TIME,3,8),
         WCFD_EVENT_LMAS_SAMPLE_TIME=paste(junk,WCFD_EVENT_LMAS_SAMPLE_TIME,sep=""),
         WCFD_EVENT_LMAS_SAMPLE_DATE2=format(WCFD_EVENT_LMAS_SAMPLE_DATE,"%m/%d/%Y"),
         WCFD_EVENT_LMAS_SAMPLE_DATE2=as.character(WCFD_EVENT_LMAS_SAMPLE_DATE2),
         WCFD_EVENT_LMAS_SAMPLE_DATE2=sub("^0", "", WCFD_EVENT_LMAS_SAMPLE_DATE2),
         WCFD_EVENT_LMAS_SAMPLE_DATE2=sub("/0","/",WCFD_EVENT_LMAS_SAMPLE_DATE2),
         WCFD_EVENT_LMAS_SAMPLE_TIME=paste(WCFD_EVENT_LMAS_SAMPLE_DATE2,WCFD_EVENT_LMAS_SAMPLE_TIME,pm,sep=" ")) %>% 
  select(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD)
ITS<-ITS %>% 
  mutate(WCFD_EVENT_LMAS_SAMPLE_DATE=as.Date(WCFD_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y")) %>% 
  mutate(WCFD_EVENT_LMAS_SAMPLE_TIME=sub(".000000","",WCFD_EVENT_LMAS_SAMPLE_TIME))

#check class
sapply(wc,class)
sapply(ITS,class)

#check if identical
identical(wc,ITS)

#check for differences between specific columns
test <- lapply(names(wc), function(name.i){
  anti_join(wc, ITS, by = name.i)
})
names(test) <- names(wc)
#test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, wc, by = name.i)
})
names(test2) <- names(ITS)
#test2

rm(list=c('field','ITS','test','test2','wc'))
