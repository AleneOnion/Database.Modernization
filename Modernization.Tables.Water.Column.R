#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")


#Alene Onion
#Feb 2020
#Script to generate a clean lci downstream tables
####################################################################################################
#Read in Data tables first
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
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  rename(LOCATION_HISTORY_ID=LOCATION_HISTORY_ID,
         EVENT_LMAS_SAMPLE_DATE=EVENT_LMAS_SAMPLE_DATE,
         EVENT_LMAS_DATA_PROVIDER=EVENT_LMAS_DATA_PROVIDER,
         EVENT_LMAS_PROJECT_CDE=EVENT_LMAS_PROJECT_CDE,
         EVENT_LMAS_TIME=EVENT_LMAS_TIME,
         WCFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         WCFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_NAME=SEI_SAMPLE_NAME)

#OW and BS samples
wc<-rec %>% 
  filter(INFO_TYPE %in% c('OW','BS')) %>% 
  select(LOCATION_ID,SAMPLE_DATE,INFO_TYPE,SAMPLE_NAME) %>% 
  rename(LOCATION_HISTORY_ID=LOCATION_ID,
         EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         WCFD_INFORMATION_TYPE=INFO_TYPE) %>% 
  distinct()

#water column table
wct<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/field.data/water_column_table_2019.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
wct<-wct %>% 
  select(GLOBALID,SAMPLE_NAME,LOCATION_HISTORY_ID,START_DEPTH,END_DEPTH,WCFD_EQUIPMENT_TYPE,WCFD_EQUIPMENT_DESC,INFORMATION_TYPE,WCFD_SAMPLE_METHOD) %>% 
  rename(WCFD_INFORMATION_TYPE=INFORMATION_TYPE,
         WCFD_START_DEPTH=START_DEPTH,
         WCFD_END_DEPTH=END_DEPTH)
wcn<-wct %>% 
  select(GLOBALID,SAMPLE_NAME,LOCATION_HISTORY_ID)
wct<-wct %>% 
  select(SAMPLE_NAME,LOCATION_HISTORY_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,WCFD_EQUIPMENT_DESC,WCFD_INFORMATION_TYPE,WCFD_SAMPLE_METHOD)
wc<-merge(wc,wcn,by=c('SAMPLE_NAME','LOCATION_HISTORY_ID'),all=TRUE)
wc<-merge(wc,wct,by=c('SAMPLE_NAME','LOCATION_HISTORY_ID','WCFD_INFORMATION_TYPE'),all=TRUE)
rm(list=c('wct','wcn'))
wc<-merge(wc,sei,by=c('LOCATION_HISTORY_ID','SAMPLE_NAME','WCFD_INFORMATION_TYPE','EVENT_LMAS_SAMPLE_DATE'),all.x=TRUE)
wc<-wc %>% 
  rename(WCFD_GLOBALID=GLOBALID) %>% 
  select(SAMPLE_NAME,LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_TIME,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBALID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
  mutate(EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))

#add in missing equipment information
missing<-wc %>% 
  filter(is.na(WCFD_GLOBALID))
wc<-anti_join(wc,missing)
#remove missing fields
missing<-missing %>% 
  select(-WCFD_START_DEPTH,-WCFD_END_DEPTH,-WCFD_EQUIPMENT_TYPE,-WCFD_EQUIPMENT_DESC,-WCFD_SAMPLE_METHOD)
add<-rec %>% 
  filter(INFO_TYPE %in% c('OW','BS')) %>% 
  select(LOCATION_ID,SAMPLE_DATE,INFO_TYPE,SAMPLE_NAME,START_DEPTH,END_DEPTH,EQUIPMENT_TYPE,EQUIPMENT_DESC,SAMPLE_METHOD) %>% 
  rename(LOCATION_HISTORY_ID=LOCATION_ID,
         EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         WCFD_INFORMATION_TYPE=INFO_TYPE,
         WCFD_START_DEPTH=START_DEPTH,
         WCFD_END_DEPTH=END_DEPTH,
         WCFD_EQUIPMENT_TYPE=EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC=EQUIPMENT_DESC,
         WCFD_SAMPLE_METHOD=SAMPLE_METHOD) %>% 
  distinct()
missing<-merge(missing,add,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE','WCFD_INFORMATION_TYPE','SAMPLE_NAME'),all.x=TRUE)
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
  rename(LOCATION_HISTORY_ID=LOCATION_ID,
         EVENT_LMAS_DATA_PROVIDER=DATA_PROVIDER,
         EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         EVENT_LMAS_TIME=TIME,
         WCFD_INFORMATION_TYPE=INFO_TYPE,
         WCFD_EQUIPMENT_TYPE=EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC=EQUIPMENT_DESC)%>% 
  filter(WCFD_INFORMATION_TYPE=="DP") %>% 
  select(SAMPLE_NAME,LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,WCFD_INFORMATION_TYPE,
         WCFD_EQUIPMENT_TYPE,WCFD_EQUIPMENT_DESC) %>% 
  distinct() %>% 
  mutate(LOCATION_HISTORY_ID=trimws(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=trimws(EVENT_LMAS_SAMPLE_DATE),
         WCFD_INFORMATION_TYPE=trimws(WCFD_INFORMATION_TYPE),
         WCFD_EQUIPMENT_TYPE=trimws(WCFD_EQUIPMENT_TYPE),
         WCFD_EQUIPMENT_DESC=trimws(WCFD_EQUIPMENT_DESC)) %>% 
  mutate(SAMPLE_TYPE="WATER COLUMN",
         EVENT_LMAS_PROJECT_CDE=NA,
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))  
#getting end depth
enddepth<-rec %>% 
  filter(Characteristic.Name=="DISSOLVED OXYGEN SATURATION") %>% 
  select(LOCATION_ID,SAMPLE_DATE,Depth) %>% 
  distinct() %>% 
  arrange(LOCATION_ID,SAMPLE_DATE,-Depth) %>% 
  distinct(LOCATION_ID,SAMPLE_DATE,.keep_all = TRUE) %>% 
  distinct() %>% 
  rename(WCFD_END_DEPTH=Depth,
         LOCATION_HISTORY_ID=LOCATION_ID,
         EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE)
#getting global id
global<-wc %>% 
  select(WCFD_GLOBALID,LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_TIME,EVENT_LMAS_DATA_PROVIDER) %>% 
  distinct()
#adding all of these
profile<-merge(profile,global,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE'),all.x=TRUE)
profile<-merge(profile,enddepth,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE'),all.x=TRUE)
profile<-profile %>% 
  mutate(WCFD_START_DEPTH=0,
         WCFD_SAMPLE_TYPE="WATER COLUMN",
         WCFD_SAMPLE_METHOD="SOP 203",
         EVENT_LMAS_PROJECT_CDE=NA) %>% 
  select(SAMPLE_NAME,LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_TIME,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBALID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD)
wc<-merge(wc,profile,all=TRUE)
wc<-wc %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_TIME,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBALID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
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
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#reformat 
cs<-cs %>% 
  select(WCFD_SAMPLE_NAME,WCFD_START_DEPTH,WCFD_INFORMATION_TYPE,WCFD_END_DEPTH,WCFD_EQUIPMENT_DESC,WCFD_EQUIPMENT_TYPE,WCFD_SAMPLE_METHOD,WCFD_DEPTH_UNIT)
SEI<-SEI %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(WCFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         WCFD_SAMPLE_NAME=SEI_SAMPLE_NAME,
         WCFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE)
cs<-merge(cs,SEI,by=c('WCFD_SAMPLE_NAME','WCFD_INFORMATION_TYPE'),all.x=TRUE)
cs<-cs %>% 
  mutate(WCFD_GLOBALID=NA,
         WCFD_GLOBALID=as.character(WCFD_GLOBALID),
         WCFD_EQUIPMENT_DESC=as.character(WCFD_EQUIPMENT_DESC)) %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_TIME,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBALID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
  distinct() 
rm(SEI)
wc<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/water_column.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
wc<-wc %>% 
  mutate(EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))

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
wc<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/water_column.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
SEI<-SEI %>% 
  mutate(LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER),
         EVENT_LMAS_TIME=as.character(EVENT_LMAS_TIME),
         SEI_SAMPLE_TYPE=as.character(SEI_SAMPLE_TYPE),
         SEI_INFORMATION_TYPE=as.character(SEI_INFORMATION_TYPE)) %>% 
  rename(WCFD_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         WCFD_INFORMATION_TYPE=SEI_INFORMATION_TYPE)
wc<-wc %>% 
  mutate(LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER),
         EVENT_LMAS_TIME=as.character(EVENT_LMAS_TIME),
         WCFD_SAMPLE_TYPE=as.character(WCFD_SAMPLE_TYPE),
         WCFD_INFORMATION_TYPE=as.character(WCFD_INFORMATION_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_TIME,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE) %>% 
  mutate(event="this")
wc<-wc %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_TIME,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE)


junk<-merge(wc,SEI,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE','EVENT_LMAS_PROJECT_CDE','EVENT_LMAS_DATA_PROVIDER','EVENT_LMAS_TIME','WCFD_SAMPLE_TYPE','WCFD_INFORMATION_TYPE'),all=TRUE)
junk<-junk %>% filter(is.na(event))

#check for fields with NA records
#only these fields should have NA: "EVENT_LMAS_PROJECT_CDE" "WCFD_GLOBALID"          "WCFD_START_DEPTH"(b/c of missing surveys)       "WCFD_END_DEPTH"         "WCFD_EQUIPMENT_DESC" 
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
  select(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_PROJECT_CDE,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
         WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
         WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
  arrange(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_PROJECT_CDE,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
          WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
          WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD)
wc<-wc %>%
  rename(WCFD_LOCATION_HISTORY_ID=LOCATION_HISTORY_ID,
         WCFD_EVENT_LMAS_SAMPLE_DATE=EVENT_LMAS_SAMPLE_DATE,
         WCFD_EVENT_LMAS_SAMPLE_TIME=EVENT_LMAS_TIME,
         WCFD_EVENT_LMAS_PROJECT_CDE=EVENT_LMAS_PROJECT_CDE,
         WCFD_EVENT_LMAS_DATA_PROVIDER=EVENT_LMAS_DATA_PROVIDER,
         WCFD_GLOBAL_ID=WCFD_GLOBALID) %>% 
  select(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_PROJECT_CDE,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
                  WCFD_GLOBAL_ID,WCFD_START_DEPTH,WCFD_END_DEPTH,WCFD_EQUIPMENT_TYPE,
                  WCFD_EQUIPMENT_DESC,WCFD_SAMPLE_METHOD) %>% 
  arrange(WCFD_LOCATION_HISTORY_ID,WCFD_EVENT_LMAS_SAMPLE_DATE,WCFD_EVENT_LMAS_SAMPLE_TIME,WCFD_EVENT_LMAS_PROJECT_CDE,WCFD_EVENT_LMAS_DATA_PROVIDER,WCFD_SAMPLE_TYPE,WCFD_INFORMATION_TYPE,
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
  mutate(WCFD_EVENT_LMAS_SAMPLE_DATE=as.Date(WCFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
ITS<-ITS %>% 
  mutate(WCFD_EVENT_LMAS_SAMPLE_DATE=as.Date(WCFD_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"))

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
test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, wc, by = name.i)
})
names(test2) <- names(ITS)
test2

rm(list=c('field','ITS','test','test2','wc'))
