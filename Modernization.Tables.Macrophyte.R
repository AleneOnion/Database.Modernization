#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")

####################################################################################################
#macrophyte table
sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  mutate(EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE) %>% 
  rename(MACRO_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         MACRO_INFORMATION_TYPE=SEI_INFORMATION_TYPE) %>% 
  filter(MACRO_SAMPLE_TYPE=="MACROPHYTE") %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_TIME,MACRO_INFORMATION_TYPE) %>% 
  distinct()
macrophyte<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/final_macrophyte_data_03062020.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
macrophyte<-macrophyte %>% 
  mutate(MACRO_SAMPLE_TYPE="MACROPHYTE",
         MACRO_PICTURE=NA) %>% 
  rename(EVENT_LMAS_SAMPLE_DATE=SAMPLE_DATE,
         MACRO_SAMPLE_PARENTGLOBALID=PARENTGLOBALID,
         MACRO_LMAS_SAMPLER=LMAS_SAMPLER,
         MACRO_TOSS_LENGTH=TOSS_LENGTH,
         MACRO_SAMPLE_LOCATION=SAMPLE_LOCATION,
         MACRO_SAMPLE_LAT=SAMPLE_LAT,
         MACRO_SAMPLE_LONG=SAMPLE_LONG,
         MACRO_DEPTH=DEPTH,
         MACRO_ABUNDANCE=ABUNDANCE,
         MACRO_SCIENTIFIC_NAME=SCIENTIFIC_NAME,
         MACRO_COMMON_NAME=COMMON_NAME,    
         MACRO_COMMENTS=COMMENTS) %>% 
  mutate(EVENT_LMAS_SAMPLE_DATE=ifelse(LOCATION_HISTORY_ID=="0601GIL0287_DH"&EVENT_LMAS_SAMPLE_DATE=="8/15/2019 4:00","8/13/2019 4:00",EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_SAMPLE_DATE=ifelse(LOCATION_HISTORY_ID=="1003ROL0168_DH"&EVENT_LMAS_SAMPLE_DATE=="8/29/2019 4:00","8/28/2019 4:00",EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y")) %>% 
  distinct() %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,
         MACRO_SAMPLE_TYPE,
         MACRO_SAMPLE_PARENTGLOBALID,MACRO_SAMPLE_GLOBALID,MACRO_LMAS_SAMPLER,MACRO_TOSS_LENGTH,MACRO_SAMPLE_LOCATION,
         MACRO_SAMPLE_LAT,MACRO_SAMPLE_LONG,MACRO_DEPTH,MACRO_ABUNDANCE,MACRO_SCIENTIFIC_NAME,MACRO_COMMON_NAME,MACRO_PICTURE,              
         MACRO_COMMENTS)
macrophyte<-merge(macrophyte,sei,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE'),all.x = TRUE)
macrophyte<-macrophyte %>% 
  select(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_TIME,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_PROJECT_CDE,
         MACRO_SAMPLE_TYPE,MACRO_INFORMATION_TYPE,
         MACRO_SAMPLE_PARENTGLOBALID,MACRO_SAMPLE_GLOBALID,MACRO_LMAS_SAMPLER,MACRO_TOSS_LENGTH,MACRO_SAMPLE_LOCATION,
         MACRO_SAMPLE_LAT,MACRO_SAMPLE_LONG,MACRO_DEPTH,MACRO_ABUNDANCE,MACRO_SCIENTIFIC_NAME,MACRO_COMMON_NAME,MACRO_PICTURE,              
         MACRO_COMMENTS) %>% 
  mutate(MACRO_COMMENTS=gsub("\n","",MACRO_COMMENTS))

write.csv(macrophyte,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/macrophyte.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=c('macrophyte','sei'))
####################################################################################################
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
macro<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/macrophyte.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
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
  rename(MACRO_SAMPLE_TYPE=SEI_SAMPLE_TYPE,
         MACRO_INFORMATION_TYPE=SEI_INFORMATION_TYPE)
macro<-macro %>% 
  mutate(LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
         EVENT_LMAS_SAMPLE_DATE=as.Date(EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         EVENT_LMAS_SAMPLE_DATE=as.character(EVENT_LMAS_SAMPLE_DATE),
         EVENT_LMAS_PROJECT_CDE=as.character(EVENT_LMAS_PROJECT_CDE),
         EVENT_LMAS_DATA_PROVIDER=as.character(EVENT_LMAS_DATA_PROVIDER),
         EVENT_LMAS_TIME=as.character(EVENT_LMAS_TIME),
         MACRO_SAMPLE_TYPE=as.character(MACRO_SAMPLE_TYPE),
         MACRO_INFORMATION_TYPE=as.character(MACRO_INFORMATION_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_TIME,MACRO_SAMPLE_TYPE,MACRO_INFORMATION_TYPE) %>% 
  mutate(event="this")
macro<-macro %>% 
  arrange(LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_PROJECT_CDE,EVENT_LMAS_DATA_PROVIDER,EVENT_LMAS_TIME,MACRO_SAMPLE_TYPE,MACRO_INFORMATION_TYPE)


junk<-merge(macro,SEI,by=c('LOCATION_HISTORY_ID','EVENT_LMAS_SAMPLE_DATE','EVENT_LMAS_PROJECT_CDE','EVENT_LMAS_DATA_PROVIDER','EVENT_LMAS_TIME','MACRO_SAMPLE_TYPE','MACRO_INFORMATION_TYPE'),all=TRUE)
junk<-junk %>% filter(is.na(event))

#check for fields with NA records
#only these fields should have NA: "EVENT_LMAS_PROJECT_CDE" "MACRO_COMMON_NAME"      "MACRO_PICTURE"          "MACRO_COMMENTS"  
colnames(macro)[colSums(is.na(macro)) > 0]

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
macro<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/macrophyte.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_MACROPHYTE_SAMPLE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,MS_EVENT_LMAS_PROJECT_CDE,
         MS_SAMPLE_TYPE,MS_INFORMATION_TYPE,
         MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
         MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_SCIENTIFIC_NAME,MS_COMMON_NAME,MS_COMMENT)%>% 
  arrange(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,MS_EVENT_LMAS_PROJECT_CDE,
          MS_SAMPLE_TYPE,MS_INFORMATION_TYPE,
          MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
          MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_SCIENTIFIC_NAME,MS_COMMON_NAME,MS_COMMENT)
macro<-macro %>%
  rename(MS_LOCATION_HISTORY_ID=LOCATION_HISTORY_ID,
         MS_EVENT_LMAS_SAMPLE_DATE=EVENT_LMAS_SAMPLE_DATE,
         MS_EVENT_LMAS_SAMPLE_TIME=EVENT_LMAS_TIME,
         MS_EVENT_LMAS_DATA_PROVIDER=EVENT_LMAS_DATA_PROVIDER,
         MS_EVENT_LMAS_PROJECT_CDE=EVENT_LMAS_PROJECT_CDE,
         MS_SAMPLE_TYPE=MACRO_SAMPLE_TYPE,
         MS_INFORMATION_TYPE=MACRO_INFORMATION_TYPE,
         MS_SAMPLE_PARENTGLOBAL_ID=MACRO_SAMPLE_PARENTGLOBALID,
         MS_SAMPLE_GLOBAL_ID=MACRO_SAMPLE_GLOBALID,
         MS_LMAS_SAMPLER=MACRO_LMAS_SAMPLER,
         MS_TOSS_LENGTH=MACRO_TOSS_LENGTH,
         MS_SAMPLE_LOCATION=MACRO_SAMPLE_LOCATION,
         MS_SAMPLE_LAT=MACRO_SAMPLE_LAT,
         MS_SAMPLE_LONG=MACRO_SAMPLE_LONG,
         MS_DEPTH=MACRO_DEPTH,
         MS_ABUNDANCE=MACRO_ABUNDANCE,
         MS_SCIENTIFIC_NAME=MACRO_SCIENTIFIC_NAME,
         MS_COMMON_NAME=MACRO_COMMON_NAME,
         MS_COMMENT=MACRO_COMMENTS) %>% 
  select(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,MS_EVENT_LMAS_PROJECT_CDE,
         MS_SAMPLE_TYPE,MS_INFORMATION_TYPE,
         MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
         MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_SCIENTIFIC_NAME,MS_COMMON_NAME,MS_COMMENT) %>% 
  arrange(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,MS_EVENT_LMAS_PROJECT_CDE,
          MS_SAMPLE_TYPE,MS_INFORMATION_TYPE,
          MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
          MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_SCIENTIFIC_NAME,MS_COMMON_NAME,MS_COMMENT)


#format as dates so can compare
macro<-macro %>% 
  mutate(MS_EVENT_LMAS_SAMPLE_DATE=as.Date(MS_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
ITS<-ITS %>% 
  mutate(MS_EVENT_LMAS_SAMPLE_DATE=as.Date(MS_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"))

#check class
sapply(macro,class)
sapply(ITS,class)

#check if identical
identical(macro,ITS)

#check for differences between specific columns
test <- lapply(names(macro), function(name.i){
  anti_join(macro, ITS, by = name.i)
})
names(test) <- names(macro)
test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, macro, by = name.i)
})
names(test2) <- names(ITS)
test2
