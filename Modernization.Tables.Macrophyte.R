#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")

#MM/DD/YYYY or MM/DD/YYYY HH24:MI

####################################################################################################
#macrophyte table

rm(list=setdiff(ls(), "data"))

sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(MS_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         MS_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         MS_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         MS_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         MS_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         MS_SAMPLE_NAME=SEI_SAMPLE_NAME,
         MS_SAMPLE_TYPE=SEI_SAMPLE_TYPE) %>% filter(MS_SAMPLE_TYPE=="MACROPHYTE") %>% 
  select(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,MS_EVENT_LMAS_SAMPLE_TIME,MS_INFORMATION_TYPE) %>% 
  distinct()

macrophyte<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/final_macrophyte_data_08_21_2020.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
macrophyte<-macrophyte %>% 
  mutate(MS_SAMPLE_TYPE="MACROPHYTE",
         MS_PICTURE=NA) %>% 
  rename(MS_LOCATION_HISTORY_ID=MS_LOCATION_HISTORY_ID,
         MS_EVENT_LMAS_SAMPLE_DATE=MS_EVENT_LMAS_SAMPLE_DATE,
         MS_SAMPLE_PARENTGLOBAL_ID=MS_SAMPLE_PARENTGLOBAL_ID,
         MS_SAMPLE_GLOBAL_ID=MS_SAMPLE_GLOBAL_ID,
         MS_LMAS_SAMPLER=MS_LMAS_SAMPLER,
         MS_TOSS_LENGTH=MS_TOSS_LENGTH,
         MS_SAMPLE_LOCATION=MS_SAMPLE_LOCATION,
         MS_SAMPLE_LAT=MS_SAMPLE_LAT,
         MS_SAMPLE_LONG=MS_SAMPLE_LONG,
         MS_DEPTH=MS_DEPTH,
         MS_ABUNDANCE=MS_ABUNDANCE,
         MS_TAXA=MS_SCIENTIFIC_NAME,
         MS_COMMON_NAME=MS_COMMON_NAME,    
         MS_COMMENT=MS_COMMENT) %>% 
  mutate(MS_EVENT_LMAS_SAMPLE_DATE=ifelse(MS_LOCATION_HISTORY_ID=="0601GIL0287_DH"&MS_EVENT_LMAS_SAMPLE_DATE=="8/15/2019","8/13/2019",MS_EVENT_LMAS_SAMPLE_DATE),
         MS_EVENT_LMAS_SAMPLE_DATE=ifelse(MS_LOCATION_HISTORY_ID=="1003ROL0168_DH"&MS_EVENT_LMAS_SAMPLE_DATE=="8/29/2019","8/28/2019",MS_EVENT_LMAS_SAMPLE_DATE),
         MS_EVENT_LMAS_SAMPLE_DATE=as.Date(MS_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  distinct() %>% 
  select(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,
         MS_SAMPLE_TYPE,
         MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
         MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_TAXA,MS_COMMON_NAME,MS_PICTURE,              
         MS_COMMENT)
#check that there are no empty values
colnames(macrophyte)[colSums(is.na(macrophyte)) > 0]

macrophyte<-merge(macrophyte,sei,by=c('MS_LOCATION_HISTORY_ID','MS_EVENT_LMAS_SAMPLE_DATE'),all.x = TRUE)
macrophyte<-macrophyte %>% 
  select(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_SAMPLE_TIME,MS_EVENT_LMAS_DATA_PROVIDER,
         MS_SAMPLE_TYPE,MS_INFORMATION_TYPE,
         MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
         MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_TAXA,MS_COMMON_NAME,MS_PICTURE,              
         MS_COMMENT) %>% 
  mutate(MS_COMMENT=gsub("\n","",MS_COMMENT))

write.csv(macrophyte,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/macrophyte.csv", na = "", quote = TRUE, row.names = FALSE)
rm(list=setdiff(ls(), "data"))
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
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(MS_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         MS_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         MS_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         MS_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         MS_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_NAME=SEI_SAMPLE_NAME,
         MS_SAMPLE_TYPE=SEI_SAMPLE_TYPE)
macro<-macro %>% 
  mutate(MS_LOCATION_HISTORY_ID=as.character(MS_LOCATION_HISTORY_ID),
         MS_EVENT_LMAS_SAMPLE_DATE=as.Date(MS_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         MS_EVENT_LMAS_DATA_PROVIDER=as.character(MS_EVENT_LMAS_DATA_PROVIDER),
         MS_EVENT_LMAS_SAMPLE_TIME=as.character(MS_EVENT_LMAS_SAMPLE_TIME),
         MS_SAMPLE_TYPE=as.character(MS_SAMPLE_TYPE),
         MS_INFORMATION_TYPE=as.character(MS_INFORMATION_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,MS_EVENT_LMAS_SAMPLE_TIME,MS_SAMPLE_TYPE,MS_INFORMATION_TYPE) %>% 
  mutate(event="this")
macro<-macro %>% 
  arrange(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,MS_EVENT_LMAS_SAMPLE_TIME,MS_SAMPLE_TYPE,MS_INFORMATION_TYPE)


junk<-merge(macro,SEI,by=c('MS_LOCATION_HISTORY_ID','MS_EVENT_LMAS_SAMPLE_DATE','MS_EVENT_LMAS_DATA_PROVIDER','MS_EVENT_LMAS_SAMPLE_TIME','MS_SAMPLE_TYPE','MS_INFORMATION_TYPE'),all=TRUE)
junk<-junk %>% filter(is.na(event))

#check for fields with NA records
#only these fields should have NA: "MS_COMMON_NAME"      "MS_PICTURE"          "MS_COMMENT"  
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
rm(list=setdiff(ls(), "data"))
macro<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/macrophyte.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_MACROPHYTE_SAMPLE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,
         MS_SAMPLE_TYPE,MS_INFORMATION_TYPE,
         MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
         MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_TAXA,MS_COMMON_NAME,MS_COMMENT)%>% 
  arrange(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,
          MS_SAMPLE_TYPE,MS_INFORMATION_TYPE,
          MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
          MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_TAXA,MS_COMMON_NAME,MS_COMMENT)
macro<-macro %>%
  select(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,
         MS_SAMPLE_TYPE,MS_INFORMATION_TYPE,
         MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
         MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_TAXA,MS_COMMON_NAME,MS_COMMENT) %>% 
  arrange(MS_LOCATION_HISTORY_ID,MS_EVENT_LMAS_SAMPLE_DATE,MS_EVENT_LMAS_DATA_PROVIDER,
          MS_SAMPLE_TYPE,MS_INFORMATION_TYPE,
          MS_SAMPLE_PARENTGLOBAL_ID,MS_SAMPLE_GLOBAL_ID,MS_LMAS_SAMPLER,MS_TOSS_LENGTH,MS_SAMPLE_LOCATION,
          MS_SAMPLE_LAT,MS_SAMPLE_LONG,MS_DEPTH,MS_ABUNDANCE,MS_TAXA,MS_COMMON_NAME,MS_COMMENT)


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
#test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, macro, by = name.i)
})
names(test2) <- names(ITS)
#test2

