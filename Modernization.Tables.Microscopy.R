#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")

#MM/DD/YYYY or MM/DD/YYYY HH24:MI

####################################################################################################
#microscopy table

rm(list=setdiff(ls(), "data"))

sei<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sei<-sei %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         SEI_EVENT_LMAS_SAMPLE_DATE=as.character(SEI_EVENT_LMAS_SAMPLE_DATE)) %>% 
  rename(MR_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         MR_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         MR_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         MR_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         MR_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         MR_SAMPLE_NAME=SEI_SAMPLE_NAME,
         MR_SAMPLE_TYPE=SEI_SAMPLE_TYPE) %>% 
  select(MR_LOCATION_HISTORY_ID,MR_EVENT_LMAS_SAMPLE_DATE,MR_EVENT_LMAS_DATA_PROVIDER,MR_EVENT_LMAS_SAMPLE_TIME,MR_INFORMATION_TYPE,MR_SAMPLE_TYPE,MR_SAMPLE_NAME) %>% 
  distinct()

microscopy<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/microscopy.table.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
microscopy<-microscopy %>% 
  rename(MR_LOCATION_HISTORY_ID=MR_LOCATION_HISTORY_ID,
         MR_EVENT_LMAS_SAMPLE_DATE=MR_LMAS_SAMPLE_DATE,
         MR_SAMPLE_TYPE=SAMPLE_TYPE,
         MR_INFORMATION_TYPE=RSLT_INFORMATION_TYPE,
         MR_EVENT_LMAS_DATA_PROVIDER=MR_LMAS_DATA_PROVIDER,
         MR_SAMPLE_NAME=MR_SAMPLE_NAME,
         MR_CHARACTERISTIC_NAME=MR_CHARACTERISTIC_NAME,
         MR_LABORATORY_NAME_CDE=MR_LABORATORY_NAME_CDE,
         MR_LABORATORY_SAMPLE_NAME=MR_LABORATORY_SAMPLE_NAME,
         MR_TAXA=MR_TAXA,
         MR_RESULT_STRING=MR_RESULT_STRING) %>% 
  select(MR_LOCATION_HISTORY_ID,
         MR_EVENT_LMAS_SAMPLE_DATE,
         MR_SAMPLE_TYPE,
         MR_INFORMATION_TYPE,
         MR_EVENT_LMAS_DATA_PROVIDER,
         MR_CHARACTERISTIC_NAME,
         MR_LABORATORY_NAME_CDE,
         MR_LABORATORY_SAMPLE_NAME,
         MR_TAXA,
         MR_RESULT_STRING) %>% 
  distinct()

#check that there are no empty
colnames(microscopy)[colSums(is.na(microscopy)) > 0]
sapply(microscopy,class)
sapply(sei,class)

microscopy<-merge(microscopy,sei,by=c('MR_LOCATION_HISTORY_ID','MR_EVENT_LMAS_SAMPLE_DATE','MR_EVENT_LMAS_DATA_PROVIDER','MR_SAMPLE_TYPE','MR_INFORMATION_TYPE'),all.x = TRUE)

microscopy<-microscopy %>% 
  select(MR_LOCATION_HISTORY_ID,
         MR_EVENT_LMAS_SAMPLE_DATE,
         MR_EVENT_LMAS_SAMPLE_TIME,
         MR_SAMPLE_TYPE,
         MR_SAMPLE_NAME,
         MR_INFORMATION_TYPE,
         MR_EVENT_LMAS_DATA_PROVIDER,
         MR_CHARACTERISTIC_NAME,
         MR_LABORATORY_NAME_CDE,
         MR_LABORATORY_SAMPLE_NAME,
         MR_TAXA,
         MR_RESULT_STRING) 


write.csv(microscopy,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/microscopy.csv", na = "", quote = TRUE, row.names = FALSE)
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
macro<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/microscopy.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
SEI<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/sample_event_information.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
SEI<-SEI %>% 
  select(SEI_LOCATION_HISTORY_ID,SEI_EVENT_LMAS_SAMPLE_DATE,SEI_EVENT_LMAS_DATA_PROVIDER,SEI_EVENT_LMAS_SAMPLE_TIME,SEI_SAMPLE_TYPE,SEI_INFORMATION_TYPE,SEI_SAMPLE_NAME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  rename(MR_LOCATION_HISTORY_ID=SEI_LOCATION_HISTORY_ID,
         MR_EVENT_LMAS_SAMPLE_DATE=SEI_EVENT_LMAS_SAMPLE_DATE,
         MR_EVENT_LMAS_SAMPLE_TIME=SEI_EVENT_LMAS_SAMPLE_TIME,
         MR_EVENT_LMAS_DATA_PROVIDER=SEI_EVENT_LMAS_DATA_PROVIDER,
         MR_INFORMATION_TYPE=SEI_INFORMATION_TYPE,
         SAMPLE_NAME=SEI_SAMPLE_NAME,
         MR_SAMPLE_TYPE=SEI_SAMPLE_TYPE)
macro<-macro %>% 
  mutate(MR_LOCATION_HISTORY_ID=as.character(MR_LOCATION_HISTORY_ID),
         MR_EVENT_LMAS_SAMPLE_DATE=as.Date(MR_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         MR_EVENT_LMAS_DATA_PROVIDER=as.character(MR_EVENT_LMAS_DATA_PROVIDER),
         MR_EVENT_LMAS_SAMPLE_TIME=as.character(MR_EVENT_LMAS_SAMPLE_TIME),
         MR_SAMPLE_TYPE=as.character(MR_SAMPLE_TYPE),
         MR_INFORMATION_TYPE=as.character(MR_INFORMATION_TYPE))


#make sure samples sorted the same
SEI<-SEI %>% 
  arrange(MR_LOCATION_HISTORY_ID,MR_EVENT_LMAS_SAMPLE_DATE,MR_EVENT_LMAS_DATA_PROVIDER,MR_EVENT_LMAS_SAMPLE_TIME,MR_SAMPLE_TYPE,MR_INFORMATION_TYPE) %>% 
  mutate(event="this")
macro<-macro %>% 
  arrange(MR_LOCATION_HISTORY_ID,MR_EVENT_LMAS_SAMPLE_DATE,MR_EVENT_LMAS_DATA_PROVIDER,MR_EVENT_LMAS_SAMPLE_TIME,MR_SAMPLE_TYPE,MR_INFORMATION_TYPE)


junk<-merge(macro,SEI,by=c('MR_LOCATION_HISTORY_ID','MR_EVENT_LMAS_SAMPLE_DATE','MR_EVENT_LMAS_DATA_PROVIDER','MR_EVENT_LMAS_SAMPLE_TIME','MR_SAMPLE_TYPE','MR_INFORMATION_TYPE'),all=TRUE)
junk<-junk %>% filter(is.na(event))

#check for fields with NA records
#only these fields should have NA: "MR_EVENT_LMAS_SAMPLE_TIME" "MR_SAMPLE_NAME"            "MR_LABORATORY_SAMPLE_NAME"
colnames(macro)[colSums(is.na(macro)) > 0]

rm(list=setdiff(ls(), "data"))

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
rm(list=setdiff(ls(), "data"))

macro<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/microscopy.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#change to character so can check for diff
macro<-macro %>% 
  mutate(MR_LOCATION_HISTORY_ID=as.character(MR_LOCATION_HISTORY_ID),
         MR_EVENT_LMAS_SAMPLE_DATE=as.Date(MR_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"),
         MR_EVENT_LMAS_DATA_PROVIDER=as.character(MR_EVENT_LMAS_DATA_PROVIDER),
         MR_EVENT_LMAS_SAMPLE_TIME=as.character(MR_EVENT_LMAS_SAMPLE_TIME),
         MR_SAMPLE_TYPE=as.character(MR_SAMPLE_TYPE),
         MR_INFORMATION_TYPE=as.character(MR_INFORMATION_TYPE)) %>% 
  select(MR_EVENT_LMAS_DATA_PROVIDER,MR_EVENT_LMAS_SAMPLE_DATE,MR_EVENT_LMAS_SAMPLE_TIME,MR_INFORMATION_TYPE,MR_SAMPLE_TYPE,
         MR_SAMPLE_NAME,MR_CHARACTERISTIC_NAME,MR_LABORATORY_NAME_CDE,MR_LABORATORY_SAMPLE_NAME,MR_TAXA) %>% 
  arrange(MR_EVENT_LMAS_DATA_PROVIDER,MR_EVENT_LMAS_SAMPLE_DATE,MR_EVENT_LMAS_SAMPLE_TIME,MR_INFORMATION_TYPE,MR_SAMPLE_TYPE,
        MR_SAMPLE_NAME,MR_CHARACTERISTIC_NAME,MR_LABORATORY_NAME_CDE,MR_LABORATORY_SAMPLE_NAME,MR_TAXA) 
  
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_MICROSCOPY_RESULT.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS <-ITS %>% 
  select(MR_EVENT_LMAS_DATA_PROVIDER,MR_EVENT_LMAS_SAMPLE_DATE,MR_EVENT_LMAS_SAMPLE_TIME,MR_INFORMATION_TYPE,MR_SAMPLE_TYPE,
         MR_SAMPLE_NAME,MR_CHARACTERISTIC_NAME,MR_LABORATORY_NAME_CDE,MR_LABORATORY_SAMPLE_NAME,MR_TAXA) %>% 
  mutate(MR_EVENT_LMAS_SAMPLE_DATE=as.Date(MR_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y")) %>% 
  arrange(MR_EVENT_LMAS_DATA_PROVIDER,MR_EVENT_LMAS_SAMPLE_DATE,MR_EVENT_LMAS_SAMPLE_TIME,MR_INFORMATION_TYPE,MR_SAMPLE_TYPE,
         MR_SAMPLE_NAME,MR_CHARACTERISTIC_NAME,MR_LABORATORY_NAME_CDE,MR_LABORATORY_SAMPLE_NAME,MR_TAXA) 
  


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
  anti_join(ITS,macro, by = name.i)
})
names(test2) <- names(ITS)
#test

#NOTES
#The result values are rounded to 5 sig digs where as our data can be more than that
#

rm(list=setdiff(ls(), "data"))
