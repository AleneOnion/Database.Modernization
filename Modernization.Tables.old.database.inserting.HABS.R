#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")



#creating file for upload to the databse
info<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/habs.event.information.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
event<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/habs.event.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
status<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/habs.status.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
field<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2019HABSdata/habs.field.to.load.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#renaming ields to merge
info<-info %>% 
  rename(LOCATION_ID=LOCATION_HISTORY_ID,
         PROJECT_CODE=PROJECT_CDE,
         DATA_PROVIDER=DATA_PROVIDER,
         INFO_TYPE=HFD_INFORMATION_TYPE,
         SAMPLE_NAME=HFD_SAMPLE_NAME) %>% 
  select(LOCATION_ID,
         SAMPLE_DATE,
         PROJECT_CODE,
         DATA_PROVIDER,
         INFO_TYPE,
         SAMPLE_NAME)
event<-event %>% 
  rename(LOCATION_ID=LOCATION_HISTORY_ID,
         SAMPLE_DATE=SAMPLE_DATE,
         PROJECT_CODE=PROJECT_CDE,
         DATA_PROVIDER=DATA_PROVIDER,
         TIME=TIME) %>% 
  select(LOCATION_ID,
         SAMPLE_DATE,
         PROJECT_CODE,
         DATA_PROVIDER,
         TIME)
new<-merge(info,event,by=c('LOCATION_ID','SAMPLE_DATE','PROJECT_CODE','DATA_PROVIDER'),all=TRUE)
status<-status %>% 
  rename(LOCATION_ID=LOCATION_HISTORY_ID,
         INFO_TYPE=HFD_INFORMATION_TYPE) %>% 
  select(LOCATION_ID,
         SAMPLE_DATE,
         DATA_PROVIDER,
         INFO_TYPE,
         HAB_STATUS,
         HAB_STATUS_DATE,
         HAB_STATUS_REMARK)
new<-merge(new,status,by=c('LOCATION_ID','SAMPLE_DATE','DATA_PROVIDER','INFO_TYPE'),all=TRUE)
field<-field %>% 
  rename(LOCATION_ID=LOCATION_HISTORY_ID,
         SAMPLE_NAME=HFD_SAMPLE_NAME,
         INFO_TYPE=HFD_INFORMATION_TYPE,
         SAMPLE_METHOD=HFD_SAMPLE_METHOD,
         EXTENT=HFD_EXTENT,
         PERCENT=HFD_PERCENT,
         BLOOM_DESC=HFD_BLOOM_DESC,
         EQUIPMENT_TYPE=HFD_EQUIPMENT_TYPE,
         EQUIPMENT_DESC=HFD_EQUIPMENT_DESC,
         REMARK=HFD_REMARK) %>% 
  select(LOCATION_ID,
         SAMPLE_NAME,
         SAMPLE_DATE,
         INFO_TYPE,
         DATA_PROVIDER,
         SAMPLE_METHOD,
         EXTENT,
         PERCENT,
         BLOOM_DESC,
         EQUIPMENT_TYPE,
         EQUIPMENT_DESC,
         REMARK)
rebeccanew<-merge(new,field,by=c('LOCATION_ID','SAMPLE_NAME','SAMPLE_DATE','INFO_TYPE','DATA_PROVIDER'),all=TRUE)
rm(list=c('info','event','field','status'))

rebeccanew<-rebeccanew %>% rename(LAKE_PROJ_CODE=PROJECT_CODE)
newsamples<-rebeccanew %>% 
  select(LOCATION_ID,SAMPLE_NAME,DATA_PROVIDER,INFO_TYPE,SAMPLE_DATE,TIME,REMARK,EXTENT,BLOOM_DESC,EQUIPMENT_DESC,EQUIPMENT_TYPE,LAKE_PROJ_CODE,SAMPLE_METHOD,PERCENT) %>% 
  distinct()
newsamples<-newsamples %>% 
  mutate(TIME="00:00:00",
         SAMPLE_ID=64500:(64499+nrow(newsamples)),
         START_DEPTH=0,
         END_DEPTH=0,
         BLOOM_LOC=NA,
         ESF.=NA,
         UFI.=NA,
         DEPTH_UNIT="m",
         WIND_DIR=NA,
         WIND_INT=NA)
write.csv(newsamples,file="samples_to_add_to_sample_table_habs.csv", na = "", quote = TRUE, row.names = FALSE)

newsampleshort<-newsamples %>% 
  select(LOCATION_ID,SAMPLE_NAME,DATA_PROVIDER,INFO_TYPE,SAMPLE_DATE,LAKE_PROJ_CODE,SAMPLE_ID) %>% 
  distinct()
newhabs<-merge(rebeccanew,newsampleshort,by=c('LOCATION_ID','SAMPLE_NAME','DATA_PROVIDER','INFO_TYPE','SAMPLE_DATE','LAKE_PROJ_CODE'),all=TRUE)
newhabs<-newhabs %>% 
  select(SAMPLE_ID,HAB_STATUS,HAB_STATUS_DATE,HAB_STATUS_REMARK) %>% 
  rename(STATUS=HAB_STATUS,
         STATUS_DATE=HAB_STATUS_DATE,
         REMARK=HAB_STATUS_REMARK) %>% 
  distinct() %>% 
  mutate(STATUS_DATE=as.Date(STATUS_DATE,format="%Y-%m-%d"),
         STATUS_DATE=format(STATUS_DATE,"%m/%d/%Y"),
         REMARK=as.character(REMARK))
write.csv(newhabs,file="habs_to_add_to_habs_table.csv", na = "", quote = TRUE, row.names = FALSE)



rm(list=setdiff(ls(), "data"))











