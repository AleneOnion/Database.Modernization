check2<-check2 %>% 
  rename(SEI_LOCATION_HISTORY_ID=EVENT_LMAS_LOCATION_HISTORY_ID,
         SEI_EVENT_LMAS_SAMPLE_DATE=EVENT_LMAS_SAMPLE_DATE,
         SEI_EVENT_LMAS_PROJECT_CDE=EVENT_LMAS_PROJECT_CDE,
         SEI_EVENT_LMAS_DATA_PROVIDER=EVENT_LMAS_DATA_PROVIDER,
         SEI_EVENT_LMAS_SAMPLE_TIME=EVENT_LMAS_SAMPLE_TIME) %>% 
  mutate(SEI_EVENT_LMAS_SAMPLE_DATE=as.Date(SEI_EVENT_LMAS_SAMPLE_DATE,format="%m/%d/%Y"))
  
check3<-merge(check2,SEI,by=c('SEI_LOCATION_HISTORY_ID','SEI_EVENT_LMAS_SAMPLE_DATE','SEI_EVENT_LMAS_PROJECT_CDE','SEI_EVENT_LMAS_DATA_PROVIDER','SEI_EVENT_LMAS_SAMPLE_TIME'),all.x = TRUE)


check<-cslap %>% 
  group_by(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  filter(n>1) %>% 
  arrange(EVENT_LMAS_LOCATION_HISTORY_ID,EVENT_LMAS_SAMPLE_DATE,EVENT_LMAS_DATA_PROVIDER)


habsnew<-field
habsnew<-habsnew %>% mutate(HFD_EVENT_LMAS_SAMPLE_DATE=as.Date(HFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
habs<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables//habs.field.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
habs<-habs %>% mutate(HFD_EVENT_LMAS_SAMPLE_DATE=as.Date(HFD_EVENT_LMAS_SAMPLE_DATE,format="%Y-%m-%d"))
sapply(habsnew,class)
sapply(habs,class)
identical(habs,habsnew)


#check for differences between specific columns
test <- lapply(names(habs), function(name.i){
  anti_join(habs, habsnew, by = name.i)
})
names(test) <- names(habs)
test


test2 <- lapply(names(habsnew), function(name.i){
  anti_join(habsnew, habs, by = name.i)
})
names(test2) <- names(habsnew)
test2

rm(list=setdiff(ls(), "data"))


results %>% 
  filter(RSLT_LOCATION_HISTORY_ID%in% c('1402EAS0055_DH','1401MOU0114_DH','1401CHE0106_DH','1402LOU0040_DH','1402RE10003_DH','1402HAW0005A_DH')) %>% 
  select(RSLT_LOCATION_HISTORY_ID,RSLT_PROFILE_DEPTH) %>% 
  filter(!is.na(RSLT_PROFILE_DEPTH)) %>% 
  distinct() %>% 
  mutate(RSLT_PROFILE_DEPTH=as.numeric(RSLT_PROFILE_DEPTH)) %>% 
  group_by(RSLT_LOCATION_HISTORY_ID) %>% 
  summarize(maxdepth=max(RSLT_PROFILE_DEPTH)) %>% 
  ungroup()




lake<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lake.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
class<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2018.raw/Lake.Master.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
class<-class %>% 
  rename(LAKE_ID=LakeID) %>% 
  select(LAKE_ID,PWLID,Waterbody_Classification,PWS,Beaches,M_BAS_NAME,M_BASIN_ID,BasinSub,ACRES,AREA,LEN,P.) %>% distinct()
lake<-merge(lake,class,by=c('LAKE_ID'),all.x=TRUE)
rm(class)

county<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/Open.Refine/Location-Clean-10-29-2019.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
county<-county %>% 
  rename(LAKE_ID=LakeID,
         LOCATION_ID=LocationID) %>% 
  select(LOCATION_ID,County,DEC.Region) %>% distinct()
location<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
location<-merge(location,county,by="LOCATION_ID",all.x=TRUE)
rm(county)

location<-location %>% 
  arrange(Type) %>% 
  distinct(LAKE_ID,.keep_all = TRUE) %>% 
  select(LAKE_ID,Type,X_Coordinate,Y_Coordinate,County,DEC.Region)
lake<-merge(lake,location,by=c('LAKE_ID'),all.x=TRUE)
lake<-lake %>% 
  select(LAKE_ID,WATER,Type,X_Coordinate,Y_Coordinate,PWLID,Waterbody_Classification,PWS,Beaches,M_BAS_NAME,County,DEC.Region)

lake1<-lake[15601:15700,]
lake1<- lake1 %>% 
  filter(!is.na(LAKE_ID))
write.csv(lake1,file="jjunk.lake.for.web.csv",row.names=FALSE)



coded<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/coded.table",na.strings=c("","NA"), stringsAsFactors=FALSE)

\\dec-home\DEC_HOME\amonion\Lakes.Database\data\2019\ITS.fixed.tables\ITS\coded.tables

setwd("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/coded.table")

folder_list <- list.files("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/coded.table")
nfolder_list = length(folder_list)
RSfile_list <- list()

for (i in 1:nfolder_list){
  temp_result <- read.table(paste(input_dir,folder_list[i],"TestResultQC_v3.txt",sep="/"),sep=",",fill=TRUE,header=FALSE,stringsAsFactors=FALSE,
                            col.names = c("sys_sample_code","lab_anl_method_name","analysis_date","fraction","column_number","test_type","lab_matrix_code","analysis_location","basis","container_id","dilution_factor","prep_method","prep_date","leachate_method","leachate_date","lab_name_code","qc_level","lab_sample_id","percent_moisture","subsample_amount","subsample_amount_unit","analyst_name","instrument_id","comment","preservative","final_volume","final_volume_unit","cas_rn","chemical_name","result_value","result_error_delta","result_type_code","reportable_result","detect_flag","lab_qualifiers","validator_qualifiers","interpreted_qualifiers","validated_yn","method_detection_limit","reporting_detection_limit","quantitation_limit","result_unit","detection_limit_unit","tic_retention_time","minimum_detectable_conc","counting_error","uncertainty","critical_value","validation_level","result_comment","qc_original_conc","qc_spike_added","qc_spike_measured","qc_spike_recovery","qc_dup_original_conc","qc_dup_spike_added","qc_dup_spike_measured","qc_dup_spike_recovery","qc_rpd","qc_spike_lcl","qc_spike_ucl","qc_rpd_cl","qc_spike_status","qc_dup_spike_status","qc_rpd_status","lab_sdg"))
  temp_sample <- read.table(paste(input_dir,folder_list[i],"Sample_v3.txt",sep="/"),sep=",",fill=TRUE,header=FALSE, stringsAsFactors=FALSE,
                            col.names = c("#data_provider","sys_sample_code","sample_name","sample_matrix_code","sample_type_code","sample_source","parent_sample_code","sample_delivery_group","sample_date","sys_loc_code","start_depth","end_depth","depth_unit","chain_of_custody","sent_to_lab_date","sample_receipt_date","sampler","sampling_company_code","sampling_reason","sampling_technique","task_code","collection_quarter","composite_yn","composite_desc","sample_class","custom_field_1","custom_field_2","custom_field_3","comment"))
  # NOTE THAT ROW ORDER OF INPUT FILE IS NOT RETAINED
  temp_RSmerge <- merge(temp_result,temp_sample,by="sys_sample_code", all=TRUE)
  
  filenm <- paste(output_dir,folder_list[i],"_RSmerge.csv", sep="")
  print(filenm)
  
  if ((nrow(temp_result)) < (nrow(temp_RSmerge))) {
    stop('SCRIPT STOPPED: Extra records created in merge. Check EDD for errors.')
  }
  if ((nrow(temp_result)) > (nrow(temp_RSmerge))) {
    stop('SCRIPT STOPPED: Not enough records created with merge. Check for errors.')
  }
  
  # Create name using EDD and added suffix, and assign to current data frame 
  mergenm <- paste0(folder_list[i], "_RSmerge")
  mergefile <- assign(mergenm, temp_RSmerge)
  
  #Add current data frame to the list of all dataframes
  RSfile_list[[i]] <- mergefile
  
}


library(stringr)
library(dplyr)
data %>% filter(LAKE_ID %in% c('1301PET0161G',
                               '1301WIC0183A',
                               '1302HEM0103H',
                               '1303ORA0239D',
                               '1305SIL0378',
                               '1310BLA0N80B',
                               '1401SWA0211',
                               '1203MET0821')) %>% select(LAKE_ID,Waterbody_Classification) %>% distinct()










