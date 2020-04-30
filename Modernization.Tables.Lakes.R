#Alene Onion
#December 2019
#Script to generate a clean Lake table

#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output
#write_csv(x, path, na = "", quote_escape = "double")

library(dplyr)
library(tidyr)

#LAKE table
lake<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2018.raw/Lake.Master.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#remove unecessary fields
lake<-lake %>% 
  select(LakeID,WATER,FIN,Waterbody_Type)
names(lake)[names(lake)=="LakeID"]<-"LAKE_ID"

#fix erroenous lake id:
lake<-lake %>% 
  mutate(LAKE_ID= ifelse((FIN == "H-240-P498" & LAKE_ID == "1702KEN1063"),"1201STUP498",LAKE_ID))


#add alternate names
alternate<-read.csv('Alternate.Names.csv',na.strings=c("","NA"), stringsAsFactors=FALSE)
alternate<-unique(alternate[c('LakeID','Alternate_Name')])
alternate<-alternate[!is.na(alternate$Alternate_Name),]
names(alternate)[names(alternate)=="LakeID"]<-"LAKE_ID"
lake<-merge(lake,alternate,by=c('LAKE_ID'),all.x=TRUE)
rm(alternate)

#Fix erroneous alternate names
lake<-lake %>% 
  mutate(Alternate_Name= ifelse(LAKE_ID=="1302UWB5124","Unnamed Water",Alternate_Name),
         Alternate_Name= ifelse(LAKE_ID=="0103UWB5168",NA,Alternate_Name))

#remove /n in lake name
lake$WATER<-gsub("\\n*","",lake$WATER)
lake$Alternate_Name<-gsub("\\n",",",lake$Alternate_Name)
lake$Alternate_Name<-gsub(",,",",",lake$Alternate_Name)
lake$Alternate_Name<-gsub(",,",",",lake$Alternate_Name)
lake$Alternate_Name<-gsub(",,",",",lake$Alternate_Name)
lake$Alternate_Name<-gsub(",,",",",lake$Alternate_Name)
lake$Alternate_Name<-gsub(",,",",",lake$Alternate_Name)
lake$FIN<-gsub("\\n*","",lake$FIN)

#remove white space from lake id
lake$LAKE_ID<-trimws(lake$LAKE_ID)


#make sure the file has only one lake id,fin combination 
lake<-lake %>% 
  arrange(LAKE_ID,FIN,WATER,Alternate_Name) %>% 
  distinct(LAKE_ID,FIN,.keep_all = TRUE)

#merge with fin layer
fin<-read.csv("FIN_layer.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
fin<-fin %>% 
  select(FIN,WATER)
fin$here<-"X"
fin$WATER<-NULL
fin<-distinct(fin)

lake<-merge(lake,fin,by=c('FIN'),all=TRUE)

#add in waterbody names for those fins without LAKE_IDs
fin<-read.csv("FIN_layer.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
fin<-fin %>% 
  select(FIN,WATER)
fin$WATER_fin<-fin$WATER
fin$WATER<-NULL
fin<-distinct(fin)

lake<-merge(lake,fin,by=c('FIN'),all.x=TRUE)
lake<-lake %>% 
  mutate(WATER= ifelse(is.na(WATER),WATER_fin,WATER))
rm(fin)
lake$WATER_fin<-NULL


#create distinct file
lake<-lake %>% 
  arrange(LAKE_ID,FIN,WATER,Alternate_Name) %>% 
  distinct(LAKE_ID,FIN,.keep_all = TRUE)


#add p number
pnum<-read.csv("p.numbers.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
lake<-merge(lake,pnum,by=c('FIN'),all.x = TRUE)
rm(pnum)
lake<-unique(lake[c('LAKE_ID','FIN','WATER','Alternate_Name','Waterbody_Type','P.')])
#create distinct file
lake<-lake %>% 
  arrange(LAKE_ID,FIN,WATER,Alternate_Name) %>% 
  distinct(LAKE_ID,FIN,.keep_all = TRUE)

#turn types into toupper
lake$Waterbody_Type<-toupper(lake$Waterbody_Type)
#replace all NA waterbody types as LAKE
lake$Waterbody_Type<-ifelse(is.na(lake$Waterbody_Type),"LAKE",lake$Waterbody_Type)

#test how many lake ids don't match the fin
length(unique(lake[is.na(lake$FIN),]$LAKE_ID))
#157
#test how many fins don't have lake ids 
length(unique(lake[is.na(lake$LAKE_ID),]$FIN))
#103

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#and here are some of the missing lake IDs
fins<-read.csv("LAKE.ids.for.new.FINs.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
fins<-unique(fins[c('FIN','P','LakeID')])
names(fins)[names(fins)=="LakeID"]<-"LAKE_ID_fin"
names(fins)[names(fins)=="P"]<-"P_fin"
lake<-merge(lake,fins,by=c('FIN'),all.x=TRUE)
rm(fins)
lake<-lake %>% 
  mutate(LAKE_ID=ifelse(is.na(LAKE_ID),LAKE_ID_fin,LAKE_ID),
         P.=ifelse(is.na(LAKE_ID),P_fin,P.))
lake$P_fin<-NULL
lake$LAKE_ID_fin<-NULL
lake<-distinct(lake)

#########################################################################################################
#and here is the final set
fins<-read.csv("LAKE.ids.for.new.FINs.2.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
fins<-unique(fins[c('FIN.x','change')])
names(fins)[names(fins)=="FIN.x"]<-"FIN"
names(fins)[names(fins)=="change"]<-"LAKE_ID_fin"
lake<-merge(lake,fins,by=c('FIN'),all.x=TRUE)
rm(fins)
lake<-lake %>% 
  mutate(LAKE_ID=ifelse(!is.na(LAKE_ID_fin),LAKE_ID_fin,LAKE_ID))
lake$LAKE_ID_fin<-NULL
lake<-distinct(lake)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#lake ids with no fin values need to pull centroids to give to the GIS team
nofinids<-lake[is.na(lake$FIN),]
nofinids<-nofinids[!is.na(nofinids$LAKE_ID),]
#merge in location table
location<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2018.raw/Location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
#keep only necessary fields
location<-unique(location[c('LakeID','Type','Y_Coordinate','X_Coordinate')])
names(location)[names(location)=="LakeID"]<-"LAKE_ID"
location$LAKE_ID<-trimws(location$LAKE_ID)
location$LAKE_ID<-toupper(location$LAKE_ID)
#merge with no fin table
nofinids<-merge(nofinids,location,by=c('LAKE_ID'),all.x = TRUE)
rm(location)
#add in 2019 locations from NYHABs
NYHABS<-read.csv("nofinids_KAR.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
nofinids<-merge(nofinids,NYHABS,all=TRUE)
rm(NYHABS)
#capitalize Type so all one field
nofinids$Type<-toupper(nofinids$Type)
#restrict to only one location per lake
nofinids<-nofinids %>% 
  mutate(Type = factor(Type, levels = c("Deep Hole","Centroid","Facility Public Water Supply (PWS)",
                                        "River/Stream","Outlet","Shore","Other"))) %>% 
  arrange(LAKE_ID,Type) %>% 
  distinct(LAKE_ID,FIN,WATER,.keep_all = TRUE)
nofinids$Type<-NULL
write.csv(nofinids,file="nofinids.csv",row.names=FALSE)

#fix fields where there is this weird special character "?" 
#which you can only see when you click a table in the file list of R studio and select view
lake<-lake %>% 
  mutate(WATER=ifelse(LAKE_ID=="1404SKY0390","Sky Lake",WATER),
         WATER=ifelse(LAKE_ID=="1305MUD0410","Mud Pond",WATER),
         Alternate_Name=ifelse(LAKE_ID=="0801UWB0782","Fulton Chain Third Lake",Alternate_Name))

#Remove this waterbody because isn't real in the fin layer:
lake<-lake[lake$LAKE_ID!="1203DUC5459",]

#add this waterbody because there's a mistake in the fin layer: one fin has two associated ponded waters
new<-matrix(c('ONT-19-81-18-P782','0801UWB0782C','Third Lake','Fulton Chain Third Lake','LAKE','782'),ncol=6,byrow=TRUE)
colnames(new)<-c('FIN','LAKE_ID','WATER','Alternate_Name','Waterbody_Type','P.')
lake<-merge(lake,new,all=TRUE)

#remove leading zeros in these pond numbers
lake<-lake %>% 
  mutate(P.=ifelse(LAKE_ID=="1701UWB0690","690",P.),
         P.=ifelse(LAKE_ID=="1701UWB0691","691",P.))

#generate lake table with FIN
write.csv(lake, file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lake.csv", na = "", quote = TRUE, row.names = FALSE)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#adding Rebecca's lakes
lake<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lake.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
updates<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/rebeccas.updatestolake.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#restrict to necessary fields
updates<-updates %>% 
  select(LAKE_ID,WATER,Alternate_Name,Waterbody_Type,P.) %>% 
  mutate(FIN=NA) %>% 
  distinct()
junk<-anti_join(updates,lake,by=c('LAKE_ID'))
write.csv(updates,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2020.03.25.updatestolakemaster.csv",row.names=FALSE)
lake<-merge(lake,updates,all=TRUE)
lake<-lake %>% filter(!LAKE_ID %in% c('0402UWBXXX1','1702WOOXX1'))
write.csv(lake,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lake.csv",row.names=FALSE, na = "", quote = TRUE)
updates<-updates %>% 
  rename(LAKE_HISTORY_ID=LAKE_ID,
         LAKE_FIN=FIN,
         LAKE_WATERBODY_NAME=WATER,
         LAKE_ALTERNATE_NAME=Alternate_Name,
         LAKE_WATERBODY_TYPE=Waterbody_Type,
         LAKE_POND_NUMBER=P.)
write.csv(updates,file="//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/updates/2020.03.25.updatestolakemaster.ITS.csv",row.names=FALSE)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#Check that file returned from ITS matches
#check that lakes table matches its
lake<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/lake.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
ITS<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/2019/ITS.fixed.tables/ITS/L_LAKE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)

#truncate to necessary fields
ITS<-ITS %>% 
  select(LAKE_FIN,LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,LAKE_ALTERNATE_NAME,LAKE_WATERBODY_TYPE,LAKE_POND_NUMBER)
lake<-lake %>% 
  rename(LAKE_HISTORY_ID=LAKE_ID,
         LAKE_FIN=FIN,
         LAKE_WATERBODY_NAME=WATER,
         LAKE_ALTERNATE_NAME=Alternate_Name,
         LAKE_WATERBODY_TYPE=Waterbody_Type,
         LAKE_POND_NUMBER=P.)
#change NA to 0
lake<-lake %>% 
  mutate(LAKE_FIN=ifelse(is.na(LAKE_FIN),0,LAKE_FIN),
         LAKE_POND_NUMBER=ifelse(is.na(LAKE_POND_NUMBER),0,LAKE_POND_NUMBER))

#change to character so can check for diff
lake<-lake %>% 
  mutate(LAKE_FIN=as.character(LAKE_FIN),
         LAKE_HISTORY_ID=as.character(LAKE_HISTORY_ID),
         LAKE_WATERBODY_NAME=as.character(LAKE_WATERBODY_NAME),
         LAKE_ALTERNATE_NAME=as.character(LAKE_ALTERNATE_NAME),
         LAKE_WATERBODY_TYPE=as.character(LAKE_WATERBODY_TYPE),
         LAKE_POND_NUMBER=as.character(LAKE_POND_NUMBER))
ITS<-ITS %>% 
  mutate(LAKE_FIN=as.character(LAKE_FIN),
         LAKE_HISTORY_ID=as.character(LAKE_HISTORY_ID),
         LAKE_WATERBODY_NAME=as.character(LAKE_WATERBODY_NAME),
         LAKE_ALTERNATE_NAME=as.character(LAKE_ALTERNATE_NAME),
         LAKE_WATERBODY_TYPE=as.character(LAKE_WATERBODY_TYPE),
         LAKE_POND_NUMBER=as.character(LAKE_POND_NUMBER))


#check class
sapply(lake,class)
sapply(ITS,class)

#make sure samples sorted the same
ITS<-ITS %>% 
  arrange(LAKE_FIN,LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,LAKE_ALTERNATE_NAME,LAKE_WATERBODY_TYPE,LAKE_POND_NUMBER)
rownames(ITS)<-NULL
lake<-lake %>% 
  arrange(LAKE_FIN,LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,LAKE_ALTERNATE_NAME,LAKE_WATERBODY_TYPE,LAKE_POND_NUMBER)
rownames(lake)<-NULL


#check if identical
identical(lake,ITS)

#check for differences between specific columns
test <- lapply(names(lake), function(name.i){
  anti_join(lake, ITS, by = name.i)
})
names(test) <- names(lake)
test


test2 <- lapply(names(ITS), function(name.i){
  anti_join(ITS, lake, by = name.i)
})
names(test2) <- names(ITS)
test2

rm(list=setdiff(ls(), "data"))

#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#List of functions above which might affect downstream data:
#lake$LAKE_ID<-trimws(lake$LAKE_ID)
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
