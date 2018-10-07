library(googlesheets)
library(tidyverse)
library(ggmap)
library(sf)

## devtools::install_github("dkahle/ggmap")

source("scripts/api_key.R")

##Read in most recent data #####
list.geojson<-list.files(path="data/Agency_UGA",pattern=".geojson")
#Function for most recent file date
filedate<-function(filename){
  t<-substr(filename,18,27)
  #t1<-as.POSIXct(t)
  t
}
list.geojson.date<-lapply(list.geojson,filedate) 
names(list.geojson.date)<-1:length(list.geojson.date)
date<-bind_rows(list.geojson.date) %>%
  gather() %>%
  summarize(max=max(value))
date<-as.character(date[1,1])
recentfile<-paste("data/Agency_UGA/geocodedsitelist_",date,".geojson",sep ="")

sites_xy<-st_read(recentfile) %>% #Replace with current file
  select(Name_of_Lo,Address,City,lon,lat,loctype) %>%
  distinct() ## retain only unique/distinct rows from an input tbl

st_geometry(sites_xy)<-NULL

##Download agency sites and clean ####
sites<-gs_url("https://docs.google.com/spreadsheets/d/1iIiBpQY-i_57UUb33Fn9icV4oWOy7aAZCs3K4HafR7o/edit#gid=0")
sites_df<-gs_read(sites,range=cell_rows(2:1000)) %>%
  select(Agency:`Target Audience`) %>%
  rename("Project"=`Project = Farmers Market, Elementary Youth etc.`,
         "Name_of_Lo"=`Site Location (School district, school name, food bank, etc.)`,
         "Street_Num"=`Address building number`,
         "Address"=`Street Name`,
         "Zipcode"=`Zip Code`,
         "Target_Aud"=`Target Audience`) %>%
  mutate(filtervar=paste(Project, Name_of_Lo, Street_Num, Address, Zipcode)) %>%
  filter(filtervar!="NA NA NA NA NA") %>%
  select(-filtervar)

##Change agency name ####
#agency_crosswalk<-read_csv("data/agency_crosswalk.csv")
#sites_df<-left_join(sites_df,agency_crosswalk)

#Join data
sites_join<-sites_df %>%
  left_join(sites_xy) 

##Geocode blanks ####
sites_blank<-sites_join %>% 
  filter(is.na(lat)==TRUE) %>%
  mutate(numeric=substr(Address,1,1), #See if the house number is in the address field
         search_add1=if_else(numeric %in% c(0:9),
                             Address,paste(Street_Num,Address,sep=" ")),
         search_add=paste(search_add1,City,"GA",Zipcode,sep=",")) %>%
  select(-numeric) %>%
  filter(search_add!="NA NA") %>%
  mutate(na_check=substr(search_add,1,2)) %>% #Check for PO boxes or no house number
  filter(na_check!="NA") %>%
  select(-na_check,search_add1)

sites_blank_gc<-sites_blank %>% 
  mutate_geocode(search_add)
sites_blank_gc<-sites_blank_gc %>%
  mutate(lat=lat1,
         lon=lon1) %>%
  select(-lat1,-lon1)

#ggplot(sites_blank_gc,aes(x=lon,y=lat))+geom_point()

#write_csv(sites_blank_gc,"sites_blank_gc.csv") #To look at points


##Create new data file ####
sites_new<-sites_blank<-sites_join %>% 
  filter(is.na(lat)==FALSE) %>%
  bind_rows(sites_blank_gc) %>%
  mutate(numeric=substr(Address,1,1), #See if the house number is in the address field
         search_add1=if_else(numeric %in% c(0:9),
                             Address,paste(Street_Num,Address,sep=" ")),
         search_add=paste(search_add1,City,"GA",Zipcode,sep=",")) %>%
  select(-numeric,-search_add1)

write_csv(sites_new,paste("data/Agency_UGA/geocodedsitelist_uga_",Sys.Date(),".csv",sep=""))
sites_new_sf<-st_as_sf(sites_new,coords=c("lon","lat"),crs=4326,remove=FALSE)
filename=paste("data/Agency_UGA/geocodedsitelist_uga_",Sys.Date(),".geojson",sep="")
st_write(sites_new_sf,filename)