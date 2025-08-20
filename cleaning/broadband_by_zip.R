library(tidyverse)
library(readxl)
library(stringr)

### Create 2010 zip code database including county names/fips, 2000 population
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/Misc")


## map 2010 zip codes to counties and get pre-treatment populations for zips

#2010 crosswalk of zip codes to county fips
zpco=read_excel("zip_county2010.xlsx")

#datset of county fips to county names
cfips=cdlTools::census2010FIPS
cfips$county_fips=paste(str_pad(as.character(cfips$State.ANSI),2,pad="0"),
                        str_pad(as.character(cfips$County.ANSI),3,pad="0"),sep = "")
cfips$county_st=paste(cfips$County.Name,cfips$State,sep="-")


zpco2=merge(zpco,cfips,by="county_fips")

c2010=zpco2 %>%
  group_by(county_fips) %>%
  summarise(nzip=n()) %>%
  merge(cfips,by="county_fips")


#df of 2010 zips, remove unneeded columns
zip_co2010=merge(zpco,c2010,by="county_fips")
zip_co2010=zip_co2010 %>%
  select(zip,County.Name,county_fips,State,county_st,nzip)






#zip code changes from 2000 to 2010
sheet=excel_sheets("zipchanges_2000_2010.xlsx")

zipchanges=lapply(setNames(sheet,sheet),
                  function(x) read_excel("zipchanges_2000_2010.xlsx",
                                         sheet=x))
#get rid of unneeded columns
for (i in 1:51) {
  zipchanges[[i]]=zipchanges[[i]][,c(1,2,4,6,10)]
}
zipchanges=bind_rows(zipchanges,.id="State")[,-7]

#drop any zip code changes that are for PO boxes
zipchanges=zipchanges %>%
  filter(!str_detect(Comments,"Post Office Boxes")) %>%
  filter(!str_detect(Comments,"Post Office boxes")) %>%
  filter(!str_detect(Comments,"post office boxes")) %>%
  select(-Comments)

colnames(zipchanges)=c("state","zip2000","zip2010","newcounty","oldcounty")

#convert zip codes to characters
zipchanges$zip2000=str_pad(as.character(zipchanges$zip2000),5,pad="0")
zipchanges$zip2010=str_pad(as.character(zipchanges$zip2010),5,pad="0")


#load county data with zcta population weights
countyzip_pop=read.csv("county_weightedzip_pops.csv")

#adjust fips codes and zips
countyzip_pop$county_fips=str_pad(countyzip_pop$county_fips,5,pad="0")
countyzip_pop$zcta5=str_pad(countyzip_pop$zcta5,5,pad="0")

#create new variable to account for any zip code changes
zcta5_2010=list()
for (i in 1:nrow(countyzip_pop)) {
  zcta=countyzip_pop$zcta5[i]
  if (zcta %in% zipchanges$zip2000) {
    zctmp=filter(zipchanges,zip2000==zcta)
    zcta5_2010[[i]]=zctmp$zip2010
  } else {
    zcta5_2010[[i]]=zcta
  }
}

#cbind list of 2010 zips to 2000 zcta pop
countyzip_pop2=cbind(countyzip_pop,I(zcta5_2010))
countyzip_pop3=countyzip_pop2 %>%
  separate_longer_delim(zcta5_2010,delim=", ")

#get rid of extra characters coerced by separate_longer_delim()
countyzip_pop3$zcta5_2010=countyzip_pop3$zcta5_2010 %>%
  str_remove("c") %>%
  str_remove("\\(") %>%
  str_remove("\\)") %>%
  str_remove_all("\"")

#number of 2010 zips per 2000 zcta
zipn=c()
for (i in 1:length(zcta5_2010)) {
  zipn[i]=length(zcta5_2010[[i]])
}
sum(zipn) #43508, this should be the n rows in countyzip_pop3

#add county-st variable form zip_co2010
countyzip_pop3=merge(countyzip_pop3,cfips[,6:7],by="county_fips")

saveRDS(countyzip_pop3,file="countytozip_w2000pop.rds")






### zip codes from broadband rollout, when did each zip code receive an ISP
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/Broadband")

#load in datasets
zip1=read_excel("hzip1299.xls")
zip2=read_excel("hzip0600.xls")
zip4=read_excel("hzip0601.xls")
zip6=read_excel("hzip0602.xls")
zip8=read_excel("hzip0603.xls")
zip10=read_excel("hzip0604.xls")
zip12=read_excel("hzip0605.xls")
zip14=read_excel("hzip0606.xls")
zip17=read_excel("hzip0608.xls")
zip3=read_excel("hzip1200.xls")
zip5=read_excel("hzip1201.xls")
zip7=read_excel("hzip1202.xls")
zip9=read_excel("hzip1203.xls")
zip11=read_excel("hzip1204.xls")
zip13=read_excel("hzip1205.xls")
zip15=read_excel("hzip1206.xls")
zip16=read_excel("hzip1207.xls")


#add all datasets to a list
zip_isps=list(zip1,zip2,zip3,zip4,zip5,zip6,zip7,zip8,zip9,zip10,zip11,
              zip12,zip13,zip14,zip15,zip16,zip17)

#rename all zipcode columns
for (i in 1:17) {
  colnames(zip_isps[[i]])[2]="zip"
  colnames(zip_isps[[i]])[1]="state"
  zip_isps[[i]]=zip_isps[[i]] %>%
    filter(state!="PR")
}

#standardize 4-digit zip codes to 5 digits 
for (i in 12:17) {
  zip_isps[[i]]=zip_isps[[i]] %>%
    mutate(zip=ifelse(nchar(zip)==4,paste("0",zip,sep = ""),zip))
}

#when does each county enter the fcc zip codes database (when does it get an ISP)
isp_entry=c()
for (i in 1:nrow(zip_co2010)) {
  stop=FALSE
  zp=zip_co2010$zip[i]
  for (z in 1:17) {
    if (zp %in% zip_isps[[z]]$zip) {
      stop=TRUE
      break }
  }
  if (isTRUE(stop)) { 
    isp_entry[i]=z
    next } else {isp_entry[i]=NA
    next }
}
isp_entry
table(isp_entry) %>% sum()

isp_entry=cbind(select(zip_co2010,zip,County.Name,State,county_st),isp_entry)
saveRDS(isp_entry,"ispentry_by_zip.rds")

