
library(tidyverse)
library(zipcodeR)
library(stringr)
library(ggpubr)
library(haven)

#load in data
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperData")
epp=read_dta("EP_panel_1995_2010.dta")


#get single observation per newspaper
np_list=ep %>%
  filter(year==2000) %>%
  mutate(zip=as.character(zip),
         zip=ifelse(nchar(zip)<5,paste("0",zip,sep=""),zip))

#fix error in Raleigh county, WV zip


np_counties=c()
for (i in 1:nrow(np_list)) {
  np_counties[i]=reverse_zipcode(np_list$zip[i])$county
}

np_list=cbind(np_counties,np_list) 


#manually add counties for updated zips
np_list[620,1]="Suffolk County"
np_list[624,1]="Suffolk County"
np_list[702,1]="Washoe County"
np_list[855,1]="Albany County"
np_list[1372,1]="Windsor County"

#create variable for state to get county-state id
np_list=np_list %>% 
  mutate(state=str_sub(NPNAME1,-2,-1),
         county_st=paste(np_counties,state,sep = "-"))

#add county fips codes
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/Misc")
zipcounty=readRDS("countytozip_w2000pop.rds")


#merge np_list with index
np_list2=merge(np_list,zipcounty[,c(1,8)],by="county_st",all.x = T)
sum(is.na(np_list2$county_fips))

#drop kinston register herald, this appears to be a mistaken entry
np_list2=np_list2[-721,]



### determine isp entry rates for home county of each newspaper
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/Broadband")
isp_entry=readRDS("ispentry_by_zip.rds")

#select the zip codes that are in the home counties in the np_list2 dataset
np_homecounty_zips=isp_entry %>%
  filter(county_st %in% np_list2$county_st)

#filter out non-home counties from zipcounty and create merging variable
zipcounty2=zipcounty %>%
  filter(county_st %in% np_list2$county_st)
zipcounty2$zip=zipcounty2$zcta5_2010

#merge np_homecounty_zips with 2000 population data
np_homecounty_zips2=merge(np_homecounty_zips,zipcounty2,by=c("zip","county_st"))

#check merge
chk=np_homecounty_zips2 %>%
  group_by(county_st) %>%
  summarise(n_zctas=n(),
            pop_allocation=sum(afact))

#we need to adjust population allocation for zips that split 
chk2=np_homecounty_zips2 %>%
  group_by(county_st) %>%
  summarise(n_zctas=n(),
            n_unique_zcta=length(unique(zcta5)),
            pop_allocation=sum(afact),
            adj_popallo=sum(afact[!duplicated(zcta5)]))

#group by county, number of zips wo isp by 2008 and when last zip gets isp
np_county_isp=np_homecounty_zips %>%
  group_by(county_st) %>%
  summarise(nzip=n(),
            count_na=sum(is.na(isp_entry)),
            full_isp=max(isp_entry,na.rm=F))


## population weighted zipcode penetration
np_county_popw_isp=np_homecounty_zips2 %>%
  group_by(county_st) %>%
  summarise(n_unique_zcta=length(unique(zcta5)),
            adj_popallo=sum(afact[!duplicated(zcta5)]),
            full_isp=max(isp_entry,na.rm=F),
            t1_pen=sum(afact[!duplicated(zcta5)&isp_entry<=1],
                       na.rm = T)/sum(afact[!duplicated(zcta5)]),
            t2_pen=sum(afact[!duplicated(zcta5)&isp_entry<=2],
                       na.rm = T)/sum(afact[!duplicated(zcta5)]),
            t3_pen=sum(afact[!duplicated(zcta5)&isp_entry<=3],
                       na.rm = T)/sum(afact[!duplicated(zcta5)]),
            t4_pen=sum(afact[!duplicated(zcta5)&isp_entry<=4],
                       na.rm = T)/sum(afact[!duplicated(zcta5)]),
            t5_pen=sum(afact[!duplicated(zcta5)&isp_entry<=5],
                       na.rm = T)/sum(afact[!duplicated(zcta5)]),
            t6_pen=sum(afact[!duplicated(zcta5)&isp_entry<=6],
                       na.rm = T)/sum(afact[!duplicated(zcta5)]),
            t7_pen=sum(afact[!duplicated(zcta5)&isp_entry<=7],
                       na.rm = T)/sum(afact[!duplicated(zcta5)]),
            t8_pen=sum(afact[!duplicated(zcta5)&isp_entry<=8],
                       na.rm = T)/sum(afact[!duplicated(zcta5)]),
            t9_pen=sum(afact[!duplicated(zcta5)&isp_entry<=9],
                       na.rm = T)/sum(afact[!duplicated(zcta5)]),
            t10_pen=sum(afact[!duplicated(zcta5)&isp_entry<=10],
                        na.rm=T)/sum(afact[!duplicated(zcta5)]),
            t11_pen=sum(afact[!duplicated(zcta5)&isp_entry<=11],
                        na.rm=T)/sum(afact[!duplicated(zcta5)]),
            t12_pen=sum(afact[!duplicated(zcta5)&isp_entry<=12],
                        na.rm=T)/sum(afact[!duplicated(zcta5)]),
            t13_pen=sum(afact[!duplicated(zcta5)&isp_entry<=13],
                        na.rm=T)/sum(afact[!duplicated(zcta5)]),
            t14_pen=sum(afact[!duplicated(zcta5)&isp_entry<=14],
                        na.rm=T)/sum(afact[!duplicated(zcta5)]),
            t15_pen=sum(afact[!duplicated(zcta5)&isp_entry<=15],
                        na.rm=T)/sum(afact[!duplicated(zcta5)]),
            t16_pen=sum(afact[!duplicated(zcta5)&isp_entry<=16],
                        na.rm=T)/sum(afact[!duplicated(zcta5)]),
            t17_pen=sum(afact[!duplicated(zcta5)&isp_entry<=17],
                        na.rm=T)/sum(afact[!duplicated(zcta5)]))


#merge treatment vars with original np_list
np_list=merge(np_list,np_county_popw_isp,by="county_st")
#filter out large newspapers
np_list=np_list %>%
  filter(largepaper==0) %>%
  filter(t1_pen!=1)

#look at some possible treatment thresholds
trt90=c()
trt95=c()
trt85=c()
trt99=c()

for (i in 1:nrow(np_list)) {
  tpen=t(np_list[i,31:47])
  trt85[i]=ifelse(is.infinite(min(which(tpen>0.85))),
                  0,min(which(tpen>0.85)))
  trt90[i]=ifelse(is.infinite(min(which(tpen>0.90))),
                  0,min(which(tpen>0.90)))
  trt95[i]=ifelse(is.infinite(min(which(tpen>0.95))),
                  0,min(which(tpen>0.95)))
  trt99[i]=ifelse(is.infinite(min(which(tpen>0.99))),
                  0,min(which(tpen>0.99)))
}
np_list=cbind(np_list,trt85,trt90,trt95,trt99)

#determine number of counties with same treatment status bw 90 and 95
sum((trt95-trt90)!=0)

#when is each county treated, 0 is never
p85=ggplot(data = np_list, aes(x=trt85)) +
  geom_histogram(binwidth = 1)

p90=ggplot(data = np_list, aes(x=trt90)) +
  geom_histogram(binwidth = 1)

np_list=np_list %>%
  mutate(trt95_year=case_when(
    trt95==1 ~ "2000",
    trt95==2 ~ "2000.5",
    trt95==3 ~ "2001",
    trt95==4 ~ "2001.5",
    trt95==5 ~ "2002",
    trt95==6 ~ "2002.5",
    trt95==7 ~ "2003",
    trt95==8 ~ "2003.5",
    trt95==9 ~ "2004",
    trt95==10 ~ "2004.5",
    trt95==11 ~ "2005",
    trt95==12 ~ "2005.5",
  ))
p95=ggplot(data = filter(np_list,trt95<13&!is.na(trt95_year)), 
           aes(x=trt95_year)) +
  geom_histogram(stat = "count") +
  xlab("Year") + ylab("Counties with 95% Broadband Access") +
  theme(axis.text.x = element_text(angle = 45))
  
p95


p99=ggplot(data = np_list, aes(x=trt99)) +
  geom_histogram(binwidth = 1)

ggarrange(p85,p90,p95,p99)

#average penetration by year
np_penbyyear=colMeans(np_list[,31:42]) %>%
  cbind(seq(2000,2005.5,by=0.5)) %>% data.frame()
colnames(np_penbyyear)=c("bp","Year")
bplot=ggplot(data = np_penbyyear,aes(x=Year,y=bp)) +
  geom_line() + 
  ylab("Average Broadband Penetration in 
       Newspaper Home Counties") +
  ylim(c(0.5,1))
bplot



#create treatment variable to account for large jumps in broadband penetration
max_bjump=c()
max_bjump_time=c()
for (i in 1:nrow(np_list)) {
  tpen=t(np_list[i,31:47])
  max_bjump[i]=max(diff(tpen))
  max_bjump_time[i]=which.max(diff(tpen))+1
}
np_list=cbind(np_list,max_bjump,max_bjump_time)


summary(max_bjump)

ggplot(data = np_list, aes(x=max_bjump)) +
  geom_histogram()


#do more filtering, keep only largest newspaper in a given county
np_list_largebyc=np_list %>%
  group_by(county_st) %>%
  summarise(largestpaper=NPNAME1[which.max(circ_abs)])


#add county vote history
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/Misc")
county_pop=readxl::read_xlsx("pop_county2000.xlsx") %>% filter(year==2000) %>%
  select(-year)
county_pop$fips=ifelse(substring(county_pop$fips,1,1)==0,
                       substring(county_pop$fips,2),
                       substring(county_pop$fips,1))
county_vote=read.csv("countypres_2000.csv") %>%
  filter(party%in%c("DEMOCRAT","REPUBLICAN")) %>% group_by(county_fips) %>%
  summarise(twoparty_votes=sum(candidatevotes),
            dem_votes=unique(candidatevotes),
            can_name=unique(candidate)) %>% 
  filter(can_name=="AL GORE") %>%
  select(-4) %>%
  mutate(dem_voteshare=dem_votes/twoparty_votes)
colnames(county_vote)[1]="fips"

np_list5=merge(np_list, county_vote, by="fips",all.x = T)
np_list5=merge(np_list5, county_pop, by="fips")


#get main np_list dataset with only treatment vars
np_list3=select(np_list,NPNAME1,county_st,max_bjump,max_bjump_time,trt90,trt95)

np_list_final=np_list3[np_list3$NPNAME1%in%np_list_largebyc$largestpaper,]

#create dataset for balance tests on Newsbank availability
np_list_bal=np_list5 %>%
  filter(NPNAME1%in%np_list_largebyc$largestpaper) 

#get full np_list dataset with complete broadband entry through 2005
np_list4=select(np_list5,NPNAME1,county_st,max_bjump,max_bjump_time,trt90,trt95,
                t1_pen,t2_pen,t3_pen,t4_pen,t5_pen,t6_pen,t7_pen,t8_pen,
                t9_pen,t10_pen,t11_pen,t12_pen,dem_voteshare,tot_pop)
np_list_fullb=np_list4[np_list4$NPNAME1%in%np_list_largebyc$largestpaper,]



setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperData")
write.csv(np_list_final,"np_list.csv")
write.csv(np_list_fullb,"np_list_fullbroadband.csv")
write.csv(np_list_bal,"np_list_balance.csv")
