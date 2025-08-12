library(tidyverse)
library(stargazer)
library(haven)
library(reshape)
library(panelr)

#load data
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperData")
np_index=read.csv("merged_index.csv")[,-1]
np_list_fullb=read.csv("np_list_fullbroadband.csv")[,c(2,8:21)]
np_index=merge(np_index,np_list_fullb,by="NPNAME1")
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/2025_RS")
nptext=read.csv("np_full_rs5m_topics.csv")[,-1]


sen_out1=read.csv("head_sentiment5m_1.csv")[,3:5]
sen_out2=read.csv("head_sentiment5m_2.csv")[,3:5]
sen_out3=read.csv("head_sentiment5m_3.csv")[,3:5]
sen_out4=read.csv("head_sentiment5m_4.csv")[,3:5]
sen_out5=read.csv("head_sentiment5m_5.csv")[,3:5]
sen_out=rbind(sen_out1,sen_out2,sen_out3,sen_out4,sen_out5)
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/Analysis_datasets")
npnews=read.csv("np_news_rs5m_topics.csv")
nptext=cbind(nptext,sen_out)

#add section labels - see np_topicmodel.py on hpc
nptext=nptext %>%
  mutate(topic_label=case_when(topic==0~"obit",
                               topic==1~"national econ",
                               topic==2~"calendar",
                               topic==3~"local soft news",
                               topic==4~"national educ",
                               topic==5~"local govt news",
                               topic==6~"crime",
                               topic==7~"sports"))



#merge with index
nptext2=merge(nptext,np_index,by="nb_index") 

#convert article date to time period
nptext2$date=nptext2$date %>% lubridate::mdy()
nptext2=drop_na(nptext2,date)
nptext2$date_num=as.numeric(nptext2$date)

#get week num
nptext2$week=lubridate::week(nptext2$date)
#year
nptext2$year=lubridate::year(nptext2$date)
#week-year
nptext2$week_year=paste(nptext2$week,nptext2$year,sep="-")

#month-year
nptext2$month=lubridate::month(nptext2$date)
nptext2$month_year=paste(nptext2$month,nptext2$year,sep="-")


#week count
nptext2=nptext2 %>%
  mutate(week_count= week + 53*(year%%10))

npdates=nptext2[unique(nptext2$date),c("date","date_num")]
nptext2=nptext2 %>%
  mutate(time_period=case_when(date_num<11139 ~ 1,
                               date_num<11323 ~ 2,
                               date_num<11504 ~ 3,
                               date_num<11688 ~ 4,
                               date_num<11869 ~ 5,
                               date_num<12053 ~ 6,
                               date_num<12234 ~ 7,
                               date_num<12418 ~ 8,
                               date_num<12600 ~ 9,
                               date_num<12784 ~ 10,
                               date_num<12965 ~ 11,
                               date_num<13149 ~ 12))

#get broadband penetration for each time period
nptext2=nptext2 %>%
  mutate(broadband=case_when(time_period == 1 ~ t1_pen,
                             time_period == 2 ~ t2_pen,
                             time_period == 3 ~ t3_pen,
                             time_period == 4 ~ t4_pen,
                             time_period == 5 ~ t5_pen,
                             time_period == 6 ~ t6_pen,
                             time_period == 7 ~ t7_pen,
                             time_period == 8 ~ t8_pen,
                             time_period == 9 ~ t9_pen,
                             time_period == 10 ~ t10_pen,
                             time_period == 11 ~ t11_pen,
                             time_period == 12 ~ t12_pen))


##### basic test ####
nptext2=nptext2 %>%
  mutate(after90=ifelse(time_period>=trt90,1,0),
         after95=ifelse(time_period>=trt95,1,0),
         aftermaxj=ifelse(time_period>=max_bjump_time,1,0))

#add circulation and jobs for all 6 years
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperData")
ep=read_dta("EP_panel_1995_2010.dta") %>% 
  filter(year%in%2000:2005) %>% 
  select(NPNAME1,year,circ_abs,jobscount)

ep=panel_data(ep,id=NPNAME1,wave=year)
ep2=widen_panel(ep)

#what are the highest circulation papers
ep3=filter(ep2,NPNAME1%in%nptext2$NPNAME1)
circulation=arrange(ep3,desc(circ_abs_2000))

nptext2=merge(nptext2,ep2,by="NPNAME1")

#national news
nptext_n=filter(nptext2,topic==1|topic==4)

#add nat news topic labels
colnames(npnews)[11]="topic2"
nptext_n=merge(nptext_n,npnews[,c(3,11)],by="X")
nptext_n=nptext_n %>%
  mutate(topic2_label=case_when(topic2==0 ~ "security",
                                topic2==1 ~ "economy",
                                topic2==2 ~ "schools",
                                topic2==3 ~ "foreign affairs",
                                topic2==4 ~ "local",
                                topic2==5 ~ "other")) 

#local hard news with local stories pulled from natl
nptext_l=filter(nptext2,topic==5|topic==3|X%in%nptext_n[nptext_n$topic2==4,]$X)
#crime news
nptext_c=filter(nptext2,topic==6)

#remove local stories from nptext_n
nptext_n=nptext_n %>% filter(topic2!=4)

###by date ####
nptext_grouped_l=nptext_l %>% group_by(np.x,date_num) %>%
  summarise(time_period=unique(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

nptext_grouped_c=nptext_c %>% group_by(np.x,date_num) %>%
  summarise(time_period=unique(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

nptext_grouped_n=nptext_n %>% group_by(np.x,date_num) %>%
  summarise(time_period=unique(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

#local
summary(lm(prop_negative ~ aftermaxj + factor(time_period),
           data = nptext_grouped_l))

#crime
summary(lm(prop_negative ~ aftermaxj + factor(time_period),
           data = nptext_grouped_c))

#national
summary(lm(prop_negative ~ aftermaxj + factor(time_period),
           data = nptext_grouped_n))



######## Agrregating by week #######
###### SENTIMENT by topic 

nptext_weekgrouped_l=nptext_l %>% 
  group_by(np.x,week_year) %>%
  summarise(time_period=min(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

nptext_weekgrouped_c=nptext_c %>% 
  group_by(np.x,week_year) %>%
  summarise(time_period=min(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

nptext_weekgrouped_n=nptext_n %>% 
  group_by(np.x,week_year) %>%
  summarise(time_period=min(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))


#local
loc=summary(lm(prop_negative ~ after95 + factor(week_year) + factor(np.x),
               data = nptext_weekgrouped_l))
loc$coefficients[1:4,]

#crime
crime=summary(lm(prop_negative ~ after95 + factor(week_year) + factor(np.x),
                 data = nptext_weekgrouped_c))
crime$coefficients[1:4,]

#national
nat=summary(lm(prop_negative ~ after95 + factor(week_year) + factor(np.x),
               data = nptext_weekgrouped_n))
nat$coefficients[1:4,]




##### TOPIC PROPORTIONS #####
#proportions
nptext_weekgrouped=nptext2 %>% 
  group_by(np.x,week_year) %>%
  summarise(time_period=min(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            n_articles=n(),
            prop_local=sum(X%in%nptext_l$X)/n(),
            prop_crime=sum(X%in%nptext_c$X)/n(),
            prop_natl=sum(X%in%nptext_n$X)/n())


#local
loc_p=summary(lm(prop_local ~ after95 + factor(week_year) + factor(np.x),
                 data = nptext_weekgrouped))
loc_p$coefficients[1:4,]

#crime
crime_p=summary(lm(prop_crime ~ after95 + factor(week_year) + factor(np.x),
                   data = nptext_weekgrouped))
crime_p$coefficients[1:4,]

#national
nat_p=summary(lm(prop_natl ~ after95 + factor(week_year) + factor(np.x),
                 data = nptext_weekgrouped))
nat_p$coefficients[1:4,]














######## Aggregating by month #######
###### SENTIMENT by topic 

#Create final analysis topic label
nptext2=nptext2 %>% 
  mutate(analysis_topic=case_when(
    X%in%nptext_n$X ~ "n",
    X%in%nptext_c$X ~ "c",
    X%in%nptext_l$X ~ "l",
    TRUE ~ "o"
  ))

#group by topic
nptext_monthgrouped_l=nptext_l %>% 
  group_by(NPNAME1,month_year) %>%
  summarise(time_period=min(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            pop=unique(tot_pop),
            demvote=unique(dem_voteshare),
            circ=unique(circ_abs_2000),
            jobs=unique(jobscount_2000),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

nptext_monthgrouped_c=nptext_c %>% 
  group_by(NPNAME1,month_year) %>%
  summarise(time_period=min(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            pop=unique(tot_pop),
            demvote=unique(dem_voteshare),
            circ=unique(circ_abs_2000),
            jobs=unique(jobscount_2000),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

nptext_monthgrouped_n=nptext_n %>% 
  group_by(NPNAME1,month_year) %>%
  summarise(time_period=min(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            pop=unique(tot_pop),
            demvote=unique(dem_voteshare),
            circ=unique(circ_abs_2000),
            jobs=unique(jobscount_2000),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

#proportions dataset
nptext_monthgrouped=nptext2 %>% 
  group_by(NPNAME1,month_year) %>%
  summarise(time_period=min(time_period),
            after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            pop=unique(tot_pop),
            demvote=unique(dem_voteshare),
            circ=unique(circ_abs_2000),
            jobs=unique(jobscount_2000),
            n_articles=n(),
            prop_local=sum(analysis_topic=="l")/n(),
            prop_crime=sum(analysis_topic=="c")/n(),
            prop_natl=sum(analysis_topic=="n")/n())

#save all the analysis dfs
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/Analysis_datasets")
write.csv(nptext_monthgrouped,"5MRS_groupbymonth_full.csv")
write.csv(nptext_monthgrouped_l,"5MRS_groupbymonth_local.csv")
write.csv(nptext_monthgrouped_c,"5MRS_groupbymonth_crime.csv")
write.csv(nptext_monthgrouped_n,"5MRS_groupbymonth_natl.csv")


###### AGGREGATE BY 6 MONTH ######
#group by topic
nptext_6monthgrouped_l=nptext_l %>% 
  group_by(NPNAME1,time_period) %>%
  summarise(after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            pop=unique(tot_pop),
            demvote=unique(dem_voteshare),
            circ=unique(circ_abs_2000),
            jobs=unique(jobscount_2000),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

nptext_6monthgrouped_c=nptext_c %>% 
  group_by(NPNAME1,time_period) %>%
  summarise(after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            pop=unique(tot_pop),
            demvote=unique(dem_voteshare),
            circ=unique(circ_abs_2000),
            jobs=unique(jobscount_2000),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

nptext_6monthgrouped_n=nptext_n %>% 
  group_by(NPNAME1,time_period) %>%
  summarise(after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            pop=unique(tot_pop),
            demvote=unique(dem_voteshare),
            circ=unique(circ_abs_2000),
            jobs=unique(jobscount_2000),
            n_articles=n(),
            prop_negative=1-(sum(pred)/n()))

#proportions dataset
nptext_6monthgrouped=nptext2 %>% 
  group_by(NPNAME1,time_period) %>%
  summarise(after90=unique(after90),
            after95=unique(after95),
            aftermaxj=unique(aftermaxj),
            broadband=unique(broadband),
            pop=unique(tot_pop),
            demvote=unique(dem_voteshare),
            circ=unique(circ_abs_2000),
            jobs=unique(jobscount_2000),
            n_articles=n(),
            prop_local=sum(analysis_topic=="l")/n(),
            prop_crime=sum(analysis_topic=="c")/n(),
            prop_natl=sum(analysis_topic=="n")/n())

write.csv(nptext_6monthgrouped,"5MRS_groupby6month_full.csv")
write.csv(nptext_6monthgrouped_l,"5MRS_groupby6month_local.csv")
write.csv(nptext_6monthgrouped_c,"5MRS_groupby6month_crime.csv")
write.csv(nptext_6monthgrouped_n,"5MRS_groupby6month_natl.csv")

write.csv(nptext2,"analysisdata_5m.csv")







###### REGRESSIONS #####
nptext_monthgrouped=read.csv("5MRS_groupbymonth_full.csv")
nptext_monthgrouped_l=read.csv("5MRS_groupbymonth_local.csv")
nptext_monthgrouped_c=read.csv("5MRS_groupbymonth_crime.csv")
nptext_monthgrouped_n=read.csv("5MRS_groupbymonth_natl.csv")


#local
mloc=summary(lm(prop_negative ~ broadband + factor(month_year) + factor(np.x),
                data = nptext_monthgrouped_l))
mloc$coefficients[1:4,]

#crime
mcrime=summary(lm(prop_negative ~ broadband + factor(month_year) + factor(np.x),
                  data = nptext_monthgrouped_c))
mcrime$coefficients[1:4,]

#national
mnat=summary(lm(prop_negative ~ broadband + factor(month_year) + factor(np.x),
                data = nptext_monthgrouped_n))
mnat$coefficients[1:4,]




##### TOPIC PROPORTIONS #####


#local
mloc_p=summary(lm(prop_local ~ broadband + factor(month_year) + factor(np.x),
                  data = nptext_monthgrouped))
mloc_p$coefficients[1:4,]

#crime
mcrime_p=summary(lm(prop_crime ~ broadband + factor(month_year) + factor(np.x),
                    data = nptext_monthgrouped))
mcrime_p$coefficients[1:4,]

#national
mnat_p=summary(lm(prop_natl ~ broadband + factor(month_year) + factor(np.x),
                  data = nptext_monthgrouped))
mnat_p$coefficients[1:4,]


