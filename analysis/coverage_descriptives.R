## Descriptives

#load packages
library(tidyverse)
library(usmap)
library(cobalt)
library(RCT)
library(haven)
library(kableExtra)

#load datasets
setwd()
nptext=read.csv("analysisdata_5m.csv")
npnews=read.csv("np_news_rs5m_topics.csv")

#quick data cleaning and labeling
colnames(npnews)[11]="topic2"
np_natnews=filter(nptext,topic%in%c(1,4))
np_natnews=merge(np_natnews,npnews,by="X")
np_natnews=np_natnews %>%
  mutate(topic2_label=case_when(topic2==0 ~ "security",
                                topic2==1 ~ "economy",
                                topic2==2 ~ "schools",
                                topic2==3 ~ "foreign affairs",
                                topic2==4 ~ "local",
                                topic2==5 ~ "other",))



### Full data - topic proportions

#create topic labels for graphs
nptext$topic_label[nptext$topic %in% c(1,4)]="national"
nptext$topic_label[nptext$topic %in% c(3,5)]="local"
nptext$topic_label[nptext$topic %in% c(0,2)]="other"

#relabel local stories in natl dataset - cleaning up LDA models
nptext$topic_label[nptext$X%in%np_natnews$X[np_natnews$topic2==4]]="local"
tp=nptext %>% group_by(topic_label) %>%
  summarize(n_in_topic=n()) %>%
  mutate(topic_proportion=n_in_topic/nrow(nptext))
tp$topic_label=factor(tp$topic_label,
                         levels = c("other","sports",
                                    "crime","local","national"))

tpg=ggplot(data = tp) + geom_col(aes(y=topic_label,topic_proportion)) +
  xlab("Topic Proportion") + ylab("Topic")


#topic proportions by county pop
nptext_lowpop=filter(nptext,tot_pop<median(nptext$tot_pop))
nptext_highpop=filter(nptext,tot_pop>=median(nptext$tot_pop))

## Low pop proportions
tp_low=nptext_lowpop %>% group_by(topic_label) %>%
  summarize(n_in_topic=n()) %>%
  mutate(topic_proportion=n_in_topic/nrow(nptext_lowpop))
tpg_low=ggplot(data = tp_low) + geom_col(aes(y=topic_label,topic_proportion))
tpg_low

## High pop proportions
tp_high=nptext_highpop %>% group_by(topic_label) %>%
  summarize(n_in_topic=n()) %>%
  mutate(topic_proportion=n_in_topic/nrow(nptext_highpop))
tpg_high=ggplot(data = tp_high) + geom_col(aes(y=topic_label,topic_proportion))


#combined plot for paper
tp_high$group="High Pop."
tp_low$group="Low Pop."
tp=rbind(tp_high,tp_low)
tpg_pop=ggplot(data = tp) + 
  geom_col(aes(y=topic_label,x=topic_proportion,fill=group),
           position = "dodge") +
  xlab("Topic Proportion") + ylab("Topic") +
  theme(legend.position = "bottom")




### Topic Proportions by Circulation

## high circ
tp_hc=nptext %>% filter(circ_abs_2000>100000) %>% group_by(topic_label) %>%
  summarize(n_in_topic=n(),
            npapers=length(unique(NPNAME1))) %>%
  mutate(topic_proportion=n_in_topic/
           nrow(filter(nptext,circ_abs_2000>100000)),
         group="High Circulation (>100k)")
tp_hc$topic_label=factor(tp_hc$topic_label,
                         levels = c("other","sports",
                                    "crime","local","national"))


## medium circ
tp_mc=nptext %>% filter(circ_abs_2000<100000 & circ_abs_2000>10000) %>%
  group_by(topic_label) %>%
  summarize(n_in_topic=n(),
            npapers=length(unique(NPNAME1))) %>%
  mutate(topic_proportion=n_in_topic/
           nrow(filter(nptext,circ_abs_2000<100000 & circ_abs_2000>10000)),
         group="Medium Circulation (10k-100k)")
tp_mc$topic_label=factor(tp_mc$topic_label,
                         levels = c("other","sports",
                                    "crime","local","national"))


## low circ
tp_lc=nptext %>% filter(circ_abs_2000<10000) %>% group_by(topic_label) %>%
  summarize(n_in_topic=n(),
            npapers=length(unique(NPNAME1))) %>%
  mutate(topic_proportion=n_in_topic/
           nrow(filter(nptext,circ_abs_2000<10000)),
         group="Low Circulation (<10k)")
tp_lc$topic_label=factor(tp_lc$topic_label,
                         levels = c("other","sports",
                                    "crime","local","national"))

#create a single plot for all np circ groups
tp_circ=rbind(tp_hc,tp_mc,tp_lc)
tp_circ$group=factor(tp_circ$group,levels = c("High Circulation (>100k)",
                                "Medium Circulation (10k-100k)",
                                "Low Circulation (<10k)"))
tp_circg=ggplot(data = tp_circ) + 
  geom_col(aes(y=topic_label,x=topic_proportion,fill=group),
           position = "dodge") +
  xlab("Topic Proportion") + ylab("Topic") +
  theme(legend.position = "bottom",legend.title = element_blank())
tp_circg

### negativity by topic
tn=nptext %>% group_by(topic_label) %>%
  summarise(prop_negative=mean(1-pred))
tng=ggplot(data = tn) + geom_col(aes(y=topic_label,prop_negative))



## Trends over time

#negativity over time
nptext=nptext %>%
  mutate(year_month=paste(year,str_pad(month,2,pad="0"),sep = "-"))
month_nums=cbind(nptext$year_month %>% unique() %>% sort(),
                 c(1:20,22:44,49:52,55:72))
colnames(month_nums)=c("year_month","month_num")
nptext=merge(nptext,month_nums,by="year_month")

#filter out "other" topic since there's no story to tell about negativity for that
nm=nptext %>% filter(topic %in% c(1,3,4,5,6)) %>%
  group_by(year_month) %>%
  summarise(prop_negative=mean(1-pred),
            month_num=unique(month_num) %>% as.numeric()) 

#overall negativity over time
nmg=ggplot(data = nm,aes(x=month_num, y=prop_negative)) + 
  geom_point() + 
  geom_smooth(method="lm")


#negativity over time by topic
nmt=nptext %>% filter(topic %in% c(1,4,5,6)) %>%
  group_by(time_period,topic_label) %>%
  summarise(prop_negative=mean(1-pred),
            topic=unique(topic_label),
            n=n()) 

nmtg=ggplot(data = nmt,aes(x=time_period,y=prop_negative,fill = topic)) +
  geom_point() + 
  geom_smooth(method="loess") +
  scale_x_continuous(breaks=seq(1,12,1),
                     labels=c("1"="2000","2"="",
                            "3"="2001","4"="",
                            "5"="2002","6"="",
                            "7"="2003","8"="",
                            "9"="2004","10"="",
                            "11"="2005","12"="")) +
  xlab("Year") + ylab("Proportion of Stories that are Negative")


#sort np by negativity 
by_np=nptext %>%
  group_by(NPNAME1) %>%
  summarise(avg_negativity=mean(1-pred),
            n=n())



#add quarters
nptext=nptext %>%
  mutate(quarter=case_when(month_num %in% c(1,2,3) ~ 1,
                           month_num %in% c(4,5,6) ~ 2,
                           month_num %in% c(7,8,9) ~ 3,
                           month_num %in% c(10,11,12) ~ 4,
                           month_num %in% c(13,14,15) ~ 5,
                           month_num %in% c(16,17,18) ~ 6,
                           month_num %in% c(19,20,21) ~ 7,
                           month_num %in% c(22,23,24) ~ 8,
                           month_num %in% c(25,26,27) ~ 9,
                           month_num %in% c(28,29,30) ~ 10,
                           month_num %in% c(31,32,33) ~ 11,
                           month_num %in% c(34,35,36) ~ 12,
                           month_num %in% c(37,38,39) ~ 13,
                           month_num %in% c(40,41,42) ~ 14,
                           month_num %in% c(43,44,45) ~ 15,
                           month_num %in% c(46,47,48) ~ 16,
                           month_num %in% c(49,50,51) ~ 17,
                           month_num %in% c(52,53,54) ~ 18,
                           month_num %in% c(55,56,57) ~ 19,
                           month_num %in% c(58,59,60) ~ 20,
                           month_num %in% c(61,62,63) ~ 21,
                           month_num %in% c(64,65,66) ~ 22,
                           month_num %in% c(67,68,69) ~ 23,
                           month_num %in% c(70,71,72) ~ 24))

## Negativity by specific national news topics
np_natnews2=filter(np_natnews,topic.y%in%c(0,1,3))
np_topic2g=np_natnews2 %>%
  group_by(time_period,topic2_label) %>%
  summarise(prop_negative=mean(1-pred),
            topic=unique(topic2_label),
            n=n()) 


#plot economy, security, and foreign policy news story negativity over time
npt2g=ggplot(data = np_topic2g, aes(x=time_period,y=prop_negative,fill = topic,color=topic)) + 
  geom_point() + 
  geom_smooth(method="loess") +
  scale_x_continuous(breaks=seq(1,12,1),
                     labels=c("1"="2000","2"="",
                            "3"="2001","4"="",
                            "5"="2002","6"="",
                            "7"="2003","8"="",
                            "9"="2004","10"="",
                            "11"="2005","12"="")) +
  xlab("Year") + ylab("Proportion of Stories that are Negative")


# number of stories for each topic each year
nstories=ggplot(data = np_topic2g,aes(x=time_period,y=n)) + geom_col(aes(fill=topic),position = "dodge") + xlab("Year") + 
  ylab("n Articles")



## Variance in negativity over time by topic

#split into local and national news stories
nptext_l=nptext %>% filter(topic_label=="local")
nptext_n=nptext %>% filter(topic_label=="national")

nptext_ng=nptext_n %>%
  group_by(year,NPNAME1) %>%
  summarise(prop_negative=mean(1-pred),
            n=n())  %>% filter(n>50) %>%
  mutate(Topic=factor("National"))


nptext_lg=nptext_l %>%
  group_by(year,NPNAME1) %>%
  summarise(prop_negative=mean(1-pred),
            n=n()) %>% filter(n>50) %>%
  mutate(Topic=factor("Local"))

#variance dataset combined
grouped_vard=rbind(nptext_lg,nptext_ng)

#plot the distribution of negativity for local and national stories from each newspaper each year -
# local stories should have higher variance since each paper covers a different set of local news
varg=ggplot(data = grouped_vard, aes(x=as.factor(year),y=prop_negative,color=Topic)) +
  geom_boxplot() +
  ylab("Proportion of Stories that are Negative") + 
  xlab("Year")


#check sd by year
nsd=nptext_ng %>%
  group_by(year) %>%
  summarise(sd=sd(prop_negative))

lsd=nptext_lg %>%
  group_by(year) %>%
  summarise(sd=sd(prop_negative))

# load in quarterly gdp growth data and merge with the national news dataset
setwd()
gdp=read.csv("gdp_byq.csv")

econ_data=np_natnews2 %>% filter(topic2_label=="economy") %>%
  merge(gdp,by="quarter")

econ_gr=econ_data %>%
  group_by(quarter) %>%
  summarise(prop_positive=mean(pred),
            gdp=unique(gdp_growth),
            n=n())

#plot to see if negativity in economy news stories mirrors changes in the gdp growth rate
econg=ggplot(data = econ_gr,aes(quarter)) +
  geom_line(aes(y=prop_positive*100),color="navy") +
  geom_line(aes(y=gdp*2+50),color="darkorange") +
  scale_y_continuous(name = "Proportion of Economy Stories that are Positive",sec.axis = sec_axis(transform = ~./2-25,name = "GDP Growth Rate")) + 
  theme(axis.title.y.left=element_text(color="navy"),
    axis.text.y.left=element_text(color="navy"),
    axis.title.y.right=element_text(color="darkorange"),
    axis.text.y.right=element_text(color="darkorange")) +
  scale_x_continuous(breaks = seq(1,24,4),
                     labels = c("2000","2001","2002",
                              "2003","2004","2005"),
                     name = "Year",
                     limits = c(1,24))

#negativity by partisanship


#county maps, which counties appear in the sample of newspapers I collected

by_county=nptext %>%
  group_by(NPNAME1) %>%
  summarise(county_st=unique(county_st),
            count=1)
#load in county fips codes to use the usmaps package
setwd()
fipsm=read.csv("county_fips_master.csv")[,1:3] %>%
  mutate(county_st=paste(county_name,state_abbr,sep="-")) %>% 
  select(1,4)
by_county=merge(by_county,fipsm,by="county_st")

covered_counties=plot_usmap(data = by_county,
                            values = "count",
                            "counties",
                            linewidth=0.002) + 
  scale_fill_continuous(low="white",high="green",name="Covered")




#broadband penetration over time - previews what the independent variable looks like over time

brd=nptext %>% group_by(NPNAME1,time_period) %>%
  summarise(broadband=unique(broadband),
            n=n())

#boxplot- get the distribution of the treatment var during each time period
bpen=ggplot(data = brd,aes(x=factor(time_period),y=broadband))+
  geom_boxplot() + xlab("Year") + ylab("Broadband Penetration") +
  scale_x_discrete(labels=c("1"="2000","2"="",
                            "3"="2001","4"="",
                            "5"="2002","6"="",
                            "7"="2003","8"="",
                            "9"="2004","10"="",
                            "11"="2005","12"=""))


# add newspaper-level covariates for balance tests on selection into my sample that I collected from NewsBank from the universe of local newspapers 
setwd()
np_list_bal=read.csv("np_list_balance.csv")


np_list_bal=np_list_bal %>%
  mutate(in_nbsample=ifelse(NPNAME1%in%nptext$NPNAME1,1,0))

balance_test=np_list_bal %>%
  group_by(in_nbsample) %>%
  summarise(pop_m=mean(tot_pop),
            pop_sd=sd(tot_pop),
            dem_m=mean(dem_voteshare,na.rm=T),
            dem_sd=sd(dem_voteshare,na.rm=T),
            circ_m=mean(circ_abs,na.rm=T),
            circ_sd=sd(circ_abs,na.rm=T),
            jobs_m=mean(jobscount_2000,na.rm=T),
            jobs_sd=sd(jobscount_2000,na.rm=T),
            trt95_m=mean(trt95_year,na.rm=T),
            trt95_sd=sd(trt95_year,na.rm=T))
bt=bal.tab(select(np_list_bal,tot_pop,dem_voteshare,
               circ_abs,jobscount_2000,trt95_year),
        treat = np_list_bal$in_nbsample,
        disp=c("means","sds"))


#balance table - full sample
balance_table(select(np_list_bal,tot_pop,dem_voteshare,
               circ_abs,jobscount_2000,trt95_year,in_nbsample),
              "in_nbsample")

#balance table - omit top circ papers
np_list_bal_minus200k=np_list_bal %>%
  filter(circ_abs<200000)
balance_table(select(np_list_bal_minus200k,tot_pop,dem_voteshare,
               circ_abs,jobscount_2000,trt95_year,in_nbsample),
              "in_nbsample")

np_list_bal$in_nbsample=factor(np_list_bal$in_nbsample,
                               labels =c("No","Yes"))
circ_in_out=ggplot(data = np_list_bal,
                   aes(x=circ_abs,color=in_nbsample)) +
  geom_density() + scale_x_log10() +
  xlab("Circulation in 2000") + ylab("Density") + 
  theme(legend.position = "bottom") +
  guides(color=guide_legend(title="In NewsBank Sample"))



##NP Circulation and Jobs Over Time - how were newspaper finances during the study period?

#load in newspaper data from Editor & Publisher Yearbooks
setwd()
ep=read_dta("EP_panel_1995_2010.dta") %>% 
  filter(year%in%2000:2005) %>% 
  select(NPNAME1,year,circ_abs,jobscount) %>%
  filter(NPNAME1%in%nptext$NPNAME1)
ep$year=as.factor(ep$year)

#plot circulation of all newspapers over the 6 years of the sample period
circp=ggplot(data = ep, aes(x=year,y=log(circ_abs+1))) + 
  geom_boxplot()

#same for number of employees
jobsp=ggplot(data = ep, aes(x=year,y=jobscount)) + 
  geom_boxplot()



#circulation and jobs relative to 2000 levels
ep2000=ep %>% filter(year==2000) %>% select(-year)
colnames(ep2000)[2]="circ2000"
colnames(ep2000)[3]="jobs2000"

ep=merge(ep,ep2000,by="NPNAME1")

#create a variable for each newspaper-year observation, what was the circulation and n. employees compared to 2000
ep=ep %>% 
  mutate(circ_v_2000=circ_abs/circ2000,
         jobs_v_2000=jobscount/jobs2000)

#plot relative circulation and employee numbers for each newspaper
circp2=ggplot(data = ep, aes(x=year,y=circ_v_2000)) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Circulation as Proportion of 2000 Circulation") +
  geom_hline(yintercept = 1,lty=2,color="navy")


jobsp2=ggplot(data = ep, aes(x=year,y=jobs_v_2000)) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Number of Jobs as Proportion of 2000 Jobs") +
  geom_hline(yintercept = 1,lty=2,color="navy")



#density of circulation in 2000

#full sample distribution - with manual labels for newspapers at key dividing lines for circulation groups (200k, 100k, 10k)
circd=ggplot(data = ep, aes(x=circ2000)) +
  geom_density() +
  scale_x_log10() +
  xlab("Circulation in 2000") + ylab("Density") +
  geom_vline(xintercept=sort(ep2000$circ2000,decreasing = T)[25], lty=2) +
  geom_vline(xintercept=sort(ep2000$circ2000,decreasing = T)[52], lty=2) + 
  geom_vline(xintercept=sort(ep2000$circ2000,decreasing = T)[355], lty=2) +
  annotate("text", x=390000,y=0.3, size=unit(3.5,"pt"),
           label="Oklahoma City \n Oklahoman") + 
  annotate("text", x=65000,y=0.7, size=unit(3.5,"pt"),
           label="New Haven \n Register") + 
  annotate("text", x=6000,y=0.4, size=unit(3.5,"pt"),
           label="West Bend\n Daily News")


#same deistribution plot for jobs
jobd=ggplot(data=ep, aes(x=jobscount)) + 
  geom_density()





### NP List by circulation ###

#lists for the appendix - sort newspapers by ciculation levels in 2000, select out the top 50 and bottom 50 and create tables in latex code

name_merge=nptext %>% group_by(NPNAME1) %>%
  summarise(np=unique(np.x))
name_merge=name_merge[!duplicated(name_merge$NPNAME1),]
ep4=merge(ep3,name_merge,by="NPNAME1")

#top 50
top50c=ep4 %>% select(np,circ_abs_2000) %>%
  arrange(desc(circ_abs_2000)) 
top50c=top50c[1:50,]
kable(top50c,"latex")

#bottom 50
b50c=ep4 %>% select(np,circ_abs_2000) %>%
  arrange(circ_abs_2000)
b50c=b50c[1:50,]
kable(b50c,"latex")





