#load packages
library(tidyverse)
library(stargazer)
library(lfe)
library(broom)
library(dotwhisker)

#load in analysis datasets
setwd()
nptext_monthgrouped=read.csv("5MRS_groupbymonth_full.csv")
nptext_monthgrouped_l=read.csv("5MRS_groupbymonth_local.csv")
nptext_monthgrouped_c=read.csv("5MRS_groupbymonth_crime.csv")
nptext_monthgrouped_n=read.csv("5MRS_groupbymonth_natl.csv")

nptext_6monthgrouped=read.csv("5MRS_groupby6month_full.csv")
nptext_6monthgrouped_l=read.csv("5MRS_groupby6month_local.csv")
nptext_6monthgrouped_c=read.csv("5MRS_groupby6month_crime.csv")
nptext_6monthgrouped_n=read.csv("5MRS_groupby6month_natl.csv")

## negativity regressions by topic
#local
mloc1=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
                data = nptext_monthgrouped_l)

mloc2=felm(prop_negative ~ after95 | month_year + NPNAME1|0|NPNAME1,
                data = nptext_monthgrouped_l)


#crime
mcrime1=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_c)
mcrime2=felm(prop_negative ~ after95 | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_c)

#national
mnat1=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
                data = nptext_monthgrouped_n)
mnat2=felm(prop_negative ~ after95| month_year + NPNAME1|0|NPNAME1,
                data = nptext_monthgrouped_n)

stargazer(mloc1,mloc2,mcrime1,mcrime2,mnat1,mnat2,type="latex",omit.stat = "ser") 


### what topics are covered regressions
#local
ploc1=felm(prop_local ~ broadband | month_year + NPNAME1|0|NPNAME1,
                data = nptext_monthgrouped)

ploc2=felm(prop_local ~ after95 | month_year + NPNAME1|0|NPNAME1,
                data = nptext_monthgrouped)


#crime
pcrime1=felm(prop_crime ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped)
pcrime2=felm(prop_crime ~ after95 | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped)

#national
pnat1=felm(prop_natl ~ broadband | month_year + NPNAME1|0|NPNAME1,
                data = nptext_monthgrouped)
pnat2=felm(prop_natl ~ after95| month_year + NPNAME1|0|NPNAME1,
                data = nptext_monthgrouped)

stargazer(ploc1,ploc2,pcrime1,pcrime2,pnat1,pnat2,type="latex",omit.stat = "ser")



### do crime results change when separating the sample by circulation/market size, since there are more criminal incidents in large markets 

#create 3 subsets of monthgrouped datasets
nptext_monthgrouped_low=nptext_monthgrouped %>%
  filter(circ<10000)
nptext_monthgrouped_med=nptext_monthgrouped %>%
  filter(circ>10000 & circ<100000)
nptext_monthgrouped_high=nptext_monthgrouped %>%
  filter(circ>100000)


pcrime1_1=felm(prop_crime ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_low)
pcrime1_2=felm(prop_crime ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_med)
pcrime1_3=felm(prop_crime ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_high)


stargazer(pcrime1_1,pcrime1_2,pcrime1_3,
          type="latex",omit.stat = "ser")


#repeat the circ subsample regressions for local and national stories
plocal1_1=felm(prop_local ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_low)
plocal1_2=felm(prop_local ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_med)
plocal1_3=felm(prop_local ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_high)


pnatl1_1=felm(prop_natl ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_low)
pnatl1_2=felm(prop_natl ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_med)
pnatl1_3=felm(prop_natl ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = nptext_monthgrouped_high)


#get each model coef and se into df
estimate=c(coef(pcrime1_1),coef(pcrime1_2),coef(pcrime1_3),
             coef(plocal1_1),coef(plocal1_2),coef(plocal1_3),
             coef(pnatl1_1),coef(pnatl1_2),coef(pnatl1_3))
std.error=c(pcrime1_1$cse,pcrime1_2$cse,pcrime1_3$cse,
             plocal1_1$cse,plocal1_2$cse,plocal1_3$cse,
             pnatl1_1$cse,pnatl1_2$cse,pnatl1_3$cse)
split_sample_models=cbind(estimate,std.error) %>% data.frame() %>%
   mutate(ci_low=estimate-1.96*std.error,
         ci_high=estimate+1.96*std.error)
split_sample_models$model=rep(c("Low Circulation",
                                "Medium Circulation",
                                "High Circulation"),3)
split_sample_models$term=rep(c("Crime","Local","National"),each=3)

## create dotwhisker plot for coefficients on broadband in each 
spl_models=dwplot(split_sample_models) + theme(legend.position = "bottom",
                                                                              legend.title = element_blank()) +
  xlab("Coefficient on Broadband Penetration") + geom_vline(xintercept = 0,
                                                            lty=2)
spl_models


#negativity regressions split by circ

#local
mloc1=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
            data = filter(nptext_monthgrouped_l,circ<10000))
mloc2=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
            data = filter(nptext_monthgrouped_l,
                          circ>10000 & circ<100000))
mloc3=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
            data = filter(nptext_monthgrouped_l,circ<100000))


#crime
mcrime1=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
            data = filter(nptext_monthgrouped_c,circ<10000))
mcrime2=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
            data = filter(nptext_monthgrouped_c,
                          circ>10000 & circ<100000))
mcrime3=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
            data = filter(nptext_monthgrouped_c,circ<100000))


#national
mnat1=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
            data = filter(nptext_monthgrouped_n,circ<10000))
mnat2=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
            data = filter(nptext_monthgrouped_n,
                          circ>10000 & circ<100000))
mnat3=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
            data = filter(nptext_monthgrouped_n,circ<100000))

#get each model coef and se into df
estimate=c(coef(mcrime1),coef(mcrime2),coef(mcrime3),
             coef(mloc1),coef(mloc2),coef(mloc3),
             coef(mnat1),coef(mnat2),coef(mnat3))
std.error=c(mcrime1$cse,mcrime2$cse,mcrime3$cse,
             mloc1$cse,mloc2$cse,mloc3$cse,
             mnat1$cse,mnat2$cse,mnat3$cse)
split_sample_models_neg=cbind(estimate_neg,std.error_neg) %>% data.frame() %>%
  mutate(ci_low=coefs-1.96*ses,
         ci_high=coefs+1.96*ses)
split_sample_models_neg$model=rep(c("Low Circulation",
                                "Medium Circulation",
                                "High Circulation"),3)
split_sample_models_neg$term=rep(c("Crime","Local","National"),each=3)
library(dotwhisker)

spl_models_neg=dwplot(split_sample_models_neg) + theme(legend.position = "bottom",
                                                                              legend.title = element_blank()) +
  xlab("Coefficient on Broadband Penetration") + geom_vline(xintercept = 0,
                                                            lty=2)
spl_models_neg






### Grouped by 6 months - robustness check: do the results hold when aggregated to 6 months instead of just one

##negativity results - 6 month
#local
r1_mloc1=felm(prop_negative ~ broadband | time_period + NPNAME1|0|NPNAME1,
                data = nptext_6monthgrouped_l)

r1_mloc2=felm(prop_negative ~ after95 | time_period + NPNAME1|0|NPNAME1,
                data = nptext_6monthgrouped_l)


#crime
r1_mcrime1=felm(prop_negative ~ broadband | time_period + NPNAME1|0|NPNAME1,
                  data = nptext_6monthgrouped_c)
r1_mcrime2=felm(prop_negative ~ after95 | time_period + NPNAME1|0|NPNAME1,
                  data = nptext_6monthgrouped_c)

#national
r1_mnat1=felm(prop_negative ~ broadband | time_period + NPNAME1|0|NPNAME1,
                data = nptext_6monthgrouped_n)
r1_mnat2=felm(prop_negative ~ after95| time_period + NPNAME1|0|NPNAME1,
                data = nptext_6monthgrouped_n)

stargazer(r1_mloc1,r1_mloc2,r1_mcrime1,r1_mcrime2,r1_mnat1,r1_mnat2,type="latex",omit.stat = "ser") 

## proportion results - 6 month
#local
r1_ploc1=felm(prop_local ~ broadband | time_period + NPNAME1|0|NPNAME1,
                data = nptext_6monthgrouped)

r1_ploc2=felm(prop_local ~ after95 | time_period + NPNAME1|0|NPNAME1,
                data = nptext_6monthgrouped)


#crime
r1_pcrime1=felm(prop_crime ~ broadband | time_period + NPNAME1|0|NPNAME1,
                  data = nptext_6monthgrouped)
r1_pcrime2=felm(prop_crime ~ after95 | time_period + NPNAME1|0|NPNAME1,
                  data = nptext_6monthgrouped)

#national
r1_pnat1=felm(prop_natl ~ broadband | time_period + NPNAME1|0|NPNAME1,
                data = nptext_6monthgrouped)
r1_pnat2=felm(prop_natl ~ after95| time_period + NPNAME1|0|NPNAME1,
                data = nptext_6monthgrouped)

stargazer(r1_ploc1,r1_ploc2,r1_pcrime1,r1_pcrime2,r1_pnat1,r1_pnat2,type="latex",omit.stat = "ser")







#### Omitting the top 53 circulation newspapers - robustness check: are the main results being driven by the large papers in the sample
## negativity results - small/med circ
#local
r2_mloc1=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
                data = filter(nptext_monthgrouped_l,circ<100000))

r2_mloc2=felm(prop_negative ~ after95 | month_year + NPNAME1|0|NPNAME1,
                data = filter(nptext_monthgrouped_l,circ<100000))


#crime
r2_mcrime1=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = filter(nptext_monthgrouped_c,circ<100000))
r2_mcrime2=felm(prop_negative ~ after95 | month_year + NPNAME1|0|NPNAME1,
                  data = filter(nptext_monthgrouped_c,circ<100000))

#national
r2_mnat1=felm(prop_negative ~ broadband | month_year + NPNAME1|0|NPNAME1,
                data = filter(nptext_monthgrouped_n,circ<100000))
r2_mnat2=felm(prop_negative ~ after95| month_year + NPNAME1|0|NPNAME1,
                data = filter(nptext_monthgrouped_n,circ<100000))

stargazer(r2_mloc1,r2_mloc2,r2_mcrime1,r2_mcrime2,r2_mnat1,r2_mnat2,
          type="latex",omit.stat = "ser") 

## proportion results - small/med circ
#local
r2_ploc1=felm(prop_local ~ broadband | month_year + NPNAME1|0|NPNAME1,
                data = filter(nptext_monthgrouped,circ<100000))

r2_ploc2=felm(prop_local ~ after95 | month_year + NPNAME1|0|NPNAME1,
                data = filter(nptext_monthgrouped,circ<100000))


#crime
r2_pcrime1=felm(prop_crime ~ broadband | month_year + NPNAME1|0|NPNAME1,
                  data = filter(nptext_monthgrouped,circ<100000))
r2_pcrime2=felm(prop_crime ~ after95 | month_year + NPNAME1|0|NPNAME1,
                  data = filter(nptext_monthgrouped,circ<100000))

#national
r2_pnat1=felm(prop_natl ~ broadband | month_year + NPNAME1|0|NPNAME1,
                data = filter(nptext_monthgrouped,circ<100000))
r2_pnat2=felm(prop_natl ~ after95| month_year + NPNAME1|0|NPNAME1,
                data = filter(nptext_monthgrouped,circ<100000))

stargazer(r2_ploc1,r2_ploc2,r2_pcrime1,r2_pcrime2,r2_pnat1,r2_pnat2,
          type="latex",omit.stat = "ser")



