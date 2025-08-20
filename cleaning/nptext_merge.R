library(tidyverse)
library(readxl)
library(stringr)


#load np data
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperData")
nplist=read.csv("np_list.csv")

#load 2005 text data
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/2005")

nptext05=data.frame()
for (a in 1:3) {
  ltr=c("A","B","C")[a]
  filenm=paste("Headline_Scapev2_2005",ltr,".xlsx",sep="")
  tmpfile=read_xlsx(filenm)
  nptext05=rbind(nptext05,tmpfile)
  nfiles=c(91,88,88)[a]
  for (i in 1:nfiles) {
    filenm=paste("Headline_Scapev2_2005",ltr,"(",i,")",".xlsx",sep="")
    tmpfile=read_xlsx(filenm)
    nptext05=rbind(nptext05,tmpfile)
  }
}


#2004 text
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/2004")

nptext04=data.frame()
for (a in 1:3) {
  ltr=c("A","B","C")[a]
  filenm=paste("Headline_Scapev2_2004",ltr,".xlsx",sep="")
  tmpfile=read_xlsx(filenm)
  nptext04=rbind(nptext04,tmpfile)
  nfiles=c(75,40,71)[a]
  for (i in 1:nfiles) {
    filenm=paste("Headline_Scapev2_2004",ltr,"(",i,")",".xlsx",sep="")
    tmpfile=read_xlsx(filenm)
    nptext04=rbind(nptext04,tmpfile)
  }
}


##2003
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/2003")

nptext03=data.frame()
for (a in 1:2) {
  ltr=c("A","B","C")[a]
  filenm=paste("Headline_Scapev2_2003",ltr,".xlsx",sep="")
  tmpfile=read_xlsx(filenm)
  nptext03=rbind(nptext03,tmpfile)
  nfiles=c(99,93)[a]
  for (i in 1:nfiles) {
    filenm=paste("Headline_Scapev2_2003",ltr,"(",i,")",".xlsx",sep="")
    tmpfile=read_xlsx(filenm)
    nptext03=rbind(nptext03,tmpfile)
  }
}

#2002
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/2002")
nptext02=data.frame()
for (a in 1:3) {
  ltr=c("A","B","C")[a]
  filenm=paste("Headline_Scapev2_2002",ltr,".xlsx",sep="")
  tmpfile=read_xlsx(filenm)
  nptext02=rbind(nptext02,tmpfile)
  nfiles=c(90,81,87)[a]
  for (i in 1:nfiles) {
    filenm=paste("Headline_Scapev2_2002",ltr,"(",i,")",".xlsx",sep="")
    tmpfile=read_xlsx(filenm)
    nptext02=rbind(nptext02,tmpfile)
  }
}


##2001
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/2001")

nptext01=data.frame()
for (a in 1:3) {
  ltr=c("A","B","C")[a]
  filenm=paste("Headline_Scapev2_2001",ltr,".xlsx",sep="")
  tmpfile=read_xlsx(filenm)
  nptext01=rbind(nptext01,tmpfile)
  nfiles=c(70,71,57)[a]
  for (i in 1:nfiles) {
    filenm=paste("Headline_Scapev2_2001",ltr,"(",i,")",".xlsx",sep="")
    tmpfile=read_xlsx(filenm)
    nptext01=rbind(nptext01,tmpfile)
  }
}


##2000
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/2000")

nptext00=data.frame()
for (a in 1:3) {
  ltr=c("A","B","C")[a]
  filenm=paste("Headline_Scapev2_2000",ltr,".xlsx",sep="")
  tmpfile=read_xlsx(filenm)
  nptext00=rbind(nptext00,tmpfile)
  nfiles=c(49,70,77)[a]
  for (i in 1:nfiles) {
    filenm=paste("Headline_Scapev2_2000",ltr,"(",i,")",".xlsx",sep="")
    tmpfile=read_xlsx(filenm)
    nptext00=rbind(nptext00,tmpfile)
  }
}

nptext_full=rbind(nptext00,nptext01,nptext02,nptext03,nptext04,nptext05)


#clean text dataset
colnames(nptext_full)=c("headline","date","np","author","page","text")

#index of articles with page in the author column
miss_page=str_detect(nptext_full$author,"Page")
miss_page[is.na(miss_page)]=FALSE
nptext_full$page[miss_page]=nptext_full$author[miss_page]
nptext_full$author[miss_page]=NA


#nptext_full=read.csv("nptext_full.csv")

### add index


#list of newspapers
newsbank_nps=nptext_full %>%
  group_by(np) %>%
  summarise(n=n()) %>%
  filter(n>10)
#drop row for missing np names
newsbank_nps=newsbank_nps[-585,]

#assign an nb index to match newbank names to E&P
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperData")
newsbank_nps=newsbank_nps %>%
  mutate(nb_index=str_pad(as.character(row_number()),width=3,side="left",
                          pad="0"))

#check merge
nptext_full2=merge(nptext_full,newsbank_nps[,-2],by="np",all.x = T)


#manually clean up some name errors
#austin journal statesman
nptext_full2[nptext_full2$nb_index%in%c("024","025"),1]="Austin American-Statesman (TX)"

#bennington banner
nptext_full2[nptext_full2$nb_index%in%c("043","044"),1]="Bennington Banner (VT)"

#berkshire eagle
nptext_full2[nptext_full2$nb_index%in%c("047","048"),1]="Berkshire Eagle, The (MA)"

#bowie blade
nptext_full2[nptext_full2$nb_index%in%c("059","060"),1]="Bowie Blade News, The (MD)"

#greensboro news and record
nptext_full2[nptext_full2$nb_index%in%c("230","231"),1]="Greensboro News & Record (NC)"

#houston chronicle
nptext_full2[nptext_full2$nb_index%in%c("259","260"),1]="Houston Chronicle (TX)"

#intellegener journal
nptext_full2[nptext_full2$nb_index%in%c("264","271"),1]="Lancaster Intellegencer Journal (PA)"

#jersey journal
nptext_full2[nptext_full2$nb_index%in%c("275","276"),1]="Jersey Journal (NJ)"

#macon telegraph
nptext_full2[nptext_full2$nb_index%in%c("321","322"),1]="Macon Telegraph, The (GA)"

#princeton clarion
nptext_full2[nptext_full2$nb_index%in%c("429","430"),1]="Princeton Daily Clarion (IN)"

#lancaster sunday news
nptext_full2[nptext_full2$nb_index%in%c("455","511"),1]="Lancaster Sunday News (PA)"

#orlando sentinal
nptext_full2[nptext_full2$nb_index%in%c("513","395"),1]="Orlando Sentinal, The (FL)"

#san antonio express-news
nptext_full2[nptext_full2$nb_index%in%c("463","464"),1]="San Antonio Express-News (TX)"

#stl post dispatch
nptext_full2[nptext_full2$nb_index%in%c("491","492"),1]="St Louis Post-Dispatch (MO)"

#biloxi Sun herald
nptext_full2[nptext_full2$nb_index%in%c("506","507"),1]="Biloxi Sun Herald (MS)"

#tulsa world
nptext_full2[nptext_full2$nb_index%in%c("551","552"),1]="Tulsa World (OK)"

#atlanta journal constitution
nptext_full2[nptext_full2$nb_index%in%c("022","517"),1]="Atlanta Journal-Constitution (GA)"

#tampa tribune
nptext_full2[nptext_full2$nb_index%in%c("514","530"),1]="Tampa Tribune, The (FL)"

#buffalo news
nptext_full2[nptext_full2$nb_index%in%c("518","068"),1]="Buffalo News, The (NY)"

#daily oklahoman
nptext_full2[nptext_full2$nb_index%in%c("519","153"),1]="Daily Oklahoman, The (OK)"

#Dallas morning news
nptext_full2[nptext_full2$nb_index%in%c("520","174"),1]="Dallas Morning News, The (TX)"

#cedar rapids gazette
nptext_full2[nptext_full2$nb_index%in%c("521","222"),1]="Cedar Rapid Gazette, The (IA)"

#hartford courant
nptext_full2[nptext_full2$nb_index%in%c("522","237"),1]="Hartford Courant, The (CT)"

#raleigh news and observer
nptext_full2[nptext_full2$nb_index%in%c("523","369"),1]="Raleigh News & Observer, The (NC)"

#tacoma news tribune
nptext_full2[nptext_full2$nb_index%in%c("524","371"),1]="Tacoma News Tribune, The (WA)"

#oc register
nptext_full2[nptext_full2$nb_index%in%c("525","393"),1]="Orange County Register, The (CA)"

#palm beach post
nptext_full2[nptext_full2$nb_index%in%c("526","400"),1]="Palm Beach Post, The (FL)"

#sacramento bee
nptext_full2[nptext_full2$nb_index%in%c("528","456"),1]="Sacramento Bee, The (CA)"

#sd union tribune
nptext_full2[nptext_full2$nb_index%in%c("529","465"),1]="San Diego Union-Tribune, The (CA)"

#nj record
nptext_full2[nptext_full2$nb_index%in%c("527","437"),1]="New Jersey Record, The (NJ)"

#drop any newspapers with <10 articles or no name
no_nb=!is.na(nptext_full2$nb_index)
sum(no_nb)
nptext_full3=nptext_full2[no_nb,]


#re-do nb_index code with duplicates corrected
#list of newspapers
newsbank_nps2=nptext_full3 %>%
  group_by(np) %>%
  summarise(n=n()) 


#assign an nb index to match newbank names to E&P
newsbank_nps2=newsbank_nps2 %>%
  mutate(nb_index=str_pad(as.character(row_number()),width=3,side="left",
                          pad="0"))
write.csv(newsbank_nps2,"nb_index2.csv",row.names = F)



nptext_full4=merge(nptext_full3[,-7],newsbank_nps2[,-2],by="np",all.x=T)



test3=nptext_full4 %>% group_by(np) %>%
  summarise(n=n(),
            ind=unique(nb_index))


test2=nptext_full2 %>% group_by(np) %>%
  summarise(n=n(),
            ind=unique(nb_index))

#save full dataset
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText")
write.csv(nptext_full4,"nptext_full.csv",row.names = T)

#save just headlines
npheadlines_full=nptext_full4 %>% select(-text)
write.csv(npheadlines_full,"np_headlines_full.csv",row.names = T)


