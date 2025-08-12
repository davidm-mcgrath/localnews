library(tidyverse)
set.seed(11)


setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText")
#removing obits, birth notices, classfieds, no headline
nphead=read.csv("np_headlines_full.csv")

nphead$headline=tolower(nphead$headline)

#remove by key word in headline
ob_index=str_detect(nphead$headline,"death notice") #drop 225,374
nphead=nphead[!ob_index,] #26.12 mil down to 25.89 mil

ob_index2=str_detect(nphead$headline,"obituar") #drop 372,552
nphead=nphead[!ob_index2,] #down to 25.52 mil

ob_index3=str_detect(nphead$headline,"no headline") #88037
nphead=nphead[!ob_index3,] #25.43 mil

ob_index4=str_detect(nphead$headline,"classifieds") #216
nphead=nphead[!ob_index4,]

ob_index5=str_detect(nphead$headline,"births") 
nphead=nphead[!ob_index5,]

ob_index6=str_detect(nphead$headline,"funerals") 
nphead=nphead[!ob_index6,]

ob_index7=str_detect(nphead$headline,"birthdays") 
nphead=nphead[!ob_index7,]

#three words or less - prep for NER
hwords=str_split(nphead$headline,"\\s+")
num_words=sapply(hwords,length) 

nphead$num_words=num_words
nphead$under3words=ifelse(nphead$num_words<4,1,0)

nphead_u3=filter(nphead,under3words==1)
write.csv(nphead_u3,"u3_npheadlines.csv")


#reload u3 headlines after running ner in classifier_bert.py
ner_head=read.csv("ner_headlines.csv")
ner_head=ner_head[,3:13]
colnames(ner_head)[c(10,11)]=c("entities","is_person")
#drop u3 headlines that are just names
ner_head=filter(ner_head,is_person==0)

#get rownumber X of u3 headlines that aren't obits
ner_indx=ner_head$X

u3_nonobits=nphead[nphead$X%in%ner_indx,]
np_head=rbind(filter(nphead,under3words==0),u3_nonobits)

#save filtered headlines
write.csv(np_head,"np_headlines_noobits.csv",row.names = F)






#####create random samples for analysis
setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText")
np_head=read.csv("np_headlines_noobits.csv")
npfulltext=read.csv("nptext_full.csv")

#100k
rs100k=sample(np_head$X,100000)
npfull_100k=npfulltext[npfulltext$X%in%rs100k,]
nphead_100k=np_head[np_head$X%in%rs100k,]

#2M
rs2m=sample(np_head$X,2000000)
npfull_2m=npfulltext[npfulltext$X%in%rs2m,]
nphead_2m=np_head[np_head$X%in%rs2m,]

### create new text variables for full text dataframes
#100k
npfull_100k=npfull_100k %>%
  mutate(text=str_remove_all(text,"\n"))

headline_plus=c()
for (i in 1:nrow(npfull_100k)) {
  headline_plus[i]=paste(npfull_100k$headline[i],
                  paste(strsplit(npfull_100k$text[i],"(?<=\\.)\\s(?=[A-Z])",
                                  perl=T)[[1]][1:3],collapse = ". "),
                  sep = ". ")
}
npfull_100k=cbind(npfull_100k,headline_plus)

#2M
npfull_2m=npfull_2m %>%
  mutate(text=str_remove_all(text,"\n"))

headline_plus=c()
for (i in 1:nrow(npfull_2m)) {
  headline_plus[i]=paste(npfull_2m$headline[i],
                         paste(strsplit(npfull_2m$text[i],"(?<=\\.)\\s(?=[A-Z])",
                                        perl=T)[[1]][1:3],collapse = ". "),
                         sep = ". ")
}
npfull_2m=cbind(npfull_2m,headline_plus)

npfull_100k=npfull_100k %>% select(-text)
npfull_2m=npfull_2m %>% select(-text)


##5M random sample
rs5m=sample(np_head$X,5000000)
npfull_5m=npfulltext[npfulltext$X%in%rs5m,]


npfull_5m=npfull_5m %>%
  mutate(text=str_remove_all(text,"\n"))

headline_plus=c()
for (i in 1:nrow(npfull_5m)) {
  headline_plus[i]=paste(npfull_5m$headline[i],
                         paste(strsplit(npfull_5m$text[i],"(?<=\\.)\\s(?=[A-Z])",
                                        perl=T)[[1]][1:3],collapse = ". "),
                         sep = ". ")
}
npfull_5m=cbind(npfull_5m,headline_plus)
npfull_5m=npfull_5m %>% select(-text)


#Full sample - no obits
npfull=npfulltext[npfulltext$X%in%np_head$X,]
npfull1=npfull[1:6000000,]
npfull2=npfull[6000001:12000000,]
npfull3=npfull[12000001:18000000,]
npfull4=npfull[18000001:nrow(npfull),]

write.csv(npfull1,"nptext_full1.csv")
write.csv(npfull2,"nptext_full2.csv")
write.csv(npfull3,"nptext_full3.csv")
write.csv(npfull4,"nptext_full4.csv")


setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/2025_RS")
write.csv(npfull_100k,"np_full_rs100k.csv", row.names=FALSE)
write.csv(npfull_2m,"np_full_rs2m.csv", row.names=FALSE)
write.csv(nphead_100k,"np_head_rs100k.csv", row.names=FALSE)
write.csv(nphead_2m,"np_head_rs2m.csv", row.names=FALSE)

write.csv(npfull_5m,"np_full_rs5m.csv",row.names = FALSE)


