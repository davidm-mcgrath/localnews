library(tidyverse)

setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperData")
ep_index=read.csv("np_list_indexed_to_nb.csv")[,-9] %>%
  filter(!is.na(nb_index))
nb_index=read.csv("nb_index2.csv")

#merge two index csvs
indx=merge(nb_index,ep_index,by="nb_index")
write.csv(indx,"merged_index.csv")

#articles will be grouped according to ep_index and NPNAME1


setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText")
nptext=read.csv("np_headlines_noobits.csv")
