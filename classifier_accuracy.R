library(tidyverse)
library(caret)

setwd("C:/Users/dm4947/OneDrive/Documents/NYU/Dissertation/Data/NewspaperText/2025_RS")

bert=read.csv("head_sentiment10k_from100krs.csv")[,1:4] %>%
  filter(label%in%c("POSITIVE","NEGATIVE"))
gpt=read.csv("headlines_with_sentiment10k_gpt.csv") %>%
  mutate(X=row_number()-1) %>%
  filter(X%in%bert$X) %>%
  filter(sentiment!="Neutral")

bert=bert %>% filter(X%in%gpt$X)

gpt$sentiment=gpt$sentiment %>% str_to_upper()

confusionMatrix(data = factor(bert$label), reference = factor(gpt$sentiment))
