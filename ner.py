import os
import datasets as ds
import numpy as np
import pandas as pd
from transformers import AutoTokenizer, AutoModelForSequenceClassification, Trainer
from transformers import pipeline
import evaluate
import torch
import accelerate
import spacy
from collections import Counter


###named entity recognition
ner=spacy.load("en_core_web_sm")
ner_cat=["PERSON"]

#prep u3 word headline
u3_text=np_headu3["headline"].tolist()
len(u3_text) #6647986


#confirm each headline is stored as str, drop floats
tp_list=[]
for h in u3_text:
  strng=isinstance(h,str)
  tp_list.append((strng))

strfil=np.array(tp_list) 
np_arr=np.array(u3_text)
u3_text=np_arr[strfil].tolist()
len(u3_text) #6646156

#run ner model on each headline
ner_list=[]
for i in u3_text:
  doc=ner(i)
  ner_list.append((doc))
  

len(ner_list) #6646156

#extract entities
ner_ent=[]
for doc in ner_list:
  ner_ent.append(doc.ents)

#determine if entities include a person for each headline
ner_entperson=[]
for i in list(range(len(ner_ent))):
  text_ent=ner_ent[i]
  if len(text_ent)==0:
    ner_entperson.append(0) 
  else:
    ent_labs=[]
    for ent in text_ent:
      ent_labs.append(ent.label_)
    if 'PERSON' in ent_labs:
      ner_entperson.append(1)
    else: 
      ner_entperson.append(0)
        

#bind ent labels to dataframe       
np_headlinesu3=np_headu3.loc[strfil,]
ner_df=pd.DataFrame(list(zip(ner_ent,ner_entperson)))
np_headlinesu3=pd.concat([np_headlinesu3.reset_index(drop=True),ner_df],axis=1)     
  
#save df as csv
np_headlinesu3.to_csv("ner_headlines.csv")
