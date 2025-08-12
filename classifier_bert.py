import os
import datasets as ds
import numpy as np
import pandas as pd
from transformers import AutoTokenizer, AutoModelForSequenceClassification, Trainer
from transformers import pipeline, TrainingArguments
import evaluate
import torch
import accelerate
import spacy
from collections import Counter



# Create class for data preparation
class SimpleDataset:
    def __init__(self, tokenized_texts):
        self.tokenized_texts = tokenized_texts
    
    def __len__(self):
        return len(self.tokenized_texts["input_ids"])
    
    def __getitem__(self, idx):
        return {k: v[idx] for k, v in self.tokenized_texts.items()}

#load test data
os.chdir('C:\\Users\\dm4947\\OneDrive\\Documents\\NYU\\Dissertation\\Data\\NewspaperText\\2025_RS')
np_text=pd.read_csv("np_full_rs100k.csv")


#load models and build trainer
model_name = "siebert/sentiment-roberta-large-english"
tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModelForSequenceClassification.from_pretrained(model_name)
trainer = Trainer(model=model)

#build classifier
classifier=pipeline("sentiment-analysis",model="siebert/sentiment-roberta-large-english")

#tokenize test data
tr_texts=np_head.iloc[0:1000,2].tolist()
tr_tok=tokenizer(tr_texts,truncation=True,padding=True)
tr_tok
tr_data=SimpleDataset(tr_tok)


#run on test data
tr_pred=trainer.predict(tr_data)


preds = tr_pred.predictions.argmax(-1)
labels = pd.Series(preds).map(model.config.id2label)
scores = (np.exp(tr_pred[0])/np.exp(tr_pred[0]).sum(-1,keepdims=True)).max(1)

df = pd.DataFrame(list(zip(tr_texts,preds,labels,scores)), columns=['text','pred','label','score'])
df.head()
