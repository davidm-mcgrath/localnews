import pandas as pd
import numpy as np
import random
import torch
from transformers import pipeline

device = 0 if torch.cuda.is_available() else -1

# Load zero-shot classification pipeline with RoBERTa (NLI model)
classifier = pipeline("zero-shot-classification", model="facebook/bart-large-mnli",device=device)

def classify_ideology(text, labels=["liberal", "conservative"]):
    result = classifier(text, candidate_labels=labels)
    label = result["labels"][0]
    score = result["scores"][0]
    return label, score

#upload test data 
nptext_n=pd.read_csv("natnews_full_topics2.csv")

df=pd.DataFrame(data={'text': nptext_n['head_text']})
df[['predicted_label', 'confidence']] = df['text'].apply(
    lambda x: pd.Series(classify_ideology(x)))

df['X']=nptext_n['X']
df=df.drop(['text'],axis=1)
df.to_csv('natnewsfull_zsbert.csv',index=False)
