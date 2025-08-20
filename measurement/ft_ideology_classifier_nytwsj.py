import pandas as pd
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.model_selection import train_test_split
import numpy as np
import re
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
import matplotlib.pyplot as plt
from sklearn.metrics import accuracy_score
import gensim.downloader as api
import fasttext


#load in model trained on new rep-nat rev
model=fasttext.load_model('ft_ideology_nytwsj.ftz')
#predict on local news data
nptext_n=pd.read_csv("natnews_full_topics2.csv")
test_texts=nptext_n['head_text'].to_list()
#remove linebreaks
clean_texts=[]
for i in test_texts:
    clean_texts.append(re.sub("\n","",i))

#run model
ft_label=[]
ft_prob=[]
for t in clean_texts:
    label, prob = model.predict(t)
    ft_label.append(label[0].replace('__label__', ''))
    ft_prob.append(prob[0])

index=nptext_n.iloc[:,1]
nptext=pd.DataFrame(data={'X': index})

nptext['ft2_label']=ft_label
nptext['ft2_prob']=ft_prob

nptext.to_csv('natnewsfull_ft2_out.csv',index=False)
