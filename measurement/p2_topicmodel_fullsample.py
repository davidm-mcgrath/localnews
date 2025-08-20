#!/bin/env python

import datasets as ds
import numpy as np
import pandas as pd
import random
import sklearn
from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer
from sklearn.decomposition import LatentDirichletAllocation
import nltk
from joblib import dump

#load data
np_text=pd.read_csv("5M_RS/np_full_rs5m.csv")
documents=np_text.iloc[:,7].tolist()

# create function to display LDA top words 
def display_topics(model, feature_names, no_top_words):
    for topic_idx, topic in enumerate(model.components_):
        print( "Topic %d:" % (topic_idx))
        print( " ".join([feature_names[i] for i in topic.argsort()[:-no_top_words - 1:-1]]))


#define parameters
no_features=1000
n_topics=8


#vectorize texts
tf_vectorizer = CountVectorizer(max_df=0.95, min_df=2, max_features=no_features, stop_words='english')
tf = tf_vectorizer.fit_transform(documents)
tf_feature_names = tf_vectorizer.get_feature_names_out()

# Run LDA
lda = LatentDirichletAllocation(n_components=n_topics, max_iter=5, 
                                learning_method='online', 
                                learning_offset=50.,random_state=0).fit(tf)
doc_topics=lda.transform(tf)

#inspect top words for each topic
no_top_words = 15
display_topics(lda, tf_feature_names, no_top_words)

#add highest pr topic label and that probability to the main dataframe
main_topic=doc_topics.argmax(axis=1)
main_topic_pr=doc_topics.max(axis=1)

np_text['topic']=main_topic
np_text['topic_pr']=main_topic_pr

#save df as csv
np_text.to_csv('np_news_rs5m_topics.csv',index=False)



