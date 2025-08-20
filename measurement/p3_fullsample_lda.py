import datasets as ds
import numpy as np
import pandas as pd
import random
import sklearn
from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer
from sklearn.decomposition import LatentDirichletAllocation
import nltk
from joblib import load

#load data and pretrained topic models
text1=pd.read_csv("nptext_full1.csv")
text2=pd.read_csv("nptext_full2.csv")
text3=pd.read_csv("nptext_full3.csv")
text4=pd.read_csv("nptext_full4.csv")


nptext=pd.concat([text1,text2,text3,text4])
nptext=nptext.dropna(subset=['text','headline'])
#create headline plus text variable
nptext['head_text']=nptext['headline'] + " " + nptext['text']



#vectorize full text sample
documents=nptext['head_text'].tolist()

tf_vectorizer = CountVectorizer(max_df=0.95, min_df=20, max_features=1000, stop_words='english')
tf = tf_vectorizer.fit_transform(documents)
tf_feature_names1 = tf_vectorizer.get_feature_names_out()

#fit lda1
lda1 = LatentDirichletAllocation(n_components=8, max_iter=5, 
                                learning_method='online', 
                                learning_offset=50.,random_state=0).fit(tf)
doc_topics=lda1.transform(tf)

main_topic=doc_topics.argmax(axis=1)
nptext['topic1']=main_topic

# Extract top 20 words for each topic
def get_top_words(model, feature_names, n_top_words=20):
    topic_words = {}
    for topic_idx, topic in enumerate(model.components_):
        top_features_ind = topic.argsort()[:-n_top_words - 1:-1]
        top_words = [feature_names[i] for i in top_features_ind]
        topic_words[f"Topic {topic_idx}"] = top_words
    return pd.DataFrame(dict([(k, pd.Series(v)) for k,v in topic_words.items()]))



# Output top 20 words per topic
top_words_df1 = get_top_words(lda1, tf_feature_names1, n_top_words=10)
top_words_df1=top_words_df1.transpose()
top_words_df1.to_csv("lda1_topwords.csv")

nptext.to_csv("nptext_full_lda1.csv",index=False)
