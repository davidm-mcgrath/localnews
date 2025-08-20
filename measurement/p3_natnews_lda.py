import datasets as ds
import numpy as np
import pandas as pd
import random
import sklearn
from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer
from sklearn.decomposition import LatentDirichletAllocation
import nltk
from joblib import load

#load data
nptext=pd.read_csv("nptext_full_lda1.csv")


#filter natnews 
np_natnews=nptext[nptext['topic1'].isin([2])]

#vectorize natnews text sample
documents=np_natnews['head_text'].tolist()
tf_vectorizer = CountVectorizer(max_df=0.95, min_df=10, max_features=1000, stop_words='english')
tf = tf_vectorizer.fit_transform(documents)
tf_feature_names2 = tf_vectorizer.get_feature_names_out()

#run new lda model
lda2 = LatentDirichletAllocation(n_components=6, max_iter=5, 
                                learning_method='online', 
                                learning_offset=50.,random_state=0).fit(tf)

#fit lda2
doc_topics=lda2.transform(tf)

main_topic=doc_topics.argmax(axis=1)
np_natnews['topic2']=main_topic

# Extract top 20 words for each topic
def get_top_words(model, feature_names, n_top_words=20):
    topic_words = {}
    for topic_idx, topic in enumerate(model.components_):
        top_features_ind = topic.argsort()[:-n_top_words - 1:-1]
        top_words = [feature_names[i] for i in top_features_ind]
        topic_words[f"Topic {topic_idx}"] = top_words
    return pd.DataFrame(dict([(k, pd.Series(v)) for k,v in topic_words.items()]))


# Output top 20 words per topic
top_words_df2 = get_top_words(lda2, tf_feature_names2, n_top_words=10)
top_words_df2=top_words_df2.transpose()
top_words_df2.to_csv("lda2_topwords2.csv")

#save topics
np_natnews.to_csv("natnews_full_topics2.csv",index=False)
