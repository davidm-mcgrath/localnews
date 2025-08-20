import numpy as np
import pandas as pd
import random
import re
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.preprocessing import normalize
from sklearn.feature_extraction import text as sklearn_text
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize

# Define a text cleaner
def clean_text(text):
    if not isinstance(text, str):
        return ""
    
    # Lowercase
    text = text.lower()
    
    # Remove numbers and punctuation (keep only letters and spaces)
    text = re.sub(r'[^a-z\s]', '', text)
    
    # Remove extra whitespace
    text = re.sub(r'\s+', ' ', text).strip()
    
    # Remove stopwords and short/nonsense words
    stop_words = set(sklearn_text.ENGLISH_STOP_WORDS)
    tokens = [word for word in text.split() if word not in stop_words and len(word) > 2]
    
    return ' '.join(tokens)


def wordscores(reference_texts, reference_scores, virgin_texts):
    all_texts = pd.DataFrame(data={'text': reference_texts + virgin_texts})
    all_texts['cleaned_texts']=all_texts['text'].apply(clean_text)
    processed_texts=all_texts['cleaned_texts'].tolist()

    vectorizer = CountVectorizer(min_df=50)
    dtm = vectorizer.fit_transform(processed_texts)

    vocab = vectorizer.get_feature_names_out()
    ref_dtm = dtm[:len(reference_texts)]
    virgin_dtm = dtm[len(reference_texts):]

    # Compute relative frequencies for reference texts
    ref_freqs = normalize(ref_dtm, norm='l1', axis=1)
    avg_freq = ref_freqs.mean(axis=0).A1

    # Compute word scores
    word_scores = np.zeros(len(vocab))
    for j in range(len(vocab)):
        weighted_score = 0.0
        for i, score in enumerate(reference_scores):
            weighted_score += ref_freqs[i, j] * score
        word_scores[j] = weighted_score

    # Estimate virgin scores
    virgin_freqs = normalize(virgin_dtm, norm='l1', axis=1)
    virgin_scores = virgin_freqs @ word_scores

    return virgin_scores, vocab, word_scores


## load in the data and clean
#add texts
natrev=pd.read_csv("natreview_text.csv")
newrep=pd.read_csv("newrepublic_text.csv")

#add reference scores
natrev['ref_score']=1
newrep['ref_score']=-1

#full text var
natrev['fulltext']=natrev['headline'] + " " + natrev['text']
newrep['fulltext']=newrep['headline'] + " " + newrep['text']

#create full reference data with scores
refdata=pd.concat([natrev,newrep])
ref_texts=refdata['fulltext'].to_list()
#remove linebreaks
reference_texts=[]
for i in ref_texts:
    reference_texts.append(re.sub("\n","",i))

reference_scores=refdata['ref_score'].to_list()

# test data
nptext_n=pd.read_csv("natnews_full_topics2.csv")

virgin_texts=nptext_n['head_text'].to_list()

#run model
scores, vocab, word_scores = wordscores(reference_texts, reference_scores, virgin_texts)
    
print("Estimated ideology scores:", scores)
print("Top scored words:", sorted(zip(vocab, word_scores), key=lambda x: -abs(x[1]))[:10])

df=pd.DataFrame(list(zip(scores)), columns=['score'])
df['X']=nptext_n['X']
df.to_csv("nptextfull_ws_out.csv",index=False)

