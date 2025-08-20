import pandas as pd
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.model_selection import train_test_split
import numpy as np
import re
import fasttext

## load in the training data and clean
#add texts
natrev=pd.read_csv("natreview_text.csv")
newrep=pd.read_csv("newrepublic_text.csv")

#add reference scores
natrev['label']='conservative'
newrep['label']='liberal'

#full text var
natrev['fulltext']=natrev['headline'] + " " + natrev['text']
newrep['fulltext']=newrep['headline'] + " " + newrep['text']

#create full reference data with scores
refdata=pd.concat([natrev,newrep])
ref_texts=refdata['fulltext'].to_list()
#remove linebreaks
clean_texts=[]
for i in ref_texts:
    clean_texts.append(re.sub("\n","",i))
refdata['cleantext']=clean_texts


# Split into 80% train, 20% test
train_df, test_df = train_test_split(refdata, test_size=0.2, random_state=42, shuffle=True)

# reset indices
train_df = train_df.reset_index(drop=True)
test_df = test_df.reset_index(drop=True)


#train my own fasttext model
def save_fasttext_format(df, text_col, label_col, filepath):
    with open(filepath, 'w', encoding='utf-8') as f:
        for _, row in df.iterrows():
            label = row[label_col].replace(' ', '_')  # no spaces in labels
            line = f"__label__{label} {row[text_col].strip()}\n"
            f.write(line)


save_fasttext_format(train_df, text_col='cleantext', label_col='label', filepath='train.txt')
save_fasttext_format(test_df, text_col='cleantext', label_col='label', filepath='valid.txt')



# Train the model
model = fasttext.train_supervised(input="train.txt",
                                  autotuneValidationFile="valid.txt",
                                autotuneDuration=60) 



#predict on test data
test_texts=test_df['cleantext'].to_list()
ft_label=[]
ft_prob=[]
for t in test_texts:
    label, prob = model.predict(t)
    ft_label.append(label[0].replace('__label__', ''))
    ft_prob.append(prob[0])

  y_true=test_df['label'].to_list()
y_pred=ft_label


## check the accuracy for the test set
# Compute confusion matrix
cm = confusion_matrix(y_true, y_pred, labels=['liberal', 'conservative'])

# Display the matrix
disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels=['liberal', 'conservative'])
disp.plot(cmap='Blues', values_format='d')
plt.title("Confusion Matrix")
plt.show()

acc=accuracy_score(y_true,y_pred)
print(acc)

# Save it
model.save_model("fasttext_ideology_model.ftz")
