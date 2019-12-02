"""
## IST 718 BDA Final  Project


# Russian Troll Tweets Analysis
**Identifying Russian Troll Accounts on Twitter**

**Team 1**
* Adil Gokturk
* Drew Howell
* Scott Snow


"""

# %% Imports
import pandas as pd
import numpy as np
import re  
from nltk.corpus import stopwords
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn import preprocessing
from sklearn.model_selection import KFold
from sklearn.naive_bayes import MultinomialNB
from sklearn.naive_bayes import BernoulliNB
from sklearn.metrics import confusion_matrix, accuracy_score, precision_recall_fscore_support
import matplotlib.pyplot as plt
import copy
from wordcloud import WordCloud
from nltk.stem import PorterStemmer
from nltk.tokenize import sent_tokenize, word_tokenize
import random
import os

#Set random seed
random.seed(101)

path = '///Users/HAG/Desktop/Spring2019/IST718BigData/Project/Team I IST718 Final Project'
os.chdir(path)

rtFilePath = 'election-day-tweets/election_day_tweets.csv'
ftFilePath = 'tweets.csv'

realTweetsBackup = pd.read_csv(rtFilePath)
fakeTweetsBackup = pd.read_csv(ftFilePath)

realTweets = realTweetsBackup
realTweets.columns
realTweets.iloc[:10,0]
realTweets = realTweets.iloc[::100, :].reset_index().drop(columns='index')
realTweets = realTweets[realTweets['lang']=='en'].reset_index().drop(columns='index')

fakeTweets = fakeTweetsBackup
#fakeTweets = fakeTweets[fakeTweets['lang']=='en']
#fakeTweets.columns
#fakeTweets['created_str']

#df = realTweets[['text','created_at','lang','user.location']]

from datetime import datetime

## convert epoch miliseconds to datetime format


realTweets = realTweets.iloc[[x for x in range(realTweets.shape[0]) if ~realTweets.loc[:,'created_at'].isna()[x]],:]
realTweets.reset_index(inplace=True, drop=True)
#realTweets["created_at"] = [datetime.datetime.fromtimestamp(realTweets.loc[1]),"created_at"]/1000) \
#          for x in range(realTweets.shape[0])]
#realTweets.reset_index(inplace=True, drop=True)
inDate = realTweets.iloc[:,1]
Date = []
for date in inDate:
    date = datetime.strptime(date, "%Y-%m-%d %H:%M:%S")
    #date = date.strftime("%Y-%m-%d %H:%M:%S")
    Date.append(date)
realTweets['Date'] = Date
realTweets = realTweets.drop(columns='created_at')
realTweets = realTweets[['text','Date','lang','user.location','user.followers_count','user.geo_enabled']]
realTweets['real'] = 1
realTweets.dtypes

#import datetime
## subset for 2 day period before and on election day
fakeTweets.columns
type(fakeTweets['created_str'].loc[0])
fakeTweets = fakeTweets.dropna(subset=['created_str'])
inDate = fakeTweets['created_str']
Date = []
for date in inDate:
    date = datetime.strptime(date, "%Y-%m-%d %H:%M:%S")
    #date = date.strftime("%Y-%m-%d %H:%M:%S")
    Date.append(date)
fakeTweets = fakeTweets.reset_index().drop(columns='index')
fakeTweets['Date'] = Date
fakeTweets = fakeTweets.drop(columns='created_str')
fakeTweets = fakeTweets[['text','Date']]
#fakeTweets['Date']
type(fakeTweets['Date'].loc[1])
fakeTweets['real'] = 0
fakeTweetsElec = fakeTweets.iloc[[x for x in range(fakeTweets.shape[0]) if fakeTweets.loc[x,'Date']\
                                  > datetime(2016,11,7) and fakeTweets.loc[x,'Date'] < \
                                  datetime(2016,11,9)],:]
#fakeTweetsElec['Date']




fakeTweetsElec.columns
realTweets.columns
realTweetsElec = realTweets[['text','Date','real']]

df = pd.concat([realTweetsElec, fakeTweetsElec])



import nltk
from nltk.corpus import stopwords

stop = set(stopwords.words('english')) 
df['noStop'] = df['text'].apply(lambda x: ' '.join([word for word in x.split() if word not in (stop)]))

#df.loc[158,'text'].encode('ascii', 'ignore').decode('ascii')

text=[]
sep = '//'
for line in df['noStop']:
    line = line.encode('ascii', 'ignore').decode('ascii').split(sep, 1)[0]   
    text.append(line)
df = df.drop(columns='noStop')
df['noStop'] = text
#cleaning
# further cleaning to remove useless data
df['noStop'] = df['noStop'].str.replace('I ', ''). \
    str.replace("'m", ''). \
    str.replace("'t", ''). \
    str.replace('.', ''). \
    str.replace(',', ''). \
    str.replace("\\", ''). \
    str.replace("'s", ''). \
    str.replace("'s", ''). \
    str.replace("!", ''). \
    str.replace("--", ' '). \
    str.replace('"', ''). \
    str.replace('?', ''). \
    str.replace(':', ''). \
    str.replace('(', ''). \
    str.replace(')', ''). \
    str.replace('_', ''). \
    str.replace('/', ''). \
    str.replace('-', ''). \
    str.replace(';', ''). \
    str.replace('&', ''). \
    str.replace('$', ''). \
    str.replace('https', ''). \
    str.lower(). \
    str.replace('the', '').\
    str.replace('@', '')

df = df.dropna(how='any',axis=0) 
print(df.head())


df = df[df['noStop'] !='']
df = df[df['noStop'] !=' ']

df = df.sample(frac=1)

import matplotlib.pyplot as plt 
def plot_confusion_matrix(y_true, y_pred, classes,
                          normalize=False,
                          title=None,
                          cmap=plt.cm.Blues):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
    if not title:
        if normalize:
            title = 'Normalized confusion matrix'
        else:
            title = 'Confusion matrix, without normalization'

    # Compute confusion matrix
    cm = confusion_matrix(y_true, y_pred)
    # Only use the labels that appear in the data
    #classes = classes[unique_labels(y_true, y_pred)]
    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]

    fig, ax = plt.subplots()
    im = ax.imshow(cm, interpolation='nearest', cmap=cmap)
    ax.figure.colorbar(im, ax=ax)
    # We want to show all ticks...
    ax.set(xticks=np.arange(cm.shape[1]),
           yticks=np.arange(cm.shape[0]),
           # ... and label them with the respective list entries
           xticklabels=classes, yticklabels=classes,
           title=title,
           ylabel='True label',
           xlabel='Predicted label')

    # Rotate the tick labels and set their alignment.
    plt.setp(ax.get_xticklabels(), rotation=45, ha="right",
             rotation_mode="anchor")

    # Loop over data dimensions and create text annotations.
    fmt = '.2f' if normalize else 'd'
    thresh = cm.max() / 2.
    for i in range(cm.shape[0]):
        for j in range(cm.shape[1]):
            ax.text(j, i, format(cm[i, j], fmt),
                    ha="center", va="center",
                    color="white" if cm[i, j] > thresh else "black")
    fig.tight_layout()
    return ax



#get sentiment label pos and neg based on which is higher
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyzer = SentimentIntensityAnalyzer()

scores=[]
wordCount = []
#AWL = []
for row in df['noStop']:
    score = analyzer.polarity_scores(row)
    scores.append(score)
    words = row.split()
    wordCount.append(len(words))
#    AWL.append(len(row)/len(words))
    
df['SentiScore']=scores
df['negScore'] = [d['neg'] for d in scores]
df['posScore'] = [d['pos'] for d in scores]
df['neuScore'] = [d['neu'] for d in scores]
df['wordCount'] = wordCount

type(df['posScore'].loc[2])
df = df.reset_index().drop(columns='index')
sentList=[]
for i in range(0,len(df['SentiScore'])):
    if df['neuScore'].loc[i] == 1:
        s = 'neu'
    elif df.loc[i,'posScore'] > df['negScore'].loc[i]:
        s = 'pos'
    else:
        s = 'neg'
    sentList.append(s)
df['Sentiment']=sentList
#df['AWL'] = AWL

# keeping labels 
realLab = df["real"]
Date = df['Date']
SentLab = df['Sentiment']
#wordCount
negs = df['negScore']
poss = df['posScore']
neus = df['neuScore']

## Remove the labels
DF_noLabel= df[['noStop']] 

import copy
# function to put labels back on
def add_labels(df, lab = ''):
    tmp = copy.deepcopy(df)
    if lab == "real":
        tmp[lab] = realLab
    elif lab == "Date":
        tmp[lab] = Date
    elif lab == "Sentiment":
        tmp[lab] = SentLab   
    return(tmp)

# list for vectorizer input
content=[]  #empty list
for i in range(0,len(DF_noLabel)):
    word=DF_noLabel.iloc[i,0]
    content.append(word)

# create the stemmed lists (short and long)
from nltk.stem import PorterStemmer
from nltk.tokenize import word_tokenize
stemList = []
stemListLong = []
ps = PorterStemmer()
for lst in content:
    words = word_tokenize(lst)
    stemmed = [ps.stem(w) for w in words]
#    stemmedLong = [w for w in stemmed if len(w) > 5]
    stemmed = ' '.join(stemmed)
#    stemmedLong = ' '.join(stemmedLong)
    stemList.append(stemmed)
#    stemListLong.append(stemmedLong)



from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
# COUNT VECTORIZER
countVec = CountVectorizer(input="content")
vectCountFit = countVec.fit_transform(content)
MyColumnNames = countVec.get_feature_names()
vectCountDF=pd.DataFrame(vectCountFit.toarray(), columns = MyColumnNames)
vectCountDFReal = add_labels(vectCountDF, 'real')
vectCountDFSenti = add_labels(vectCountDF, 'Sentiment')

# COUNT VECTORIZER WITH STEMMED LIST
countVec = CountVectorizer(input="content")
vectCountStemmedFit = countVec.fit_transform(stemList)
StemmedColumnNames=countVec.get_feature_names()
vectCountStemmedDF=pd.DataFrame(vectCountStemmedFit.toarray(), columns = StemmedColumnNames)
vectCountStemmedDFReal = add_labels(vectCountStemmedDF, 'real')
vectCountStemmedDFSenti = add_labels(vectCountStemmedDF, 'Sentiment')

# TFIDF VECTORIZER
tfVec = TfidfVectorizer(input = 'content')
vectTFIDFfit = tfVec.fit_transform(content)
vectTFIDF_DF = pd.DataFrame(vectTFIDFfit.toarray(), columns = MyColumnNames)
vectTFIDF_DFReal = add_labels(vectTFIDF_DF, 'real')
vectTFIDF_DFSenti = add_labels(vectTFIDF_DF, 'Sentiment')



varRMNB = [vectCountDFReal, vectTFIDF_DFReal, vectCountStemmedDFReal]
varRMNBname = ['Count Frequency', 'TFIDF', 'Stemmed Count Frequency']
varSMNB = [vectCountDFSenti, vectCountStemmedDFSenti, vectTFIDF_DFSenti]
varSMNBname = ['Count Frequency', 'TFIDF', 'Stemmed Count Frequency']

# get inices for each target var to join later for equal representation
r1Ind = df[(df["real"] == 1)].index
r0Ind = df[(df["real"] == 0)].index
r0Ind = r0Ind[:len(r1Ind)]
snegInd = df[(df["Sentiment"] == "neg")].index
sposInd = df[(df["Sentiment"] == "pos")].index
sneuInd = df[(df["Sentiment"] == "neu")].index
sposInd = sposInd[:len(snegInd)]
sneuInd = sneuInd[:len(snegInd)]

# empty lists later to be filled with respective indices according to labels
trainIndR   = []
testIndR    = []
trainIndR1  = []
testIndR1   = []
trainIndR0  = []
testIndR0   = []

trainIndS   = []
testIndS    = []
trainIndSNeg  = []
testIndSNeg   = []
trainIndSPos  = []
testIndSPos   = []
trainIndSNeu  = []
testIndSNeu   = []

f10R1 = KFold(n_splits = 10, shuffle = True)
f10R1.get_n_splits(r1Ind)
f10R0 = KFold(n_splits = 10, shuffle = True)
f10R0.get_n_splits(r0Ind)

f10sneg = KFold(n_splits = 10, shuffle = True)
f10sneg.get_n_splits(snegInd)
f10spos = KFold(n_splits = 10, shuffle = True)
f10spos.get_n_splits(sposInd)
f10sneu = KFold(n_splits = 10, shuffle = True)
f10sneu.get_n_splits(sneuInd)


for train_index, test_index in f10R1.split(r1Ind):
    trainIndR1.append(train_index)
    testIndR1.append(test_index)
for train_index, test_index in f10R0.split(r0Ind):
    trainIndR0.append(train_index)
    testIndR0.append(test_index)
for i in range(10):
    trainIndR.append(trainIndR1[i] + trainIndR0[i])
    testIndR.append(testIndR1[i] + testIndR0[i])

for train_index, test_index in f10sneg.split(snegInd):
    trainIndSNeg.append(train_index)
    testIndSNeg.append(test_index)
for train_index, test_index in f10spos.split(sposInd):
    trainIndSPos.append(train_index)
    testIndSPos.append(test_index)
for train_index, test_index in f10sneu.split(sneuInd):
    trainIndSNeu.append(train_index)
    testIndSNeu.append(test_index)
for i in range(10):
    trainIndS.append(trainIndSNeu[i] + trainIndSPos[i] + trainIndSNeu[i])
    testIndS.append(testIndSNeg[i] + testIndSPos[i]+ testIndSNeu[i])


#Define a function to produce a dictionary of features sorted by importance
def feat_imp(train_df, model):
    featLogProb = []
    features = {}
    ind = 0
    for feats in train_df.columns:
        ## the following line takes the difference of the log prob of feature given model
        ## thus it measure the importance of the feature for classification.
        featLogProb.append(abs(model.feature_log_prob_[1,ind] - model.feature_log_prob_[0,ind]))
        features[(feats)] = featLogProb[ind]
        ind = ind + 1
    sortedKeys = sorted(features, key = features.get, reverse = True)[:5]
    sortedVals = sorted(features.values(), reverse = True)[:5]
    features2  = {}
    for ki in range(len(sortedKeys)):
        features2[sortedKeys[ki]] = sortedVals[ki]
    return(features2)


from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import LinearSVC
from sklearn.model_selection import cross_val_score



# MNB REAL  
features_Real = {}

avCMMNBR = []
for i in range(len(varRMNB)):    
    DF = varRMNB[i]
    name = varRMNBname[i]
    ind = 1
    x=0 
    features_Real[name] = []

    for train_ind, test_ind in zip(trainIndR, testIndR):
        train = DF.iloc[train_ind, ]
        test = DF.iloc[test_ind, ]

        trainLabels = train["real"]
        testLabels = test["real"]
        train = train.drop(["real"], axis = 1)
        test = test.drop(["real"], axis = 1)

        MyModelNB= MultinomialNB()
        MyModelNB.fit(train, trainLabels)
        Prediction = MyModelNB.predict(test)
        y_true = (testLabels).tolist()
        y_predict = (Prediction).tolist()
        labels =[0, 1]
        cm = confusion_matrix(y_true, y_predict)
        print(cm)       
        ind += 1
        acc = (cm[0,0]+cm[1,1])/cm.sum()
        acc = accuracy_score(y_true, y_predict)
        x = x + acc
        
        features_Real[name].append(feat_imp(train, MyModelNB))

        #Plot the confusion matrix
        title = str('Confusion Matrix\n' + name + ' fold ' + str(ind))
        cm_plot = plot_confusion_matrix(y_true = y_true, y_pred = y_predict, classes = labels, normalize=False, title=title)
        outpath = str('output/Real/MNB/confmat/' + name + '_fold_' + str(ind) + '.png')
        plt.savefig(outpath, bbox_inches='tight')
        plt.clf()
    avacc = x/10
    avCMMNBR.append(avacc)
                



# LINEAR SVM REAL
avCMLSVCR = []
for loc in range(len(varRMNB)):
    DF = varRMNB[loc]
    name = varRMNBname[loc]
    ind = 1
    x=0
#    features_Sent[name] = []
    for train_ind, test_ind in zip(trainIndR, testIndR):
        train = DF.iloc[train_ind, ]
        test = DF.iloc[test_ind, ]

        trainLabels = train["real"]
        testLabels = test["real"]
        train = train.drop(["real"], axis = 1)
        test = test.drop(["real"], axis = 1)
        
        MyModelNB= LinearSVC()
        MyModelNB.fit(train, trainLabels)
        Prediction = MyModelNB.predict(test)
        y_true = (testLabels).tolist()
        y_predict = (Prediction).tolist()
        labels =[0, 1]
        cm = confusion_matrix(y_true, y_predict)
        print(cm)       
        ind += 1
        acc = (cm[0,0]+cm[1,1])/cm.sum()
        acc = accuracy_score(y_true, y_predict)
        x = x + acc
#        features_Sent[name].append(feat_imp(train, MyModelNB))

        #Plot the confusion matrix
        title = str('Confusion Matrix\n' + name + ' fold ' + str(ind))
        cm_plot = plot_confusion_matrix(y_true = y_true, y_pred = y_predict, classes = labels, normalize=False, title=title)
        outpath = str('output/real/SVC/confmat/' + name + '_fold_' + str(ind) + '.png')
        plt.savefig(outpath, bbox_inches='tight')
        plt.clf()
                
    avacc = x/10
    avCMLSVCR.append(avacc)
    
    

# RANDOM FOREST REAL
#features_Sent={}
avCMRFR = []
for loc in range(len(varRMNB)):
    DF = varRMNB[loc]
    name = varRMNBname[loc]
    ind = 1
    x=0
#    features_Sent[name] = []
    for train_ind, test_ind in zip(trainIndR, testIndR):
        train = DF.iloc[train_ind, ]
        test = DF.iloc[test_ind, ]

        trainLabels = train["real"]
        testLabels = test["real"]
        train = train.drop(["real"], axis = 1)
        test = test.drop(["real"], axis = 1)
        
        MyModelNB=  RandomForestClassifier(n_estimators=250, max_depth=5, random_state=1)
        MyModelNB.fit(train, trainLabels)
        Prediction = MyModelNB.predict(test)
        y_true = (testLabels).tolist()
        y_predict = (Prediction).tolist()
        labels =[0, 1]
        cm = confusion_matrix(y_true, y_predict)
        print(cm)       
        ind += 1
        acc = (cm[0,0]+cm[1,1])/cm.sum()
        acc = accuracy_score(y_true, y_predict)
        x = x + acc
#        features_Sent[name].append(feat_imp(train, MyModelNB))

        #Plot the confusion matrix
        title = str('Confusion Matrix\n' + name + ' fold ' + str(ind))
        cm_plot = plot_confusion_matrix(y_true = y_true, y_pred = y_predict, classes = labels, normalize=False, title=title)
        outpath = str('output/real/RandomForest/confmat/' + name + '_fold_' + str(ind) + '.png')
        plt.savefig(outpath, bbox_inches='tight')
        plt.clf()
                
    avacc = x/10
    avCMRFR.append(avacc)
    
    
# LOGISTIC REGRESSION REAL
    #features_Sent={}
avCMLRR = []
for loc in range(len(varRMNB)):
    DF = varRMNB[loc]
    name = varRMNBname[loc]
    ind = 1
    x=0
#    features_Sent[name] = []
    for train_ind, test_ind in zip(trainIndR, testIndR):
        train = DF.iloc[train_ind, ]
        test = DF.iloc[test_ind, ]

        trainLabels = train["real"]
        testLabels = test["real"]
        train = train.drop(["real"], axis = 1)
        test = test.drop(["real"], axis = 1)
        
        MyModelNB=  LogisticRegression(random_state=0)
        MyModelNB.fit(train, trainLabels)
        Prediction = MyModelNB.predict(test)
        y_true = (testLabels).tolist()
        y_predict = (Prediction).tolist()
        labels =[0, 1]
        cm = confusion_matrix(y_true, y_predict)
        print(cm)       
        ind += 1
        acc = (cm[0,0]+cm[1,1])/cm.sum()
        acc = accuracy_score(y_true, y_predict)
        x = x + acc
#        features_Sent[name].append(feat_imp(train, MyModelNB))

        #Plot the confusion matrix
        title = str('Confusion Matrix\n' + name + ' fold ' + str(ind))
        cm_plot = plot_confusion_matrix(y_true = y_true, y_pred = y_predict, classes = labels, normalize=False, title=title)
        outpath = str('output/real/LogitReg/confmat/' + name + '_fold_' + str(ind) + '.png')
        plt.savefig(outpath, bbox_inches='tight')
        plt.clf()
                
    avacc = x/10
    avCMLRR.append(avacc)







# MNB SENTIMENT
features_Sent = {}

avCMMNBS = []
for loc in range(len(varSMNB)):
    DF = varSMNB[loc]
    name = varSMNBname[loc]
    ind = 1
    x=0
    features_Sent[name] = []
    for train_ind, test_ind in zip(trainIndS, testIndS):
        train = DF.iloc[train_ind, ]
        test = DF.iloc[test_ind, ]

        trainLabels = train["Sentiment"]
        testLabels = test["Sentiment"]
        train = train.drop(["Sentiment"], axis = 1)
        test = test.drop(["Sentiment"], axis = 1)

        MyModelNB= MultinomialNB()
        MyModelNB.fit(train, trainLabels)
        Prediction = MyModelNB.predict(test)
        y_true = (testLabels).tolist()
        y_predict = (Prediction).tolist()
        labels =['neg', 'pos', 'neu']
        cm = confusion_matrix(y_true, y_predict)
        print(cm)       
        ind += 1
        acc = (cm[0,0]+cm[1,1])/cm.sum()
        acc = accuracy_score(y_true, y_predict)
        x = x + acc
        features_Sent[name].append(feat_imp(train, MyModelNB))

        #Plot the confusion matrix
        title = str('Confusion Matrix\n' + name + ' fold ' + str(ind))
        cm_plot = plot_confusion_matrix(y_true = y_true, y_pred = y_predict, classes = labels, normalize=False, title=title)
        outpath = str('output/Sentiment/MNB/confmat/' + name + '_fold_' + str(ind) + '.png')
        plt.savefig(outpath, bbox_inches='tight')
        plt.clf()
                
    avacc = x/10
    avCMMNBS.append(avacc)


# LINEAR SVM SENTIMENT
avCMLSVCS = []
for loc in range(len(varSMNB)):
    DF = varSMNB[loc]
    name = varSMNBname[loc]
    ind = 1
    x=0
#    features_Sent[name] = []
    for train_ind, test_ind in zip(trainIndS, testIndS):
        train = DF.iloc[train_ind, ]
        test = DF.iloc[test_ind, ]

        trainLabels = train["Sentiment"]
        testLabels = test["Sentiment"]
        train = train.drop(["Sentiment"], axis = 1)
        test = test.drop(["Sentiment"], axis = 1)
        
        MyModelNB= LinearSVC()
        MyModelNB.fit(train, trainLabels)
        Prediction = MyModelNB.predict(test)
        y_true = (testLabels).tolist()
        y_predict = (Prediction).tolist()
        labels =['neg', 'pos', 'neu']
        cm = confusion_matrix(y_true, y_predict)
        print(cm)       
        ind += 1
        acc = (cm[0,0]+cm[1,1])/cm.sum()
        acc = accuracy_score(y_true, y_predict)
        x = x + acc
#        features_Sent[name].append(feat_imp(train, MyModelNB))

        #Plot the confusion matrix
        title = str('Confusion Matrix\n' + name + ' fold ' + str(ind))
        cm_plot = plot_confusion_matrix(y_true = y_true, y_pred = y_predict, classes = labels, normalize=False, title=title)
        outpath = str('output/Sentiment/SVC/confmat/' + name + '_fold_' + str(ind) + '.png')
        plt.savefig(outpath, bbox_inches='tight')
        plt.clf()
                
    avacc = x/10
    avCMLSVCS.append(avacc)
    
    

# RANDOM FOREST SENTIMENT
#features_Sent={}
avCMRFS = []
for loc in range(len(varSMNB)):
    DF = varSMNB[loc]
    name = varSMNBname[loc]
    ind = 1
    x=0
#    features_Sent[name] = []
    for train_ind, test_ind in zip(trainIndS, testIndS):
        train = DF.iloc[train_ind, ]
        test = DF.iloc[test_ind, ]

        trainLabels = train["Sentiment"]
        testLabels = test["Sentiment"]
        train = train.drop(["Sentiment"], axis = 1)
        test = test.drop(["Sentiment"], axis = 1)
        
        MyModelNB=  RandomForestClassifier(n_estimators=250, max_depth=5, random_state=1)
        MyModelNB.fit(train, trainLabels)
        Prediction = MyModelNB.predict(test)
        y_true = (testLabels).tolist()
        y_predict = (Prediction).tolist()
        labels =['neg', 'pos', 'neu']
        cm = confusion_matrix(y_true, y_predict)
        print(cm)       
        ind += 1
        acc = (cm[0,0]+cm[1,1])/cm.sum()
        acc = accuracy_score(y_true, y_predict)
        x = x + acc
#        features_Sent[name].append(feat_imp(train, MyModelNB))

        #Plot the confusion matrix
        title = str('Confusion Matrix\n' + name + ' fold ' + str(ind))
        cm_plot = plot_confusion_matrix(y_true = y_true, y_pred = y_predict, classes = labels, normalize=False, title=title)
        outpath = str('output/Sentiment/RandomForest/confmat/' + name + '_fold_' + str(ind) + '.png')
        plt.savefig(outpath, bbox_inches='tight')
        plt.clf()
                
    avacc = x/10
    avCMRFS.append(avacc)
    
    
# LOGISTIC REGRESSION SENTIMENT
    #features_Sent={}
avCMLRS = []
for loc in range(len(varSMNB)):
    DF = varSMNB[loc]
    name = varSMNBname[loc]
    ind = 1
    x=0
#    features_Sent[name] = []
    for train_ind, test_ind in zip(trainIndS, testIndS):
        train = DF.iloc[train_ind, ]
        test = DF.iloc[test_ind, ]

        trainLabels = train["Sentiment"]
        testLabels = test["Sentiment"]
        train = train.drop(["Sentiment"], axis = 1)
        test = test.drop(["Sentiment"], axis = 1)
        
        MyModelNB=  LogisticRegression(random_state=0)
        MyModelNB.fit(train, trainLabels)
        Prediction = MyModelNB.predict(test)
        y_true = (testLabels).tolist()
        y_predict = (Prediction).tolist()
        labels =['neg', 'pos', 'neu']
        cm = confusion_matrix(y_true, y_predict)
        print(cm)       
        ind += 1
        acc = (cm[0,0]+cm[1,1])/cm.sum()
        acc = accuracy_score(y_true, y_predict)
        x = x + acc
#        features_Sent[name].append(feat_imp(train, MyModelNB))

        #Plot the confusion matrix
        title = str('Confusion Matrix\n' + name + ' fold ' + str(ind))
        cm_plot = plot_confusion_matrix(y_true = y_true, y_pred = y_predict, classes = labels, normalize=False, title=title)
        outpath = str('output/Sentiment/LogitReg/confmat/' + name + '_fold_' + str(ind) + '.png')
        plt.savefig(outpath, bbox_inches='tight')
        plt.clf()
                
    avacc = x/10
    avCMLRS.append(avacc)
    

#d1 = {'Bernoulli Sentiment':avCMBS, 'Bernoulli Lie':avCMBL}
#d2 = {'MNB Sentiment':avCMMNBS, 'MNB Lie':avCMMNBL}
#
#MNB = pd.DataFrame(d1)
#MNB['VecType'] = varLBname
# 
#SVM = pd.DataFrame(d2)
#SVM['VecType'] = varLMNBname
#
#SVM = pd.DataFrame(d2)
#SVM['VecType'] = varLMNBname
#
#SVM = pd.DataFrame(d2)
#SVM['VecType'] = varLMNBname


#Read in both datasets
#Subset both to election day
#drop columns that aren't shared
#join
#Clean
#EDA
#Modeling (SVM, Neural Net, Random Forest, Keras)
#Get results
#Make slideshow    