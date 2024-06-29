
# load packages
import os
import numpy as np 
import pandas as pd
import re
import nltk
from nltk.corpus.reader import PlaintextCorpusReader
from nltk.corpus import words
from nltk.corpus import stopwords
from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.stem import WordNetLemmatizer
from nltk.util import ngrams
from nltk import FreqDist
from nltk.text import Text
from nltk import concordance
nltk.download('stopwords')
nltk.download('wordnet')
import collections
from collections import Counter
import contractions

# set working directory ########################################################
os.chdir('F:/GitHub/NSF_Climate_LocalPolitics/')

# set corpus root ##############################################################
corpus_root = './Modified_Data/Political/Local View/2023_LocalView_Corpus/'

# create corpus by year
corpus_year = PlaintextCorpusReader(corpus_root + '/2006/', '.*')

# convert to pandas ############################################################
raw = []
for fileid in corpus_year.fileids():
    file = fileid.split('/')
    raw.append((file, corpus_year.raw(fileid)))

df = pd.DataFrame(raw, columns = ['file','text'])

# clean data ###################################################################
df['text'] = df['text'].str.lower()                                         # convert to lowercase
filter_char = lambda c: ord(c) < 256                                        # define special characters
df['text'] = df['text'].apply(lambda x: ''.join(filter(filter_char, x)))    # remove special characters
df['text'] = df['text'].apply(lambda x: contractions.fix(x))                # expand contractions

# remove stop words ############################################################
stop_words = set(stopwords.words('english'))
custom_stops = ['pause', 'music,', 'um', 'uh', 'okay', 'really', 'hi', 'hello', 'thanks', 'thank you', 'oh', 'please', 'mr', 'mrs', 'dr', 'sir', 'just', 'thank', 'like', 'alright', 'welcome', 'good','fellas']
df['text'] = df['text'].apply(lambda x: ' '.join([word for word in x.split() if word not in (stop_words)]))
df['text'] = df['text'].apply(lambda x: ' '.join([word for word in x.split() if word not in (custom_stops)]))



# lemmatization ################################################################
lemma =  WordNetLemmatizer()
def lemmatize(tokens): # lemmatize verbs
    lemmatized_tokens = [lemma.lemmatize(token, pos= 'v') for token in tokens]

lemmatize(df['text'])

# tokenization #################################################################
df['tokens'] = df['text'].apply(lambda x: word_tokenize(x))

# count word occurrence ########################################################
counts = collections.Counter()
for sent in df['text']:
    words = nltk.word_tokenize(sent)
    counts.update(nltk.bigrams(words))

counts.most_common(10)

single_woi = ['drainage',
            'erosion',
            'flooding',
            'wind',
            'wildfire',
            'fire',
            'tornado',
            'relief',
            'rebuild',
            'relocation',
            'hurricane',
            'relief',
            'recovery',
            'emergency',
            'mitigation',
            'relocation',
            'buyout',
            'adaptation',
            'insurance',
            'risk',
            'prepare',
            'preparedness',
            'resilience',
            'resilient',
            'HOA',
            'NGO',
            'CBO',
            'organizing',
            'activism',
            'federal',
            'state',
            'county',
            'city',
            'incorporation',
            'incorporated',
            'unincorporated',
            'polycentric',
            'jurisdiction',
            'annexation',
            'deannexation',
            'funding',
            'subsidies',
            'grant',
            'grants',
            'zoning',
            'regulation',
            'regulations',
            'water',
            'sanitation',
            'parks',
            'trees',
            'streets',
            'food',
            'shelter',
            'electric',
            'property',
            'shelters',
            'airbnb',
            'unhoused',
            'homeless',
            'meeting',
            'voting',
            'leadership',
            'activism',
            'protest',
            'turnout',
            'partisanship',
            'polarization',
            'Facebook',
            'NextDoor',
            'Next Door',
            'Twitter',
            'TikTok',
            'BlueSky',
            'whatsapp',
            'discord',
            'texting',
            'slack',
            'texted',
            'television',
            'TV',
            'newsletter',
            'newsletters',
            'news',
            'trust',
            'community',
            'inequality',
            'inequalities',
            'vulnerable',
            'vulnerability',
            'demographics',
            'education',
            'technology',
            'language',
            'class',
            'income',
            'poverty',
            'ethnicity',
            'ethnic',
            'race',
            'immigration',
            'documentation',
            'dependents',
            'LGTBQIA+',
            'LGTBQIA',
            'gay',
            'lesbian',
            'LGBT',
            'homeowner',
            'renter',
            'disability',
            'disabled',
            'economy',
            'money',
            'nasa',
            'tourism',
            'stormwater',
            'infrastructure',
            'resaca',
            'tidal',
            'surge',
            'displacement',
            'replenishment',
            'resacas',
            'dunes',
            'levees',
            'dikes',
            'mangroves',
            'reefs',
            'airbandb']


