
# load packages
import os
import numpy as np 
import pandas as pd
import nltk
import spacy
from nltk.corpus.reader import PlaintextCorpusReader
from nltk.corpus import stopwords
from collections import Counter
from nltk.stem import WordNetLemmatizer
nlp = spacy.load("en_core_web_sm")
import en_core_web_sm
nlp = en_core_web_sm.load()


# set working directory
os.chdir('F:/GitHub/NSF_Climate_LocalPolitics/')

# set corpus root
corpus_root = './Modified_Data/Political/Local View/2023_LocalView_Corpus/'
# corpus = PlaintextCorpusReader(corpus_root, '.*')

# load sample file
with open(corpus_root + '2009-06-01_Illinois_Elmhurst city_Ts6_i7ma5Sk.txt', 'r') as tf:
    text = tf.read()

# tokenize the text into its individual words
words = text.split()

# load text into spacy
text = nlp(text)

# create lemmatization function
def lemmatize_text(text):
    text = nlp(text)
    text = ' '.join([word.lemma_ if word.lemma_ != '-PRON' else word.text for word in text])
    return text



# states that a sentence can have
climate = 'climate'
disaster = 'disaster'
unknown = 'unknown'
both = 'both'

# define words
hazard_words = set(['climate change', 
                    'drainage', 
                    'storm water', 
                    'stormwater', 
                    'insufficient services', 
                    'sea level rise',
                    'erosion',
                    'flooding',
                    'wind',
                    'wildfire',
                    'fire',
                    'tornado',
                    'relief',
                    'rebuild',
                    'relocation'])


climate_words = set(['climate', 'climate change', 'weather', 'global warming', 'climate crisis', 'climate emergency', 'global heating', 'greenhouse gasses', 'temperature change', 'changing climate', 'anthropogenic','ozone', 'ozone layer'])

disaster_words = set(['disaster', 'state of emergency', 'emergency', 'disaster declaration', 'declare disaster', 'hurricane', 'wildfire', 'tornado', 'earthquake','landslide', 'tsunami', 'sinkhole', 'heat wave', 'flood', 'lightning', 'thunderstorm', 'avalanche', 'volcano', 'blizzard', 'snow', 'ice', 'freeze'])

# define function to sort sentences into one of four states (above)
def nd(words):                                          # define naturaldisaster(words)
    cwlen = len(climate_words.intersection(words))      # climate word length
    dwlen = len(disaster_words.intersection(words))     # disaster word length
    
    if cwlen > 0 and dwlen == 0:        # if there's climate words, but no disaster words 
        return climate                  # mark the sentence climate
    elif cwlen == 0 and dwlen >0:       # if there's no climate words, but there's disaster words
        return disaster                 # mark the sentence disaster
    elif cwlen >0 and dwlen >0:         # if there's climate and disaster words
        return both                     # mark the sentence both
    else:                               #otherwise
        return unknown                  # mark the sentence unknown
    
# set up counter to count the number of climate or disaster related words
def count_nd(sentences):                # define the counter for natural disaster words in each sentence
    
    sents = Counter()                   # set up sentence counter
    words = Counter()                   # set up word counter
    
    for sentence in sentences:          # for each sentences
        nd = nd(sentence)               # check if it includes a natural disaster word
        sents[nd] += 1                  # increment sentences
        words[nd] += len(sentence)      # all the words in the sentence will be considered as belonging to that natural disaster
        
        return sents, words             # return the value for sents and words
    
def parse_nd(text):                                                     # parse 
    sentences = [
        [word.lower() for word in nltk.word_tokenize(sentence)]
        for sentence in nltk.sent_tokenize(text)]
    sents, words = count_nd(sentences)
    total = sum(words.values())
        
    for gender, count in words.items():
        pcent = count/total*100
        nsents = sents[nd]
            
        print(
            "{0.3}% {} ({} sentences).format(pcent, nd, nsents)"
)    

parse_nd(corpus)