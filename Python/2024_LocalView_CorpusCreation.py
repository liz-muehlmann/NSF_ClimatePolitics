# this file extracts the caption_text_clean column from the parquet files and 
# saves them to a .txt file for use in the 2024_LocalView_TextAnalysis.py
# file as a corpus. 

# it also pre-processes the text by removing HTML, expanding contractions, 
# removes accented characters, converts the text to lower case, lemmatizes words,
# removes special characters, removes stop words, and removes numerical digits

# load packages
import os                                           # file operations      
import glob                                         # pathname operations
import pyarrow.parquet as pq                        # work with parquet format
import numpy as np                                  # math and arrays  
import pandas as pd                                 # data analysis and manipulation
import re                                           # regular expressions               
import logging                                      # error logging
import nltk                                         # natural language processing
import contractions
from contractions import CONTRACTION_MAP            # import contractions 

# set working directory
os.chdir('f:/GitHub/NSF_Climate_LocalPolitics/')

# set path for saving files
path = r'F:/GitHub/NSF_Climate_LocalPolitics/Modified_Data/Political/Local View/2023_LocalView_Corpus/'
drc = r'F:/GitHub/NSF_Climate_LocalPolitics/Original_Data/Political/2023_LocalViewData'

# basic configuration for logging errors
logging.basicConfig(filename= path + f'LOG.log', 
                    format='[%(asctime)s %(file)s:%(lineno)-15s %(levelname)s %(name)s ] %(message)s',
                    level=logging.DEBUG, 
                    filemode='w')

# suppress .loc warning for pandoc
pd.options.mode.chained_assignment = None  # default='warn'

# expand contractions 
def expand_contractions(text, contraction_mapping=CONTRACTION_MAP):
    contractions_pattern = re.compile('({})'.format('|'.join(contraction_mapping.keys())), flags=re.IGNORECASE|re.DOTALL)
    def expand_match(contraction):
        match = contraction.group(0)
        first_char = match[0]
        expanded_contraction = contraction_mapping.get(match)\
                                if contraction_mapping.get(match)\
                                else contraction_mapping.get(match.lower())
        expanded_contraction = first_char+expanded_contraction[1:]
        return expanded_contraction
    expanded_text = contractions_pattern.sub(expand_match, text)
    expanded_text = re.sub("'", "", expanded_text)


# set row counter to zero
n=0

# loop through each parquet file and print the contents of caption_text_clean
# to its own file
for fname in os.listdir(drc):
    f = os.path.join(drc, fname)                                                                        # link path and filename
    if not f.endswith('parquet'):                                                                       # skip non parquet files
        continue
    df = pd.read_parquet(f)                                                                             # open parquet file
    df_trim = df[~df['caption_text_clean'].str.contains('<No caption available')]                       # remove rows without captions
    pattern = r'[^ a-zA-Z0-9,.\-\']+'                                                                       # define special characters
    df_trim['caption_text_clean'] = df_trim['caption_text_clean'].replace(pattern, '', regex=True)      # remove special characters
    try:
        for index, row in df_trim.iterrows():                                                           # loop through rows
            n=n+1                                                                                       # increase row counter        
            date = row['meeting_date']                                                                  # extract meeting date    
            state = row['state_name']                                                                   # extract state name    
            place = row['place_name']                                                                   # extract place name    
            vid_id = row['vid_id']                                                                      # extract video id            
            filename =  path + f'{date}_{state}_{place}_{vid_id}.txt'                                   # define filename structure
            # uncomment 54-57 to add 1, 2, + at the end of the filename to prevent overwriting.    
            # i = 0                                                                                     # set filename counter            
            # while os.path.isfile(f'{filename}_{i}'):                                                  # while the filename exists    
            #     i += 1                                                                                # increment
            
            # uncomment 60-63 to create a logfile with any transcript with less than 50 words
            # logfile = path + 'missing_files.txt'                                                      # define error filename
            # if len(row['caption_text_clean']) < 50:                                                   # if the caption is less than 50 words
            #     with open(logfile, "a+") as f:                                                        # save its information to the logfile
            #         f.write(f'Danger, Will Robinson! \n {date} \n {vid_id} \n')                       # with the date and video id
            # else:                                                                                     # otherwise
            #     continue                                                                              # move on

            # write caption_text_clean to file
            with open(filename, "w") as f:                                                            # open file
              f.write(row['caption_text_clean'])                                                      # write caption to file and save    
    except UnicodeEncodeError as err:                                                                   # if there's a UnicodeEncodeError
        logger = logging.getLogger(__name__)                                                            # log it as an error
        logging.warning('this was logged to a file')                                                    # warn user
    print(n)                                                                                            # print final row count
    

