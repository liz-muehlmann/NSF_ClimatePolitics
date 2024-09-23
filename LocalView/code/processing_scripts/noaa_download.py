### file description ###########################################################
##                                                                            ##
## This file downloads National Oceans and Atmospheric Administration's Storm ##
##      data via FTP.                                                         ##
##                                                                            ##
##                                                                            ##
##      Data included:                                                        ##
##          NOAA Storm Events 2004-2024                                       ##
##              https://www.ncdc.noaa.gov/stormevents/ftp.jsp                 ##
##              ./processing_scripts/noaa.r                                   ##
##                                                                            ##
## Output:                                                                    ##
##      /LocalView/data/original_data/noaa
##                                                                            ##
################################################################################


#   ____________________________________________________________________________
#   load libraries

from bs4 import BeautifulSoup
from pprint import pprint
from urllib.parse import urlparse
import requests
import re
import os

#   ____________________________________________________________________________
#   preliminaries                                                           ####

save_loc = "/GitHub/NSF_ClimatePolitics/LocalView/data/original/noaa/"
url = "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
pattern = "_d(\d{4})_"
start = 2004
end = 2024

#   ____________________________________________________________________________
#   define functions                                                        ####

##  ............................................................................
##  filter links function   

def get_noaa_links():
    r = requests.get(url)
    soup = BeautifulSoup(r.content, "html.parser")
    all_links = soup.findAll('a')
    noaa_links = [url + link['href'] for link in all_links if link['href'].endswith('.csv.gz')]
    filtered_links = []
    for link in noaa_links:
        match = re.search(pattern, link)
        if match:
            year = int(match.group(1))
            if start <= year <= end:
                filtered_links.append(link)
    return filtered_links

# create list of links
noaa_links = get_noaa_links()


##  ............................................................................
##  download links                                                          ####

def download_files(save_dir):
    os.makedirs(save_dir, exist_ok = True)
    for link in noaa_links:
        file_name = link.split("/")[-1]
        file_path = os.path.join(save_dir, file_name)
        
        print("Downloading file:%s"%file_name)
        r = requests.get(link, stream = True)
        
        with open(file_name, 'wb') as f:
            for chunk in r.iter_content(chunk_size = 1024*1024):
                if chunk:
                    f.write(chunk)
                    
        print("%s downloaded!\n"%file_name)
        
    print("All videos downloaded!")
    return

download_files(save_loc)



