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

import requests
from bs4 import BeautifulSoup
import re

#   ____________________________________________________________________________
#   preliminaries                                                           ####

save_loc = "./LocalView/data/original/noaa/"
url = "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
regex = r"\_[d]([0-9]{4})\_"

reqs = requests.get(url)
soup = BeautifulSoup(reqs.text, 'html.parser')

urls = []
for link in soup.find_all('a'):
    r = requests.get(url + str(page))
    source = r.content
    page_source = html.fromstring(source)
    urls.extend(page_source.xpath(""))
    link.get('href')

#   ____________________________________________________________________________
#   define functions                                                        ####





def get_storm_links():
    r = requests.get(url)
    soup = BeautifulSoup(r.content, "html.parser")
    links = soup.findAll('a')
    storm_links = [url + link['href'] for link in links if link['href'].endswith('.csv.gz')]
    return storm_links

def download_files():
    for link in storm_links:
        file_name = link.split("/")[-1]
        print("Downloading file:%s"%file_name)

r = requests.get(link, stream = TRUE)

with open(file_name, 'wb') as f:
    for chunk in r.iter_content(chunk_size = 1024*1024):
        if chunk:
            f.write(chunk)
print( "%s downloaded!\n"%file_name)
    print("All files downloaded!")
    return

if ___name___ == "___main___":
    storm_links = get_storm_links()
    download_files(storm_links)

