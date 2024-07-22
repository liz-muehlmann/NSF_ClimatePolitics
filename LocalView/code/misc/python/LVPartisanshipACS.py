################################################################################
##                                                                            ##
## This file merges the local view data with ACS and election data for all    ##
##      years. It also subsets the data for mentions of climate change and    ##
##      saves to parquet format for use in Code/LocalView/LVTextAnalysis      ##  
##      Code/LocalView/LVTextAnalysis/TAClimateChange.r                       ##
##                                                                            ##    
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##    
##  Political data was downloaded from the Harvard Dataverse for Algara &     ##
##    Sharif (2021) "Replication Data for: Partisanship & Nationalization     ##
##    in American Elections: Evidence from Presidential, Senatorial, &        ##
##    Gubernatorial Elections in the U.S. Counties, 1872-2020",               ##
##      https://doi.org/10.7910/DVN/DGUMFI                                    ##
##                                                                            ##
##  ACS data was downloaded using the Tidy Census package                     ##
##      2006-2010 ACS > 2008                                                  ##
##      2011-2015 ACS > 2012 Election                                         ##
##      2016-2020 ACS > 2016 & 2020 Elections                                 ##
##                                                                            ##
## county names are downloaded using the Tigris package                       ##
##                                                                            ##
## Election + ACS + County data were merged in                                ##        
##      Code/Political/AlgaraACSPlaces.r                                      ##
##                                                                            ##
## ** Majority counties were determined using ArcGIS' Tabulate Intersection   ##
##    function and processed using Google Docs.                               ##  
##    Methodology is available upon request.                                  ##  
##                                                                            ##
################################################################################

import os
import nltk