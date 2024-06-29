# NSF climate change and local politics <!-- omit in toc -->
Shared github for NSF DRMS Climate and Local Political Participation Project. 

## Table of Contents <!-- omit in toc -->
- [Research Team](#research-team)
- [Contents](#contents)
  - [Code](#code)
    - [Cartography](#cartography)
      - [majorityCounties.r](#majoritycountiesr)
      - [TXPopChoropleth.r](#txpopchoroplethr)
    - [FEMA](#fema)
    - [IRS](#irs)
    - [LocalView](#localview)
    - [Political](#political)
  - [Data](#data)
  - [GIS](#gis)
  - [Results](#results)
    - [IRS](#irs-1)
    - [LocalView](#localview-1)
    - [Samples](#samples)

____

## Research Team
[Sara Constantino](https://cssh.northeastern.edu/faculty/sara-constantino/),  Northeastern University  
[Alicia Cooperman](https://www.aliciacooperman.com/), George Washington University  
[Manuela Muñoz](https://bush.tamu.edu/pols/degrees/phd/mmunoz/), Texas A&M  
[Kyle A. Trojahn](https://kyletrojahn.com/), University of Texas - Austin  
[Allison Donine Deese](https://cssh.northeastern.edu/student/allison-donine/), Northeastern University  
[Liz Muehlmann](https://liz-muehlmann.github.io/), University of California - Irvine
____
## Contents
### [Code](/Code/)  
The code folder includes the code necessary to process and analyze the datasets used regarding climate change and political meetings.
   #### [Cartography](/Code/Cartography/)
   The cartography folder includes code specifically related to cartographic processing. 
   ##### majorityCounties.r
   This file uses boundary intersection statistics from ArcGIS to aggregate places to the county level. 
   | type   | name                      | source     | description                                                        |
   | ------ | ------------------------- | ---------- | ------------------------------------------------------------------ |
   | Input  | 2020_IPIntersections      | ArcGIS     | Percent overlap incorporated places / county                       |
   | Input  | 2020_CDPIntersections     | ArcGIS     | Percent overlap census designated places / county                  |
   | Input  | LVPlaceOnly               | Local View | Places present in Local View data                                  |
   | Input  | 2020_PlaceBoundaryChanges | US Census  | County boundary changes between 2010 & 2020                        |
   | Input  | 2020_VACountyEquivalents  | US Census  | List of county equivalents in Virginia                             |
   | Input  | Counties                  | Tigris     | County boundaries                                                  |
   | Output | 2020_AllPlaces            |            | Place data aggregated to the county level based on percent overlap |
##### TXPopChoropleth.r
   This file creates a choropleth of county population for the US and for Texas for 2021

| type   | name   | source     | description                 |
| ------ | ------ | ---------- | --------------------------- |
| Input  | county | tigris     | County boundaries           |
| Input  | pop    | tidycensus | Population estimates        |
| Output | US map |            | US population choropleth    |
| Output | TX map |            | Texas population choropleth |

   #### [FEMA](/Code/FEMA/)
   #### [IRS](/Code/IRS/)
   #### [LocalView](/Code/LocalView/)
   #### [Political](/code/Political/)
### Data 
(not tracked due to Github's file limits)
### GIS 
(not tracked due to Github's file limits)
### [Results](/Results/)
   #### [IRS](/Results/IRS/)
   #### [LocalView](/Results/LocalView/)
   #### [Samples](/Results/Samples/)


