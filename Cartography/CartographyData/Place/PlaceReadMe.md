# Place Read Me
This file is an overview of the processing steps taken to determine the overlap between Census Designated and Incorporated Places and various counties across the United States. For a detailed description of how to complete each step see: [PlaceProcessing.md](/Cartography/CartographyProcessing/PlaceProcessing.md)

1. [2020_CDPBoundaryIntersection](/Cartography/CartographyData/Place/2020_CDPBoundaryIntersection.csv)  & [2020_IncPlaceBoundaryIntersection](/Cartography/CartographyData/Place/2020_IncPlaceBoundaryIntersection.csv)  
    * Table is exported from ArcGIS after using the Tabulate Intersection function. It includes the Census Designated Place (CDP), Incorporated Place (IP) and County FIPS, Name, and Object IDs. The area column calculates the area overlap between the CDP/IP and county. The percentage column is the percentage of the CDP/IP that overlaps with each county.
    * The file was then imported into Google Sheets. 
        * US Territories were deleted (GEOID for counties - renamed to full_fips <57 were kept).
        * CDP/IPs that fall 100% within a single county were extracted to a new sheet (CDP/IP 100% Coverage).
        * For CDP/IPs that fall across multiple statements, if statements were used to compare county overlap. The majority county was selected (CDP/IP < 100% Coverage)
        * Manually verified overlap majority for CDP/IPs that overlap more than two counties. 
        * All CDPs/IPs with greater than 51% coverage in one county were extracted to a new sheet (CDP/IP Majority Counties)
    * FIPS for CDP/IPs that were evenly split among multiple counties (between 49-51%) were used to filter the ArcGIS map for only the CDP/IPs in question.
        * Visually inspected the CDP/IPs to determine if the overlapping counties leaned democratic or republican in the 2020 election (based on voting percentage). 
        * CDP/IPs that overlap counties that have the same partisanship (VP) were extracted to a new sheet (CDP/IP No VP Split).
        * CDP/IPs that had differing partisanship (VP) were extracted to a new sheet (CDP/IP Split). 
            * Extracted the FIPS for these CDPs and checked if they were included in the Local View data. None of the CDPs with a VP split have accompanying Local View data so the county with the majority coverage was selected. Only one IP with a VP split is included in the Local View Data (Carrollton City, Texas, FIPS 48-13024). For all other IPs, the county with the majority coverage was selected.
    * CDP/IPs and their majority counties were merged into one sheet (IP & CDP)
