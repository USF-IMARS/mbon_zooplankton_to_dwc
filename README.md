# MBON South Florida Zooplankton
This repository is used to setup a pipeline to convert zooplankton data collected in South Florida into DarwinCore to be added to OBIS.

# 3 Main tasks
1. Ingest cruise metadata
    - Merge all cruise data into one .csv
    - Correct errors along the way
    - Attempt to document changes made to raw data
  
2. Ingest zooplankton counts
    - Pull new files from Box where raw count data is stored
    - Add species information from WoRMS using a customized taxa_search (base from `robis`)
    - Check for new species names and add to a master list of species names
      - This list contains the "verbatim name", "species name" to search, "larval stage" if exists, and matching Aphia IDs from WoRMS
    - Save all new raw files into one file with a timestamp
    - TODO: append new files to previous merged data
  
3. Convert to DarwinCore format 
    - This loads:
      - merged `metadata`
      - merged `abudance data`
      - master `taxa lists` to get phylogentic tree
    - Creates 3 csv files of `event`, `occurence`, and `Measurement or Fact`
