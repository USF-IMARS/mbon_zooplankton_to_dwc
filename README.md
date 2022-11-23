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

================================================================================

Steps:
1. [setup_project.RMD](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/setup_project.Rmd)
    - If it's the first time. This will setup file struture and allow to set directory to cloud storage files
    
2. [taxa_match_up_WoRMs.Rmd](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/taxa_match_up_WoRMs.Rmd)
    - Create master taxa sheet if one doesn't exist in `~/data/metadata/aphia_id`
    - Take new files and match taxa, reformat for later use
    - Ability to fix non-matched taxa
    - Save all reformatted files as individuals and one fully merged
    - Save a log of files ran to skip next time
    - 
3. [combine_zoo_logsheets.Rmd](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/combine_zoo_logsheets.Rmd)

4. [create_obis_event.Rmd](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/create_obis_event.Rmd)

================================================================================
# Functions
# match_taxa_fix.R
Particularly useful for starting conversion process. This takes in a vector of scientific names and will work similar to `obistools::match_taxa`, but this allows the option
for fixing names that have no matches by typing in your own. 

