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

===========================================================================

Steps:
1. [setup_project.RMD](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/setup_project.Rmd)
- If it's the first time, this will set up the .Rprofile (used at start up of Rstudio)
- info for .Rprofile:
    1. creates file struture used for this project
    2. set directory of cloud storage location
    3. load custom functions into the `search path`
    4. ask to download new files from cloud storage if in .Rprofile: 
    - `copy_files_box(ask = TRUE)` to ask which files to download, all, some or none
    - `copy_files_box(ask = TRUE, auto = TRUE)` to auto download all new files without asking
    - `copy_files_box(ask = FALSE, auto = FALSE)` to not download new file at all

example `.Rprofile`
```
box_dir = "<drive>:/<main-directory>/<sub-directory>/<location-of-folder-with-data>"

# packages used in .Rmd and scripts
.First.sys() # loads base R 

librarian::shelf(
    librarian, ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr,
    forcats, lubridate, glue, fs, magrittr, here,
    # broom, # optional
    
    quiet = TRUE
    )


# source scripts with functions in new environment
my_funcs <- new.env()

# create directory structure if does not exists and create tree map
source(here("scripts","create_dir.R"), local = my_funcs)
# copy files from box
source(here("scripts", "copy_files_from_box.R"), local = my_funcs)
# loading check for taxa
source(here("scripts", "taxa_list_check.R"), local = my_funcs)

attach(my_funcs)
rm(my_funcs)

copy_files_box(ask = TRUE)
```
    
2. [taxa_match_up_WoRMs.Rmd](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/taxa_match_up_WoRMs.Rmd)
- Create master taxa sheet if one doesn't exist in `~/data/metadata/aphia_id`
- Take new files and match taxa, reformat for later use
- Ability to fix non-matched taxa
- Save all reformatted files as individuals and one fully merged
- Save a log of files ran to skip next time
 
3. [combine_zoo_logsheets.Rmd](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/combine_zoo_logsheets.Rmd)
- Rough way of taking cruise metadata and merge all cruises together
- TODO: clean up code, maybe create a function out of it

4. [create_obis_event.Rmd](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/create_obis_event.Rmd)
- TODO: needs updating for final submission
- loads metadata, species data and master taxa sheet
- master taxa sheet adds more taxa information using ` worrms::wm_record()`
- metadata, raw data, and additional taxa informatino is merged into one dataframe
- processing steps
    1. record level
    2. event 
    3. occurence
    4. measurement or fact

===========================================================================
# Custom Functions
[match_taxa_fix.R](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/scripts/match_taxa_fix.R)
- Useful for creating a master taxa sheet with `verbatim names`, and `scientific name`. 
- This takes in a vector of scientific names and will work similar to `obistools::match_taxa`, but this allows the option
for fixing names that have no matches by typing in your own. 

