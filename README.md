# MBON South Florida Zooplankton
This repository is used to setup a pipeline to convert zooplankton data collected in South Florida into DarwinCore to be added to OBIS.

Link to data collection method: [Zooplankton Methodology, Collection & Identification - a field manual](https://drs.nio.org/drs/handle/2264/95)

# 3 Main tasks
### ***1. Ingest zooplankton counts***
- Pull new files from cloud service (i.e. Box) where raw count data is stored
   - You may ignore using a cloud directory if set `.choose = NA` in `rprofile_setup()` in `00_setup_project.Rmd`
- Add species information from WoRMS using a custom `match_taxa()` function (based from `obistools::match_taxa()`)
  - If not starting from scratch, you can set the location and base name for one using 
```
file_expr(
          # Location from the .Rproject direcotry, the functino here::here() will find the root relative to the .Rproj file
          loc = here::here("<child dir from .Rproj root>", "<child 2>"), 
          # The master taxonomic file base name, i.e. `aphia_taxa_20230101_120000` will look for base name `aphia_taxa`
          file_base = "<regex base i.e. aphia_taxa>"
)
``` 
- Check for new species names and add to a master list of species names
  - This list contains the "verbatim name", "species name" to search, "larval stage" if exists, and matching Aphia IDs from WoRMS
- Save all new raw files into one file with a timestamp
- Append new files to previous merged data
   
### ***2. Ingest cruise metadata***
- Merge all cruise data into one .csv
- Correct errors along the way
- Attempt to document changes made to raw data
  
### ***3. Convert to DarwinCore format***
- This loads:
  - merged `metadata`
  - merged `abudance data`
  - master `taxa lists` to get phylogentic tree
- Creates 3 csv files of `event`, `occurence`, and `Measurement or Fact`


## Steps:
### 1. [setup_project.RMD](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/00_setup_project.Rmd)
- If it's the first time, this will set up the .Rprofile (used at start up of Rstudio)
- info for .Rprofile:
    1. creates file structure used for this project
    2. set directory of cloud storage location (options for `.choose` in `rprofile_setup()`)
    - `TRUE`: opens file explorer to choose directory
    - `FALSE`: in .Rprofile sets `cloud_dir = "EDIT HERE"` to edit file directory manually in line 1 
    - `NA`: ignore using a cloud directory, used if you copied it directky
    - `"\<drive>:/\<path>"`: set manually (i.e `here::here("\<path>")` or `"\<drive>:/\<path>/"`)
    3. load custom functions into the `search path`
    4. ask to download new files from cloud storage if in .Rprofile: 
    - `copy_files_cloud(ask = TRUE)` to ask which files to download, all, some or none
    - `copy_files_cloud(ask = TRUE, auto = TRUE)` to auto download all new files without asking
    - `copy_files_cloud(ask = FALSE, auto = FALSE)` to not download new file at all
    - [Example .Rprofile](https://github.com/sebastiandig/obis_zooplankton_setup#example-rprofile)

    
### 2. [taxa_match_up_WoRMs.Rmd](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/01_taxa_match_up_WoRMs.Rmd)
- Create master taxa sheet if one doesn't exist in `~/data/metadata/aphia_id`
- Take new files and match taxa, reformat for later use
- Ability to fix non-matched taxa
- Save all reformatted files as individuals and one fully merged
- Save a log of files ran to skip next time
 
### 3. [combine_zoo_logsheets.Rmd](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/02_combine_zoo_logsheets.Rmd)
- Rough way of taking cruise metadata and merge all cruises together
- TODO: clean up code, maybe create a function out of it

### 4. [create_obis_event.Rmd](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/Rmd/03_create_obis_event.Rmd)
- TODO: needs updating for final submission
- loads metadata, species data and master taxa sheet
- master taxa sheet adds more taxa information using ` worrms::wm_record()`
- metadata, raw data, and additional taxa informatino is merged into one dataframe
- processing steps:
    1. record level
    2. event 
    3. occurence
    4. measurement or fact


## Custom Functions
### [taxa_list_check](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/scripts/taxa_list_check.R)
- Load all the data from [zooplankton counts format](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/cruise_id_stn_mesh_blank_count.xlsx).
  - This is used for all cruises, stations and mesh size. 
- Useful when extracting taxonomic names from a dataset and creating a master sheet to merge with all data. 
- This will create/search for a master sheet.
- Check for unmatched taxa.
- Pull/Push a master sheet from a cloud directory if using.
- Save merged data with master taxa sheet in an `all merged` file and individual files per cruise, station and mesh.
- You wll be able to set the file expression for the location and base file name for the master sheet.

### [match_taxa_fix.R](https://github.com/sebastiandig/obis_zooplankton_setup/blob/main/scripts/match_taxa_fix.R)
- Useful for creating a master taxa sheet with `verbatim names`, and `scientific name`. 
- This takes in a vector of scientific names and will work similar to `obistools::match_taxa`, but this allows the option
for fixing names that have no matches by typing in your own. 


# Example `.Rprofile`
```
cloud_dir = "<drive>:/<main-directory>/<sub-directory>/<location-of-folder-with-data>"

# packages used in .Rmd and scripts
# base R
.First.sys()

# external packages
librarian::shelf(
    librarian, ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr,
    forcats, lubridate, glue, fs, magrittr, here,
    # broom, # optional
    
    quiet = TRUE
    )

library("conflicted")

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# source scripts with functions in new environment
source(here("scripts", "attach_funcs.R"))
func_attach()
rm(func_attach)

# Copy files from cloud server
if (!exists("cloud_dir")) {
	rlang::abort(c("x" = "`cloud_dir` doesn't exist.",
                   "Please run `rprofile_setup()` to add."))
        }

if (cloud_dir == "EDIT HERE" & !is.na(cloud_dir)) {
	rlang::abort(c("x" = "`cloud_dir` = EDIT HERE",
                   "Please edit this in .Rprofile or", 
                   "Run `rprofile_setup(.choose = TRUE)` to add."))
	}

if (!is.na(cloud_dir)) {
	cloud_dir_raw <- here(cloud_dir, "raw")
	} else {cloud_dir_raw <- cloud_dir}

copy_files_cloud(.cloud_dir = cloud_dir_raw, ask = TRUE)
```
