##%######################################################%##
#                                                          #
####     Create Measurement or Fact .csv for later      ####
#                                                          #
##%######################################################%##
# ---- DESCRIPTION: ------
# This is the basis for the creation the MoF file to be used
#
# ---- INPUTS: -----------
# NA  
#
# ---- OUTPUTS: ----------
# mof_base = Tibble of Measurments or Facts
#
# ---- NOTES: ------------
#
# ---- REFERENCES(s): ----
#
# ---- AUTHOR(s): --------
# Sebastian Di Geronimo (2023-01-12 17:03:19)

mof_read <- function() {
    #' Create Measurement or Fact .csv for later
    #'
    #' This is the basis for the creation the MoF file to be used
    #'
    #' @examples
    #' mof_read()
    
    # libraries 
    library("here")
    library("fs")
    library("cli")
    library("readr")

    if (!exists("last_mod")) {
        source(here("scripts", "misc_functions.R"))
    }
    
    mof_file <- 
      dir_ls(
        path    = here(), 
        regexp  = "mof_sheet.*\\.csv", 
        recurse = TRUE) %>%
      last_mod(.)


    # ======================================================================== #
    # ---- Create table ----
    # ======================================================================== # 
    # if file doesnt exists, will create a stock one based on previous information
    if (identical(as.character(mof_file), character())) {
        
      mof_path <- here("data", "metadata", "mof")
      cli_alert_info("An MoF base file doesn't exist.")
      cli_inform("One will be created in {.file {mof_path}} using info from Jan 2023")
      
      dir_create(mof_path)
        
      mof_base <-
        tibble::tribble(
            ~orig_term,             ~measurementType, ~measurementType_uri, ~measurementAccuracy, ~measurementUnit, ~measurementUnit_uri, ~measurementValue,
            "net_type",                 "bongo nets", "L22/current/NETT0176/",                NA,               NA,                   NA,                NA,
            "distance_m_2", "Length of sampling track", "P01/current/LENTRACK/",                NA,         "metres", "P06/current/ULAA/",                NA,
            "net_size", "Sampling device aperture diameter", "Q01/current/Q0100012/",         NA,         "metres", "P06/current/ULAA/",                NA,
            "net_area", "Sampling device aperture surface area", "Q01/current/Q0100017/", NA,   "per square metre", "P06/current/PMSQ/",                NA,
            "flowmeter_in",            "flow meters", "L05/current/388/",                 NA,                   NA,                    NA,               NA,
            "flowmeter_out",           "flow meters", "L05/current/388/",                 NA,                   NA,                    NA,               NA,
            
            "inpeller_constant",            NA, NA, NA, NA, NA, NA,            
            "dillution",                      "",                     "",                 NA,         "milliliter",                    NA,               NA,
            "dillution_factor",               "",                     "",                 NA,                   NA,                    NA,               NA, 
            # "split_size",                     "",                     "",                 NA,                    NA,                     NA,              NA, 
            "total_split_frac", "Sub-sampling coefficient", "P01/current/SSAMPC01/",          NA,            "decimal",                    NA,               NA,
            "splits_analyzed",                "",                     "",                 NA,                   NA,                    NA,               NA, 
            # "filtered_volume_m3"
            "volume_filt_cubic_m", "Sample volume (filtration) by measuring cylinder", "P01/current/VOLFMCXX/",  NA,     "per cubic metre",   "P06/current/PCUM/",               NA, 
            "pipette_vol_m_l",                "",                     "",                 NA,          "milliliter",                    NA,               NA,               
            # not used "mean_ind_dil_factor",            "",                     "",                 NA,                    NA,                    NA,               NA, 
            "ind_m3",                "Abundance", "S06/current/S0600002/",                NA,"Number per cubic metre", "P06/current/UPMM/",               NA, 
            
            # "ind_m3", "Abundance of mesozooplankton [Size: 200-500um] per unit volume of the water body by optical microscopy", "P01/current/ZU00M00C/", NA, "Number per cubic metre", "P06/current/UPMM/", NA, 
            "number_ind_sample", "Count (in assayed sample) of biological entity specified elsewhere", "P01/current/OCOUNT01/", NA, "Count of Biological Entity", "P09/current/OCNT/",              NA, 
            "tow_time_min",    "Sample Duration", "P01/current/AZDRZZ01/",                NA,             "Minutes",    "P06/current/UMIN/",              NA,
            "ship_speed_knots","Speed of towing platform", "P01/current/TOWSPEED/",       NA, "Knots (nautical miles per hour)", "P06/current/UKNT/",     NA, 
            "tow_speed_m_sec",           "Speed", "S06/current/S0600152/",                NA,   "Metres per second",    "P06/current/UVAA/",              NA, 
            "mesh",     "Sampling net mesh size", "Q01/current/Q0100015/",                NA,         "Micrometres",    "P06/current/UMIC/",              NA,
            "microscopy",           "microscopy",    "S04/current/S0419/",                NA,                    NA,                     NA,              NA
        )  %>%
      mutate(
        event_occur = case_when(
          str_detect(orig_term, "ind_m3|number_ind_sample") ~ "occur",
          .default = "event"
          ),
        .after = orig_term
         )
      # ==================================================================== #
      # ---- Save Table ----
      # ==================================================================== #    
      
      # file name
      file_mof <- 
        glue::glue(
          "{here(mof_path,'mof_sheet')}",
          "{format(Sys.time(), '_%Y%m%d_%H%M%S')}",
          ".csv"
          )
      
      readr::write_csv(
        mof_base, 
        file_mof
        )
    
    } else {
      cli_inform("Loading MoF table: {.file {mof_file}}")
      mof_base <- readr::read_csv(mof_file, show_col_types = FALSE)
    }
    
    
    # ---- add website used for definitions
    def_web <- "http://vocab.nerc.ac.uk/collection/"
    
    # ---- add mof_info
    mof_base <-
      mof_base %>%
      mutate(
        across(
          contains("_uri"),
          .fns = \(.x) 
            if_else(
              !is.na(.x),
              paste0(def_web, .x),
              NA_character_
            )
          )
        ) %>%
      rename(
        "measurementTypeID" = measurementType_uri,
        "measurementUnitID" = measurementUnit_uri
      ) %>%
      select(-measurementValue)
    
    
    return(mof_base)
}

