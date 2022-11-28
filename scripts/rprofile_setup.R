rprofile_setup <- function(prof, .choose = FALSE) {
################################################################################
#                                                                              # 
#               Create .Rprofile file for startup                              #
#                                                                              #    
################################################################################
# ---- DESCRIPTION: ------
# This script help create a .Rprofile file for the first time using this project
# and upon restarting R, should ask to pull data from cloud server and give 
# access to the functions that are used.    
#
# ---- INPUTS: -----------
# prof    = location and name of .Rprofile
# .choose = whether or not to choose the directory through the 
#           `rstudioapi::selectDirectory()` function and auto-add to .Rprofile
#           
#           TRUE:  opens file explorer to choose directory
#           
#           FALSE: opens .Rprofile with `box_dir = "EDIT HERE"` to edit file 
#                  directory manually in line 1
#
# ---- OUTPUTS: ----------
# Creation of .Rprofile.
#
# ---- NOTES: ------------
#
# ---- REFERENCES(s): ----
#
# ---- AUTHOR(s): --------
# Sebastian Di Geronimo (2022-11-28 00:49:10)
    # defaults
    pkgs <- paste(
    "# packages used in .Rmd and scripts",
    ".First.sys()\n # loads base R", 
    "librarian::shelf(
    librarian, ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr,
    forcats, lubridate, glue, fs, magrittr, here,
    # broom, # optional
    
    quiet = TRUE
    )",
    sep = "\n")
    
    funcs <- paste(
        '\n# source scripts with functions in new environment',
        'my_funcs <- new.env()\n',
        '# create directory structure if does not exists and create tree map',
        'source(here("scripts","create_dir.R"), local = my_funcs)',
        '# copy files from box',
        'source(here("scripts", "copy_files_from_box.R"), local = my_funcs)',
        '# loading check for taxa',
        'source(here("scripts", "taxa_list_check.R"), local = my_funcs)\n',
        'attach(my_funcs)',
        'rm(my_funcs)\n',
        'copy_files_box(ask = TRUE)\n',
        sep = "\n"
    )
    
    if (!file.exists(prof)) {
        
        # create .Rprofile file
        cat(character(0), file = prof)
        
        # add pkgs and functions to .Rprofile
        cat(paste(pkgs, funcs, sep = "\n\n"), file = prof, 
            append = TRUE)
        
        # source copy_file_from_box to ask for box_dir variable
        source(here("scripts", "copy_files_from_box.R"))
        
        copy_files_box(ask = TRUE, .choose = .choose)
        
    } else {
        
        line1 <- readLines(prof) 

        # check if packages exists
        strt <- grep("# packages used in .Rmd and scripts", line1)
        if (identical(strt, integer(0))) {
            cat(c(pkgs, "\n"), file = prof, append = TRUE)
        }
        
        # check if function loading exists
        strt <- grep("# source scripts with functions in new environment", line1)
        if (identical(strt, integer(0))) {
            cat(c(funcs, "\n"), file = prof, append = TRUE)
        }
        
        # check if cloud directory exists
        strt <- grep("box_dir", line1)
        if (identical(strt, integer(0))) {
            copy_files_box(ask = TRUE, .choose = TRUE)
        }
    }
    cli::cli_inform("\n")
    cli::cli_alert_info("\n{.strong Please restart R.}\n To do this: ")
    cli::cli_ul(
        c("{.strong Session > Restart R}  -or-",
          "{.strong command/ctrl + shift + F10 }"))
}

