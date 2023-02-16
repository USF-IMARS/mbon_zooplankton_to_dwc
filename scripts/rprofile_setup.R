##%######################################################%##
#                                                          #
####         Create .Rprofile file for startup          ####
#                                                          #
##%######################################################%##
rprofile_setup <- function(prof, .choose = FALSE) {
    #' Create .Rprofile file for startup 
    #'
    #'This script help create a .Rprofile file for the first time using this 
    #'project and upon restarting R, should ask to pull data from cloud server 
    #'and give access to the functions that are used. 
    #'
    #' @param prof location of .Rprofile, should be in the folder with .Rproj
    #' @param .choose whether or not to choose the directory through the 
    #'                `rstudioapi::selectDirectory()` function and auto-add to 
    #'                .Rprofile
    #'           
    #'                TRUE: opens file explorer to choose directory
    #'           
    #'                FALSE: opens .Rprofile with `cloud_dir = "EDIT HERE"` to 
    #'                       edit file directory manually in line 1
    #'
    #' @return Creation of .Rprofile to set cloud path, attach functions at
    #' startup of R and copy new files to local directory.
    #' 
    #' @author Sebastian Di Geronimo (2022-11-28 00:49:10)
    #' 
    #' @examples
    #' # rprofile_setup(prof = here::here(), .choose = FALSE)

    
    # defaults
    pkgs  <- paste(
    "# packages used in .Rmd and scripts",
    "# base R",
    ".First.sys()\n",
    "# external packages",
    "librarian::shelf(
    librarian, ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr,
    forcats, lubridate, glue, fs, magrittr, here,
    # broom, # optional
    
    quiet = TRUE
    )",
    sep = "\n")
    
    funcs <- paste(
        '# source scripts with functions in new environment',
        'source(here("scripts", "attach_funcs.R"))',
        'func_attach()',
        'rm(func_attach)',
        '\n# Copy files from cloud server',
        'if (!exists("cloud_dir")) {
        rlang::abort(c("x" = "`cloud_dir` doesn\'t exist.",
                   "Please run `rprofile_setup()` to add."))
        }\n\n',
        'if (cloud_dir == "EDIT HERE") {
        rlang::abort(c("x" = "`cloud_dir` = EDIT HERE",
                   "Please edit this in .Rprofile or", 
                   "Run `rprofile_setup(.choose = TRUE)` to add."))
        }\n',
        'copy_files_cloud(.cloud_dir = here(cloud_dir, "raw"), ask = TRUE)\n',
        sep = "\n"
        )
    
    # check if location for .Rprofile is file path
    if (!fs::is_dir(prof)) {
        rlang::abort(message = c("x" = "prof needs to be a directory.\n", 
                                 "!" = "Preferably use prof = `here::here()`"),
                                        use_cli_format = TRUE)
        }
    
    prof <- here(prof, ".Rprofile")
    
    if (!file.exists(prof)) {
        
        # create .Rprofile file
        cat(character(0), file = prof)
        
        # set cloud_dir location
        if (.choose) {
            # select interatively
            new_dir <- rstudioapi::selectDirectory()
            cli::cli_text("{.file {new_dir}}")
            new_dir <- glue("cloud_dir <- \"{new_dir}\"")
        } else {
            # edit later
            new_dir <- "cloud_dir <- \"EDIT HERE\""
            cli::cli_alert_warning(c("within {.file {prof}}, you will have to ", 
                                   "edit {.var cloud_dir} because ", 
                                   "{.var .choose} was set to {.var FALSE}\n",
                                   "Currently, {.var {new_dir}}"))
        }
        
        # add pkgs and functions to .Rprofile
        cat(c(new_dir, pkgs, funcs), sep = "\n\n", 
            file = prof, 
            append = TRUE)
        
    } else {
        # if .Rprofile exists
        line1 <- readLines(prof) 
            
        # check if cloud directory exists
        strt  <- grep("cloud_dir <-", line1)
        if (identical(strt, integer(0))) {
            # copy_files_cloud(ask = TRUE, .choose = TRUE)
            cli::cli_alert_info("Adding {.var cloud_dir} to {.var .Rprofile}")
            if (.choose) {
                new_dir <- rstudioapi::selectDirectory()
                new_dir <- glue("cloud_dir <- \"{new_dir}\"")
                cli::cli_text(new_dir)
                cat(c(new_dir, "", line1), sep = "\n", file = prof)
                
            } else {
                # edit later
                new_dir <- "cloud_dir <- \"EDIT HERE\""
                cli::cli_alert_warning(c("within {.file {prof}}, you will have to ", 
                                         "edit {.var cloud_dir} because ", 
                                         "{.var .choose} was set to {.var FALSE}\n",
                                         "Currently, {.var {new_dir}}"))
                cat(c(new_dir, "", line1), sep = "\n", file = prof)
            }
        }
        
        strt  <- grep("cloud_dir <- \"EDIT HERE\"", line1) 
        if (.choose & identical(strt, 1L)) {
            new_dir <- rstudioapi::selectDirectory()
            new_dir <- glue("cloud_dir <- \"{new_dir}\"")
            cli::cli_text(new_dir)
            cat(c(new_dir, line1[-1]), sep = "\n", file = prof)
        }
        
        # check if packages exists
        strt  <- grep("# packages used in .Rmd and scripts", line1)
        if (identical(strt, integer(0))) {
            cli::cli_alert_info("Adding {.var packages} to {.var .Rprofile}")
            cat(c(line1, pkgs, funcs), sep = "\n\n", file = prof)
        }
    
    }
    
    cli::cli_inform("\n")
    cli::cli_alert_info("\n{.strong Please restart R.}\n To do this: ")
    cli::cli_ul(
        c("{.strong Session > Restart R}  -or-",
          "{.strong command/ctrl + shift + F10 }"))
}
