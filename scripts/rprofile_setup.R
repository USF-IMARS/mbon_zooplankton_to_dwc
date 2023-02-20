##%######################################################%##
#                                                          #
####         Create .Rprofile file for startup          ####
#                                                          #
##%######################################################%##
rprofile_setup <- function(prof, .choose = FALSE, edit_path = FALSE) {
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
    #'                FALSE: in .Rprofile sets `cloud_dir = "EDIT HERE"` to 
    #'                       edit file directory manually in line 1
    #'                       
    #'                NA: ignore using a cloud directory
    #'                
    #'                string: set manually (i.e here::here("<path>") or 
    #'                "C:/<path>/")
    #'
    #' @param edit_path Select TRUE/FALSE when wanting to edit the `cloud_dir`
    #'                  path
    #' 
    #' @return Creation of .Rprofile to set cloud path, attach functions at
    #' start up of R and copy new files to local directory.
    #' 
    #' @author Sebastian Di Geronimo (2022-11-28 00:49:10)
    #' 
    #' @examples
    #' # rprofile_setup(prof = here::here(), .choose = FALSE)

    librarian::shelf(
        librarian, ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr,
        forcats, lubridate, glue, fs, magrittr, here,
        # broom # optional
        
        # additional
        
    )
    
    
    # defaults
    pkgs  <- paste(
    "# packages used in .Rmd and scripts",
    "# base R",
    ".First.sys()\n",
    "# external packages",
    'librarian::shelf(
    librarian, ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr,
    forcats, lubridate, glue, fs, magrittr, here,
    # broom, # optional
    
    quiet = TRUE
    )\n', 
    'library("conflicted")\n',
    'conflict_prefer("filter", "dplyr")',
    'conflict_prefer("select", "dplyr")',
    sep = "\n")
    
    funcs <- paste(
        '# source scripts with functions in new environment',
        'source(here("scripts", "attach_funcs.R"))',
        'func_attach()',
        'rm(func_attach)',
        '\n# Copy files from cloud server',
        
        'if (!exists("cloud_dir")) {',
        '\trlang::abort(c("x" = "`cloud_dir` doesn\'t exist.",
                   "Please run `rprofile_setup()` to add."))
        }\n',
        
        'if (cloud_dir == "EDIT HERE" & !is.na(cloud_dir)) {',
        '\trlang::abort(c("x" = "`cloud_dir` = EDIT HERE",
                   "Please edit this in .Rprofile or", 
                   "Run `rprofile_setup(.choose = TRUE)` to add."))',
        '\t}\n',
        
        'if (!is.na(cloud_dir)) {',
        '\tcloud_dir_raw <- here(cloud_dir, "raw")', 
        '\t} else {cloud_dir_raw <- cloud_dir}\n',
        
        'copy_files_cloud(.cloud_dir = cloud_dir_raw, ask = TRUE)\n',
        sep = "\n"
    )
    
    # ---- set expression for `.choose` options
    cld_dir_expr <- expression(
        # set cloud_dir location
        if (is.character(.choose)) {
            new_dir <- .choose
            cli::cli_alert_info("Setting cloud_dir to {.file {new_dir}}")
            new_dir <- glue("cloud_dir <- \"{new_dir}\"")
            
        } else if (is.na(.choose)) {
            new_dir <- .choose
            cli::cli_alert_info(c("Setting cloud_dir to `{new_dir}`. ",
                                  "If this was a ", 
                                  "{col_red(style_underline('mistake'))}, ",
                                  "change {.var {col_red('.choose')}} ",
                                  "and re-run {.fun rprofile_setup}"))
            new_dir <- glue("cloud_dir <- {new_dir}")
            
        } else if (.choose) {
            # select interactively
            new_dir <- rstudioapi::selectDirectory()
            cli::cli_alert_info("Setting cloud_dir to {.file {new_dir}}")
            new_dir <- glue("cloud_dir <- \"{new_dir}\"")
        } else {
            # edit later
            cli::cli_alert_info("Setting cloud_dir to {.file EDIT HERE}")
            new_dir <- "cloud_dir <- \"EDIT HERE\""
            cli::cli_alert_warning(c("Within {.file {prof}}, you will have to ", 
                                     "edit {.var cloud_dir} because ", 
                                     "{.var .choose} was set to {.var FALSE}\n")
            )
        }
    )
    
    # check if location for .Rprofile is file path
    if (!fs::is_dir(prof)) {
        rlang::abort(message = c("x" = "prof needs to be a directory.\n", 
                                 "!" = "Preferably use `prof = here::here()`"),
                     use_cli_format = TRUE)
    }
    
    prof <- here(prof, ".Rprofile")
    
    if (!file.exists(prof)) {
        # abort if didn't set `.choose`
        if (is.null(.choose)) {
            cli::cli_alert_danger(
                c("{.strong {col_red(style_underline(\"cot Creating\"))}} ",
                  "{.var .Rprofile}"))
            
            cli::cli_alert_danger(
                c("{.strong {col_yellow(style_underline(\"Aborting\"))}}: ",
                  "{.var {col_red('.choose')}} was set to ", 
                  "{.var {col_yellow('NULL')}}.",
                  "\nSetting `{col_red('.choose = TRUE')}` will open a dialog ", 
                  "box to choose the location of your ", 
                  "cloud directory.",
                  "\nSetting `{col_red('.choose = FALSE')}` will ",
                  "add a  placeholder in {.var .Rprofile} to ",
                  "`{col_red('cloud_dir = ",
                  "\"EDIT HERE\"')}` for manually editing.",
                  "\nSetting `{col_red('.choose = \"<drive>:/<path>/\"')}` ",
                  "will manually set the cloud directory path.",
                  "\nSetting `{col_red('.choose = NA')}` will ignore using a ",
                  "cloud directory."))
            
            rlang::abort("Please re-run after setting `.choose`.")
        }
        
        cli::cli_alert_info("Creating `{col_yellow('.Rprofile')}`!")
        
        # create .Rprofile file
        cat(character(0), file = prof)
        
        eval(cld_dir_expr)
        
        # add pkgs and functions to .Rprofile
        cat(c(new_dir, pkgs, funcs), sep = "\n\n", 
            file = prof, 
            append = TRUE)
        
    } else {
        # ---- if .Rprofile exists
        line1 <- readLines(prof) 
        
        # ---- check existence of cloud directory in `.Rprofile`
        strt  <- grep("cloud_dir <-", line1)
        if (identical(strt, integer(0))) {
            # ---- not existing
            cli::cli_alert_info("Adding {.var cloud_dir} to {.var .Rprofile}")
            
            eval(cld_dir_expr)
            
            cat(c(new_dir, "", line1), sep = "\n", file = prof)
           
        } else if (length(strt) >= 1 && edit_path) {
            # ---- editing
            cli::cli_alert_info("Editing {.var cloud_dir} in {.var .Rprofile}")
            
            eval(cld_dir_expr)
            
            cat(c(new_dir, "", line1[-c(strt, strt + 1)]), 
                sep = "\n", file = prof)  
            
        } else {
            # ---- no editing
            cli::cli_alert_info(c("Not changing {.var cloud_dir} in ", 
                                  "{.var .Rprofile}. Set ",
                                  "{.var {col_red(\"edit_path = TRUE\")}} ",
                                  "to change {.var cloud_dir}."))
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
    
    # ---- end function
}
