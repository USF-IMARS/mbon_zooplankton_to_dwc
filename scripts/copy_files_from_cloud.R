# ============================================================================ #
#                                                                              # 
#                        Copy file from Cloud Directory                        #
#                                                                              #    
# ============================================================================ #
# ---- DESCRIPTION: ------
# The first time this function is run, it will ask were is the cloud directory
# located on your computuer.
# 
# The second time this function is run, it will ask which files you would like
# to download, all, some or none.
#
# ---- INPUTS: -----------
# .cloud_dir = location of cloud directory 
# new_dir    = location of local directory 
# .choose    = TRUE/FALSE to select cloud directory location on first attempt
# ask        = TRUE/FAlSE to ask if want to download new files
#              - Yes  = download all
#              - No   = skip download
#              - Some = select which files to download  
# auto       = automatically download all new files
#
# ---- OUTPUTS: ----------
# NA
#
# ---- NOTES: ------------
#
# ---- REFERENCES(s): ----
#
# ---- AUTHOR(s): --------
# Sebastian Di Geronimo (2022-12-12 18:58:34)
copy_files_cloud <- function(.cloud_dir = cloud_dir, new_dir = NULL,
                           .choose = FALSE, ask = FALSE, auto = FALSE) {
    
    # ---- load libraries ----
    library("fs")
    library("magrittr")
    library("cli")
    library("stringr")
    library("glue")
    
    # ---- ignore files that contain ----
    ignore_files = paste("blank", sep = "|")

    # ---- check if cloud_dir var exist in .Rprofile ----
    if (!exists("cloud_dir")) {
        cli_alert_info(c(
            "You need to setup {.file .Rprofile} ",
            "with the location of cloud directory in your computer.\n",
            "Use the format: ",
            "{.code {col_yellow('cloud_dir = \" \"')}}.",
            "\n(i.e. {.code {col_yellow('cloud_dir = \"C:/<my-directory>\"')}})",
            "\n\nThen save and restart R.\n"
        ))
        
        old <- readLines(here(".Rprofile"))
        
        if (.choose) {
            new_dir <- rstudioapi::selectDirectory()
            new_dir <- glue("cloud_dir = \"{new_dir}\"")
            cli::cli_text(new_dir)
            cat(c(new_dir, "", old), sep = "\n", file = here(".Rprofile"))
            
        } else {
            usethis::edit_r_profile(scope = "project")
            cat(c("cloud_dir = \"EDIT HERE\"\n", old), sep = "\n", file = here(".Rprofile"))
        }
        
        return()
        
        }
    
    # ---- list all sub-directories in main cloud directory ----
    cloud         <- .cloud_dir  %>%
        fs::dir_ls(., type = "directory")
    
    
    test <- fs::dir_ls("C:/Users/spd19/Box/zoo_test", type = "directory")
    cloud[!str_detect(test, "cruise")]
    # ---- location for local directory ----
    if (is.null(new_dir)) {
        new_dir <- here::here("data","raw")
    }

    cli::cli_alert_info("Cloud directory: {.file {(cloud_dir)}}")
    cli::cli_alert_info("Local directory: {.file {new_dir}}")
    cli_inform("")
    
    # ---- create same directories from cloud repo ----
    fs::dir_create(
        here(new_dir, basename(cloud))
    )
    
    # ---- copy files from cloud to local if needed ----
    for (i in seq(cloud)) {
        # check if files exists
        new_files <- 
            !fs::file_exists(
                here(new_dir, 
                     basename(cloud[i]), 
                     basename(fs::dir_ls(cloud[i], 
                                         regexp = ignore_files, 
                                         invert = TRUE)))
                )

        
        # ---- display files that are to be copied ----
        new_files <- dir_ls(cloud[i], 
                            regexp = ignore_files, 
                            invert = TRUE)[new_files]
        
        info_txt  <- str_extract(basename(cloud[i]), '[:number:]*')
        mesh_txt  <- "{.strong {glue_col('{red {info_txt} \u03BCm}')}} mesh"
        
        if (!rlang::is_empty(new_files)) {
            cli_alert_info(
                c("Files that need {.strong {col_yellow('copying')}} ",
                  "into {.file {new_dir}} for ",
                  mesh_txt))
            
            cli_ul(basename(new_files))
            
        } else {
            cli_alert_danger(
                c("{.strong {col_yellow('No new')}} files for zooplankton at ", 
                  mesh_txt) 
            )
        
        }
        cli_inform("")
        
        # ---- copy files  ----
        if (!rlang::is_empty(new_files) & auto) {
            
            copy_file(new_dir, new_files, mesh_txt, info_txt)
            
        } else if (!rlang::is_empty(new_files) & ask) {
            
        cli_text("Do you want to copy these files? (Yes/No)")
        copy <- utils::menu(c("Yes", "No", "Some"))
        
        if (copy == 3) {
            cli_text("Select which files to copy")
            nums      <- utils::select.list(basename(new_files), 
                                     multiple = TRUE)
            nums      <- glue("{dirname(new_files)[1]}/{nums}")
            new_files <- new_files[new_files %in% nums]
            copy      <-  1
            }
        
        if (copy == 1) {
            copy_file(new_dir, new_files, mesh_txt, info_txt)
            
        } else {
            cli_alert_danger(c("{.strong {col_red('Skipping')}} ",
                                "files that need ",
                                "{.strong {col_yellow('copying')}} ",
                               "from ", mesh_txt))
        }
        cli_inform("")
        } else if (!rlang::is_empty(new_files)) {
            cli_alert_warning(c("Not copying files. In ",
                                "{.fn {col_yellow('copy_files_cloud')}} set ",
                                "ask = {.code {col_red(TRUE)}}."))
            cli_inform("")
        }
    }
}

copy_file <- function(new_dir, new_files, mesh_txt, info_txt) {
    cli_alert_info(
        c("{.strong {col_yellow('Copying')}}: ",
          "({.file {new_dir}} for ", mesh_txt, ")"))
    cli_ul(basename(new_files))

    fs::file_copy(new_files, here(new_dir, basename(cloud[i])))
}
