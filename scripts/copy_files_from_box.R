copy_files_box <- function(.box_dir = box_dir, new_dir = NULL,
                           .choose = FALSE) {
    
    # ---- load libraries ----
    library("fs")
    library("magrittr")
    library("cli")
    library("stringr")
    library("glue")
    
    # ---- check if box_dir var exist in .Rprofile ----
    if (!exists("box_dir")) {
        cli_alert_info(c(
            "You need to setup {.file .Rprofile} ",
            "with the location of box directory in your computer.\n",
            "Use the format: ",
            "{.code {col_yellow('box_dir = \" \"')}}",
            "\n(i.e. {.code {col_yellow('box_dir = \"C:/<my-directory>\"')}})",
            "\n\nThen save and restart R\n"
        ))
        
        usethis::edit_r_profile(scope = "project")
        
        if (.choose) {
            new_dir <- rstudioapi::selectDirectory()
            cli::cli_text("new_dir = \"{new_dir}\"")
        }
        
        invokeRestart("abort")
        }
    
    # ---- list all sub-directories in main box directory ----
    box         <- .box_dir  %>%
        fs::dir_ls(., type = "directory")
    
    # ---- location for local directory ----
    if (is.null(new_dir)) {
        new_dir <- here::here("data","raw")
    }

    cli::cli_alert_info("Box directory: {.file {(.box_dir)}}")
    cli::cli_alert_info("Local directory: {.file {new_dir}}")
    
    # ---- create directories from box ----
    fs::dir_create(
        paste0(new_dir, "/", basename(box)
        )
    )
    
    # ---- copy files from box to local if needed ----
    for (i in seq(box)) {
        # check if files exists
        new_files <- !file_exists(
            paste0(new_dir, "/",
                   basename(box[i]), "/",
                   basename(fs::dir_ls(box[i]))
            )
        )
        
        # ---- display files that arwe to be copied ----
        new_files <- dir_ls(box[i])[new_files]
        info_txt  <- str_extract(basename(box[i]), '[:number:]*')
        mesh_txt  <- "{.strong {glue_col('{red {info_txt} \u03BCm}')}} mesh size"
        
        if (!rlang::is_empty(new_files)) {
            cli_alert_info(
                c("{.strong {col_yellow('Copying')}} these files for ", 
                  mesh_txt, " into {.file {new_dir}}") 
                  )
            cli_ul(basename(new_files))
        } else {
            cli_alert_danger(
                c("{.strong {col_yellow('No new')}} files for zooplankton at ", 
                  mesh_txt) 
            )
        }
        
        # ---- copy files  ----
        new_files %>%
            fs::file_copy(.,
                          paste0(new_dir, "/",
                                 basename(box[i]), "/"))
    }
}

copy_files_box()