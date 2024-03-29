# ============================================================================ #
# ---- Misc Functions ----
# ============================================================================ #    

##%######################################################%##
#                                                          #
####             Most Recently Created File             ####
#                                                          #
##%######################################################%##
#' Most Recently Created File
#'
#' This function should be used after an `fs::dir_ls` search with a specific 
#' file that may have multiple versions. When there are multiple matches to 
#' a search, this will take the most recent version of it.
#'
#' @param fpath The fs_path object created from `fs::dir_ls`
#' @param check Optionally check the most recent file. This can be set to 
#' either `TRUE` or `FALSE`.
#'
#' @return A vector of the most recent created file as `fs_path` object
#' @examples
#' # NA
#' 
last_mod <-  function(fpath, check = TRUE) {

    if (!check) return(fpath)
    
    ftime <- file.mtime(fpath) 
    
    return(fpath[which.max(ftime)]) 
    
    # ---- end of function
}

##%######################################################%##
#                                                          #
####      Base File Name and File Name Expression       ####
#                                                          #
##%######################################################%##
#' Base File Name and File Name Expression
#'
#' This function takes a location to save file and a base name to 
#' search for in either locally or the cloud
#'
#' @param loc Location to save aphia ID file
#' @param file_base Base name to search for
#' @param exts Extension to save file.
#' @param time_stamp_fmt Time stamp format as the suffix to the base file name. 
#'                       - default = YYYYMMDD_HHMMSS (i.e "%Y%m%d_%H%M%S")
#'                       - if no suffix, `NULL`
#'                       - if want custom, `<custom_message>`
#'                       - if help, will give a table of formats with examples
#'
#' @returns Returns a list of three:
#'          - file location  = file_loc
#'          - base file name = file_base,
#'          - file expression to be evaluated = file_expr
#' 
#' @details
#' The time stamp can be formated based on 
#' Code	Meaning	Code	Meaning
#'      %a - Abbreviated weekday	
#'      %A - Full weekday
#'      %b - Abbreviated month	
#'      %B - Full month
#'      %c - Locale-specific date and time	
#'      %d - Decimal date
#'      %H - Decimal hours (24 hour)	
#'      %I - Decimal hours (12 hour)
#'      %j - Decimal day of the year	
#'      %m - Decimal month
#'      %M - Decimal minute	
#'      %p - Locale-specific AM/PM
#'      %S - Decimal second	
#'      %U - Decimal week of the year (starting on Sunday)
#'      %w - Decimal Weekday (0=Sunday)	
#'      %W - Decimal week of the year (starting on Monday)
#'      %x - Locale-specific Date	
#'      %X - Locale-specific Time
#'      %y - 2-digit year	
#'      %Y - 4-digit year
#'      %z - Offset from GMT	
#'      %Z - Time zone (character)
#'      
#' @author Sebastian Di Geronimo (June, 2023)
#'      
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
file_expr <- function(loc       = here::here("data", "metadata", "aphia_id"),
                      file_base = "aphia_taxa",
                      exts      = "csv",
                      time_stamp_fmt = "%Y%m%d_%H%M%S") {
    
    # ---- help for deciding time stamp format
    if (!is.null(time_stamp_fmt) && str_detect(time_stamp_fmt, "help")) {
        return(tribble(
            ~code, ~meaning,
            "%a",  "Abbreviated weekday", 
            "%A",  "Full weekday",
            "%b",  "Abbreviated month", 
            "%B",  "Full month", 
            "%c",  "Locale-specific date and time", 
            "%d",  "Decimal date", 
            "%H",  "Decimal hours (24 hour)", 
            "%I",  "Decimal hours (12 hour)", 
            "%j",  "Decimal day of the year",	
            "%m",  "Decimal month", 
            "%M",  "Decimal minute",	
            "%p",  "Locale-specific AM/PM", 
            "%S",  "Decimal second", 
            "%U",  "Decimal week of the year (starting on Sunday)", 
            "%w",  "Decimal Weekday (0=Sunday)", 
            "%W",  "Decimal week of the year (starting on Monday)", 
            "%x",  "Locale-specific Date", 
            "%X",  "Locale-specific Time", 
            "%y",  "2-digit year", 
            "%Y",  "4-digit year", 
            "%z",  "Offset from GMT",	
            "%Z",  "Time zone (character)", 
        ) %>%
            mutate(
                example = format(ymd_hms("2000-01-01 02:11:51"), code),
                example = sprintf("%s == `%s`", "2000-01-01 02:11:51", example)
            ))
    }
    
    # catch time stamp format if NULL  
    time_stamp_fmt <-
        tryCatch(
            {
                glue("_", format(Sys.time(), time_stamp_fmt))
                expr(glue("_", format(Sys.time(), !!time_stamp_fmt)))
            },
            error = function(e) {
                NULL
            }
        )
    
    # add period to extension
    exts <- glue(".{exts}")
    
    file_expr <-
        # create expression for the file name
        expr(
            here::here(
                !!loc,
                glue(
                    !!file_base,
                    !!time_stamp_fmt,
                    !!exts,
                    .null = ""
                )
            )
        )
    
    list(
        file_base = file_base,
        file_expr = file_expr,
        file_loc  = loc
    )
    
    # ---- end of function
}

##%######################################################%##
#                                                          #
####                   Save .csv File                   ####
#                                                          #
##%######################################################%##
#' Save .csv File
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .data The data.frame or tibble to be saved.
#' @param save_location Folder to save file. Will create folder if doesn't exist.
#' @param save_name Prefix name of file. Will be saved with `_<timestamp>.csv`
#' @param overwrite `TRUE` or `FALSE` to re-save file if exists, or keep current. 
#' @param verbose `TRUE` or `FALSE` to print location in script.
#' @param time_stamp_fmt Time stamp format as the suffix to the base file name.
#'                       - default = YYYYMMDD_HHMMSS (i.e "%Y%m%d_%H%M%S")
#'                       - if no suffix, `NULL`
#'                       - if want custom, `<custom_message>`
#' @param utf_8 Logical() `TRUE` or `FALSE` to use `readr::write_excel_csv` or 
#'              `readr::write_csv` to indicate to Excel the csv is UTF-8 
#'              encoded. 
#'
#' @return NULL, save file
#'
#' @author Sebastian Di Geronimo (June 02, 2023)
#'
#' @examples
#' # ADD_EXAMPLES_HERE
save_csv <- function(
        .data         = NULL,        
        save_location = NULL,
        save_name     = NULL,
        overwrite     = FALSE,
        verbose       = TRUE,
        time_stamp_fmt = "%Y%m%d_%H%M%S",
        utf_8          = FALSE) {
    
    # ---- checking input parameters
    if (is.null(.data)) {
        cli::cli_abort(
            c("Check input, {.var {col_yellow(\".data\")}} is `NULL`.", 
              "{col_red(\"Stopping\")}"))
    }
    if (is.null(save_location)) {
        cli::cli_abort(
            c("Check input, {.var {col_yellow(\"save_location\")}} is `NULL`.",
              "{col_red(\"Stopping\")}"))
    }
    if (is.null(save_name)) {
        cli::cli_abort(
            c("Check input, {.var {col_yellow(\"save_name\")}} is `NULL`.",
              "{col_red(\"Stopping\")}"))
    }
    if (is.null(overwrite)) {
        cli::cli_abort(
            c("Check input, {.var {col_yellow(\"overwrite\")}} is `NULL`.",
              "{col_red(\"Stopping\")}"))
    }
    
    # ---- file name
    data_f <- 
        file_expr(
            save_location,
            save_name,
            exts = "csv",
            time_stamp_fmt
        )
    
    if (any(class(data_f) %in% c("tbl_df", "tbl", "data.frame"))) {
        print(data_f)
        return(data_f)
    }
    
    data_f <- eval(data_f$file_expr)
    
    if (verbose) {
        cli::cli_h1("Base File Name: {.var {save_name}}")
        cli::cli_alert_info(
            c("File Information:\n",
              "Rows:      {nrow(.data)}\n",
              "Columns:   {ncol(.data)}\n",
              "Location:  {.file {save_location}}\n",
              "File Name: {.file {basename(data_f)}}")
        )
    }
    # ---- check if folder exists and create otherwise
    if (!dir_exists(save_location)) {
        
        if (verbose) 
            cli::cli_alert_info(
                "{col_green(\"Creating\")} folder location!")
        
        fs::dir_create(save_location)
    }
    
    # ---- check if need to create file
    file_loc <- 
        fs::dir_ls(
            save_location,
            regexp = save_name
        ) 
    
    create_f <- rlang::is_empty(file_loc)
    
    if (!create_f && !overwrite) {
        # return early if no need to create
        if (verbose)
            cli::cli_alert_info(
                "File exist and {.emph is not} being {col_green(\"overwritten\")}!\n"
            )
        return(invisible())
    }
    
    if (!create_f && overwrite && verbose) {
        cli::cli_alert_info(
            "File exist and {.emph is} being {col_red(\"overwritten\")}!")
    } else if (create_f && verbose) {
        cli::cli_alert_info("File does not exists, {col_green(\"creating\")}!")
    }
    
    
    # ---- saving file
    if (verbose) cli::cli_alert_info("Saving file!")
    
    if (utf_8) {
        # save with UTF-8 
        cli::cli_alert_info(
            c("Note: saving to indicate to excel as {.var UTF-8}.\n", 
              "Using: `{col_yellow(\"readr::write_excel_csv()\")}` ",
              "instead of `{col_red(\"readr::write_csv()\")}`"))
        readr::write_excel_csv(
            x    = .data,
            file = data_f,
            na   = ""
        )
    } else {
        readr::write_csv(
            x    = .data,
            file = data_f,
            na   = ""
        )
    }
    
    if (verbose) cli::cli_alert_success("Saved!\n\n")
    
    # ---- end of function
}

##%######################################################%##
#                                                          #
####                   Save gg Plots                    ####
#                                                          #
##%######################################################%##
file_sv <- function(plt, filename, device = c("jpeg", "svg"), 
                    height = 15, width = 30, ...) {
    device <- match.arg(device)
    
    height <- if (is.null(height)) 3.71 else height
    
    cowplot::save_plot(
        filename    = filename,
        plot        = plt,
        base_height = height,
        base_width  = width,
        device      = device,
        ...
    )
    
    # ---- end of function
}


##%######################################################%##
#                                                          #
####            Add eDNA path to `.Rprofile`            ####
#                                                          #
##%######################################################%##
#' Add eDNA path to `.Rprofile`
#'
#' FUNCTION_DESCRIPTION
#'
#' @param start_path Start path if not found.
#' @param full_path If know where cloud directory is located.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
add_edna_path <- function(start_path = here(),
                          full_path = NULL) {
    
    # ---- either select directory manually or input directly
    if (is.null(full_path)) {
        dir_edna <- rstudioapi::selectDirectory(path = start_path)
    } else {
        dir_edna <- full_path
    }
    
    edna_path <- glue(
        "\n# ---- eDNA file path\n",
        "dir_edna <- \"{dir_edna}\"\n\n")
    
    prof <- here(".Rprofile")
    
    # ---- check that .Rprofile exists
    if (!file_exists(prof)) {
        # create .Rprofile file
        cat(character(0), file = prof)
    }
    
    # ---- read .Rprofile info
    line1 <- readLines(prof) 
    
    # ---- save .Rprofile
    cat(edna_path, file = prof, append = TRUE)
    
    return(dir_edna)
    # ---- end of function
}
