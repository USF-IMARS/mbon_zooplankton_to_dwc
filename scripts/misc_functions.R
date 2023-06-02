# ============================================================================ #
# ---- Misc Functions ----
# ============================================================================ #    

##%######################################################%##
#                                                          #
####              Most Recent Created File              ####
#                                                          #
##%######################################################%##
#' Most Recent Created File
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
last_mod <-  function(fpath, check = TRUE) {

    if (!check) return(fpath)
    
    ftime <- file.mtime(fpath) 
    
    return(fpath[which.max(ftime)]) 
    
    # ---- end of function ----
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
#' @param time_stamp_fmt Timestamp format. 
#'                       - default = YYYYMMDD_HHMMSS (i.e "%Y%m%d_%H%M%S")
#'                       - if no suffix, `NULL`
#'                       - if want custom, `<custom_message>`
#'
#' @return NULL, save file
#' @examples
#' # ADD_EXAMPLES_HERE
save_csv <- function(
        .data         = NULL,        
        save_location = NULL,
        save_name     = NULL,
        overwrite     = FALSE,
        verbose       = TRUE,
        time_stamp_fmt = "%Y%m%d_%H%M%S") {
    
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

    time_stamp_fmt <- 
        tryCatch({
            glue("_", format(Sys.time(), time_stamp_fmt))
            }, error = function(e) {NULL})
    
    # ---- file name
    data_f <-
      here(
        save_location,
        glue(
          "{save_name}",
          time_stamp_fmt,
          ".csv",
          .null = ""
        )
      )
    
    if (verbose) {
        cli::cli_h1("{save_name}")
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
    
    readr::write_csv(
        x    = .data,
        file = data_f,
        na   = ""
    )
    
    if (verbose) cli::cli_alert_success("Saved!\n\n")

    # ---- end of function ----
}

