# ============================================================================ #
#
# Title : Functions to get taxa IDs
#    By : Sebastian Di Geronimo
#  Date : 2022-10-20
#
# ============================================================================ #

# ---- larval stages ----
lifestage <-
    str_c("copepodite",
          "nauplii",
          "larvae",
          "larva$",
          "juvenile",
           "eggs",
          "egg",
          "zoea",
          "protozoea",
          "cypris",
          "megalopa",
          sep = "|")  
   
##%######################################################%##
#                                                          #
####               ---- 1. Load data ----               ####
#                                                          #
##%######################################################%##

# works only for raw abundance data for this project
load_data <- function(file_taxa, verbose = TRUE) {
  if (verbose) cli::cli_alert_info(basename(file_taxa))

  # find the number of skips to start of taxa information
  skips <- which(
    readxl::read_xlsx(
      path         = file_taxa,
      range        = cellranger::cell_cols("A"),
      col_names    = FALSE,
      .name_repair = "unique_quiet"
    ) == "Taxa"
  ) - 1

  # extract values for calculating individuals per cubic meter
  calc <-
    readxl::read_xlsx(
      path      = file_taxa,
      range     = glue("A1:B{skips}"),
      col_names = c("x1", "x2")
    ) %>%
    mutate(
      x1 = str_replace(x1, ".*ate of.*", "date analyzed")
    ) %>%
    pivot_wider(
      names_from  = x1,
      values_from = x2
    ) %>%
    janitor::clean_names() %>%
    mutate(
      date_collected = janitor::convert_to_date(date_collected),
      date_analyzed  = janitor::convert_to_date(date_analyzed)
    ) %>%
    hablar::retype()

  # load the data set
  taxa <-
    readxl::read_xlsx(
      path         = file_taxa,
      skip         = skips,
      .name_repair = janitor::make_clean_names
    ) %>%
    bind_cols(calc, .) %>%
    select("cruise_id" = sample_id, everything()) %>%
    filter(mean > 0) %>%
    mutate(
      site = as.character(site),
      across(
        c(split_amount, splits_analyzed, mesh, filtered_volume_m3),
        ~ as.numeric(.x)
      )
    )

  return(taxa)
  # ---- end of function
}

##%######################################################%##
#                                                          #
####      ---- 2. Merge taxa with taxa list -----       ####
#                                                          #
##%######################################################%##
#' Merge OBIS Taxonomic Information with List of Taxonomic Names
#'
#' @description 
#' This function requires a vector of taxonomic names. This will then try to 
#' look for any previous `aphia_taxa` list locally. It there are none, then
#' it will look within the `cloud` directory. If there still are none, then
#' it will create a list and search the OBIS database using a modified 
#' verion of `obistools::match_taxa()` to ask for input if doesn't match.
#' 
#' There are options to check previously unmatched taxa using `check = TRUE`.
#'
#' @param taxa_list A one column tibble or vector of only verbatim taxonomic 
#' names. Further, these names will separate lifestages and be searched 
#' within OBIS
#' @param file_base The base name that will be used to search for previous 
#' taxonomic lists. 
#' @param .file_expr Used when checking file names, allows to save
#' @param regex_lifestage A list of lifestage name used to parse out of 
#' taxonomic names. This allows for better search within OBIS. 
#' @param check Whether to check any unmatched names.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
merge_taxa <- function(taxa_list,
                       .file_expr = file_expr(),
                       regex_lifestage = lifestage,
                       check = FALSE,
                       ...) {
  if (!check) {
    cli::cli_alert_warning(
      c(
        "Will {col_red('NOT')} be checking for {.emph unmatched names} in ",
        "{.strong taxonomic species} list.",
        "\nIf want to check and fix, set {.code check = TRUE} .\n"
      )
    )
  }

  Sys.sleep(1)

  taxa_list <-
    tibble(taxa_orig = taxa_list) %>%
    unnest(taxa_orig) %>%
    rename("taxa_orig" = 1) %>%
    distinct()

  # ---- load previous creation of aphiaID list or create new one
  cli::cli_alert_info(
    "Loading previous file of aphiaIDs or creating new one\n"
  )

  Sys.sleep(1)

  # * function call: taxa_list_check ----
  taxa_aphia <-
    taxa_list_check(
      taxa_list       = taxa_list,
      regex_lifestage = regex_lifestage,
      .file_expr      = .file_expr,
      check           = FALSE,
      ...
    )

  # ---- merge data with taxa
  Sys.sleep(1)
  cli::cli_alert_info("Moving to joining section")

  taxa_aphia <-
    full_join(
      taxa_aphia,
      taxa_list,
    ) %>%
    distinct(taxa_orig, .keep_all = TRUE)

  # ---- check NA names
  if (check) {
    taxa_aphia <-
      # * function call: taxa_unmatch ----
      taxa_unmatch(
        taza_matched    = taxa_aphia,
        regex_lifestage = regex_lifestage,
        .file_expr      = .file_expr,
        save_file       = TRUE,
        ...
      )
  }

  return(arrange(taxa_aphia, taxa_orig))
  # ---- end of function
}


##%######################################################%##
#                                                          #
####     ---- 3. Load Taxonomic List from File ----     ####
#                                                          #
##%######################################################%##
#' Load Taxonomic List from File
#'
#' This function is used to load a taxa list if one has been created. 
#' Optionally, can check if previous taxa has not found a match.
#'
#' @param loc The location where the `aphia ID` file is located
#' @param .file_expr Used when checking file names, allows to save
#' @param file_base The file base to search for.
#' @param regex_lifestage A list of lifestage name used to parse out of 
#' taxonomic names. This allows for better searcg within OBIS. 
#' @param check Whether to check any unmatched names.
#' @param .recurse When looking for `aphia ID` file, whether or not to 
#' recursively look through each sub-directory.
#' 
#' @return Taxa sheet
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
load_taxa_list <- function(loc             = here::here(),
                           .file_expr      = file_expr(),
                           regex_lifestage = lifestage,
                           check           = FALSE,
                           .recurse        = TRUE,
                           ...) {
    
    # load file most recent taxa with aphiaID 
    
    # search for aphia_id
    taxa_file <-  
        fs::dir_ls(path   =  loc,
                   # regexp = glue("{file_base}.*\\.csv$"), 
                   regexp = glue("{.file_expr[[1]]}.*\\.csv$"), 
                   recurse = .recurse)  %>%
        last_mod(.)
    
    assertthat::assert_that(
        assertthat::not_empty(taxa_file),
        # msg = glue("Taxa list file with base '{file_base}'",
        msg = glue("Taxa list file with base '{.file_expr[[1]]}'",
                   "does not exist in {loc}.",
                   "\nYou may have to set `recurse = TRUE` or",
                   "\nIf one exists in your cloud directory, run",
                   "`master_taxa_list()` to pull and save",
                   "to your local directory.", 
                   .sep = " "))
    
    cli::cli_alert_info("Reading: {taxa_file}")
    
    taxa_matched <-  
        readr::read_csv(taxa_file, 
                        show_col_types = FALSE)
    
    if (check) {
        taxa_matched <- 
        # * function call: taxa_unmatch ----
        taxa_unmatch(
            taxa_matched    = taxa_matched,
            regex_lifestage = regex_lifestage,
            # file_base       = file_base,
            .file_expr      = .file_expr,
            ...)
        }
    
    return(taxa_matched)
    
    # ---- end of function
}

##%######################################################%##
#                                                          #
####            ---- 4. Check taxa list ----            ####
#                                                          #
##%######################################################%##
taxa_list_check <- function(loc             = here::here(),
                            taxa_list       = NULL,
                            regex_lifestage = NULL,
                            .file_expr      = file_expr(),
                            check           = FALSE,
                            use_cloud       = FALSE,
                            file_verbat     = NULL,
                            ...) {
  if (is.null(file_verbat)) {
    # load file most recent taxa with aphiaID
    taxa_file_expr <-
      expression(
        fs::dir_ls(
          path    = loc,
          regexp  = glue("{.file_expr[[1]]}.*\\.csv$"),
          recurse = TRUE
        ) %>%
          last_mod(.)
      )

    taxa_file <- eval(taxa_file_expr)
  } else if (is_file(file_verbat) & file_exists(file_verbat)) {
    cli_alert_info("A file was given. Using {file_verbat}")
    taxa_file <- file_verbat
  }

  # ---- search cloud directory for master aphia ID list
  # if no file exists, will look in root of cloud directory
  # copy it to data/metadata/aphia_id directory
  if (is_empty(taxa_file) & exists("cloud_dir") & use_cloud) {
    try(
      # * function call: master_taxa_list ----
      master_taxa_list(
        .cloud_dir = cloud_dir,
        where_to   = "local"
      ),
      silent = TRUE
    )

    taxa_file <- eval(taxa_file_expr)
  }

  # ---- initialize taxa file
  if (is_empty(taxa_file)) {
    cli::cli_alert_info("Creating a taxonomic list!")

    assertthat::assert_that(
      assertthat::not_empty(taxa_list),
      msg = cli_abort(c(
        "A {.code taxa_list} needs to be supplied if it's the firs",
        "\bt time."
      ))
    )

    taxa_list <-
      tibble(taxa_orig = taxa_list) %>%
      unnest(taxa_orig) %>%
      rename("taxa_orig" = 1) %>%
      distinct()

    assertthat::assert_that(
      length(names(taxa_list)) == 1,
      msg = cli_abort(
        "{.code taxa_list} must contain one column of taxa names."
      )
    )

    # extract taxa names, separating names and life stages
    taxa_name <-
      taxa_list %>%
      sep_life(., .regex_lifestage = regex_lifestage)

    taxa_matched <- tryCatch(
      {
        # ---- run taxa matching with WoRMS database
        taxa_matched <-
          match_taxa_fix(taxa_name$taxa, fuzzy = TRUE, ask = TRUE)

        taxa_matched <- taxa_matched %>%
          bind_cols(taxa_name, .) %>%
          arrange(taxa_orig, scientificName) %>%
          distinct(taxa_orig, .keep_all = TRUE)

        # ---- save first WoRMS database search
        # filename <- eval(.file_expr)
        filename <- eval(.file_expr[[2]])

        cli::cli_alert_info(c(
          "File created: {.file {basename(filename)}}\n",
          "Located in: {.file {dirname(filename)}}"
        ))
        Sys.sleep(1)

        write_csv(taxa_matched, filename, na = "")

        return(taxa_matched)
      },
      interrupt = function(e) {
        cli::cli_alert_danger("An {.emph interrupt} was detected")
        cli::cli_alert("No change was made\n")
        Sys.sleep(1)
        rlang::abort("Stopping functions! You need a aphiaID list!")
      }
    )
  } else {
    # ---- read taxa file
    cli::cli_alert_info(
      c(
        "Reading taxa list file: ",
        "{.file {basename(taxa_file)}}\n"
      )
    )

    taxa_matched <-
      readr::read_csv(
        taxa_file,
        show_col_types = FALSE
      )

    Sys.sleep(1)
  }

  # checking for duplicated taxa_orig
  dupes <- suppressMessages(janitor::get_dupes(taxa_matched, taxa_orig))

  if (assertthat::not_empty(dupes)) {
    cli::cli_alert_warning("Duplicates in {.code orig_taxa} found!\n")
    Sys.sleep(1)

    taxa_matched <-
      taxa_matched %>%
      arrange(taxa_orig, taxa) %>%
      distinct(taxa_orig, .keep_all = TRUE)

    # filename <- eval(.file_expr)
    filename <- eval(.file_expr[[2]])

    cli::cli_alert_info(c(
      "File created from removing dupes: ",
      "{.file {basename(filename)}}\n",
      "Located in: {.file {dirname(filename)}}\n"
    ))

    Sys.sleep(1)

    write_csv(taxa_matched, filename, na = "")
  }

  # ---- optional check NAs
  if (check) {
    # ---- fix non-matched taxa names

    Sys.sleep(1)

    taxa_matched <-
      # * function call: taxa_unmatch
      taxa_unmatch(taxa_matched,
        regex_lifestage = regex_lifestage,
        .file_expr      = .file_expr,
        save_file       = TRUE,
        ...
      )
  } else {
    cli::cli_alert_info(c(
      "In {.fun taxa_list_check}",
      "not checking for any {.emph unmatched names} ",
      "(i.e. NA) in {.code scientificName} of aphiaID ",
      "list.\n"
    ))
    Sys.sleep(1)
  }

  return(taxa_matched)

  # ---- end of function
}

##%######################################################%##
#                                                          #
####      ---- 5. Find unmatched taxa and fix ----      ####
#                                                          #
##%######################################################%##
taxa_unmatch <- function(taxa_matched = NULL,
                         regex_lifestage = NULL,
                         .file_expr = NULL,
                         save_file = FALSE,
                         viewer = FALSE,
                         ...) {
  assertthat::assert_that(
    assertthat::not_empty(taxa_matched),
    msg = cli_abort(
      "{.strong taxa_matched} is missing. Please supply a variable!\n"
    )
  )

  cli::cli_alert_info(c(
    "Checking if any {.emph unmatched names} ",
    "(i.e. NA) are present in ",
    "{.code scientificName} of the aphiaID list.\n"
  ))

  # TODO: see why doesn't work currently ?? seems to be working
  taxa_ntmtch <-
    taxa_matched %>%
    distinct(taxa_orig, .keep_all = TRUE) %>%
    filter(is.na(scientificName)) %>%
    select(taxa_orig) %>%
    sep_life(., .regex_lifestage = regex_lifestage)


  # ---- run taxa matching with WoRMS database
  if (assertthat::not_empty(taxa_ntmtch)) {
    taxa_matched <- tryCatch(
      {
        cli_alert_info(c(
          "Found {col_red({nrow(taxa_ntmtch)})} NAs in ",
          "{.code scientificName}."
        ))

        if (viewer) View(taxa_ntmtch, "Unmatched Taxa")

        cli_alert("Re-running taxa search.\n")
        Sys.sleep(1)

        taxa_matched <-
          match_taxa_fix(taxa_ntmtch$taxa, fuzzy = TRUE, ask = TRUE) %>%
          bind_cols(taxa_ntmtch, .) %>%
          bind_rows(taxa_matched, .) %>%
          arrange(taxa_orig, scientificName) %>%
          distinct(taxa_orig, .keep_all = TRUE)
      },
      interrupt = function(e) {
        cli::cli_alert_danger("An {.emph interrupt} was detected")
        cli::cli_alert("No change was made\n")
        Sys.sleep(1)

        # stop from saving file if interrupt
        save_file <<- FALSE
      }
    )
  } else {
    cli::cli_alert_info("No NAs found\n")
    Sys.sleep(1)
  }

  if (save_file) {
    # ---- save WoRMS database search
    filename <- eval(.file_expr[[2]])

    cli::cli_alert_info(c(
      "Writing new file: ",
      "{.file {basename(filename)}}\n"
    ))
    Sys.sleep(1)

    write_csv(taxa_matched, filename, na = "")
  } else {
    cli::cli_alert_warning("Not saving current Aphia ID list.")
  }

  return(taxa_matched)

  # ---- end of function
}

##%######################################################%##
#                                                          #
####           ---- 6. Save merged data ----            ####
#                                                          #
##%######################################################%##
save_merged <- function(taxa_matched_merg, .taxa_file = NULL,
                        loc = here::here("data", "processed"),
                        ind_file = TRUE,
                        append = TRUE,
                        ...) {
  dir <- here::here(loc, "ind_file_merg")

  fs::dir_create(loc)
  fs::dir_create(dir)

  # ---- save processed taxa data
  file.name <-
    here::here(
      loc,
      glue(
        "all_merged_",
        "processed",
        "{format(Sys.time(), '_%Y%m%d_%H%M%S')}",
        ".csv"
      )
    )
  old_merged <- fs::dir_ls(loc, regexp = "all_merged") %>%
    last_mod(.)

  # ---- save merged list
  if (!is_empty(old_merged) & append) {
    # ---- append all merged
    old_merged %>%
      read_csv(
        show_col_types = FALSE,
        name_repair    = "unique_quiet"
      ) %>%
      add_row(select(taxa_matched_merg, -files)) %>%
      distinct() %>%
      write_csv(file.name, na = "")
  } else {
    # ---- create list
    taxa_matched_merg %>%
      select(-files) %>%
      write_csv(file.name, na = "")
  }

  # ---- save individual files
  if (ind_file) {
    taxa_matched_merg <-
      taxa_matched_merg %>%
      mutate(
        files =
          here(
            dir,
            glue(
              "{basename(path_ext_remove(taxa_matched_merg$files))}_",
              "processed",
              "{format(Sys.time(), '_%Y%m%d_%H%M%S')}",
              ".csv"
            )
          ),
        .before = 1
      )


    taxa_matched_merg %>%
      group_by(files) %>%
      group_walk(
        ~ write_csv(.x, .y$files, na = "")
      )

    cli::cli_alert_info("Saving merged files in {.file {dirname(file.name)}}")
    cli::cli_alert_info("Saving individual files in {.file {dir}}")

    Sys.sleep(1)
  } else {
    cli::cli_alert_warning("Not saving individual files.")
  }

  if (!is.null(.taxa_file)) skip_file(.taxa_file, check = FALSE, ...)

  # ---- end of function
}

##%######################################################%##
#                                                          #
####     ---- 7. Save list of processed files ----      ####
#                                                          #
##%######################################################%##
#' Skip Files
#'
#' This function will skip files that have already been processed using a 
#' file with the suffix set in `file_suf`.
#'
#' @param file_taxa List of files currently in local directory to be 
#' processed.
#' 
#' @param file_name The file containing the processed data file paths.
#' @param check Whether to check any unmatched names.
#' 
#' TRUE = check list and remove old files from processesing \cr
#' FALSE = use entire list of files
#'
#' @return List of files to be processed.
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
skip_file <- function(file_taxa, 
                      loc       = here::here("data", "metadata"),
                      file_name = "processed_files", 
                      check     = TRUE) {
    
    # load file most recent taxa with aphiaID 
    files_lists <-  
        fs::dir_ls(path    =  here::here(),
                   regexp  = glue("{file_name}.*\\.csv$"),
                   recurse = TRUE)
    
    # check list of files to remove from search
    if (isTRUE(check)) {
        files_lists <- files_lists %>%
            {if (!is_empty(.))
                read_csv(., show_col_types = FALSE) %>%
                    pull(files)
            }
        
        if (all(class(file_taxa) != "character")) file_taxa <- pull(file_taxa, 1)
        
        file_taxa <-  file_taxa[!(file_taxa %in% files_lists)]
        
        assertthat::assert_that(
            assertthat::not_empty(file_taxa),
            msg = "No files need processing!")
        
        # return(file_taxa)
        return(tibble(files = file_taxa)) 
    } 
    
    if (str_detect(check, "(?i)ig")) {
        cli::cli_alert_info("Ignoring checks and returning same input!")
        return(tibble(files = file_taxa))
    }
    
    file.name <- here::here(loc, glue("{file_name}.csv"))
    
    # ---- initialize file list
    if (is_empty(files_lists)) {
        cli::cli_alert_info("Creating a list of processed files!")
        cli::cli_alert_info(c("File created: {.file {basename(file.name)}}\n",
                              "Located in: {.file {dirname(file.name)}}"))

        as_tibble(file_taxa) %>%
            rename("files" = 1) %>%
            mutate(time = Sys.time()) %>%
            write_csv(
                file.name
            )
        
    } else {
        cli::cli_alert_info("Appending the list of processed files!")
        cli::cli_alert_info(c("File appended: {.file {basename(file.name)}}\n",
                              "Located in: {.file {dirname(file.name)}}"))
        
        files_lists %>%
            read_csv(., show_col_types = FALSE) %>%
            bind_rows(tibble(file_taxa, time = Sys.time())) %>%
            distinct(files, .keep_all = TRUE) %>%
            write_csv(
                file.name
            )
    }
    
    return(as_tibble(file_taxa))

    # ---- end of function
}

##%######################################################%##
#                                                          #
#### ---- 8. Push/Pull Master List to Cloud/Local ----  ####
#                                                          #
##%######################################################%##
#' Push/Pull Master Taxonomic List to Cloud/Local
#'
#' @description 
#' This function will either pull master taxa list from cloud, or push from
#' a local taxa list into the cloud. This will depend on the direction that 
#' is set in the `where_to` input ("local" or "cloud"). The `file_base` can 
#' changed if you choose to have it name someting else. The `file_expr` is 
#' an expression to save the file name. 
#' 
#' By default, it's "aphia_taxa_<yyyymmddd_hhmmss>.csv".
#'
#' @param taxa_list List of taxa to save
#' @param .cloud_dir Location of cloud directory
#' @param file_base The base of the file to search for
#' @param where_to Location to either pull or push \cr
#' `cloud` = push taxa sheet to cloud from local \cr
#' `local` = pull taxa sheet from cloud to local
#' @param .file_expr a filename expression to save the taxa file list. 
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
master_taxa_list <- function(taxa_list  = NULL,
                             .cloud_dir,
                             where_to   = NULL,
                             save       = FALSE,
                             .file_expr = file_expr(),
                             sheet_name = "Master Taxa Sheet") {
    
  # TODO: break into two functions
  # 1. pull_taxa_from_cloud
  # 2. update_taxa_in_cloud
  assertthat::assert_that(
    !rlang::is_null(where_to),
    msg = glue(
      "Need to set `where_to` to ",
      "\n`cloud` if want to save `{sheet_name}` to cloud ",
      "\nor \n`local` to pull `{sheet_name}` from cloud"
    )
  )

  file_location <- here(.cloud_dir, glue("{.file_expr[[1]]}.csv"))

  # ========================================================================== #
  # ---- update
  # ========================================================================== #
  if (str_detect(where_to, "cloud") & !is.null(taxa_list) & save) {
    file_location %>%
      write_excel_csv(taxa_list, ., na = "")

    cli::cli_alert_success(c(
      "Pushing ",
      "{.strong {col_green(sheet_name)}} ",
      "to {.path {cloud_dir}}"
    ))

    return(invisible(NULL))
  }

  # ========================================================================== #
  # ---- pull from cloud
  # ========================================================================== #    
  if (str_detect(where_to, "local") & !save) {
    file_location <-
      here::here(.cloud_dir) %>%
      fs::dir_ls(regexp = .file_expr[[1]])

    # if more than 1 file if found, choose one
    if (length(file_location) > 1) {
      cli::cli_alert_info(c(
        "There was more than one file found. ",
        "Choose one:"
      ))
      file_location <-
        file_location[menu(basename(file_location))]
    }

    assertthat::assert_that(fs::file_exists(file_location),
      msg = glue(
        "`{.file_expr[[1]]}.csv` does not exists ",
        "in {.cloud_dir}"
      )
    )

    file_location %>%
      fs::file_copy(., eval(.file_expr[[2]]))

    cli::cli_alert_success(c(
      "Copying ",
      "{.strong {col_green(sheet_name)}} ",
      "to {.path {dirname(eval(.file_expr[[2]]))}}"
    ))

    return(invisible(NULL))
  }

  # ========================================================================== #
  # ---- update local
  # ========================================================================== #    
  if (str_detect(where_to, "local") & !is.null(taxa_list) & save) {
    file_location <- eval(.file_expr[[2]])

    cli::cli_alert_success(c(
      "Saving ",
      "{.strong {col_green(sheet_name)}} ",
      "to {.path {dirname(file_location)}}"
    ))
    write_csv(taxa_list,
      file = file_location,
      na   = ""
    )

    return(invisible(NULL))
  }

  cli::cli_alert_warning(c(
    "Nothing was pushed or pulled.\nIf pushing to",
    "{.var cloud}, make sure to have to set",
    "{.var taxa_list}"
  ))

  return(invisible(NULL))

  # ---- end of function
}


##%######################################################%##
#                                                          #
####    ---- 8.1 Pull Taxonomic List from Cloud ----    ####
#                                                          #
##%######################################################%##
#' Pull Taxonomic List from Cloud
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .cloud_dir DESCRIPTION.
#' @param .file_expr DESCRIPTION.
#' @param sheet_name DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
pull_taxa_from_cloud <- function(
        .cloud_dir,       
        .file_expr = NULL,                         
        sheet_name = "Master Taxa Sheet") {

  # ========================================================================== #
  # ---- pull from cloud
  # ========================================================================== # 
  file_location <-
    here::here(.cloud_dir) %>%
    fs::dir_ls(regexp = .file_expr[[1]])

  # if more than 1 file if found, choose one
  if (length(file_location) > 1) {
    cli::cli_alert_info(c(
      "There was more than one file found. ",
      "Choose one:"
    ))
    file_location <-
      file_location[menu(basename(file_location))]
  }

  assertthat::assert_that(
    fs::file_exists(file_location),
    msg = glue(
      "`{.file_expr[[1]]}.csv` does not exists ",
      "in {.cloud_dir}"
    )
  )

  file_location %>%
    fs::file_copy(., eval(.file_expr[[2]]))

  cli::cli_alert_success(c(
    "Copying ",
    "{.strong {col_green(sheet_name)}} ",
    "to {.path {dirname(eval(.file_expr[[2]]))}}"
  ))

  return(invisible(NULL))

  # ---- end of function
}

##%######################################################%##
#                                                          #
####        ---- 8.2 Update Taxonomic List ----         ####
#                                                          #
##%######################################################%##
#' Update Taxonomic List
#'
#' FUNCTION_DESCRIPTION
#'
#' @param taxa_list DESCRIPTION.
#' @param .cloud_dir DESCRIPTION.
#' @param where_to DESCRIPTION.
#' @param save DESCRIPTION.
#' @param overwrite DESCRIPTION.
#' @param .file_expr DESCRIPTION.
#' @param sheet_name DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
update_taxa_list <- function(taxa_list = NULL,
                             .cloud_dir,
                             where_to = c("local", "cloud"),
                             save = FALSE,
                             overwrite = save,
                             .file_expr =  NULL,
                             sheet_name = "Master Taxa Sheet") {
  assertthat::assert_that(
    !is.null(taxa_list),
    msg = paste("`taxa_list` needs to contains a `data.frame` or `tibble` with", 
                "taxonomic information")
  )
    
  assertthat::assert_that(
    fs::dir_exists(.cloud_dir),
    msg = "`.cloud_dir` path does not exists! Please supply one that exists."
  )
    
  assertthat::assert_that(
    save,
    msg = paste("`save` was set to `FALSE` so stopping early. Set to `TRUE`", 
                "if you want to update a local/cloud master taxa sheet.")
  )
  
  assertthat::assert_that(
    !is.null(.file_expr) & is.list(.file_expr),
    msg = paste("`.file_expr` is expected to be the output of the function", 
                "`file_expr()` to set the file base name.")
  )
   
  where_to <- match.arg(where_to)  
  
  # ========================================================================== #
  # ---- update cloud
  # ========================================================================== # 
  if (str_detect(where_to, "cloud")) {
    here(.cloud_dir, glue("{.file_expr[[1]]}.csv")) #%>%
      # write_excel_csv(taxa_list, ., na = "")

    cli::cli_alert_success(c(
      "Updating ",
      "{.strong {col_green(sheet_name)}} ",
      "in {.path {cloud_dir}}"
    ))
    
    save_csv(
        .data         = taxa_list,        
        save_location = .cloud_dir,
        save_name     = .file_expr[[1]],
        overwrite     = overwrite,
        verbose       = TRUE,
        time_stamp_fmt = NULL,
        utf_8          = TRUE
    )
    
    return(invisible(NULL))
  }

  # ========================================================================== #
  # ---- update local
  # ========================================================================== #
  if (str_detect(where_to, "local")) {
    file_location <- eval(.file_expr[[2]])

    cli::cli_alert_success(c(
      "Saving another version of ",
      "{.strong {col_green(sheet_name)}} ",
      "to {.path {dirname(file_location)}}"
    ))
    
    save_csv(
        .data = taxa_list,
        save_location = file_exprs2[[3]],
        save_name = file_exprs2[[1]],
        overwrite = overwrite,
        verbose = TRUE,
        utf_8 = TRUE
    )
    
    return(invisible(NULL))
  }

  cli::cli_alert_warning(c(
    "Nothing was pushed or pulled.\nIf pushing to",
    "{.var cloud}, make sure to have to set",
    "{.var taxa_list}"
  ))

  return(invisible(NULL))

  # ---- end of function
}

##%######################################################%##
#                                                          #
####          ---- 9. Separate Life Stage ----          ####
#                                                          #
##%######################################################%##
sep_life <- function(.x, .regex_lifestage = NULL) {

  lifer <- str_replace_all(
    str_c("(?i)", .regex_lifestage),
    "([a-z]+)\\|",
    "\\1\\|(?i)"
  )

  values <- .x %>%
    mutate(
      .,
      # remove extra info from taxa
      taxa = str_remove(
        taxa_orig,
        regex("sp{0,2}\\.|\\(unknown\\)|\\(.*\\)",
          ignore_case = TRUE
        )
      ),
      taxa = str_trim(taxa),

      # life stage
      lifestage = if_else(
        str_detect(taxa_orig, lifer),
        str_extract(taxa_orig, lifer),
        NA_character_
      ),
      lifestage = str_to_lower(lifestage),

      # cleaned taxa for OBIS search
      taxa = str_remove(
        taxa,
        regex(.regex_lifestage,
          ignore_case = TRUE
        )
      ), 
      taxa = str_trim(taxa)
    )

  return(values)
  # ---- end of function
}

##%######################################################%##
#                                                          #
####  ---- 10. Extract WoRMS Extended Information ----  ####
#                                                          #
##%######################################################%##
#' Extract WoRMS Extended Information
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .x Aphia ID to search
#' @param .y The original taxonomic name
#' @param .z A row number
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
worrms_info_extract <- 
    function(.x, .y, .z = NULL) {
    
    if (is.null(.z)) .z <- 0
            
    tryCatch(
        {
            if (.z == 1) {
                cat(sprintf(
                    "\n%-4s | %-7s | names\n%s",
                    "Row", "aphia",
                    stringi::stri_dup("-", 37)
                ))
            }  
            
            if (.z == 0) {
            cat(sprintf("\n%6s | %s | ", .x, .y))
                
            } else {
                cat(sprintf("\n%-4d | %6s | %s | ", .z, .x, .y))
            }
            
            x <- worrms::wm_record(.x)
            
            cat(x$scientificname)
            
            return(x)
        },
        error = function(e) {
            return(NULL)
        }
    )
}
