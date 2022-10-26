#  ------------------------------------------------------------------------
#
# Title : Functions to get taxa IDs
#    By : Sebastian Di Geronimo
#  Date : 2022-10-20
#
#  ------------------------------------------------------------------------

if (!exists("my_funcs")) my_funcs <- FALSE

file_expr <- expression(paste0(
    here::here("data", "metadata", "aphia_id", file_base),
    format(Sys.time(), '_%Y%m%d_%H%M%S'),
    ".csv"))

# ---- larval stages ----
lifestage <- 
    paste0("copepodite|nauplii|larvae|larva|juvenile|",
           "eggs|egg|zoea|protozoea|cypris|megalopa")  

# ---- functions ----
# returns most recent modified file path
last_mod <-  function(fpath) {
    ftime <- file.mtime(fpath)           
    return(fpath[which.max(ftime)]) 
}

source(here::here("scripts", "match_taxa_fix.R"), local = my_funcs)

# ---- 1. Load data --------------------------------------------------------------
load_data <- function(file.taxa) {
    
    cli::cli_alert_info(basename(file.taxa))
    # find the number of skips to start of taxa information
    skips <-  which(readxl::read_xlsx(file.taxa, range = cell_cols("A"), 
                                      col_names = FALSE ) == "Taxa") - 1
    
    # extract values for calculating individuals per cubic meter
    calc <-
        readxl::read_xlsx(
            file.taxa,
            range = glue("A1:B{skips}"),
            col_names = c("x1", "x2")
        ) %>%
        mutate(
            x1 = str_replace(x1, ".*ate of.*", "date analyzed")
        ) %>% 
        # select(-x3) %>%
        pivot_wider(,
                    names_from  = x1,
                    values_from = x2
        ) %>%
        janitor::clean_names() %>%
        mutate(
            date_collected = as.Date(as.numeric(date_collected), origin = "1899-12-30"),
            date_analyzed  = as.Date(as.numeric(date_analyzed), origin = "1899-12-30"),
        ) %>%
        hablar::retype()
    
    # load the data set
    taxa <- 
        readxl::read_xlsx(file.taxa,  
                          skip = skips, 
                          .name_repair = janitor::make_clean_names) %>%
        mutate(
            cruise_id = calc$sample_id,
            .before = everything()
        ) %>%
        full_join(x = calc, by = c("sample_id" = "cruise_id")) %>%
        select(cruise_id = sample_id, everything()) %>%
        filter(mean > 0) %>%
        mutate(
            site = as.character(site),
            across(c(split_amount, splits_analyzed, mesh, filtered_volume_m3), 
                   ~ as.numeric(.x))
        )
    
    return(taxa)
}

# ---- 2. Merge taxa with taxa list ----------------------------------------------
merge_taxa <- function(dat, file_base = "aphia_taxa", 
                       .file_expr = file_expr, 
                       regex_lifestage = lifestage, check = FALSE) {
    if (!check) cli::cli_alert_warning(
        c("Will {col_red('not')} be checking for {.emph NAs} in ",
          "{.strong taxa aphiaID} list.",
          "\nSet check to {.code TRUE} if want to check.\n"))
    
    Sys.sleep(1)
    
    taxa_list <- dat %>%
        select(taxa) %>%
        distinct()
    
    # ---- load previous creation of aphiaID list or create new one ----
    cli::cli_alert_info(
        "Loading previous file of aphiaIDs or creating new one\n")
    Sys.sleep(1)
    
    taxa_aphia <- taxa_list_check(taxa_list = taxa_list, 
                                  regex_lifestage = regex_lifestage,
                                  file_base = file_base,
                                  .file_expr = .file_expr,
                                  check = FALSE)
    
    # ---- merge data with taxa ----
    Sys.sleep(1)
    cli::cli_alert_info("Moving to joining section")
    
    taxa_matched_merg <- right_join(
        taxa_aphia, 
        taxa_list,
        by = c("taxa_orig" = "taxa")) %>%
        distinct(taxa_orig, .keep_all = TRUE)
    
    taxa_matched_merg <-
        taxa_unmatch(taxa_matched_merg, regex_lifestage = regex_lifestage,
                 .file_expr = .file_expr, 
                 file_base = file_base, check = check) %>%
        right_join(., dat, by = c("taxa_orig" = "taxa"))
}

# ---- 3. Load taxa list ---------------------------------------------------------
load_taxa_list <- function(.file_expr = file_expr,
                           file_base = "aphia_taxa",
                           regex_lifestage = lifestage,
                           check = FALSE) {
    
    # load file most recent taxa with aphiaID 
    taxa_file <-  
        fs::dir_ls(path =  here::here("data", "metadata", "aphia_id"),
                   regexp = glue("{file_base}.*\\.csv$"))  %>%
        last_mod(.)
    
    assertthat::assert_that(assertthat::not_empty(taxa_file),
                            msg = glue("Taxa list file with '{file_base}'",
                                       "does not exist",.sep = " "))
    taxa_matched <-  
        readr::read_csv(taxa_file, 
                        show_col_types = FALSE)
    
    if (check) taxa_matched <- taxa_unmatch(taxa_matched = taxa_matched, 
                 regex_lifestage = regex_lifestage,
                 file_base = file_base, 
                 .file_expr = .file_expr, 
                 check = check)
    
    return(taxa_matched)
}


# ---- 4. Load taxa list ---------------------------------------------------------
taxa_list_check <- function(taxa_list = NULL, regex_lifestage = NULL, 
                            file_base = NULL, .file_expr = NULL,
                            check = FALSE) {

    # load file most recent taxa with aphiaID 
    taxa_file <-  
        fs::dir_ls(path =  here::here("data", "metadata", "aphia_id"),
                   regexp = glue("{file_base}.*\\.csv$"))  %>%
        last_mod(.)

    # ---- initialize taxa file ----
    if (is_empty(taxa_file)) {
        cli::cli_alert_info("This is the creation of a taxanomic list!")
        
        assertthat::assert_that(
            assertthat::not_empty(taxa_list),
            msg = cli_abort(
                "{.code taxa_list} needs to be supplied if it's the first time."))

        assertthat::assert_that(
            "taxa" %in% names(taxa_list),
            msg = cli_abort("{.code taxa_list} must contain {.code taxa}."))
        
        # extract taxa names, separating names and life stages
        taxa.name <-
            taxa_list  %>% 
            select(taxa_orig = taxa) %>%
            mutate(
                # remove extra info from taxa
                taxa = str_remove(
                    taxa_orig, 
                    regex("sp{0,2}\\.|\\(unknown\\)|\\(.*\\)", 
                        ignore_case = TRUE)
                ),
                
                # life stage
                lifeStage = str_extract_all(
                    taxa,
                    regex(regex_lifestage,
                        ignore_case = TRUE),
                    simplify = TRUE
                ) %>% str_to_lower(),
                
                # cleaned taxa for OBIS search
                taxa = str_remove(
                    taxa,
                    regex(regex_lifestage,
                        ignore_case = TRUE)
                )  %>%
                    str_trim()
            ) 
        
        # ---- run taxa matching with WoRMS database ----
        taxa_matched <- 
            match_taxa_fix(taxa.name$taxa, fuzzy = TRUE, ask = TRUE)  %>%
            bind_cols(taxa.name, .)

        # ---- save first WoRMS database search ----
        filename <- eval(.file_expr)

        cli::cli_alert_info(c("File created: {.file {basename(filename)}}\n",
                              "Located in: {.file {dirname(filename)}}"))
        Sys.sleep(1)
        
        write_csv(taxa_matched, filename, na = "")
        
    } else {
        # ---- read taxa file ----
        cli::cli_alert_info(
           c("Reading taxa list file: ",
             "{.file {basename(taxa_file)}}\n"))
        
        taxa_matched <-  
            readr::read_csv(taxa_file, 
                            show_col_types = FALSE) 
        
        Sys.sleep(1)
    }
    
    # checking for duplicated taxa_orig
    dupes <- suppressMessages(janitor::get_dupes(taxa_matched, taxa_orig))

    if (assertthat::not_empty(dupes)) {
        
        cli::cli_alert_warning("Duplicates in {.code orig_taxa} found!\n")
        Sys.sleep(1)
        
        taxa_matched <- taxa_matched %>%
            arrange(taxa_orig, taxa) %>%
        distinct(taxa_orig, .keep_all = TRUE)
        
        filename <- eval(.file_expr)
        
        cli::cli_alert_info(c("File created from removing dupes: ", 
                              "{.file {basename(filename)}}\n",
                              "Located in: {.file {dirname(filename)}}\n"))
        Sys.sleep(1)
        write_csv(taxa_matched, filename, na = "")
    }
    
    # ---- optional check NAs ----
    if (check) {
        # ---- fix non-matched taxa names ----
        cli::cli_alert_info(c("Checking file if any NAs are present in ",
                            "{.code scientificName}.\n"))
        Sys.sleep(1)
        taxa_matched <- 
            taxa_unmatch(taxa_matched, 
                         regex_lifestage = regex_lifestage,
                         file_base = file_base, 
                         .file_expr = .file_expr,
                         check = check) 
    } else {
        cli::cli_alert_info("Not checking for NAs in aphiaID list.\n")
        Sys.sleep(1)
    }
    
    return(taxa_matched)
}

# ---- 5. Find unmatched taxa and fix -----------------------------------------
taxa_unmatch <- function(taxa_matched = NULL, regex_lifestage = NULL,
                         file_base = NULL, 
                         .file_expr = NULL, 
                         check = FALSE) {
    
    assertthat::assert_that(
    assertthat::not_empty(taxa_matched), 
    msg = cli_abort(
        "{.strong taxa_matched} is missing. Please supply a variable!\n")
    )
    
    if (!check) {
        return(taxa_matched)
    }
    
    taxa_ntmtch <-
        taxa_matched  %>%
        distinct(taxa_orig, .keep_all = TRUE) %>%
        filter(is.na(scientificName)) %>%
        select(taxa_orig) %>%
        mutate(
            # remove extra info from taxa  
            taxa      = str_remove(
                taxa_orig, 
                regex("sp{0,2}\\.|\\(unknown\\)|\\(.*\\)", 
                      ignore_case = TRUE)
            ),
            
            # life stage  
            lifeStage = str_extract_all(
                taxa,
                regex(regex_lifestage,
                      ignore_case = TRUE),
                simplify = TRUE
            ) %>% str_to_lower(),
            
            # cleaned taxa for OBIS search
            taxa      = str_remove(
                taxa,
                regex(regex_lifestage,
                      ignore_case = TRUE)
            ) %>% str_trim()
        )
    
    # ---- run taxa matching with WoRMS database ----
    if (assertthat::not_empty(taxa_ntmtch)) {
        
        taxa_matched <- tryCatch({
            cli_alert_info(c("Found {col_red({nrow(taxa_ntmtch)})} NAs in ", 
                       "{.code scientificName}."))
        
            View(taxa_ntmtch, "Unmatched Taxa")
            
            cli_alert("Re-running taxa search.\n")
            Sys.sleep(1)
            taxa_matched <- 
                match_taxa_fix(taxa_ntmtch$taxa, fuzzy = TRUE, ask = T) %>%
                bind_cols(taxa_ntmtch, .) %>%
                bind_rows(taxa_matched, .) %>%
                arrange(taxa_orig) %>%
                distinct(taxa_orig, .keep_all = TRUE)
            
            # ---- save WoRMS database search ----
            filename <-  eval(.file_expr)
    
            cli::cli_alert_info(c("Writing new file: ",
                                  "{.file {basename(filename)}}\n"))
            Sys.sleep(1)
            write_csv(taxa_matched, filename, na = "")
   
            return(taxa_matched)
            
            }, interrupt = function(e){
                cli::cli_alert_danger("An {.emph interrupt} was detected")
                cli::cli_alert("No change was made\n")
                Sys.sleep(1)
                taxa_matched
        })
    } else {
        cli::cli_alert_info("No NAs found\n")
        Sys.sleep(1)
    }
    
    return(taxa_matched)
}

# ---- 6. Save merged data ----------------------------------------------------
save_merged <- function(taxa_matched_merg, .taxa_file = NULL) {
    # browser()
    # ---- save processed taxa data ----
    file.name <-
        glue(
            "{here::here('data', 'processed')}/
        \ball_merged_
        \bprocessed
        \b{format(Sys.time(), '_%Y%m%d_%H%M%S')}
        \b.csv"
        ) %>%
        str_remove_all("(\\\n\\\b)") 
    dir <- here::here('data', 'processed', 'ind_file_merg')
    
    taxa_matched_merg %>% 
        select(-Files) %>%
        write_csv(file.name, na = "")
    
    taxa_matched_merg <- taxa_matched_merg %>%
        mutate(
            Files =  glue(
                "{dir}/
                \b{(taxa_matched_merg$Files)}_
                \bprocessed
                \b{format(Sys.time(), '_%Y%m%d_%H%M%S')}
                \b.csv"
            ) %>%
            str_remove_all("(\\\n\\\b)|(\\.xlsx)"),
        .before = 1)

    taxa_matched_merg %>%
        group_by(Files) %>%
        group_walk(
            ~ write_csv(.x, .y$Files, na = "")
        )
    
    cli::cli_alert_info("Saving merged files in {.file {dirname(file.name)}}")
    cli::cli_alert_info("Saving individual files in {.file {dir}}")
    
    Sys.sleep(1)
    
    if (!is.null(.taxa_file)) skip_file(.taxa_file, check = FALSE)
}

# ---- 7. Save list of processed files ----------------------------------------
skip_file <- function(file.taxa, check = TRUE) {
    
    # load file most recent taxa with aphiaID 
    files_lists <-  
        fs::dir_ls(path =  here::here("data", "metadata"),
                   regexp = "processed_files.*\\.csv$")
    
    if (check) {
        files_lists <- files_lists %>%
            {if (!is_empty(.))
                read_csv(., show_col_types = FALSE) %>%
                    pull(files)
            }
        
        file.taxa <-  file.taxa[!(file.taxa %in% files_lists)]
        
        assertthat::assert_that(
            assertthat::not_empty(file.taxa),
            msg = cli::cli_alert_danger("No files need processing!"))
        
        return(file.taxa)
    }
    
    file.name <- paste0(here::here("data", "metadata", "processed_files"), 
                                   ".csv")
    
    # ---- initialize file list ----
    if (is_empty(files_lists)) {
        cli::cli_alert_info("Creating a list of processed files!")
        cli::cli_alert_info(c("File created: {.file {basename(file.name)}}\n",
                              "Located in: {.file {dirname(file.name)}}"))
        
        tibble(files = file.taxa, time = Sys.time()) %>%
        write_csv(
            file.name
        )
    } else {
        cli::cli_alert_info("Appending the list of processed files!")
        cli::cli_alert_info(c("File appended: {.file {basename(file.name)}}\n",
                              "Located in: {.file {dirname(file.name)}}"))
        
        files_lists %>%
            read_csv(., show_col_types = FALSE) %>%
            bind_rows(tibble(files = file.taxa, time = Sys.time())) %>%
            write_csv(
                file.name
            )
    }
}
