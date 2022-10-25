#  ------------------------------------------------------------------------
#
# Title : Functions to get taxa IDs
#    By : Sebastian Di Geronimo
#  Date : 2022-10-20
#
#  ------------------------------------------------------------------------

if (!exists("my_funcs")) my_funcs <- FALSE

# ---- functions ----
# returns most recent modified file path
last_mod <-  function(fpath) {
    ftime <- file.mtime(fpath)           
    return(fpath[which.max(ftime)]) 
}

source(here::here("scripts", "match_taxa_fix.R"), local = my_funcs)

# ---- Load taxa list --------------------------------------------

taxa_list_check <- function(taxa_list, regex_lifestage, 
                            file_base, file_expr,
                            check = FALSE) {

    # load file most recent taxa with aphiaID 
    taxa_file <-  
        fs::dir_ls(path =  here::here("data", "metadata", "aphia_id"),
                   regexp = glue("{file_base}.*\\.csv$"))  %>%
        last_mod(.)

    # ---- initialize taxa file ----
    if (is_empty(taxa_file)) {
        cat("This is the creation of a taxanomic list!")
        # extract taxa names, separating names and life stages
        taxa.name <-
            taxa_list  %>% 
            select(taxa_orig = taxa) %>%
            mutate(
                taxa = str_remove(
                    taxa_orig, 
                    regex("sp{0,2}\\.|\\(unknown\\)|\\(.*\\)", 
                        ignore_case = TRUE)
                ),
                
                lifeStage = str_extract_all(
                    taxa,
                    regex(regex_lifestage,
                        ignore_case = TRUE),
                    simplify = TRUE
                ) %>% str_to_lower(),
                
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
        filename <- eval(file_expr)
        
        cat(sprintf("File created: '%s'", basename(filename)),
            sprintf("\nLocated in: '%s'", dirname(filename)))
        
        write_csv(taxa_matched, filename, na = "")
        
    } else {
        # ---- read taxa file ----
        cat(sprintf("Reading taxa list file: '%s' \n", basename(taxa_file)))
        
        taxa_matched <-  
            readr::read_csv(taxa_file, 
                            show_col_types = FALSE) 
    }
    
    # ---- optional check NAs ----
    if (check) {
        # ---- fix non-matched taxa names ----
        cat("Checking file if any NAs are present in scientificName.\n")
        taxa_matched <- taxa_unmatch(taxa_matched) 
    } else {
        cat("Not checking for NAs in aphiaID list")
    }
    
    return(taxa_matched)
}

# ---- Find unmatched taxa and fix --------------------------------------------
taxa_unmatch <- function(taxa_matched, regex_lifestage,
                         # file_base, 
                         file_expr, 
                         check = FALSE) {
    
    if (!check) {
        return(taxa_matched)
    }
    
    taxa_ntmtch <-
        taxa_matched  %>%
        distinct(taxa_orig, .keep_all = TRUE) %>%
        filter(is.na(scientificName)) %>%
        select(taxa_orig) %>%
        mutate(
            taxa = str_remove(
                taxa_orig, 
                regex("sp{0,2}\\.|\\(unknown\\)|\\(.*\\)", 
                      ignore_case = TRUE)
            ),
            
            lifeStage = str_extract_all(
                taxa,
                regex(regex_lifestage,
                      ignore_case = TRUE),
                simplify = TRUE
            ) %>% str_to_lower(),
            
            taxa = str_remove(
                taxa,
                regex(regex_lifestage,
                      ignore_case = TRUE)
            ) %>% str_trim()
        )
    
    # ---- run taxa matching with WoRMS database ----
    if (assertthat::not_empty(taxa_ntmtch)) {
        
        cat("Found NAs in scientificName.",
            "Re-running taxa search\n", sep = "\n")
        
        taxa_matched <- 
            match_taxa_fix(taxa_ntmtch$taxa, fuzzy = TRUE, ask = T) %>%
            bind_cols(taxa_ntmtch, .) %>%
            bind_rows(taxa_matched, .) %>%
            arrange(taxa_orig) %>%
            distinct(taxa_orig, .keep_all = TRUE)
        
        # ---- save WoRMS database search ----
        filename <-  eval(file_expr)
        
        cat(sprintf("Writing new file: '%s'", basename(filename)))

        write_csv(taxa_matched, filename, na = "")
        
    } else {
        cat("No NAs found")
    }
    
    return(taxa_matched)
}

# ---- Merge taxa with taxa list ----------------------------------------------
merge_taxa <- function(dat, file_base = "aphia_taxa", check = FALSE) {
    
    if (!check) message("Will not be checking for NAs in taxa aphiaID list.",
                    "\nSet check to `TRUE` if want to check.\n")
    
    file_expr <- expression(paste0(
        here::here("data", "metadata", "aphia_id", file_base),
        format(Sys.time(), '_%Y%m%d_%H%M%S'),
        ".csv")
    )
    
    # ---- larval stages ----
    regex_lifestage <- 
        paste0("copepodite|nauplii|larvae|larva|juvenile|",
               "eggs|egg|zoea|protozoea|cypris|megalopa")  
    
    taxa_list <- dat %>%
    select(taxa) 
    
    # load previous creation of aphiaID list or create new one
    taxa_aphia <- taxa_list_check(taxa_list = taxa_list, 
                                  regex_lifestage = regex_lifestage,
                                  file_base = file_base,
                                  file_expr = file_expr,
                                  check = check)
    
    # ---- merge data with taxa ----
    taxa_matched_merg <- right_join(
        taxa_aphia, 
        taxa_list,
        by = c("taxa_orig" = "taxa")) %>%
        distinct(taxa_orig, .keep_all = TRUE)

    taxa_unmatch(taxa_matched_merg, regex_lifestage = regex_lifestage,
                 file_expr, check = check) %>%
    right_join(., dat, by = c("taxa_orig" = "taxa"))
}

# ---- Load taxa list ---------------------------------------------------------
load_taxa_list <- function(file_base = "aphia_taxa") {
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
    
    return(taxa_matched)
}

# ---- Load data --------------------------------------------------------------
load_data <- function(file.taxa) {
    
    cli::cli_alert_info(basename(file.taxa))
    # find the number of skips to start of taxa information
    skips <-  which(readxl::read_xlsx(file.taxa, range = cell_cols("A"), 
                                      col_names = FALSE ) == "Taxa") - 1
    
    # extract values for calculating individuals per cubic meter
    calc <-
        readxl::read_xlsx(
            # file.taxa, 
            filetest,
            # n_max = skips,
            range = glue("A1:B{skips}"),
            col_names = c("x1", "x2")
            # col_names = c("x1", "x2", "x3")
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
            date_analyzed  = as.Date(as.numeric(date_analyzed), origin = "1899-12-30")
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
        filter(mean > 0)
    
    return(taxa)
}