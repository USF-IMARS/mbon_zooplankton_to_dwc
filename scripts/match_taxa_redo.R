library(magrittr)
library(tidyverse)
library(glue)
library(cli)
taxa_type <- "taxa"
fuzzy <- TRUE
bad <- tibble(taxa_orig = "fdasf")

rename_taxa <- function(.x) {
    withRestarts({
        cli::cli_h3("")
        cli::cli_alert_danger("Nothing matched {cli::col_red(.x)}")
        new.name <- readline(cli::cli_text(
            c(
                "What do you want to try? ",
                "({cli::style_underline('if blank, will skip')})"
            )
        ))
        if (new.name == "") {
            cli_alert_warning("Skipping {cli::col_red(.x)}.")
            new.name <- NA_character_
            taxa_type <- NA
            # next
        } else {
            cli::cli_alert_info("Do you want to use common name? (y/n) ")
            cli::cli_text("(If you mispelled taxa, type {.code missed})")
            r <- readline("")
            taxa_type <-
                if_else(stringr::str_detect(r, "y|Y"), "common", "taxa")
            if (stringr::str_detect(r, "m|M")) {
                cli::cli_text(c("Seems you {col_yellow('mispelled')} your ",
                                "taxa, trying again!"))
                invokeRestart("retry")
            }
        }
        
        tibble(new.name = new.name, taxa_type = taxa_type)
        
    },
    retry = function()
        rename_taxa(.x))
}


{# taxa tak
tt <- load_taxa_list() %>%
    slice_head(n = 19) %>%
    add_row( bad, .before = 2)

# tt <- bad
ntw <- tt %>%
    select(taxa_orig = scientificName) %>%
    filter(!is.na(taxa_orig)) %>% 
    slice_tail(n = 8)

td <- 
    tt %>%
    select(taxa_orig) %>%
    slice_head(n = 12) %>%
    add_row(ntw) %>%
    print() %>%
    rename(taxa = 1) %>%
    mutate(
      indices = row_number(),
      add_count(., name = "levels"),
      # pages = as.integer((seq_along(taxa) - 1)/50)
      pages = as.integer((seq_along(taxa) - 1)/5)
    ) %>%
    arrange(taxa) %>%
    group_by(pages) %>%
      nest()

rst <- td %>%
    mutate(
        matches = purrr::map(data,  function(.x) {
        .x %>%
            mutate(AphiaID = paged_worms_taxamatch_call(
                taxa, .taxa_type = taxa_type, .fuzzy = fuzzy)) %>%
            unnest(AphiaID, keep_empty = TRUE)
    }))  %>%
    ungroup()

cmt <- rst %>%
    mutate(
        num = map(matches, function(.x) {
                .x %>%
                 count(taxa, AphiaID, name = "counts") %>%
                 mutate( 
                     counts = case_when(
                         is.na(AphiaID) ~ 0L,
                         TRUE ~ counts))
            })
    ) %>%
    unnest(num)

counting <- 
    tibble(type = c("No Match", "Multiple Matches", "Matched"),
           counts = c(0,0,0)) %>%
    full_join(.,cmt  %>%
    mutate(type = case_when(
        counts == 0 ~ "No Match",
        counts > 1 ~ "Multiple Matches",
        TRUE ~ "Matched")) %>%
    group_by(type) %>%
    count(name = "counts")) %>%
    arrange(type, desc(counts)) %>%
    distinct(type, .keep_all = TRUE)
}


{
cli::cli_inform("Info on matches:")
cli::cli_ul("{counting$counts[1]} {counting$type[1]}")
cli::cli_ul("{counting$counts[3]} {counting$type[3]}")
cli::cli_ul("{counting$counts[2]} {counting$type[2]}")
}


ask = TRUE
if (ask) {
    proceed <- NA
    while (is.na(proceed)) {
        cli::cli_h3("")
        r <- readline(
            prompt = cli::cli_inform("Proceed to resolve names (y/n/info)? "))
        if (stringr::str_detect(r,"y|Y"))  proceed <- TRUE
        if (stringr::str_detect(r,"n|N")) {
            proceed <- TRUE
            ask     <- FALSE
        }
        if (stringr::str_detect(r,"i|I")) {
            cli::rule(center = crayon::underline("Information"))
            cli::cli_alert_info("Taxa with {cli::col_green('matched ID')}:")
            filter(cmt, counts == 1) %$%
                cli::cli_ul(taxa)
            Sys.sleep(1.75)
            cli::cli_h3("")
            cli::cli_alert_info(c("Taxa with ",
                                "{cli::col_yellow('multiple matched ID')} :"))
            filter(cmt, counts > 1) %$%
                cli::cli_ul(taxa)
            Sys.sleep(1.75)
            cli::cli_h3("")
            cli::cli_alert_info("Taxa with {cli::col_red('no matched ID')}:")
            filter(cmt, counts == 0) %$%
                cli::cli_ul(taxa)
            Sys.sleep(3)
        }
    }
}
    

{
    
taxa_extract <-  rst %>%
unnest(matches) %>% 
select(-c(1,2))

matched <- taxa_extract %>%
    filter(!is.na(scientificname)) %>%
    group_by(scientificname) %>%
    filter(n() == 1) 

no_match <- taxa_extract %>%
    filter(is.na(scientificname)) %>%
    select(taxa, indices)

multiple <- taxa_extract %>%
    filter(!is.na(scientificname)) %>%
    group_by(indices) %>%
    # group_by(scientificname) %>%
    filter(n() > 1) 

}
# ---- search mutliple or no match ----
# TODO: multiple matches
# TODO: no matches
# no matches
if (ask & nrow(no_match) > 0 ) {
    cli::cli_h2("Checking no matches")
    new_names <- no_match %>%
    mutate(
        new_try = map(taxa, ~ rename_taxa(.x)) 
    ) %>%
      unnest(new_try) %>%
        mutate(
            AphiaID = 
                purrr::map2(.x = new.name, .y = taxa_type,  
                        function(.x, .y) {
                    paged_worms_taxamatch_call(
                    .x, .taxa_type = .y, .fuzzy = TRUE, .tryfix = TRUE)  %>%            
                    { if (.y == "taxa") {
                    tibble(AphiaID = .) %>%
                    unnest(AphiaID, keep_empty = TRUE)
                    } else {.}}
        })
    ) %>%
      unnest(AphiaID) 
    
    multiple <-
        new_names %>%
        filter(!is.na(scientificname)) %>%
        group_by(indices) %>%
        filter(n() > 1) %>%
        select(-new.name, -taxa_type) %>%
        bind_rows(multiple, .)
    
    matched <- 
        new_names %>%
        filter(!is.na(scientificname)) %>%
        group_by(indices) %>%
        filter(n() == 1) %>%
        select(-new.name, -taxa_type) %>%
        bind_rows(matched, .)
    
    }


# multiple
if (ask & nrow(multiple) > 0) {
    
}





# ---- all together now! ----
tt %>%
    select(taxa_orig) %>%
    left_join(., matched, by = c("taxa_orig" = "taxa")) %>%
    select(taxa_orig, scientificname, lsid, match_type)



