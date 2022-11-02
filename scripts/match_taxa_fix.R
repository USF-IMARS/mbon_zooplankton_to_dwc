

# TODO: remove
test_names = c("Acartia spp.",			
          "Candacia spp.",
          "Candacia spp.",
          "Calanopia spp.")

test_names = c("Ac")



# selects worrms ID search by taxonomic or common name
taxa.match <- function(.page, .fuzzy, .taxa_type = "taxa") {
    # select type of search
    if (.taxa_type == "taxa") {
        cli::cli_h2("Searching {.strong scientific} names\n")
        obistools:::cache_call(
            .page, 
            expression(worrms::wm_records_taxamatch(.page, fuzzy = .fuzzy)))
    } else if (.taxa_type == "common") {
        cli::cli_h2("Searching {.strong common} names\n")
        obistools:::cache_call(
            .page, 
            expression(worrms::wm_records_common(.page, fuzzy = .fuzzy)))
    }
}

# used to match taxa with WoRMS ID
paged_worms_taxamatch_call <- function(page, 
                                       .fuzzy = FALSE, 
                                       .taxa_type = "taxa", 
                                       .tryfix = FALSE) {
    withRestarts(
        tryCatch({
            taxa.match(page, .fuzzy, .taxa_type)
        }, warning = function(w) {
            message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))
            
        }, error = function(e) {
            if (.tryfix) {
                # message(sprintf("Couldn't find `%s`\n", page))
                cli::cli_alert_danger(c("Couldn't find ", 
                                      "{.emph {cli::style_underline(page)}}\n"))
                
                invokeRestart("retry")
            } 
        }),
        retry = function() {
            cli::cli_alert_warning(c("{.emph {cli::style_underline(page)}} ",
                                     "might be mispelled!"))
            new.name <- readline(
                cli_text(c("What do you want to try? ", 
                           "({cli::style_underline('if blank, will skip')})")))
            
            if (new.name == "") {
                cli_alert_warning("Skipping {cli::style_underline(page)}.")
                
            } else {
                r <- readline(
                    cli::cli_inform("Do you want to use common name? (y/n)"))
                if (stringr::str_detect(r,"y|Y",)) {
                    paged_worms_taxamatch_call(new.name, 
                                               .fuzzy = TRUE, 
                                               .taxa_type = "common", 
                                               .tryfix = TRUE)
                } else {
                    paged_worms_taxamatch_call(new.name, 
                                               .fuzzy = TRUE, 
                                               .taxa_type = "taxa", 
                                               .tryfix = TRUE)
                }
            }
        }
    )
} 
{
cli_alert_danger("Danger alert")
cli_alert_warning("Warning alert")
cli_alert_info("Info alert")
cli_alert_success("Success alert")
}
page = test_names
test <- paged_worms_taxamatch_call(test_names, TRUE, .tryfix = TRUE)

stop()
match_taxa_fix <- function(names, ask = TRUE, fuzzy = TRUE, taxa_type = "taxa") 
{
    
    # functions ------------------------------
    
    
   
    
    # function if taxa name has multiple lsid's, to choose one, if not, NA
    multi_name <- function(match, unames, row) {
        message(unames)
        print(match %>% dplyr::select(AphiaID,
                               scientificname,
                               authority,
                               status,
                               match_type))
        n <-
            readline(prompt = 
                         cli_text("Multiple matches, pick a number or leave empty to skip, -1 to rename: "))

        if (!is.na(as.numeric(n))) {
            s <- as.integer(n)
        } else if (str_length(n) > 0) {
            s <- -1
        } else {
            message(sprintf("Skipping %s", unames))
            return(row)
        }
        print(n)
        print(s)
        
        
        if (!is.na(s) & s > 0 & s <= nrow(match)) {
            row$scientificName = match$scientificname[s]
            row$scientificNameID = match$lsid[s]
            row$match_type = match$match_type[s]
        }
        if (!is.na(s) & s == -1) {
            if (str_length(n) > 1) {
                new.name <- n
                cat(sprintf("Using name: %s", new.name))
            } else {
            new.name <- readline(prompt = sprintf(
                "%s failed, what do you want to try? This can be a common name. ", 
                unames[i]))
            }
            r <- readline(prompt = sprintf(
                "Do you want to use common name? (y/n) "))
            if (r == "y") {
                taxa_type = "common" 
            } else {
                taxa_type = "taxa" 
            }
            
            new.match <-
                paged_worms_taxamatch_call(new.name,
                                           .fuzzy = fuzzy,
                                           .tryfix = TRUE,
                                           .taxa_type = taxa_type)
            
            if (inherits(new.match, "list")) {
                new.match <- cbind(new.match[[1]])
            }
            if (!is.null(new.match) && nrow(new.match) == 1) {
                row$scientificName = new.match$scientificname
                row$scientificNameID = new.match$lsid 
                row$match_type = new.match$match_type
            } else if (!is.null(new.match) && nrow(new.match) > 1) {
                row <- multi_name(new.match, unames[i], row)
            }  
        } 
        return(row)
    }
    
    # name and setup variables ------------------------------
    f <- as.factor(names)
    indices <- as.numeric(f)
    unames <- levels(f)
    
    # creates vector of species below 50 (to decrease search problems)
    pages <- split(unames, as.integer((seq_along(unames) - 1)/50))
    
    # creates NA dataframe to add data to later
    results <- data.frame(scientificName = character(), scientificNameID = character(), 
                          match_type = character(), stringsAsFactors = FALSE)
    
    # main part of script  ------------------------------
    # attempt to find lsid from taxa name
    matches <- unlist(lapply(pages, paged_worms_taxamatch_call, .taxa_type = taxa_type, .fuzzy = fuzzy), 
                      recursive = FALSE)
    
    # creates empty lists if all taxa don't return name
    if (is.null(matches)) {
        matches <- vector(mode = "list", length = length(indices))
        # nm <- paste0(as.integer((seq_along(unames) - 1)/50))
        nm <- names(unlist(lapply(pages, function(page) seq_along(page)), recursive = F))
        names(matches) <- nm
    }
    
    # info on # of matches, # of no match, and # of multiple matches
    no <- NULL
    multiple <- NULL
    no.data <-  NULL
    for (i in 1:length(matches)) {
        if (is.data.frame(matches[[i]])) {
            if (nrow(matches[[i]]) > 1) {
                multiple <- c(multiple, unames[i])
                
            }
            if (nrow(matches[[i]]) < 1) {
                no <- c(no, unames[i])
                no.data <- c(no.data, unames[i])
            }
        }   else {
            no <- c(no, unames[i])
            no.data <- c(no.data, unames[i])
        }
    }
    
    message(sprintf("%s names, %s without matches, %s with multiple matches", 
                    length(unames), length(no), length(multiple)))

    
    # if ask is TRUE, ask if want to try to fix data
    if (ask) {
        proceed <- NA
        while (is.na(proceed)) {
            r <- readline(prompt = "Proceed to resolve names (y/n/info)? ")
            if (substr(r, 1, 1) == "y") {
                proceed <- TRUE
            }
            else if (substr(r, 1, 1) == "n") {
                proceed <- TRUE
                ask <- FALSE
            }
            else if (substr(r, 1, 1) == "i") {
                if (!is.null(no.data)) cat("Taxa with no ID matches:\n", no.data, sep = "\n"); cat("\n")
                
                if (!is.null(multiple)) cat("Taxa with multiple ID matches:\n",multiple, sep = "\n")
                Sys.sleep(4)
            }
        }
    }

    # create results for each organism
    for (i in seq_along(matches)) {
        # create variable to accept each specie as a list with 3 datum
        row <- list(
            scientificName = NA,
            scientificNameID = NA,
            match_type = NA
        )
        # match is new var that is the single list specie
        match <- matches[[i]]

        if (is.data.frame(match) && !is.null(match) && nrow(match) > 0) {
            if (nrow(match) == 1) {
                row$scientificName = match$scientificname
                row$scientificNameID = match$lsid
                row$match_type = match$match_type
            }
            else if (ask) {
                row <- multi_name(match, unames[i],  row)
            } 
        } else {
            if (ask) {
                    new.name <- readline(prompt = sprintf(
                        "%s failed, what do you want to try? This can be a common name. ", 
                        unames[i]))
                    r <- readline(prompt = sprintf(
                        "Do you want to use common name? (y/n) "))
                    if (r == "y") {
                        taxa_type = "common" 
                    } else {
                        taxa_type = "taxa" 
                    }
                    new.match <-
                        paged_worms_taxamatch_call(new.name,
                                                .fuzzy = fuzzy,
                                                .tryfix = TRUE,
                                                .taxa_type = taxa_type)
                    
                    if (inherits(new.match, "list")) {
                        new.match <- cbind(new.match[[1]])
                    }
                    if (!is.null(new.match) && nrow(new.match) == 1) {
                        row$scientificName = new.match$scientificname
                        row$scientificNameID = new.match$lsid 
                        row$match_type = new.match$match_type
                    } else if (!is.null(new.match) && nrow(new.match) > 1) {
                        row <- multi_name(new.match, unames[i], row)
                    } 
                
            } 
        }
        
        results <- bind_rows(results, row)
    }
    return(results[indices, ])
}
