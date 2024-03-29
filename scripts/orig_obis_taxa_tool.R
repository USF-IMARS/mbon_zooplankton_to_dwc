orig.obis.taxa <- function (names, ask = TRUE) 
{
    f <- as.factor(names)
    indices <- as.numeric(f)
    unames <- levels(f)
    pages <- split(unames, as.integer((seq_along(unames) - 1)/50))
    paged_worms_taxamatch_call <- function(page) {
        print(page)
        obistools:::cache_call(page, expression(worrms::wm_records_taxamatch(page)))
    }
    matches <- unlist(lapply(pages, paged_worms_taxamatch_call), 
                      recursive = FALSE)
    results <- data.frame(scientificName = character(), scientificNameID = character(), 
                          match_type = character(), stringsAsFactors = FALSE)
    no <- NULL
    multiple <- NULL
    for (i in 1:length(matches)) {
        if (is.data.frame(matches[[i]])) {
            if (nrow(matches[[i]]) > 1) {
                multiple <- c(multiple, unames[i])
            }
        }
        else {
            no <- c(no, unames[i])
        }
    }
    message(sprintf("%s names, %s without matches, %s with multiple matches", 
                    length(unames), length(no), length(multiple)))
    if (ask) {
        proceed <- NA
        while (is.na(proceed)) {
            r <- readline(prompt = "Proceed to resolve names (y/n/info)? ")
            if (r == "y") {
                proceed <- TRUE
            }
            else if (r == "n") {
                proceed <- TRUE
                ask <- FALSE
            }
            else if (substr(r, 1, 1) == "i") {
                print(multiple)
            }
        }
    }
    for (i in seq_along(matches)) {
        row <- list(scientificName = NA, scientificNameID = NA, 
                    match_type = NA)
        match <- matches[[i]]
        if (is.data.frame(match) & nrow(match) > 0) {
            if (nrow(match) == 1) {
                row$scientificName = match$scientificname
                row$scientificNameID = match$lsid
                row$match_type = match$match_type
            }
            else if (ask) {
                print(match %>% select(AphiaID, scientificname, 
                                       authority, status, match_type))
                message(unames[i])
                n <- readline(prompt = "Multiple matches, pick a number or leave empty to skip: ")
                s <- as.integer(n)
                if (!is.na(n) & n > 0 & n <= nrow(match)) {
                    row$scientificName = match$scientificname[s]
                    row$scientificNameID = match$lsid[s]
                    row$match_type = match$match_type[s]
                }
            }
        }
        results <- bind_rows(results, row)
    }
    return(results[indices, ])
}