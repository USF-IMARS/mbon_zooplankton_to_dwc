
names = taxa.name %$% as.vector(Taxa)[1:7]
names = c("Acartia spp.",			
"Candacia spp.",
"Candacia spp.",
"Calanopia spp.")


f <- as.factor(names)
indices <- as.numeric(f)
unames <- levels(f)
pages <- split(unames, as.integer((seq_along(unames) - 1)/50))
results <- data.frame(scientificName = character(), scientificNameID = character(), 
                      match_type = character(), stringsAsFactors = FALSE)


f
indices
unames
pages

unames[[2]]

paged_worms_taxamatch_call <- function(page, fuz = fuzzy) {
    # tryCatch({
    #     obistools:::cache_call(page, expression(worrms::wm_records_taxamatch(page, fuzzy = fuz)))
    #
    # }, warning = function(w) {
    #     message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))
    #
    # }, error = function(e) {
    #     (message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]])))
    #
    #     page <- readline(prompt = "What do you want to try? ")
    # })
    taxa.match <- function(page, fuzzy) {
        obistools:::cache_call(page, expression(worrms::wm_records_taxamatch(page, fuzzy = fuz)))
        
    }
    
    withRestarts(
        tryCatch({
            taxa.match(page, fuzzy)
            
        }, warning = function(w) {
            message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))
            
        }, error = function(e) {
            if (tryfix) {
                (message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]])))
                invokeRestart("retry")
            }
        }),
        retry = function() {
            new.name <-
                new.name <- readline(prompt = sprintf("%s might be mispelled \nWhat do you want to try? (if blank, will skip) ", page))
            if (new.name == "") {
                message(sprintf("Skipping %s ", new.name))
            } else {
                paged_worms_taxamatch_call(new.name, fuzzy)
                
            }
        }
    )
    
} 

tryfix = TRUE
fuzzy = TRUE
matches <- unlist(lapply(setNames(pages$`0`, pages$`0`), paged_worms_taxamatch_call ), 
                  recursive = FALSE)
matches


test <-  paged_worms_taxamatch_call(names[7])

# this test if the matches have 1 match, multiple matches or no matches
no <- NULL
multiple <- NULL
for (i in 1:length(matches)) {
    if (is.data.frame(matches[[i]])) {
        if (nrow(matches[[i]]) > 1) {
            multiple <- c(multiple, unames[i])
            
        }
        if (nrow(matches[[i]]) < 1) {
            no <- c(no, unames[i])
        }
    }   else {
        no <- c(no, unames[i])
    }
}
message(sprintf("%s names, %s without matches, %s with multiple matches", 
                length(unames), length(no), length(multiple)))





multi_name <- function(match, unames, i, row) {
  print(match %>% select(AphiaID,
                         scientificname,
                         authority,
                         status,
                         match_type))
  message(unames[i])
  n <-
      readline(prompt = "Multiple matches, pick a number or leave empty to skip: ")
  s <- as.integer(n)
  if (!is.na(n) & n > 0 & n <= nrow(match)) {
      row$scientificName = match$scientificname[s]
      row$scientificNameID = match$lsid[s]
      row$match_type = match$match_type[s]
  }
}
