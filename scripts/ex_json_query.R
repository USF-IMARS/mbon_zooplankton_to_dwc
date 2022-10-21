library(jsonlite) #https://cran.r-project.org/web/packages/jsonlite/
library(httr)

#Fill in the AphiaID you need
AphiaID <- 127160

#Build the URL to get the data from
url <- sprintf("https://www.marinespecies.org/rest/AphiaClassificationByAphiaID/%d", AphiaID);

#Get the actual data from the URL
classificationTree <- fromJSON(url)

#Walk the classification tree
currentTreeItem = classificationTree
while (!is.null(currentTreeItem )) {
    print(sprintf("ID: %d, RANK: %s, ScientificName: %s",
                  currentTreeItem$AphiaID,
                  currentTreeItem$rank,
                  currentTreeItem$scientificname
    ));
    #You can access the variables individually
    #print(currentTreeItem$AphiaID);
    #print(currentTreeItem$scientificname);
    #print(currentTreeItem$rank);
    
    #Get next item in the tree
    currentTreeItem <- currentTreeItem$child;
}


# info on how caching works
# saves the results in an rds file
# when creating a query, a unique identifier is created
# this is created from digest::digest, where is have same exact
# list of inputs, no matter the order, will create a unique cached
# name
# i.e. cachefile <- file.path(cache_dir, 
#                   paste0("call_", digest::digest(list(key = key, 
#                   expr = expr)), ".rds"))
# the key is the 
obistools::match_taxa
obistools:::cache_call
worrms:::cc
worrms:::as_log
worrms::wm_records_taxamatch

worrms:::as_log(1)
args <- worrms:::cc(list(marine_only = worrms:::as_log(1)))

args <- c(args, 
          stats::setNames(
              as.list(name), 
              rep("scientificnames[]", 
                  length(name)
              )
          )
)
worrms:::wm_GET
worrms:::wm_base()
wm_GET(file.path(worrms:::wm_base(), "AphiaRecordsByMatchNames"), 
       query = args, ...)

temp <- crul::HttpClient$new(url = file.path(worrms:::wm_base(), "AphiaRecordsByMatchNames"))$get(query = args)

jsonlite::fromJSON(temp$parse("UTF-8"), flatten = TRUE)

cache_call(page,
           expression(worrms::wm_records_taxamatch(page)))

cache_dir <- rappdirs::user_cache_dir("obistools")
cachefile <- file.path(cache_dir, paste0("call_",
                                         "09810d37d118d9881a6b5241e6ce82cf",
                                         # digest::digest(list(key = key, 
                                         # expr = expr)), 
                                         ".rds"))
readRDS(cachefile)

obistools::match_taxa
names <- c("copepoda", "cats", "copelandia", LETTERS, letters)
f <- as.factor(names)
indices <- as.numeric(f)
unames <- levels(f)
pages <- split(unames, as.integer((seq_along(unames) - 1)/50))
indices
unames
pages

key = pages$`0`
exprs = expression(worrms::wm_records_taxamatch(key))
digest::digest(list(key = key, expr = exprs), serialize = T)

cachefile <- file.path(cache_dir, paste0("call_", digest::digest(list(key = key, 
                                                                      expr = expr)), ".rds"))

result <- eval(exprs, envir = parent.frame(), enclos = NULL)

readRDS(cachefile)
saveRDS(result, cachefile)
rm(result)
readRDS(cachefile)

paged_worms_taxamatch_call <- function(page) {
    obistools:::cache_call(page, expression(worrms::wm_records_taxamatch(page)))
}
lapply(pages, paged_worms_taxamatch_call)
unlist(lapply(pages, paged_worms_taxamatch_call), 
       recursive = FALSE)


readRDS(file.path(cache_dir,"call_bf64a3920aaf23d252c413145359ebc5.rds"))

file.info(file.path(cache_dir))
