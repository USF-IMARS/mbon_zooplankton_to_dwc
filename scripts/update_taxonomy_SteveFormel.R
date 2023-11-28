# Update taxonomy to align with WoRMS

library(dplyr)
library(future)

occ <- readxl::read_excel("mbonFKNMSeDNA.xlsx", sheet = "ocurrence")
dna <- readxl::read_excel("mbonFKNMSeDNA.xlsx", sheet = "DNAderivedDataExtension")

# collapse taxonomy to verbatimIdentification

occ <- occ %>%
  tidyr::unite("verbatimIdentification", Kingdom:Species, sep = ";")

# Get new taxonomy from WorMS

# Set mode to parallel
plan(multisession, workers = 6)

x <-
  unique(
    sub(
      occ$scientificNameID,
      pattern = ".*:",
      replacement = ""
    )
  ) %>%
  as.numeric()


worms_search <- function(y) {
  worrms::wm_classification(y) %>%
    filter(rank %in% c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) %>%
    select(rank, scientificname) %>%
    tidyr::spread(key = rank, value = scientificname) %>%
    mutate(scientificNameID = paste0("urn:lsid:marinespecies.org:taxname:", y))
}

worms_out <-
  furrr::future_map(
    .x = x, ~ worms_search(y = .x),
    .options = furrr::furrr_options(seed = TRUE)
    # function(y){
    #
    # worrms::wm_classification(y) %>%
    #   filter(rank %in% c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) %>%
    #   select(rank, scientificname) %>%
    #   tidyr::spread(key = rank, value = scientificname) %>%
    #   mutate(scientificNameID = paste0("urn:lsid:marinespecies.org:taxname:", y))
    #
    # }
  ) %>%
  # purrr::list_rbind()
  data.table::rbindlist(., fill = TRUE) %>%
  select(., c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "scientificNameID")) %>%
  rename_with(., tolower)

plan(sequential)

# join results with occ
occ <- left_join(occ, worms_out, by = c("scientificNameID" = "scientificnameid"))


# write to csv instead of excel, for upload to IPT ------------------------

# write to new file
write.dwc <- function(..., fileEncoding = "UTF-8", row.names = F, na = "") {
  utils::write.csv(..., row.names = row.names, na = na)
}


lapply(c("occ", "dna"), function(x) {
  filename_string <-
    here::here(
      # paste(
      # "data_processed", "dwc",
      # # basename(rstudioapi::getActiveProject()),
      paste0(x, "_", Sys.Date(), ".csv")
      # Sys.Date()
    )
  print(get(x))
  print(filename_string)
  # write.dwc(get(x), file = paste0(filename_string, ".csv"))
  write.dwc(x = get(x), file = filename_string)
})

