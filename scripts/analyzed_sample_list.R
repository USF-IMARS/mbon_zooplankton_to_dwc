##%######################################################%##
#                                                          #
####        Create list of all samples analyzed         ####
#                                                          #
##%######################################################%##
##
librarian::shelf(
    librarian, conflicted, tibble, tidyr, dplyr, stringr,
    lubridate, glue, fs, magrittr, here
)

conflicts_prefer(
    dplyr::filter(), 
    dplyr::select()
)

# query folder in cloud directory for data names
sample_list <- 
    dir_ls(here(cloud_dir, "raw"), type = "directory", regexp = "samples") %>%
    dir_ls() %>%
    tibble(files = .) %>%
    filter(!str_detect(files, "blank")) %>%
    
    # extract basename 
    mutate(
        base = basename(files),
        base = tools::file_path_sans_ext(base)
    ) %>%
    
    # extract info from filename
    separate(base, c("cruise_id", "station", "mesh"), sep = "_")

# save file
sample_list %>%
    select(-1) %>%
    mutate(
        processed_by = "Natalia Lopez Figueroa"
    ) %>%
    writexl::write_xlsx(
        path = here(cloud_dir, "list_sample_analyzed.xlsx")
    )
