# will create a set directory if does not exists
# useful for new projects

showTree <- function(showtree = TRUE) {
    if (showtree) {
        cli::cli_text("Here is the {.strong project} structure:")
        fs::dir_tree(type = "directory")
    }
}

subDir_deflt <- c(
    "data/raw",
    "data/processed",
    "data/plots",
    "data/metadata",
    "Rmd",
    "scripts"
)

cust_dir <- c(
    "data/processed/ind_file_merg",
    "data/metadata/aphia_id",
    "data/metadata/cruise_logsheets"
)

subDir <- here::here(c(subDir_deflt, cust_dir))
subDir <- subDir[which(!fs::dir_exists(subDir))]

show   <- FALSE
if (!length(subDir) == 0) {
    cli::cli_alert_info("Creating {length(subDir)} director{?y/ies}.")
    cli::cli_ul(subDir, .close = T)
    show <- TRUE
    } else {
        cli::cli_alert_info("No new directories created.")
        }

fs::dir_create(path = subDir)
if (show) showTree(showtree = TRUE)
rm(subDir_deflt, cust_dir, subDir)



