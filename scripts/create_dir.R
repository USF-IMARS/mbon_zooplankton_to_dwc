# will create a set directory if does not exists
# useful for new projects

showTree <- function(showtree = TRUE) {
    # treees <- data.frame(
    #     stringsAsFactors = FALSE,
    #     package = c(
    #         here::here(),"data","raw","processed","plots",
    #         "metadata", "Rmd", "scripts", "ind_file_merg", "aphia_id"
    #     ),
    #     dependences = I(
    #         list(
    #             c("data", "Rmd", "scripts"),
    #             c("metadata", "plots", "processed", "raw"),
    #             character(0), c("ind_file_merg"), character(0), c("aphia_id"),
    #             character(0), character(0), character(0), character(0))))
    if (showtree) {
        cli::cli_text("Here is the {.strong project} structure:")
        fs::dir_tree(type = "directory")
        # print(cli::tree(treees))
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
    "data/metadata/aphia_id"
)
subDir <- here::here(c(subDir_deflt, cust_dir))
subDir <- subDir[which(!fs::dir_exists(subDir))]

if (!length(subDir) == 0) {
    cli::cli_alert_info("Creating {length(subDir)} director{?y/ies}.")
    cli::cli_ul(subDir, .close = T)
    showTree(showtree = TRUE)
    } else {
        cli::cli_alert_info("No new directories created.")
        }

fs::dir_create(path = subDir)
rm(subDir_deflt, cust_dir, subDir)



