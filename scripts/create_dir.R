# will create a set directory if does not exists
# useful for new projects

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
    cat("Creating directory(s):", subDir, sep = "\n\t")
} else {
    cat("No new directories created.")
}

fs::dir_create(path = subDir)
rm(subDir_deflt, cust_dir, subDir)