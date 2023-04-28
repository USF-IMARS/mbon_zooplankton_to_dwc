# ============================================================================ #
# ---- Misc Functions ----
# ============================================================================ #    

##%######################################################%##
#                                                          #
####              Most Recent Created File              ####
#                                                          #
##%######################################################%##
#' Most Recent Created File
#'
#' This function should be used after an `fs::dir_ls` search with a specific 
#' file that may have multiple versions. When there are multiple matches to 
#' a search, this will take the most recent version of it.
#'
#' @param fpath The fs_path object created from `fs::dir_ls`
#' @param check Optionally check the most recent file. This can be set to 
#' either `TRUE` or `FALSE`.
#'
#' @return A vector of the most recent created file as `fs_path` object
#' @examples
#' # NA
last_mod <-  function(fpath, check = TRUE) {

    if (!check) return(fpath)
    
    ftime <- file.mtime(fpath) 
    
    return(fpath[which.max(ftime)]) 
    
    # ---- end of function ----
}