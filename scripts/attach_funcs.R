# ============================================================================ #
# ---- Attach Functions ----
# ============================================================================ #    
func_attach <- function() {
    #' Attach All Custom Functions
    #'
    #' This will attach all the functions to the `searchpath` of `my_funcs` if 
    #' it is not already attached.
    #'
    #' @return Functions and scripts
    #' @examples
    #' func_attach()
    if (!rlang::is_attached('my_funcs')) { 
        cli::cli_alert_info(
            c("Attaching project functions to the {.code searchpath} as ",
              "{.code my_funcs}."))
        
        # source scripts with functions in new environment
        my_funcs <- new.env()
        
        # create directory structure if does not exists and create tree map
        source(here::here("scripts","create_dir.R"), local = my_funcs)
        
        # copy files from a cloud service
        source(here::here("scripts", "copy_files_from_cloud.R"), local = my_funcs)
        
        # loading check for taxa 
        source(here::here("scripts", "taxa_list_check.R"), local = my_funcs)
        
        attach(my_funcs)
    } else {
        cli::cli_alert_info(c("Skipping attach,{.var my_funcs} is already ", 
                            "attached to the searchpath."))
    }
}
