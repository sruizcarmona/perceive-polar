############################################################################################################
## GET FILES FOR ATHLETE 
## given an athlete name, look for all the files and return list
###
## input athlete name
## output dlist of files
############################################################################################################

get_list_files <- function(zipfile) {
  files_in_zip <- unzip(zipfile, list = TRUE) %>% 
    filter(Length > 0, !str_detect(Name, "__MACOSX"), !str_detect(Name, ".DS_Store")) %>%
    arrange(Name) 
  return(files_in_zip)
}

get_files_id <- function(id, zipfile) {
  files_in_zip <- get_list_files(zipfile) %>% 
    pull(Name)
  id_files <- files_in_zip[str_detect(files_in_zip, paste0(id))]
  return(id_files)
}
