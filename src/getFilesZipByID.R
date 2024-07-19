############################################################################################################
## GET FILES FOR ATHLETE 
## given an athlete name, look for all the files and return list
###
## input athlete name
## output dlist of files
############################################################################################################

get_list_files <- function(id, zipfile) {
  files_in_zip <- unzip(zipfile, list = TRUE) %>% 
    filter(Length > 0, !str_detect(Name, "__MACOSX")) %>% 
    arrange(Name) %>% 
    pull(Name)
  ath_files <- files_in_zip[str_detect(files_in_zip, paste0(id))]
  return(ath_files)
}