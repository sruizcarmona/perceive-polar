############################################################################################################
## WRAPPER FUNCTION 
## check for errors in file and activity
## process file
###
## input filename and id.id
## output activity info
############################################################################################################
process_polarfile <- function(zipfile, filename, id, maxhr) {
  
  # first, check if file is too big (20MB), and ignore (errors in shinyapps)
  if(get_list_files(zipfile) %>% filter(Name == filename) %>% pull(Length) > 20000000) {
    act.err <- onerow.df(c(id,rep('file error (too big)', length(act.err.names)-2),filename), act.err.names) %>% mutate(date = NA_Date_)
    return(act.err)
  }
  
  # read file and create polar
  # polar_file <- unz(zipfile, filename)
  polar <- try(readTCX(zipfile, filename), silent = T)
  
  
  ###################
  # check errors in file or before processing the activity
  # skip files with errors
  if (class(polar) == "try-error"){
    act.err <- onerow.df(c(id,rep('file error (reading)', length(act.err.names)-2),filename), act.err.names) %>% mutate(date = NA_Date_)
    return(act.err)
  }
  
  # discard if there are no records or it's shorter than a minute
  if (is.null(dim(polar)) || dim(polar)[1] < 60) {
    act.err <- onerow.df(c(id,rep('activity error (short/no data)',length(act.err.names)-2),filename), act.err.names) %>% mutate(date = NA_Date_)
    return(act.err)
  }
  
  # discard if activity contains more than 1 row
  # if(length(polar$session$sport) > 1 | length(polar$sport$sport) > 1) {
  #   act.err <- onerow.df(c(id.id,rep('activity error (multiple sports/rows)',length(act.err.names)-2),filename), act.err.names) %>% mutate(date = NA_Date_)
  #   return(act.err)
  # }
  # PERCEIVE comment
  
  ###################
  # process activity
  ###################
  act <- try(get_act_info_from_polar(polar, maxhr, etrimp_addon = T), silent=T)
  # print(act) #debug
  ###################
  # check errors in activity
  # skip files with errors in processing activity
  if (class(act) == "try-error"){
    act.err <- onerow.df(c(id, rep('activity error (unknown)', length(act.err.names) - 2),filename), act.err.names) %>% mutate(date = NA_Date_)
    return(act.err)
  }
  # act hr higher than maxhr
  # if(!is.na(act$maxhr_activity) & act$maxhr_activity > act$maxhr_participant) {
  #   act.err <- onerow.df(c(id,rep('activity error (maxHR too high)',length(act.err.names)-2),filename),act.err.names)
  #   return(act.err)
  # }
  # timestamp missing
  if(is.na(act$date)) {
    act.err <- onerow.df(c(id,rep('activity error (missing timestamp)',length(act.err.names)-2),filename), act.err.names) %>% mutate(date = NA_Date_)
    return(act.err)
  }
  # wrong year (in the future), hard to find an auto fix and only 1 or 2 examples
  if (act$year > as.numeric(format(as.Date(Sys.Date()),"%Y"))) {
    act.err <- onerow.df(c(id,rep('activity error (wrong year)',length(act.err.names)-2),filename), act.err.names) %>% mutate(date = NA_Date_)
    return(act.err)
  }
  ###################
  # save activity
  ###################
  # update session to be counted and add the info to results df
  act.info <- cbind(id, act, file = filename)
  return(act.info)
}

############################################################################################################
############################################################################################################
############################################################################################################