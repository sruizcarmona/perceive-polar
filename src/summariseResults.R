############################
# summarise results from main run
# perceive_all as input, return all activities cleaned, errors, duplicates and participant summary
############################


sum_results <- function(perceive) {
  # process results to find errors, duplicates, etc
  error.sessions <- perceive %>% filter(str_detect(datenum,"error")) %>% select(id, file, reason = datenum) %>% 
    mutate(file_id = paste0(id, sapply(str_split(file,"/"),function(x) paste0('/*', str_sub(last(x),-36,-1)))))  %>% 
    select(-file)
  # update all.activities
  all.activities <- perceive %>%
    filter(!str_detect(datenum,"error")) %>% 
    mutate(file_id = paste0(id, sapply(str_split(file,"/"),function(x) paste0('/*', str_sub(last(x),-36,-1))))) %>% 
    mutate_if(is.factor, as.character) %>% 
    select(-file)
  # clean columns, so they all have the correct type (numbers are not chars)
  all.activities <- type.convert(all.activities, as.is=T)
  # get total time in hr zones
  all.activities <- all.activities %>% 
    mutate(hr_zones_total_time = hr_z0_time + hr_z1_time + hr_z2_time + hr_z3_time + hr_z4_time + hr_z5_time,
           hr_zones_total_perc = hr_z0 + hr_z1 + hr_z2 + hr_z3 + hr_z4 + hr_z5) %>% 
    relocate(hr_zones_total_time, .after = hr_z5_time) %>%
    relocate(hr_zones_total_perc, .after = hr_z5)
  ############################
  # summarise all results
  perceive_info <- all.activities %>%
    group_by(id) %>%
    summarise(total_activities=n(), 
              # fix categories once correct sports are taken (resistance vs aerobic maybe?)
              n_cycling=sum(sport =="Cycling", na.rm = T),
              n_running=sum(sport =="Running", na.rm = T),
              n_other=sum(!grepl("Running|Cycling",sport )),
              n_heart=sum(!is.na(hr_avg)),
    ) %>% 
    mutate(perc_heart=round(n_heart/total_activities*100,1),
    )
    
  
  # add error and duplicate count
  for (i in perceive_info$id) {
    perceive_info[perceive_info$id==i,'n_error'] <- sum(!is.na(str_extract(error.sessions$file,i)))
  }
  # add year count
  year.info <- all.activities %>% group_by(id,year) %>% tally() %>% mutate(n=as.numeric(n)) %>% arrange(year)
  for (i in perceive_info$id) {
    for (year in unique(year.info$year)){
      year_n <- as.numeric(year.info[as.character(year.info$id) == i & year.info$year == year,"n"])
      perceive_info[perceive_info$id==i,paste0("n_",year)] <- if(is.na(year_n)){0}else{year_n}
    }
  }
  # reorganize columns
  perceive_info <- perceive_info %>% 
    relocate(n_error, .after = total_activities)
  return (list(perceive_info=perceive_info,
               all.activities=all.activities,
               error.sessions=error.sessions))
}
