############################################################################################################
## SINGLE ROW DF (REPETITIVE CALL)
## return a df of 1 row and colnames for error activities
############################################################################################################
onerow.df <- function(values, colnames){
  return(setNames(data.frame(matrix(values, ncol=length(colnames), nrow = 1), stringsAsFactors = FALSE), colnames))
}

############################################################################################################
## SMOOTH DATA
## input point and return them smoothed with a window w
###
## input point vector and window w
## output same vector with the data points smoothed with a window w
############################################################################################################

smooth.data <- function(data,w,r=2){
  sm <- rollapply(data,width=w,function(...) {mean(...,na.rm=T)},partial=T,align="center")
  return(round(sm,r))
}

# error names to populate table
act.err.names <- c("id","datenum","date","year","month","week","start_time","duration_min","total_dist_km","device_model_name","sport", "sport_f",
                   "maxhr_participant","maxhr_activity","maxhr_perc","maxhr_intensity","hr_avg",
                   "hr_z0","hr_z1","hr_z2","hr_z3","hr_z4","hr_z5",
                   "hr_z0_time","hr_z1_time","hr_z2_time","hr_z3_time","hr_z4_time","hr_z5_time","etrimp","file")