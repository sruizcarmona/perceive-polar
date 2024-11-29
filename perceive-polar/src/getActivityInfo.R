############################################################################################################
## MAIN FUNCTION 
## extract heart rate from a file
###
## input filename and maxHR
## output dataframe with hr,power, speed and correlations in columns
############################################################################################################
get_act_info_from_polar <- function(polar, maxhr, etrimp_addon) {
  ####
  #process
  ####
  # get timezone from lat lon and correct times
  tz <- try(replace_na(tz_lookup_coords(na.omit(polar$LatitudeDegree)[1], na.omit(polar$LongitudeDegrees)[1], method = "fast", warn = F), 
                       'Australia/Melbourne'), silent = T)
  if (class(tz) == "try-error"){ tz <- 'Australia/Melbourne'}
  
  polar <- polar %>% 
    mutate(Time = ymd_hms(Time, tz=tz, quiet = T))
  
  #other checks and name changes for consistency
  polar <- polar %>%
    plyr::rename(replace=c(AltitudeMeters="altitude",
                           HeartRateBpm="heart_rate",
                           Time="datetime",
                           DistanceMeters="distance",
                           Cadence="cadence",
                           LatitudeDegrees="lat",
                           LongitudeDegrees="lon",
                           Watts="power",
                           SensorState = "sensor"
    ),
    warn_missing=FALSE) %>%
    # select(time, one_of("lat", "lon", "altitude", "power", "sensor", "distance", "heart_rate")) %>%
    select_if(colnames(.) %in% c("datetime", "lat", "lon", "distance", "heart_rate")) %>% 
    # select(datetime, one_of("lat", "lon", "distance", "heart_rate")) %>%
    # 'heart_rate' column, if it exists, is always numeric, and the pipeline continues without errors if the column does not exist
    mutate_at(if('heart_rate' %in% names(.)) 'heart_rate' else integer(0), as.numeric) %>%
    mutate(
      day=format(as.POSIXct(datetime),"%y%m%d"),
      timeofday=format(as.POSIXct(datetime),"%H:%M:%S"))
  
  ###################
  # initialize activity (a) summary
  ###################
  start_time <- polar$datetime[1]
  start_date <- as_date(start_time)
  a <- onerow.df(format(start_date,"%Y%m%d"),"datenum")
  a$date <- start_date
  a$year <- format(start_date,"%Y")
  a$month <- format(start_date,"%m")
  a$week <- format(start_date,"%W")
  a$start_time <- as.character(format(start_time,"%H:%M"))
  a$duration_min <- round(as.numeric(difftime(last(polar$datetime),first(polar$datetime), units="mins")), 2)
  a$total_dist_km <- if('distance' %in% names(polar)){round(last(polar$distance)/1000,2)} else {NA}
  # add info about device (brand and product code and name)
  a$device_model_name <- ifelse(!is.null(attributes(polar)$device_model_name),
                                attributes(polar)$device_model_name,
                                "unknown")
  # sport info
  a$sport <- ifelse(!is.null(attributes(polar)$sport),
                      attributes(polar)$sport,
                      NA)
  # heart stuff
  a$maxhr_participant <- maxhr
  ###################
  # remove first and last 10 seconds, to reduce risk of peaks in sensor pairing, gps or other stuff
  # fd <- polar$record[11:(dim(polar$record)[1]-10),]
  # comment above, as it is not needed for perceive
  hr <- polar %>% select(heart_rate)
  rownames(hr) <- NULL
  
  ###################
  # get info from heart_rate
  # check that heart_rate is present in polar and its above 10%, otherwise return NA
  if ("heart_rate" %in% names(hr) & sum(!is.na(hr))/length(hr) > 0.1 & sum(hr != 0, na.rm=T)/length(hr) > 0.1){
    # if (sum(!is.na(hr))/length(hr) > 0.1 & sum(hr != 0, na.rm=T)/length(hr) > 0.1){
    # if, after that, the first 20 values are above 180, clean them too, as they are probably artifacts
    # if(any(hr[1:20] > 180, na.rm = T)){
    #   hr[1:20] <- NA
    # }
    ## comment above for perceive
    # clean hr=0, as sometimes the sensor fails
    hr$heart_rate[hr$heart_rate == 0] <- NA
    # smooth hr
    # hr above maxHR reduce to maxHR as it has already been processed when getting the zones (heavier smooth)
    # smooth.hr <- smooth.data(hr,10)
    # smooth.hr[smooth.hr=="NaN"] <- NA
    ## PERCEIVE no smooth for now
    # smooth.hr[smooth.hr > ath.maxHR] <- ath.maxHR
    # process all from smooth.hr
    a$maxhr_activity <- round(max(hr$heart_rate, na.rm=T),0)
    a$maxhr_perc <- round(a$maxhr_activity/a$maxhr_participant*100,1)
    a$maxhr_intensity <- round(a$maxhr_perc * a$duration_min,1)
    a$hr_avg <- round(mean(hr$heart_rate,na.rm=T),0)
    # get hr_zones and times (standard %s)
    hr_zones <- quantile(c(0:a$maxhr_participant),probs=seq(0,1,by=0.1))
    hr$hr_zones <- findInterval(hr$heart_rate,hr_zones[6:10])
    hr_zones_table <- round(table(hr$hr_zones)/length(hr$hr_zones)*100,2)
    hr_zones_table[c("0","1","2","3","4","5")[!c("0","1","2","3","4","5") %in% names(hr_zones_table)]] <- 0
    a$hr_z0 <- as.numeric(hr_zones_table['0'])
    a$hr_z1 <- as.numeric(hr_zones_table['1'])
    a$hr_z2 <- as.numeric(hr_zones_table['2'])
    a$hr_z3 <- as.numeric(hr_zones_table['3'])
    a$hr_z4 <- as.numeric(hr_zones_table['4'])
    a$hr_z5 <- as.numeric(hr_zones_table['5'])
    a$hr_z0_time <- as.numeric(round(a$hr_z0/100 * a$duration_min,1))
    a$hr_z1_time <- as.numeric(round(a$hr_z1/100 * a$duration_min,1))
    a$hr_z2_time <- as.numeric(round(a$hr_z2/100 * a$duration_min,1))
    a$hr_z3_time <- as.numeric(round(a$hr_z3/100 * a$duration_min,1))
    a$hr_z4_time <- as.numeric(round(a$hr_z4/100 * a$duration_min,1))
    a$hr_z5_time <- as.numeric(round(a$hr_z5/100 * a$duration_min,1))
    # calculate trimp scores
    a$etrimp <- round(a$hr_z1_time * 1 + a$hr_z2_time * 2 + a$hr_z3_time * 3 + a$hr_z4_time * 4 + a$hr_z5_time * 5,2)
    # recalculate hravg, hrmax and etrimp score with 5 min increments starting from 0 ending at 100% of duration
    if (etrimp_addon == TRUE) {
      p_inc <- polar %>% 
        mutate(inc = as.numeric(difftime(datetime,first(polar$datetime), units="mins")))
      for (i in seq(5, a$duration_min, by = 5)){
        ainc <- a %>% select(datenum, maxhr_participant)
        hrinc <- p_inc %>% select(heart_rate) %>% filter(p_inc$inc <= i)
        hrinc$heart_rate[hrinc$heart_rate == 0] <- NA
        rownames(hrinc) <- NULL
        avgcol <- paste0("hr_avg_",i)
        maxcol <- paste0("hr_max_",i)
        etrimpcol <- paste0("etrimp_",i)
        a[,avgcol] <- round(mean(hrinc$heart_rate,na.rm=T),0)
        a[,maxcol] <- round(max(hrinc$heart_rate, na.rm=T),0)
        # get hr_zones and times (standard %s)
        hrinc$hr_zones <- findInterval(hrinc$heart_rate,hr_zones[6:10])
        hr_zones_table <- round(table(hrinc$hr_zones)/length(hrinc$hr_zones)*100,2)
        hr_zones_table[c("0","1","2","3","4","5")[!c("0","1","2","3","4","5") %in% names(hr_zones_table)]] <- 0
        ainc$hr_z0 <- as.numeric(hr_zones_table['0'])
        ainc$hr_z1 <- as.numeric(hr_zones_table['1'])
        ainc$hr_z2 <- as.numeric(hr_zones_table['2'])
        ainc$hr_z3 <- as.numeric(hr_zones_table['3'])
        ainc$hr_z4 <- as.numeric(hr_zones_table['4'])
        ainc$hr_z5 <- as.numeric(hr_zones_table['5'])
        ainc$hr_z0_time <- as.numeric(round(ainc$hr_z0/100 * i,1))
        ainc$hr_z1_time <- as.numeric(round(ainc$hr_z1/100 * i,1))
        ainc$hr_z2_time <- as.numeric(round(ainc$hr_z2/100 * i,1))
        ainc$hr_z3_time <- as.numeric(round(ainc$hr_z3/100 * i,1))
        ainc$hr_z4_time <- as.numeric(round(ainc$hr_z4/100 * i,1))
        ainc$hr_z5_time <- as.numeric(round(ainc$hr_z5/100 * i,1))
        # calculate trimp scores
        a[,etrimpcol] <- round(ainc$hr_z1_time * 1 + ainc$hr_z2_time * 2 + ainc$hr_z3_time * 3 + ainc$hr_z4_time * 4 + ainc$hr_z5_time * 5,2)
      }
    }
  } else {
    # add same columns with NA
    a <- cbind(a, onerow.df(NA,colnames=c("maxhr_activity","maxhr_perc","maxhr_intensity","hr_avg",
                                          "hr_z0","hr_z1", "hr_z2","hr_z3","hr_z4","hr_z5",
                                          "hr_z0_time", "hr_z1_time","hr_z2_time","hr_z3_time","hr_z4_time","hr_z5_time",
                                          "etrimp")))
    if (etrimp_addon == TRUE) {
      for (i in seq(5, a$duration_min, by = 5)){
        avgcol <- paste0("hr_avg_",i)
        maxcol <- paste0("hr_max_",i)
        etrimpcol <- paste0("etrimp_",i)
        a[,avgcol] <- NA
        a[,maxcol] <- NA
        a[,etrimpcol] <- NA
      }
    }
  }
  return(a)
}
