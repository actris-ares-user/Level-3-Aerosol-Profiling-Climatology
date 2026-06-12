ssn <- list(3:5, 6:8, 9:11, c(1, 2, 12))

bl_counts <- matrix(NA, nrow = 4, ncol = 20)
tl_counts <- matrix(NA, nrow = 4, ncol = 20)
# lr_counts <- array(NA, dim = c(4, 3, 20))  #OLD
lr_counts <- array(NA, dim = c(4, 3, 31))
# pd_counts <- array(NA, dim = c(4, 3, 20))  #OLD
pd_counts <- array(NA, dim = c(4, 3, 22))
aod_counts <- array(NA, dim = c(4, 3, 20))
extinction_counts <- array(NA, dim = c(4, 3, 20))
com_counts <- array(NA, dim = c(4, 3, 20))
ib_counts <- array(NA, dim = c(4, 3, 20))
backscatter_counts <- array(NA, dim = c(4, 3, 20))

# Define time data
val_time <- as.numeric(as.POSIXct(paste0("2000-", sprintf("%02d", c(1, 4, 7, 10)), "-15 23:59:59"), tz = "GMT", origin = "1970-01-01 00:00:00"))

# Defining the start and end periods for each column
year_flag <- mapply(is_leap_year, release[2])

start_dates <- c("12-01", "03-01", "06-01", "09-01")
if (year_flag){
  end_dates <- c("02-29", "05-31", "08-31", "11-30")
} else {
  end_dates <- c("02-28", "05-31", "08-31", "11-30")
}

# Initialize the matrix
time_bounds_data <- matrix(NA, nrow = 2, ncol = 4)

# Populate the matrix with the calculated values
for (i in 1:4) {
  start_date <- if (i == 1) paste0(release[1] - 1, "-", start_dates[i], " 00:00:00") else paste0(release[1], "-", start_dates[i], " 00:00:00")
  end_date <- paste0(release[2], "-", end_dates[i], " 23:59:59")
  
  if (i == 1){
    time_bounds_data[, 4] <- c(
      as.numeric(as.POSIXct(start_date, tz = "GMT", origin = "1970-01-01 00:00:00")),
      as.numeric(as.POSIXct(end_date, tz = "GMT", origin = "1970-01-01 00:00:00"))
    )
  } else {
    time_bounds_data[, i - 1] <- c(
      as.numeric(as.POSIXct(start_date, tz = "GMT", origin = "1970-01-01 00:00:00")),
      as.numeric(as.POSIXct(end_date, tz = "GMT", origin = "1970-01-01 00:00:00"))
    )
  }
}

for (p in seq_along(loc2)) {
  station <- loc2[p]
  
  for (k in 1:4){
    db_355 <- db_int_layers[db_int_layers$Station == station &
                             as.numeric(db_int_layers$Month) %in% ssn[[k]] &
                             as.numeric(db_int_layers$Wavelength) == 355, ]
    db_532 <- db_int_layers[db_int_layers$Station == station &
                             as.numeric(db_int_layers$Month) %in% ssn[[k]] &
                             as.numeric(db_int_layers$Wavelength) == 532, ]
    db_1064 <- db_int_layers[db_int_layers$Station == station &
                              as.numeric(db_int_layers$Month) %in% ssn[[k]] &
                              as.numeric(db_int_layers$Wavelength) == 1064, ]
    db1 <- layers_base_top[layers_base_top$Station == station &
                            as.numeric(layers_base_top$Month) %in% ssn[[k]], ]
    
    pd_vtt_355 <- as.numeric(db_355$ParticleDep[db_355$Type == "b" | db_355$PdF == 1])
    pd_vtt_532 <- as.numeric(db_532$ParticleDep[db_532$Type == "b" | db_532$PdF == 1])
    pd_vtt_1064 <- as.numeric(db_1064$ParticleDep)
    
    bl_counts[k, ] <- hist(db1$Base_Layer, breaks = altitude_breaks, plot = F)$counts
    tl_counts[k, ] <- hist(db1$Top_Layer, breaks = altitude_breaks, plot = F)$counts
    
    lr_counts_355 <- safe_hist_lr_counts(db_355$LidarRatio, lr_breaks)
    lr_counts_532 <- safe_hist_lr_counts(db_532$LidarRatio, lr_breaks)
    lr_counts_1064 <- safe_hist_lr_counts(db_1064$LidarRatio, lr_breaks)
    lr_counts[k, , ] <- rbind(lr_counts_355, lr_counts_532, lr_counts_1064)
    
    # pd_counts_355 <- hist(pd_vtt_355, breaks = pd_breaks, plot = F)$counts
    # pd_counts_532 <- hist(pd_vtt_532, breaks = pd_breaks, plot = F)$counts
    # pd_counts_1064 <- hist(pd_vtt_1064, breaks = pd_breaks, plot = F)$counts
    pd_counts_355 <- safe_hist_pd_counts(pd_vtt_355, pd_breaks)
    pd_counts_532 <- safe_hist_pd_counts(pd_vtt_532, pd_breaks)
    pd_counts_1064 <- safe_hist_pd_counts(pd_vtt_1064, pd_breaks)
    pd_counts[k, , ] <- rbind(pd_counts_355, pd_counts_532, pd_counts_1064)
    
    aod_counts_355 <- safe_hist_counts(db_355$AOD, aod_breaks)
    aod_counts_532 <- safe_hist_counts(db_532$AOD, aod_breaks)
    aod_counts_1064 <- rep(NA, times = 20)
    aod_counts[k, , ] <- rbind(aod_counts_355, aod_counts_532, aod_counts_1064)
    
    extinction_counts_355 <- safe_hist_counts(db_355$Extinction, extinction_breaks)
    extinction_counts_532 <- safe_hist_counts(db_532$Extinction, extinction_breaks)
    extinction_counts_1064 <- rep(NA, times = 20)
    extinction_counts[k, , ] <- rbind(extinction_counts_355, extinction_counts_532, extinction_counts_1064)
    
    com_counts_355 <- safe_hist_counts(db_355$CenterMass, altitude_breaks)
    com_counts_532 <- safe_hist_counts(db_532$CenterMass, altitude_breaks)
    com_counts_1064 <- safe_hist_counts(db_1064$CenterMass, altitude_breaks)
    com_counts[k, , ] <- rbind(com_counts_355, com_counts_532, com_counts_1064)
    
    ib_counts_355 <- safe_hist_counts(db_355$IntBs, ib_breaks)
    ib_counts_532 <- safe_hist_counts(db_532$IntBs, ib_breaks)
    ib_counts_1064 <- safe_hist_counts(db_1064$IntBs, ib_breaks)
    ib_counts[k, , ] <- rbind(ib_counts_355, ib_counts_532, ib_counts_1064)
    
    backscatter_counts_355 <- safe_hist_counts(db_355$Backscatter, backscatter_breaks)
    backscatter_counts_532 <- safe_hist_counts(db_532$Backscatter, backscatter_breaks)
    backscatter_counts_1064 <- safe_hist_counts(db_1064$Backscatter, backscatter_breaks)
    backscatter_counts[k, , ] <- rbind(backscatter_counts_355, backscatter_counts_532, backscatter_counts_1064)
  }
  
  bl_counts[is.na(bl_counts)] <- 9.9692099683868690e+36
  tl_counts[is.na(tl_counts)] <- 9.9692099683868690e+36
  lr_counts[is.na(lr_counts)] <- 9.9692099683868690e+36
  pd_counts[is.na(pd_counts)] <- 9.9692099683868690e+36
  aod_counts[is.na(aod_counts)] <- 9.9692099683868690e+36
  extinction_counts[is.na(extinction_counts)] <- 9.9692099683868690e+36
  com_counts[is.na(com_counts)] <- 9.9692099683868690e+36
  ib_counts[is.na(ib_counts)] <- 9.9692099683868690e+36
  backscatter_counts[is.na(backscatter_counts)] <- 9.9692099683868690e+36
  
  source_name <- lev2db[lev2db$Station == station, ]
  source_data <- paste(source_name[, 1], collapse = " ")
  
  # Dimensions are created
  nv <- ncdim_def("nv", "", 1:2, longname = "")
  wavelength <- ncdim_def("wavelength", "nm", c(355, 532, 1064), longname = "Wavelength of the transmitted laser pulse")  
  time <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", val_time, calendar = "gregorian", longname = "Time")
  n_char <- ncdim_def("n_char", "", 1:nchar(source_data), longname = "")
  breaks <- ncdim_def("breaks", "", 1:20, longname = "Histogram breaks")
  breaks_lr <- ncdim_def("breaks_lr", "", 1:31, longname = "Lidar Ratio histogram breaks")
  breaks_pd <- ncdim_def("breaks_pd", "", 1:22, longname = "Particle Depolarizzation histogram breaks")
  
  # The variables are created
  altitude_intervals <- ncvar_def(
    "altitude_intervals",
    units = "m",
    dim = breaks,
    prec = "double"
  )
  lidar_ratio_intervals <- ncvar_def(
    "lidar_ratio_intervals",
    units = "sr",
    dim = breaks_lr,
    prec = "double"
  )
  extinction_intervals <- ncvar_def(
    "extinction_intervals",
    units = "1/km",
    dim = breaks,
    prec = "double"
  )
  particle_depolarization_intervals <- ncvar_def(
    "particle_depolarization_intervals",
    units = "1",
    dim = breaks_pd,
    prec = "double"
  )
  aerosol_optical_depth_intervals <- ncvar_def(
    "aerosol_optical_depth_intervals",
    units = "1",
    dim = breaks,
    prec = "double"
  )
  integrated_backscatter_intervals <- ncvar_def(
    "integrated_backscatter_intervals",
    units = "(10^3 sr)^-1",
    dim = breaks,
    prec = "double"
  )
  backscatter_intervals <- ncvar_def(
    "backscatter_intervals",
    units = "1/Mm*sr",
    dim = breaks,
    prec = "double"
  )
  base_layer_altitude_frequency <- ncvar_def(
    "base_layer_altitude_frequency",
    units = "",
    dim = list(time, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the base layer altitude values",
    prec = "double"
  )
  top_layer_altitude_frequency <- ncvar_def(
    "top_layer_altitude_frequency",
    units = "",
    dim = list(time, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the top layer altitude values",
    prec = "double"
  )
  center_of_mass_altitude_frequency <- ncvar_def(
    "center_of_mass_altitude_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the center of mass altitude values",
    prec = "double"
  )
  lidar_ratio_frequency <- ncvar_def(
    "lidar_ratio_frequency",
    units = "",
    dim = list(time, wavelength, breaks_lr),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the lidar ratio values",
    prec = "double"
  )
  extinction_frequency <- ncvar_def(
    "extinction_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the extinction frequency",
    prec = "double"
  )
  particle_depolarization_frequency <- ncvar_def(
    "particle_depolarization_frequency",
    units = "",
    dim = list(time, wavelength, breaks_pd),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the linear particle depolarization ratio",
    prec = "double"
  )
  aerosol_optical_depth_frequency <- ncvar_def(
    "aerosol_optical_depth_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the aerosol optical depth values",
    prec = "double"
  )
  integrated_backscatter_frequency <- ncvar_def(
    "integrated_backscatter_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the integrated backscatter values",
    prec = "double"
  )
  backscatter_frequency <- ncvar_def(
    "backscatter_frequency",
    units = "",
    dim = list(time, wavelength, breaks),
    missval = 9.9692099683868690e+36,
    longname = "Frequency distribution of the backscatter values",
    prec = "double"
  )
  time_bounds <- ncvar_def(
    "time_bounds",
    units = "seconds since 1970-01-01T00:00:00Z",
    dim = list(time, nv),
    prec = "double"
  )
  latitude <- ncvar_def(
    "latitude",
    units = "degrees_north",
    dim = list(),
    longname = "latitude of station",
    prec = "float"
  )
  longitude <- ncvar_def(
    "longitude",
    units = "degrees_east",
    dim = list(),
    longname = "longitude of station",
    prec = "float"
  )
  source <- ncvar_def(
    "source",
    units = "",
    dim = n_char,
    longname = "source files",
    prec = "char"
  )
  station_altitude <- ncvar_def(
    "station_altitude",
    units = "m",
    dim = list(),
    longname = "station altitude above sea level",
    prec = "float"
  )
  
  variabili <- list(
    base_layer_altitude_frequency,
    top_layer_altitude_frequency,
    center_of_mass_altitude_frequency,
    lidar_ratio_frequency,
    extinction_frequency,
    particle_depolarization_frequency,
    aerosol_optical_depth_frequency,
    integrated_backscatter_frequency,
    backscatter_frequency,
    altitude_intervals,
    lidar_ratio_intervals,
    extinction_intervals,
    particle_depolarization_intervals,
    aerosol_optical_depth_intervals,
    integrated_backscatter_intervals,
    backscatter_intervals,
    time_bounds,
    latitude,
    longitude,
    source,
    station_altitude
  )
  
  # Defining the file by putting variables inside it
  season_file <- nc_create(
    paste0(
      "ACTRIS_AerRemSen_",
      station,
      "_Lev03_NorSea",
      release_suffix,
      "Lay_v03_qc040.nc"
    ),
    variabili,
    force_v4 = TRUE
  )
  
  # Add attributes to variables
  ncatt_put(season_file, "time", "axis", "T", prec = "text")
  ncatt_put(season_file, "time", "standard_name", "time", prec = "text")
  ncatt_put(season_file, "time", "bounds", "time_bounds", prec = "text")
  ncatt_put(season_file, latitude, "standard_name", "latitude", prec = "text")
  ncatt_put(season_file, longitude, "standard_name", "longitude", prec = "text")
  ncatt_put(
    season_file,
    source,
    "description",
    "List of level 2 files from which are retrieved values averaged in this file",
    prec = "text"
  )
  
  descr <- "Histogram interval bounds are reported. The n-th value represents the lower bound of the n-th interval, while the higher bound is the (n+1)-th value, since intervals are adiacent. The last interval (n=20) has no higher bound, since it is right-open."
  descr_lr <- "Histogram interval bounds are reported. The n-th value represents the lower bound of the n-th interval, while the higher bound is the (n+1)-th value, since intervals are adiacent. The last interval (n=31) has no higher bound, since it is right-open."
  descr_pd <- "Histogram interval bounds are reported. The n-th value represents the lower bound of the n-th interval, while the higher bound is the (n+1)-th value, since intervals are adiacent. The last interval (n=22) has no higher bound, since it is right-open."
  
  ncatt_put(season_file, altitude_intervals, "description", descr)
  ncatt_put(season_file, lidar_ratio_intervals, "description", descr_lr)
  ncatt_put(season_file, extinction_intervals, "description", descr)
  ncatt_put(season_file, particle_depolarization_intervals, "description", descr_pd)
  ncatt_put(season_file, aerosol_optical_depth_intervals, "description", descr)
  ncatt_put(season_file, integrated_backscatter_intervals, "description", descr)
  ncatt_put(season_file, backscatter_intervals, "description", descr)
  ncatt_put(season_file, base_layer_altitude_frequency, "histogram_intervals", "altitude_intervals")
  ncatt_put(season_file, top_layer_altitude_frequency, "histogram_intervals", "altitude_intervals")
  ncatt_put(season_file, center_of_mass_altitude_frequency, "histogram_intervals", "altitude_intervals")
  ncatt_put(season_file, lidar_ratio_frequency, "histogram_intervals", "lidar_ratio_intervals")
  ncatt_put(season_file, extinction_frequency, "histogram_intervals", "extinction_intervals")
  ncatt_put(season_file, particle_depolarization_frequency, "histogram_intervals", "particle_depolarization_intervals")
  ncatt_put(season_file, aerosol_optical_depth_frequency, "histogram_intervals", "aerosol_optical_depth_intervals")
  ncatt_put(season_file, integrated_backscatter_frequency, "histogram_intervals", "integrated_backscatter_intervals")
  ncatt_put(season_file, backscatter_frequency, "histogram_intervals", "backscatter_intervals")
  
  # Enter the values into the file
  ncvar_put(season_file, base_layer_altitude_frequency, bl_counts)
  ncvar_put(season_file, top_layer_altitude_frequency, tl_counts)
  ncvar_put(season_file, center_of_mass_altitude_frequency, com_counts)
  ncvar_put(season_file, lidar_ratio_frequency, lr_counts)
  ncvar_put(season_file, extinction_frequency, extinction_counts)
  ncvar_put(season_file, particle_depolarization_frequency, pd_counts)
  ncvar_put(season_file, aerosol_optical_depth_frequency, aod_counts)
  ncvar_put(season_file, integrated_backscatter_frequency, ib_counts)
  ncvar_put(season_file, backscatter_frequency, backscatter_counts)
  ncvar_put(season_file, altitude_intervals, altitude_intervals_data)
  ncvar_put(season_file, lidar_ratio_intervals, lr_intervals_data)
  ncvar_put(season_file, extinction_intervals, extinction_intervals_data)
  ncvar_put(season_file, particle_depolarization_intervals, pd_intervals_data)
  ncvar_put(season_file, aerosol_optical_depth_intervals, aod_intervals_data)
  ncvar_put(season_file, integrated_backscatter_intervals, ib_intervals_data)
  ncvar_put(season_file, backscatter_intervals, backscatter_intervals_data)
  ncvar_put(season_file, time_bounds, time_bounds_data)
  ncvar_put(season_file, source, source_data)
  ncvar_put(season_file, latitude, latit[p])
  ncvar_put(season_file, longitude, longit[p])
  ncvar_put(season_file, station_altitude, station_alt[p])
  
  # Definition of global attributes
  ncatt_put(season_file,
            0,
            attname = "processor_name",
            attval = "EAR_clim_v1.exe",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "processor_version",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "processor_institution",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "system",
            attval = sist[p],
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "location",
            attval = loc[p],
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "institution",
            attval = institution[p],
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "PI",
            attval = PI[p],
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "PI_affiliation",
            attval = institution[p],
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "PI_affiliation_acronym",
            attval = acronym[p],
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "PI_address",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "PI_phone",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "PI_email",
            attval = PI_contact[p],
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_affiliation",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_affiliation_acronym",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_address",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_phone",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_originator_email",
            attval = "earlinetdb@actris.imaa.cnr.it",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider",
            attval = "ACTRIS ARES",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_affiliation",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_affiliation_acronym",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_address",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_phone",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "data_provider_email",
            attval = "earlinetdb@actris.imaa.cnr.it",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "conventions",
            attval = "C.F. - 1.8",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "references",
            attval = "link doc earlinet.org",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "station_ID",
            attval = station,
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "__file_format_version",
            attval = "",
            prec = "text")
  ncatt_put(season_file,
            0,
            attname = "history",
            attval = paste0(Sys.time(), "Generated by free software R, using package ncdf4"),
            prec = "text")
  ncatt_put(season_file, 0, attname = "title", attval = "Seasonal distribution of layer optical values - Normal")
  
  nc_close(season_file)
  
  file_name <- paste0(
    "ACTRIS_AerRemSen_",
    station,
    "_Lev03_NorSea",
    release_suffix,
    "Lay_v03_qc040.nc"
  )
  file.rename(
    from = file.path(getwd(), file_name),
    to = file.path(getwd(), "Level3/Layers", station, file_name)
  )
  
}