if (flag_355) {
  db_b_s_355[, 3:54] = signif(db_b_s_355[, 3:54], 6)
  db_e_s_355[, 3:54] = signif(db_e_s_355[, 3:54], 6)
  db_pd_s_355[, 3:26] = signif(db_pd_s_355[, 3:26], 6)
} 

if (flag_532) {
  db_b_s_532[, 3:54] = signif(db_b_s_532[, 3:54], 6)
  db_e_s_532[, 3:54] = signif(db_e_s_532[, 3:54], 6)
  db_pd_s_532[, 3:26] = signif(db_pd_s_532[, 3:26], 6)
}

if (flag_1064) {
  db_b_s_1064[, 3:54] = signif(db_b_s_1064[, 3:54], 6)
  db_pd_s_1064[, 3:26] = signif(db_pd_s_1064[, 3:26], 6)
}

if (flag_ang_s) {
  db_ang_s_tot[, 3:26] = signif(db_ang_s_tot[, 3:26], 6)
}

if (flag_pbl_s) {
  db_pbl_s_tot[, 3:6] = signif(db_pbl_s_tot[, 3:6], 6)
}

# Generating the data
integral_bounds_data = c(0, 1)

# Process each station
for (p in seq_along(loc2)) {
  station <- loc2[p]
  
  for (y in release[1]:release[2]) {
    # Initialize the app data frame with the desired columns
    app <- data.frame(Season_Year = character(), Null = logical(), stringsAsFactors = FALSE)
    
    # Create the season list
    sss <- c(
      paste0("DecJanFeb_", y - 1, "/", y),
      paste0(c("MarAprMay_", "JunJulAug_", "SepOctNov_"), y)
    )
    
    # Populate the data frame app
    app <- data.frame(Season_Year = sss, Null = NA, stringsAsFactors = FALSE)
    
    # Apply the function to various databases
    if (flag_355) {
      db_e355 <- filter_and_merge_seas_y(db_e_s_355, station, y, app)
      db_b355 <- filter_and_merge_seas_y(db_b_s_355, station, y, app)
      db_pd355 <- filter_and_merge_seas_y(db_pd_s_355, station, y, app)
    }
    
    if (flag_532) {
      db_e532 <- filter_and_merge_seas_y(db_e_s_532, station, y, app)
      db_b532 <- filter_and_merge_seas_y(db_b_s_532, station, y, app)
      db_pd532 <- filter_and_merge_seas_y(db_pd_s_532, station, y, app)
    }
    
    if (flag_1064) {
      db_b1064 <- filter_and_merge_seas_y(db_b_s_1064, station, y, app)
      db_pd1064 <- filter_and_merge_seas_y(db_pd_s_1064, station, y, app)
    }
    
    if (flag_pbl_s) {
      db_pbl <- filter_and_merge_seas_y(db_pbl_s_tot, station, y, app)
    }
    
    angstrom_data <- array(NA, dim = c(4, 2, 5))
    if (flag_ang_s) {
      db_ang <- filter_and_merge_seas_y(db_ang_s_tot, station, y, app)
      
      # Angstrom data
      angstrom_data[, 1, ] <- cbind(db_ang[, 3], db_ang[, 4], db_ang[, 9], db_ang[, 15], db_ang[, 21])
      angstrom_data[, 2, ] <- cbind(db_ang[, 5], db_ang[, 6], db_ang[, 11], db_ang[, 17], db_ang[, 23])
      angstrom_data[is.na(angstrom_data)] <- 9.9692099683868690e+36
    }
    
    # Aerosol optical depth data
    aerosol_optical_depth_data <- array(NA, dim = c(4, 2, 3, 5))
    if (flag_355) {
      aerosol_optical_depth_data[, 1, 1, ] <- cbind(db_e355[, 4], db_e355[, 5], db_e355[, 17], db_e355[, 30], db_e355[, 43])
      aerosol_optical_depth_data[, 2, 1, ] <- cbind(db_e355[, 6], db_e355[, 7], db_e355[, 19], db_e355[, 32], db_e355[, 45])
    } 
    if (flag_532) {
      aerosol_optical_depth_data[, 1, 2, ] <- cbind(db_e532[, 4], db_e532[, 5], db_e532[, 17], db_e532[, 30], db_e532[, 43])
      aerosol_optical_depth_data[, 2, 2, ] <- cbind(db_e532[, 6], db_e532[, 7], db_e532[, 19], db_e532[, 32], db_e532[, 45])
    } 
    aerosol_optical_depth_data[, , 3, ] <- array(NA, dim = c(4, 2, 5))
    aerosol_optical_depth_data[is.na(aerosol_optical_depth_data)] <- 9.9692099683868690e+36
    
    # Lidar ratio data
    lidar_ratio_data <- array(NA, dim = c(4, 2, 3, 5))
    if (flag_355) {
      lidar_ratio_data[, 1, 1, ] <- cbind(db_e355[, 10], db_e355[, 11], db_e355[, 23], db_e355[, 36], db_e355[, 49])
      lidar_ratio_data[, 2, 1, ] <- cbind(db_e355[, 12], db_e355[, 13], db_e355[, 25], db_e355[, 38], db_e355[, 51])
    } 
    if (flag_532) {
      lidar_ratio_data[, 1, 2, ] <- cbind(db_e532[, 10], db_e532[, 11], db_e532[, 23], db_e532[, 36], db_e532[, 49])
      lidar_ratio_data[, 2, 2, ] <- cbind(db_e532[, 12], db_e532[, 13], db_e532[, 25], db_e532[, 38], db_e532[, 51])
    } 
    lidar_ratio_data[, , 3, ] <- array(NA, dim = c(4, 2, 5))
    lidar_ratio_data[is.na(lidar_ratio_data)] <- 9.9692099683868690e+36
    
    # Integrated backscatter data
    integrated_backscatter_data <- array(NA, dim = c(4, 2, 3, 5))
    if (flag_355) {
      integrated_backscatter_data[, 1, 1, ] <- cbind(db_b355[, 4], db_b355[, 5], db_b355[, 17], db_b355[, 30], db_b355[, 43])
      integrated_backscatter_data[, 2, 1, ] <- cbind(db_b355[, 6], db_b355[, 7], db_b355[, 19], db_b355[, 32], db_b355[, 45])
    } 
    if (flag_532) {
      integrated_backscatter_data[, 1, 2, ] <- cbind(db_b532[, 4], db_b532[, 5], db_b532[, 17], db_b532[, 30], db_b532[, 43])
      integrated_backscatter_data[, 2, 2, ] <- cbind(db_b532[, 6], db_b532[, 7], db_b532[, 19], db_b532[, 32], db_b532[, 45])
    } 
    if (flag_1064) {
      integrated_backscatter_data[, 1, 3, ] <- cbind(db_b1064[, 4], db_b1064[, 5], db_b1064[, 17], db_b1064[, 30], db_b1064[, 43])
      integrated_backscatter_data[, 2, 3, ] <- cbind(db_b1064[, 6], db_b1064[, 7], db_b1064[, 19], db_b1064[, 32], db_b1064[, 45])
    } 
    integrated_backscatter_data[is.na(integrated_backscatter_data)] <- 9.9692099683868690e+36
    
    # Center of mass data
    center_of_mass_data <- array(NA, dim = c(4, 2, 3, 5))
    if (flag_355) {
      center_of_mass_data[, 1, 1, ] <- cbind(db_b355[, 10], db_b355[, 11], db_b355[, 23], db_b355[, 36], db_b355[, 49])
      center_of_mass_data[, 2, 1, ] <- cbind(db_b355[, 12], db_b355[, 13], db_b355[, 25], db_b355[, 38], db_b355[, 51])
    } 
    if (flag_532) {
      center_of_mass_data[, 1, 2, ] <- cbind(db_b532[, 10], db_b532[, 11], db_b532[, 23], db_b532[, 36], db_b532[, 49])
      center_of_mass_data[, 2, 2, ] <- cbind(db_b532[, 12], db_b532[, 13], db_b532[, 25], db_b532[, 38], db_b532[, 51])
    } 
    if (flag_1064) {
      center_of_mass_data[, 1, 3, ] <- cbind(db_b1064[, 10], db_b1064[, 11], db_b1064[, 23], db_b1064[, 36], db_b1064[, 49])
      center_of_mass_data[, 2, 3, ] <- cbind(db_b1064[, 12], db_b1064[, 13], db_b1064[, 25], db_b1064[, 38], db_b1064[, 51])
    } 
    center_of_mass_data[is.na(center_of_mass_data)] <- 9.9692099683868690e+36
    
    # Aerosol boundary layer data
    aerosol_boundary_layer_data <- array(NA, dim = c(4, 5))
    if (flag_pbl_s) {
      aerosol_boundary_layer_data <- cbind(db_pbl[, 3], rep(NA, times = 4), db_pbl[, 4], db_pbl[, 5], db_pbl[, 6])
    }
    aerosol_boundary_layer_data[is.na(aerosol_boundary_layer_data)] <- 9.9692099683868690e+36
    
    # h63 of aerosol optical depth data
    h63_of_aerosol_optical_depth_data <- array(NA, dim = c(4, 3, 5))
    if (flag_355) {
      h63_of_aerosol_optical_depth_data[, 1, 1] <- db_e355[, 3]
      h63_of_aerosol_optical_depth_data[, 1, 3:5] <- cbind(db_e355[, 16], db_e355[, 29], db_e355[, 42])
    } 
    if (flag_532) {
      h63_of_aerosol_optical_depth_data[, 2, 1] <- db_e532[, 3]
      h63_of_aerosol_optical_depth_data[, 2, 3:5] <- cbind(db_e532[, 16], db_e532[, 29], db_e532[, 42])
    } 
    h63_of_aerosol_optical_depth_data[is.na(h63_of_aerosol_optical_depth_data)] <- 9.9692099683868690e+36
    
    # h63 of integrated backscatter data
    h63_of_integrated_backscatter_data <- array(NA, dim = c(4, 3, 5))
    if (flag_355) {
      h63_of_integrated_backscatter_data[, 1, 1] <- db_b355[, 3]
      h63_of_integrated_backscatter_data[, 1, 3:5] <- cbind(db_b355[, 16], db_b355[, 29], db_b355[, 42])
    } 
    if (flag_532) {
      h63_of_integrated_backscatter_data[, 2, 1] <- db_b532[, 3]
      h63_of_integrated_backscatter_data[, 2, 3:5] <- cbind(db_b532[, 16], db_b532[, 29], db_b532[, 42])
    } 
    if (flag_1064) {
      h63_of_integrated_backscatter_data[, 3, 1] <- db_b1064[, 3]
      h63_of_integrated_backscatter_data[, 3, 3:5] <- cbind(db_b1064[, 16], db_b1064[, 29], db_b1064[, 42])
    } 
    h63_of_integrated_backscatter_data[is.na(h63_of_integrated_backscatter_data)] <- 9.9692099683868690e+36
    
    # Particle depolarization data
    particle_depolarization_data <- array(NA, dim = c(4, 2, 3, 5))
    if (flag_355) {
      particle_depolarization_data[, 1, 1, ] <- cbind(db_pd355[, 3], db_pd355[, 4], db_pd355[, 9], db_pd355[, 15], db_pd355[, 21])
      particle_depolarization_data[, 2, 1, ] <- cbind(db_pd355[, 5], db_pd355[, 6], db_pd355[, 11], db_pd355[, 17], db_pd355[, 23])
    } 
    if (flag_532) {
      particle_depolarization_data[, 1, 2, ] <- cbind(db_pd532[, 3], db_pd532[, 4], db_pd532[, 9], db_pd532[, 15], db_pd532[, 21])
      particle_depolarization_data[, 2, 2, ] <- cbind(db_pd532[, 5], db_pd532[, 6], db_pd532[, 11], db_pd532[, 17], db_pd532[, 23])
    } 
    if (flag_1064) {
      particle_depolarization_data[, 1, 3, ] <- cbind(db_pd1064[, 3], db_pd1064[, 4], db_pd1064[, 9], db_pd1064[, 15], db_pd1064[, 21])
      particle_depolarization_data[, 2, 3, ] <- cbind(db_pd1064[, 5], db_pd1064[, 6], db_pd1064[, 11], db_pd1064[, 17], db_pd1064[, 23])
    } 
    particle_depolarization_data[is.na(particle_depolarization_data)] <- 9.9692099683868690e+36
    
    # The matrix is initialized
    time_bounds_data <- matrix(NA, nrow = 2, ncol = 4)
    
    # The matrix is populated with the calculated values
    time_bounds_data[, 1] <- c(
      as.numeric(as.POSIXct(paste0(y - 1, "-12-01 00:00:00"), tz = "GMT")),
      as.numeric(as.POSIXct(paste0(y, "-02-", days_in_february(y), " 23:59:59"), tz = "GMT"))
    )
    time_bounds_data[, 2] <- c(
      as.numeric(as.POSIXct(paste0(y, "-03-01 00:00:00"), tz = "GMT")),
      as.numeric(as.POSIXct(paste0(y, "-05-31 23:59:59"), tz = "GMT"))
    )
    time_bounds_data[, 3] <- c(
      as.numeric(as.POSIXct(paste0(y, "-06-01 00:00:00"), tz = "GMT")),
      as.numeric(as.POSIXct(paste0(y, "-08-31 23:59:59"), tz = "GMT"))
    )
    time_bounds_data[, 4] <- c(
      as.numeric(as.POSIXct(paste0(y, "-09-01 00:00:00"), tz = "GMT")),
      as.numeric(as.POSIXct(paste0(y, "-11-30 23:59:59"), tz = "GMT"))
    )
    
    # Getting source names
    source_name <- lev2db[lev2db$Place == station & lev2db$Year == y, 1]
    
    # Join source names into a single string and add a leading space
    source_data <- paste(source_name, collapse = " ")
    source_data <- paste0(" ", source_data)
    
    # Dimensions are created
    nv <- ncdim_def("nv", "", create_dimvar = FALSE, 1:2)
    wavelength <- ncdim_def("wavelength", "nm", c(355, 532, 1064), longname = "Wavelength of the transmitted laser pulse")
    val_time <- as.numeric(as.POSIXct(paste0(as.character(y), "-", sprintf("%02d", c(1, 4, 7, 10)), "-15 23:59:59"), tz = "GMT", origin = "1970-01-01 00:00:00"))
    time <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", val_time, calendar = "gregorian", longname = "Time")
    n_char <- ncdim_def("n_char", "", create_dimvar = FALSE, 1:nchar(source_data))
    stats <- ncdim_def("stats", "", 0:4, longname = "statistics")
    
    # The variables are created  
    integral_bounds <- ncvar_def(
      name = "integral_bounds",
      units = "",
      dim = nv,
      longname = "integral bounds of integrated values",
      prec = "byte"
    )
    aerosol_optical_depth <- ncvar_def(
      name = "aerosol_optical_depth",
      units = "1",
      dim = list(time, nv, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "aerosol optical depth",
      prec = "double"
    )
    integrated_backscatter <- ncvar_def(
      name = "integrated_backscatter",
      units = "1/sr",
      dim = list(time, nv, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "integrated backscatter",
      prec = "double"
    )
    lidar_ratio <- ncvar_def(
      name = "lidar_ratio",
      units = "sr",
      dim = list(time, nv, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "aerosol extinction-to-backscatter ratio",
      prec = "double"
    )
    aerosol_boundary_layer <- ncvar_def(
      name = "aerosol_boundary_layer",
      units = "m",
      dim = list(time, stats),
      longname = "altitude of the upper bound of the aerosol planet boundary layer",
      missval = 9.9692099683868690e+36,
      prec = "double"
    )
    h63_of_aerosol_optical_depth <- ncvar_def(
      name = "h63_of_aerosol_optical_depth",
      units = "m",
      dim = list(time, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "altitude below which stays the 63% of the total aerosol optical depth",
      prec = "double"
    )
    h63_of_integrated_backscatter <- ncvar_def(
      name = "h63_of_integrated_backscatter",
      units = "m",
      dim = list(time, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "altitude below which stays the 63% of the total integrated backscatter",
      prec = "double"
    )
    center_of_mass <- ncvar_def(
      name = "center_of_mass",
      units = "m",
      dim = list(time, nv, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "center of mass",
      prec = "double"
    )
    particle_depolarization <- ncvar_def(
      name = "particle_depolarization",
      units = "1",
      dim = list(time, nv, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "aerosol linear particle depolarization ratio",
      prec = "double"
    )
    angstrom_coefficient <- ncvar_def(
      name = "angstrom_coefficient",
      units = "1",
      dim = list(time, nv, stats),
      missval = 9.9692099683868690e+36,
      longname = "angstrom coefficient",
      prec = "double"
    )
    time_bounds <- ncvar_def(
      name = "time_bounds",
      units = "seconds since 1970-01-01T00:00:00Z",
      dim = list(time, nv),
      prec = "double"
    )
    latitude <- ncvar_def(
      name = "latitude",
      units = "degrees_north",
      dim = list(),
      longname = "latitude of the station",
      prec = "float"
    )
    longitude <- ncvar_def(
      name = "longitude",
      units = "degrees_east",
      dim = list(),
      longname = "longitude of the station",
      prec = "float"
    )
    source <- ncvar_def(
      name = "source",
      units = "",
      dim = n_char,
      longname = "source files",
      prec = "char"
    )
    station_altitude <- ncvar_def(
      name = "station_altitude",
      units = "m",
      dim = list(),
      longname = "station altitude above sea level",
      prec = "float"
    )
    
    variabili <- list(
      integral_bounds,
      aerosol_optical_depth,
      integrated_backscatter,
      lidar_ratio,
      aerosol_boundary_layer,
      h63_of_aerosol_optical_depth,
      h63_of_integrated_backscatter,
      center_of_mass,
      particle_depolarization,
      angstrom_coefficient,
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
        "_Lev03_Season_",
        as.character(y),
        "_Int_v03_qc040.nc"
      ),
      variabili,
      force_v4 = TRUE
    )
    
    # Add attributes to variables
    ncatt_put(season_file, integral_bounds, "flag_value", "0,1", prec = "text")
    ncatt_put(season_file, integral_bounds, "flag_meaning", "0:total, 1:aerosol boundary layer", prec = "text")
    
    ncatt_put(season_file, "stats", "flag_value", "0,1,2,3,4", prec = "text")
    ncatt_put(
      season_file,
      "stats",
      "flag_meaning",
      "0:mean, 1:statistical error mean, 2:median, 3: standard deviation, 4:number of values aggregated"
    )
    
    ncatt_put(season_file, "time", "axis", "T", prec = "text")
    ncatt_put(season_file, "time", "standard_name", "time", prec = "text")
    ncatt_put(season_file, "time", "bounds", "time_bounds", prec = "text")
    
    ncatt_put(season_file,
      aerosol_optical_depth,
      "standard_name",
      "atmosphere_optical_thickness_due_to_ambient_aerosol_particles",
      prec = "text")
    
    ncatt_put(season_file, latitude, "standard_name", "latitude", prec = "text")
    ncatt_put(season_file, longitude, "standard_name", "longitude", prec = "text")
    
    ncatt_put(season_file,
      source,
      "description",
      "List of level 2 files from which are retrieved values averaged in this file",
      prec = "text")
    
    # Enter the values into the file
    ncvar_put(season_file, integral_bounds, integral_bounds_data)
    ncvar_put(season_file, aerosol_optical_depth, aerosol_optical_depth_data)
    ncvar_put(season_file, integrated_backscatter, integrated_backscatter_data)
    ncvar_put(season_file, lidar_ratio, lidar_ratio_data)
    ncvar_put(season_file, aerosol_boundary_layer, aerosol_boundary_layer_data)
    ncvar_put(season_file, h63_of_aerosol_optical_depth, h63_of_aerosol_optical_depth_data)
    ncvar_put(season_file, h63_of_integrated_backscatter, h63_of_integrated_backscatter_data)
    ncvar_put(season_file, center_of_mass, center_of_mass_data)
    ncvar_put(season_file, particle_depolarization, particle_depolarization_data)
    ncvar_put(season_file, angstrom_coefficient, angstrom_data)
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
              attval = syst[p, y - 1999],
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
    ncatt_put(season_file,
              0,
              attname = "title",
              attval = paste0("Seasonal average integrated measurements - year ", as.character(y)))
    nc_close(season_file)
    
    file_name <- paste0(
      "ACTRIS_AerRemSen_",
      station,
      "_Lev03_Season_",
      as.character(y),
      "_Int_v03_qc040.nc"
    )
    file.rename(
      from = file.path(getwd(), file_name),
      to = file.path(getwd(), "Level3/Integrated", station, file_name)
    )
    
  }
}