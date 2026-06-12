if (flag_355) {
  db_prof_ns_355[, 4:21] <- signif(db_prof_ns_355[, 4:21], 6)
}

if (flag_532) {
  db_prof_ns_532[, 4:21] <- signif(db_prof_ns_532[, 4:21], 6)
}

if (flag_1064) {
  db_prof_ns_1064[, 4:10] <- signif(db_prof_ns_1064[, 4:10], 6)
  db_prof_ns_1064[, 12:14] <- signif(db_prof_ns_1064[, 12:14], 6)
}

# Global data is generated
altitude_data <- seq(from = 200, to = 12000, by = 200)
appo <- data.frame(Altitude = altitude_data, Null = NA)

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
  
  time_bounds_data[, i] <- c(
    as.numeric(as.POSIXct(start_date, tz = "GMT")),
    as.numeric(as.POSIXct(end_date, tz = "GMT"))
  )
}

# Define time data
val_time <- as.numeric(as.POSIXct(paste0("2000-", sprintf("%02d", c(1, 4, 7, 10)), "-15 23:59:59"), tz = "GMT", origin = "1970-01-01 00:00:00"))
ssn <- c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")

# Process each station
for (p in seq_along(loc2)) {
  station <- loc2[p]
  
  # Initialize arrays for data
  extinction_data <- array(NA, dim = c(60, 4, 3, 5))
  backscatter_data <- array(NA, dim = c(60, 4, 3, 5))
  volume_depolarization_data <- array(NA, dim = c(60, 4, 3, 5))
  
  if (flag_355) {
    db_355 <- db_prof_ns_355[db_prof_ns_355$Station == station, c(2:27)]
    colnames(db_355)[3:26] = paste0(colnames(db_355)[3:26], "_355")
  }
  if (flag_532) {
    db_532 = db_prof_ns_532[db_prof_ns_532$Station == station, c(2:27)]
    colnames(db_532)[3:26] = paste0(colnames(db_532)[3:26], "_532")
  }
  if (flag_1064) {
    db_1064 = db_prof_ns_1064[db_prof_ns_1064$Station == station, c(2:19)]
    colnames(db_1064)[3:18] = paste0(colnames(db_1064)[3:18], "_1064")
  }
  
  for (l in 1:4) {
    if (flag_355 && ssn[l] %in% unique(db_355$Season)) {
      db_355_new <- merge(db_355[db_355$Season == ssn[l], ], appo, by = "Altitude", all = TRUE)[, -2]
      
      # Size check and fit
      if (nrow(db_355_new) < 60) {
        db_355_new <- rbind(db_355_new, data.frame(matrix(NA, nrow = 60 - nrow(db_355_new), ncol = ncol(db_355_new))))
      }
      
      extinction_data[, l, 1, ] <- as.matrix(db_355_new[1:60, c(2, 3, 8, 14, 23)])
      backscatter_data[, l, 1, ] <- as.matrix(db_355_new[1:60, c(4, 5, 10, 16, 24)])
      volume_depolarization_data[, l, 1, ] <- as.matrix(db_355_new[1:60, c(6, 7, 12, 18, 25)])
    } else {
      extinction_data[, l, 1, ] <- matrix(NA, nrow = 60, ncol = 5)
      backscatter_data[, l, 1, ] <- matrix(NA, nrow = 60, ncol = 5)
      volume_depolarization_data[, l, 1, ] <- matrix(NA, nrow = 60, ncol = 5)
    }
    
    if (flag_532 && ssn[l] %in% unique(db_532$Season)) {
      db_532_new <- merge(db_532[db_532$Season == ssn[l], ], appo, by = "Altitude", all = TRUE)[, -2]
      
      # Size check and fit
      if (nrow(db_532_new) < 60) {
        db_532_new <- rbind(db_532_new, data.frame(matrix(NA, nrow = 60 - nrow(db_532_new), ncol = ncol(db_532_new))))
      }
      
      extinction_data[, l, 2, ] = as.matrix(db_532_new[1:60, c(2, 3, 8, 14, 23)])
      backscatter_data[, l, 2, ] = as.matrix(db_532_new[1:60, c(4, 5, 10, 16, 24)])
      volume_depolarization_data[, l, 2, ] = as.matrix(db_532_new[1:60, c(6, 7, 12, 18, 25)])
    } else {
      extinction_data[, l, 2, ] <- matrix(NA, nrow = 60, ncol = 5)
      backscatter_data[, l, 2, ] <- matrix(NA, nrow = 60, ncol = 5)
      volume_depolarization_data[, l, 2, ] <- matrix(NA, nrow = 60, ncol = 5)
    }
    
    if (flag_1064 && ssn[l] %in% unique(db_1064$Season)) {
      db_1064_new <- merge(db_1064[db_1064$Season == ssn[l], ], appo, by = "Altitude", all = TRUE)[, -2]
      
      # Size check and fit
      if (nrow(db_1064_new) < 60) {
        db_1064_new <- rbind(db_1064_new, data.frame(matrix(NA, nrow = 60 - nrow(db_1064_new), ncol = ncol(db_1064_new))))
      }
      
      extinction_data[, l, 3, ] = matrix(NA, nrow = 60, ncol = 5)
      backscatter_data[, l, 3, ] = as.matrix(db_1064_new[1:60, c(2, 3, 6, 10, 16)])
      volume_depolarization_data[, l, 3, ] = as.matrix(db_1064_new[1:60, c(4, 5, 8, 12, 17)])
    } else {
      extinction_data[, l, 3, ] <- matrix(NA, nrow = 60, ncol = 5)
      backscatter_data[, l, 3, ] <- matrix(NA, nrow = 60, ncol = 5)
      volume_depolarization_data[, l, 3, ] <- matrix(NA, nrow = 60, ncol = 5)
    }
  }
  
  source_name <- lev2db[lev2db$Station == station, ]
  source_data <- paste(source_name[, 1], collapse = " ")
  
  extinction_data[is.na(extinction_data)] = 9.9692099683868690e+36
  backscatter_data[is.na(backscatter_data)] = 9.9692099683868690e+36
  volume_depolarization_data[is.na(volume_depolarization_data)] = 9.9692099683868690e+36
  
  # Dimensions are created
  nv <- ncdim_def("nv", "", 1:2, longname = "")
  altitude <- ncdim_def("altitude", "m", altitude_data, longname = "Altitude of middle point of layer")
  wavelength <- ncdim_def("wavelength", "nm", c(355, 532, 1064), longname = "Wavelength of the transmitted laser pulse")
  time <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", val_time, calendar = "gregorian", longname = "Time")
  n_char <- ncdim_def("n_char", "", 1:nchar(source_data), longname = "")
  stats <- ncdim_def("stats", "", 0:4, longname = "statistics")
  
  # The variables are created
  extinction <- ncvar_def(
    "extinction",
    "1/m",
    list(altitude, time, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "mean of aerosol particle extinction coefficient",
    prec = "double"
  )
  backscatter <- ncvar_def(
    "backscatter",
    "1/m*sr",
    list(altitude, time, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "mean of aerosol particle backscatter coefficient",
    prec = "double"
  )
  volume_depolarization <- ncvar_def(
    "volume_depolarization",
    "",
    list(altitude, time, wavelength, stats),
    missval = 9.9692099683868690e+36,
    longname = "mean of aerosol volume depolarization coefficient",
    prec = "double"
  )
  time_bounds <- ncvar_def("time_bounds",
                           "seconds since 1970-01-01T00:00:00Z",
                           list(nv, time),
                           prec = "double")
  source <- ncvar_def("source", "", n_char, longname = "source files", prec = "char")
  latitude <- ncvar_def("latitude",
                        "degrees_north",
                        list(),
                        longname = "latitude of the station",
                        prec = "float")
  longitude <- ncvar_def("longitude",
                         "degrees_east",
                         list(),
                         longname = "longitude of the station",
                         prec = "float")
  station_altitude <- ncvar_def("station_altitude",
                                "m",
                                list(),
                                longname = "station altitude above sea level",
                                prec = "float")
  variabili <- list(
    extinction,
    backscatter,
    volume_depolarization,
    time_bounds,
    source,
    latitude,
    longitude,
    station_altitude
  )
  
  # Defining the file by putting variables inside it
  ns_file <- nc_create(
    paste0(
      "ACTRIS_AerRemSen_",
      station,
      "_Lev03_NorSea",
      release_suffix, 
      "Pro_v03_qc040.nc"
    ),
    variabili,
    force_v4 = TRUE
  )
  
  # Add attributes to variables
  ncatt_put(ns_file, "altitude", "axis", "Z", prec = "text")
  ncatt_put(ns_file, "altitude", "positive", "up", prec = "text")
  ncatt_put(ns_file, "altitude", "standard_name", "altitude", prec = "text")
  
  ncatt_put(ns_file, "time", "axis", "T", prec = "text")
  ncatt_put(ns_file, "time", "standard_name", "time", prec = "text")
  ncatt_put(ns_file, "time", "bounds", "time_bounds", prec = "text")
  
  ncatt_put(ns_file, "stats", "flag_value", "0,1,2,3,4", prec = "text")
  ncatt_put(ns_file,
            "stats",
            "flag_meaning",
            "0:mean, 1:statistical error mean, 2:median, 3: standard deviation, 4:number of profiles aggregated")
  ncatt_put(ns_file,
            extinction,
            "standard_name",
            "volume_extinction_coefficient_in_air_due_to_ambient_aerosol_particles",
            prec = "text")
  
  ncatt_put(ns_file, latitude, "standard_name", "latitude", prec = "text")
  ncatt_put(ns_file, longitude, "standard_name", "longitude", prec = "text")
  ncatt_put(ns_file,
            source,
            "description",
            "List of level 2 files from which are retrieved values averaged in this file")
  
  # Enter the values into the file
  ncvar_put(ns_file, "altitude", altitude_data)
  ncvar_put(ns_file, extinction, extinction_data)
  ncvar_put(ns_file, backscatter, backscatter_data)
  ncvar_put(ns_file, volume_depolarization, volume_depolarization_data)
  ncvar_put(ns_file, time_bounds, time_bounds_data)
  ncvar_put(ns_file, latitude, latit[p])
  ncvar_put(ns_file, longitude, longit[p])
  ncvar_put(ns_file, station_altitude, station_alt[p])
  ncvar_put(ns_file, source, source_data)
  
  # Global attributes
  ncatt_put(ns_file,
            0,
            attname = "processor_name",
            attval = "EAR_clim_v1.exe",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "processor_version",
            attval = "",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "processor_institution",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "system",
            attval = sist[p],
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "location",
            attval = loc[p],
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "institution",
            attval = institution[p],
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "PI",
            attval = PI[p],
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "PI_affiliation",
            attval = institution[p],
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "PI_affiliation_acronym",
            attval = acronym[p],
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "PI_address",
            attval = "",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "PI_phone",
            attval = "",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "PI_email",
            attval = PI_contact[p],
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_originator",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_originator_affiliation",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_originator_affiliation_acronym",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_originator_address",
            attval = "",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_originator_phone",
            attval = "",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_originator_email",
            attval = "earlinetdb@actris.imaa.cnr.it",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_provider",
            attval = "ACTRIS ARES",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_provider_affiliation",
            attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_provider_affiliation_acronym",
            attval = "CNR - IMAA",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_provider_address",
            attval = "",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_provider_phone",
            attval = "",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "data_provider_email",
            attval = "earlinetdb@actris.imaa.cnr.it",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "conventions",
            attval = "C.F. - 1.8",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "references",
            attval = "link doc earlinet.org",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "station_ID",
            attval = station,
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "__file_format_version",
            attval = "",
            prec = "text")
  ncatt_put(ns_file,
            0,
            attname = "history",
            attval = paste0(Sys.time(), "Generated by free software R, using package ncdf4"),
            prec = "text")
  ncatt_put(ns_file, 
            0, 
            attname = "title", 
            attval = "Season average profile measurements - Normal")
  
  nc_close(ns_file)
  
  file_name <- paste0(
    "ACTRIS_AerRemSen_",
    station,
    "_Lev03_NorSea",
    release_suffix,
    "Pro_v03_qc040.nc"
  )
  file.rename(
    from = file.path(getwd(), file_name),
    to = file.path(getwd(), "Level3/Profiles", station, file_name)
  )
  
}
