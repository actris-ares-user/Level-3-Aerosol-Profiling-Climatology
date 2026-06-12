if (flag_355) {
  db_prof_year_355[, 4:21] = signif(db_prof_year_355[, 4:21], 6)
}

if (flag_532) {
  db_prof_year_532[, 4:21] = signif(db_prof_year_532[, 4:21], 6)
}

if (flag_1064) {
  db_prof_year_1064[, c(4, 5, 8, 9, 12, 13)] = signif(db_prof_year_1064[, c(4, 5, 8, 9, 12, 13)], 6)
}

# Global data is generated
altitude_data <- seq(200, 12000, 200)
appo <- data.frame(Altitude = altitude_data, Null = NA)

for (p in seq_along(loc2)) {
  station <- loc2[p]
  
  for (y in release[1]:release[2]) {
    # Initialize arrays for data
    extinction_data <- array(NA, dim = c(60, 3, 5))
    backscatter_data <- array(NA, dim = c(60, 3, 5))
    volume_depolarization_data <- array(NA, dim = c(60, 3, 5))
    
    if (flag_355) {
      db_355 <- db_prof_year_355[db_prof_year_355$Station == station &
                                   db_prof_year_355$Year == y, c(3:27)]
      colnames(db_355)[2:25] <- paste0(colnames(db_355)[2:25], "_355")
      
      db_355 <- merge(db_355, appo, by = "Altitude", all = TRUE)
      
      extinction_data[, 1, ] <- as.matrix(db_355[, c(2, 3, 8, 14, 23)])
      backscatter_data[, 1, ] <- as.matrix(db_355[, c(4, 5, 10, 16, 24)])
      volume_depolarization_data[, 1, ] <- as.matrix(db_355[, c(6, 7, 12, 18, 25)])
    } else {
      extinction_data[, 1, ] <- matrix(NA, nrow = 60, ncol = 5)
      backscatter_data[, 1, ] <- matrix(NA, nrow = 60, ncol = 5)
      volume_depolarization_data[, 1, ] <- matrix(NA, nrow = 60, ncol = 5)
    }
    
    if (flag_532) {
      db_532 <- db_prof_year_532[db_prof_year_532$Station == station &
                                   db_prof_year_532$Year == y, c(3:27)]
      colnames(db_532)[2:25] <- paste0(colnames(db_532)[2:25], "_532")
      
      db_532 <- merge(db_532, appo, by = "Altitude", all = TRUE)
      
      extinction_data[, 2, ] <- as.matrix(db_532[, c(2, 3, 8, 14, 23)])
      backscatter_data[, 2, ] <- as.matrix(db_532[, c(4, 5, 10, 16, 24)])
      volume_depolarization_data[, 2, ] <- as.matrix(db_532[, c(6, 7, 12, 18, 25)])
    } else {
      extinction_data[, 2, ] <- matrix(NA, nrow = 60, ncol = 5)
      backscatter_data[, 2, ] <- matrix(NA, nrow = 60, ncol = 5)
      volume_depolarization_data[, 2, ] <- matrix(NA, nrow = 60, ncol = 5)
    }
    
    if (flag_1064) {
      db_1064 <- db_prof_year_1064[db_prof_year_1064$Station == station &
                                     db_prof_year_1064$Year == y, c(3:19)]
      colnames(db_1064)[2:17] <- paste0(colnames(db_1064)[2:17], "_1064")
      
      db_1064 <- merge(db_1064, appo, by = "Altitude", all = TRUE)
      
      extinction_data[, 3, ] <- matrix(NA, nrow = 60, ncol = 5)
      backscatter_data[, 3, ] <- as.matrix(db_1064[, c(2, 3, 6, 10, 16)])
      volume_depolarization_data[, 3, ] <- as.matrix(db_1064[, c(4, 5, 8, 12, 17)])
    } else {
      extinction_data[, 3, ] <- matrix(NA, nrow = 60, ncol = 5)
      backscatter_data[, 3, ] <- matrix(NA, nrow = 60, ncol = 5)
      volume_depolarization_data[, 3, ] <- matrix(NA, nrow = 60, ncol = 5)
    }
    
    
    source_name <- subset(lev2db, Station == station & lev2db$Year == y, 1)
    source_data <- paste(source_name[, 1], collapse = " ")
    
    extinction_data[is.na(extinction_data)] = 9.9692099683868690e+36
    backscatter_data[is.na(backscatter_data)] = 9.9692099683868690e+36
    volume_depolarization_data[is.na(volume_depolarization_data)] = 9.9692099683868690e+36
    
    time_bounds_data <- as.numeric(as.POSIXct(paste0(
      as.character(y), c("-01-01 00:00:00", "-12-31 23:59:59")
    ), tz = "GMT", origin = "1970-01-01 00:00:00"))
    
    # Dimensions are created
    nv <- ncdim_def("nv", "", 1:2)
    altitude <- ncdim_def("altitude", "m", altitude_data, longname = "Altitude")
    wavelength <- ncdim_def("wavelength", "nm", c(355, 532, 1064), longname = "Wavelength of the transmitted laser pulse")
    val_time <- as.numeric(as.POSIXct(paste0(as.character(y), "-06-30 23:59:59"), tz = "GMT", origin = "1970-01-01 00:00:00"))
    time <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", val_time, calendar = "gregorian", longname = "Time")
    n_char <- ncdim_def("n_char", "", 1:nchar(source_data))
    stats <- ncdim_def("stats", "", 0:4, longname = "statistics")
  
    # The variables are created
    extinction <- ncvar_def(
      "extinction",
      "1/m",
      list(altitude, time, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "aerosol particle extinction coefficient",
      prec = "double"
    )
    backscatter <- ncvar_def(
      "backscatter",
      "1/m*sr",
      list(altitude, time, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "aerosol particle backscatter coefficient",
      prec = "double"
    )
    volume_depolarization <- ncvar_def(
      "volume_depolarization",
      "",
      list(altitude, time, wavelength, stats),
      missval = 9.9692099683868690e+36,
      longname = "aerosol volume depolarization coefficient",
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
    year_file <- nc_create(
      paste0(
        "ACTRIS_AerRemSen_",
        station,
        "_Lev03_Annual_",
        as.character(y),
        "_Pro_v03_qc040.nc"
      ),
      variabili,
      force_v4 = TRUE
    )
    
    # Add attributes to variables
    ncatt_put(year_file, "altitude", "axis", "Z", prec = "text")
    ncatt_put(year_file, "altitude", "positive", "up", prec = "text")
    ncatt_put(year_file, "altitude", "standard_name", "altitude", prec = "text")
    
    ncatt_put(year_file, "time", "axis", "T", prec = "text")
    ncatt_put(year_file, "time", "standard_name", "time", prec = "text")
    ncatt_put(year_file, "time", "bounds", "time_bounds", prec = "text")
    
    ncatt_put(year_file, "stats", "flag_value", "0,1,2,3,4", prec = "text")
    ncatt_put(
      year_file,
      "stats",
      "flag_meaning",
      "0:mean, 1:statistical error mean, 2:median, 3: standard deviation, 4:number of profiles aggregated"
    )
    
    ncatt_put(
      year_file,
      extinction,
      "standard_name",
      "volume_extinction_coefficient_in_air_due_to_ambient_aerosol_particles",
      prec = "text"
    )
    
    ncatt_put(year_file, latitude, "standard_name", "latitude", prec = "text")
    ncatt_put(year_file, longitude, "standard_name", "longitude", prec = "text")
    ncatt_put(
      year_file,
      source,
      "description",
      "List of level 2 files from which are retrieved values averaged in this file"
    )
    
    # Enter the values into the file
    ncvar_put(year_file, "altitude", altitude_data)
    ncvar_put(year_file, extinction, extinction_data)
    ncvar_put(year_file, backscatter, backscatter_data)
    ncvar_put(year_file, volume_depolarization, volume_depolarization_data)
    ncvar_put(year_file, time_bounds, time_bounds_data)
    ncvar_put(year_file, latitude, latit[p])
    ncvar_put(year_file, longitude, longit[p])
    ncvar_put(year_file, station_altitude, station_alt[p])
    ncvar_put(year_file, source, source_data)
    
    # Global attributes
    ncatt_put(year_file,
              0,
              attname = "processor_name",
              attval = "EAR_clim_v1.exe",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "processor_version",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "processor_institution",
              attval = "CNR - IMAA",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "system",
              attval = syst[p, y - 1999],
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "location",
              attval = loc[p],
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "institution",
              attval = institution[p],
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "PI",
              attval = PI[p],
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "PI_affiliation",
              attval = institution[p],
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "PI_affiliation_acronym",
              attval = acronym[p],
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "PI_address",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "PI_phone",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "PI_email",
              attval = PI_contact[p],
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator",
              attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_affiliation",
              attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_affiliation_acronym",
              attval = "CNR - IMAA",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_address",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_phone",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_originator_email",
              attval = "earlinetdb@actris.imaa.cnr.it",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider",
              attval = "ACTRIS ARES",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_affiliation",
              attval = "Consiglio Nazionale delle Ricerche - Istituto di Metodologie per l'Analisi Ambientale",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_affiliation_acronym",
              attval = "CNR - IMAA",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_address",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_phone",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "data_provider_email",
              attval = "earlinetdb@actris.imaa.cnr.it",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "conventions",
              attval = "C.F. - 1.8",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "references",
              attval = "link doc earlinet.org",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "station_ID",
              attval = station,
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "__file_format_version",
              attval = "",
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "history",
              attval = paste0(Sys.time(), "Generated by free software R, using package ncdf4"),
              prec = "text")
    ncatt_put(year_file,
              0,
              attname = "title",
              attval = paste0("Annual average profile measurements - year ", as.character(y)))
    
    nc_close(year_file)
    
    file_name <- paste0(
      "ACTRIS_AerRemSen_",
      station,
      "_Lev03_Annual_",
      as.character(y),
      "_Pro_v03_qc040.nc"
    )
    file.rename(
      from = file.path(getwd(), file_name),
      to = file.path(getwd(), "Level3/Profiles", station, file_name)
    )
  }
}
