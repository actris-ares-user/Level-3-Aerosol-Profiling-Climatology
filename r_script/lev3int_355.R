if (flag_355) {
  db_e <- rbind(db_int_e[db_int_e$Wavelength == "0355", ], db_int_e[db_int_e$Wavelength == "0351" &
                                                                      db_int_e$Station == "laq", ])
  db_b <- rbind(db_int_b[db_int_b$Wavelength == "0355", ], db_int_b[db_int_b$Wavelength == "0351" &
                                                                      db_int_b$Station == "laq", ])
  
  ################################# Monthly averages #################################
  
  db_e_m <- db_e[, c(2, 5:6, 8:20)]
  db_b_m <- db_b[, c(2, 5:6, 8:20)]
  
  db_e_m_mean <- aggregate(
    db_e_m[4:16],
    by = list(db_e_m$Station, db_e_m$Year, db_e_m$Month),
    FUN = mean,
    na.rm = TRUE
  )
  db_b_m_mean <- aggregate(
    db_b_m[4:16],
    by = list(db_b_m$Station, db_b_m$Year, db_b_m$Month),
    FUN = mean,
    na.rm = TRUE
  )
  
  colnames(db_e_m_mean)[1:3] <- c("Station", "Year", "Month")
  colnames(db_b_m_mean)[1:3] <- c("Station", "Year", "Month")
  
  db_e_m_mean <- db_e_m_mean[order(db_e_m_mean$Station, db_e_m_mean$Year, db_e_m_mean$Month), ]
  db_e_m_mean[is.na(db_e_m_mean)] <- NA
  rownames(db_e_m_mean) <- 1:nrow(db_e_m_mean)
  db_b_m_mean <- db_b_m_mean[order(db_b_m_mean$Station, db_b_m_mean$Year, db_b_m_mean$Month), ]
  db_b_m_mean[is.na(db_b_m_mean)] <- NA
  rownames(db_b_m_mean) <- 1:nrow(db_b_m_mean)
  
  db_e_m_cnt <- aggregate(
    db_e_m[4:16],
    by = list(db_e_m$Station, db_e_m$Year, db_e_m$Month),
    FUN = cnt_int
  )
  db_b_m_cnt <- aggregate(
    db_b_m[4:16],
    by = list(db_b_m$Station, db_b_m$Year, db_b_m$Month),
    FUN = cnt_int
  )
  colnames(db_e_m_cnt)[1:3] <- c("Station", "Year", "Month")
  colnames(db_b_m_cnt)[1:3] <- c("Station", "Year", "Month")
  
  db_e_m_cnt <- db_e_m_cnt[order(db_e_m_cnt$Station, db_e_m_cnt$Year, db_e_m_cnt$Month), ]
  db_e_m_cnt[is.na(db_e_m_cnt)] <- NA
  rownames(db_e_m_cnt) <- 1:nrow(db_e_m_cnt)
  db_b_m_cnt <- db_b_m_cnt[order(db_b_m_cnt$Station, db_b_m_cnt$Year, db_b_m_cnt$Month), ]
  db_b_m_cnt[is.na(db_b_m_cnt)] <- NA
  rownames(db_b_m_cnt) <- 1:nrow(db_b_m_cnt)
  
  ################################# Annual averages #################################
  
  db_e_y <- db_e[, c(2, 5, 8:20)]
  db_b_y <- db_b[, c(2, 5, 8:20)]
  
  wgs_e <- aggregate(db_e_m_cnt[4:16],
                     by = list(db_e_m_cnt$Station, db_e_m_cnt$Year),
                     FUN = ws)
  wgs_b <- aggregate(db_b_m_cnt[4:16],
                     by = list(db_b_m_cnt$Station, db_b_m_cnt$Year),
                     FUN = ws)
  
  colnames(wgs_e)[1:2] <- c("Station", "Year")
  colnames(wgs_b)[1:2] <- c("Station", "Year")
  
  wgs_e <- wgs_e[order(wgs_e$Station, wgs_e$Year), ]
  wgs_b <- wgs_b[order(wgs_b$Station, wgs_b$Year), ]
  
  db_e_y_mean <- aggregate(
    db_e_m_mean[4:16],
    by = list(db_e_m_mean$Station, db_e_m_mean$Year),
    FUN = mean,
    na.rm = TRUE
  )
  db_b_y_mean <- aggregate(
    db_b_m_mean[4:16],
    by = list(db_b_m_mean$Station, db_b_m_mean$Year),
    FUN = mean,
    na.rm = TRUE
  )
  
  colnames(db_e_y_mean)[1:2] <- c("Station", "Year")
  colnames(db_b_y_mean)[1:2] <- c("Station", "Year")
  db_e_y_mean[is.na(db_e_y_mean)] <- NA
  db_b_y_mean[is.na(db_b_y_mean)] <- NA
  db_e_y_mean <- db_e_y_mean[order(db_e_y_mean$Station, db_e_y_mean$Year), ]
  db_b_y_mean <- db_b_y_mean[order(db_b_y_mean$Station, db_b_y_mean$Year), ]
  
  db_e_y_c <- aggregate(db_e_y[3:15],
                        by = list(db_e_y$Station, db_e_y$Year),
                        FUN = c)
  db_b_y_c <- aggregate(db_b_y[3:15],
                        by = list(db_b_y$Station, db_b_y$Year),
                        FUN = c)
  colnames(db_e_y_c) <- colnames(db_e_y)
  colnames(db_b_y_c) <- colnames(db_b_y)
  db_e_y_c <- db_e_y_c[order(db_e_y_c$Station, db_e_y_c$Year), ]
  db_b_y_c <- db_b_y_c[order(db_b_y_c$Station, db_b_y_c$Year), ]
  
  db_e_y_median <- data.frame(matrix(NA, nrow = nrow(db_e_y_c), ncol = 15))
  colnames(db_e_y_median) <- colnames(db_e_y_c)
  db_e_y_median[, 1:2] <- db_e_y_c[, 1:2]
  db_b_y_median <- data.frame(matrix(NA, nrow = nrow(db_b_y_c), ncol = 15))
  colnames(db_b_y_median) <- colnames(db_b_y_c)
  db_b_y_median[, 1:2] <- db_b_y_c[, 1:2]
  db_e_y_sd <- data.frame(matrix(NA, nrow = nrow(db_e_y_c), ncol = 15))
  colnames(db_e_y_sd) <- colnames(db_e_y_c)
  db_e_y_sd[, 1:2] <- db_e_y_c[, 1:2]
  db_b_y_sd <- data.frame(matrix(NA, nrow = nrow(db_b_y_c), ncol = 15))
  colnames(db_b_y_sd) <- colnames(db_b_y_c)
  db_b_y_sd[, 1:2] <- db_b_y_c[, 1:2]
  
  for (j in 3:15) {
    db_e_y_median <- assign_with_fallback(db_e_y_median, db_e_y_c, wgs_e, j, w_median_int)
    db_b_y_median <- assign_with_fallback(db_b_y_median, db_b_y_c, wgs_b, j, w_median_int)
    db_e_y_sd <- assign_with_fallback(db_e_y_sd, db_e_y_c, wgs_e, j, w_sd_int)
    db_b_y_sd <- assign_with_fallback(db_b_y_sd, db_b_y_c, wgs_b, j, w_sd_int)
  }
  
  db_e_y_cnt <- aggregate(db_e_m[4:16],
                          by = list(db_e_m$Station, db_e_m$Year),
                          FUN = cnt_int)
  db_b_y_cnt <- aggregate(db_b_m[4:16],
                          by = list(db_b_m$Station, db_b_m$Year),
                          FUN = cnt_int)
  
  colnames(db_e_y_cnt)[1:2] <- c("Station", "Year")
  colnames(db_b_y_cnt)[1:2] <- c("Station", "Year")
  db_e_y_cnt <- db_e_y_cnt[order(db_e_y_cnt$Station, db_e_y_cnt$Year), ]
  db_b_y_cnt <- db_b_y_cnt[order(db_b_y_cnt$Station, db_b_y_cnt$Year), ]
  
  colnames(db_e_y_mean)[3:15] <- paste0(colnames(db_e_y_mean)[3:15], "_Mean_355")
  colnames(db_b_y_mean)[3:15] <- paste0(colnames(db_b_y_mean)[3:15], "_Mean_355")
  
  colnames(db_e_y_median)[3:15] <- paste0(colnames(db_e_y_median)[3:15], "_Median_355")
  colnames(db_b_y_median)[3:15] <- paste0(colnames(db_b_y_median)[3:15], "_Median_355")
  
  colnames(db_e_y_sd)[3:15] <- paste0(colnames(db_e_y_sd)[3:15], "_StDev_355")
  colnames(db_b_y_sd)[3:15] <- paste0(colnames(db_b_y_sd)[3:15], "_StDev_355")
  
  colnames(db_e_y_cnt)[3:15] <- paste0(colnames(db_e_y_cnt)[3:15], "_NumberValues_355")
  colnames(db_b_y_cnt)[3:15] <- paste0(colnames(db_b_y_cnt)[3:15], "_NumberValues_355")
  
  rownames(db_b_y_mean) <- 1:nrow(db_b_y_mean)
  rownames(db_b_y_median) <- 1:nrow(db_b_y_mean)
  rownames(db_b_y_sd) <- 1:nrow(db_b_y_mean)
  rownames(db_b_y_cnt) <- 1:nrow(db_b_y_mean)
  
  rownames(db_e_y_mean) <- 1:nrow(db_e_y_mean)
  rownames(db_e_y_median) <- 1:nrow(db_e_y_mean)
  rownames(db_e_y_sd) <- 1:nrow(db_e_y_mean)
  rownames(db_e_y_cnt) <- 1:nrow(db_e_y_mean)
  
  db_b_y_355 <- cbind(db_b_y_mean, db_b_y_median[3:15], db_b_y_sd[3:15], db_b_y_cnt[3:15])
  db_e_y_355 <- cbind(db_e_y_mean, db_e_y_median[3:15], db_e_y_sd[3:15], db_e_y_cnt[3:15])
  
  ################################# Seasonal Averages #################################
  
  db_e_s <- db_e[, c(2, 5:6, 8:20)]
  db_b_s <- db_b[, c(2, 5:6, 8:20)]
  
  db_e_s$Season_Year <- mapply(seas, db_e_s[, 3], db_e_s[, 2])
  db_b_s$Season_Year <- mapply(seas, db_b_s[, 3], db_b_s[, 2])
  
  db_e_s <- db_e_s[, c(1, 17, 4:16)]
  db_b_s <- db_b_s[, c(1, 17, 4:16)]
  
  db_e_s_mean <- aggregate(
    db_e_s[3:15],
    by = list(db_e_s$Station, db_e_s$Season_Year),
    FUN = mean,
    na.rm = TRUE
  )
  colnames(db_e_s_mean)[1:2] <- c("Station", "Season_Year")
  db_e_s_mean <- db_e_s_mean[order(db_e_s_mean$Station,
                                   substr(db_e_s_mean$Season_Year, 11, 14),
                                   match(
                                     substr(db_e_s_mean$Season_Year, 1, 9),
                                     c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                   )), ]
  rownames(db_e_s_mean) <- 1:nrow(db_e_s_mean)
  db_e_s_mean[is.na(db_e_s_mean)] <- NA
  
  db_b_s_mean <- aggregate(
    db_b_s[3:15],
    by = list(db_b_s$Station, db_b_s$Season_Year),
    FUN = mean,
    na.rm = TRUE
  )
  colnames(db_b_s_mean)[1:2] <- c("Station", "Season_Year")
  db_b_s_mean <- db_b_s_mean[order(db_b_s_mean$Station,
                                   substr(db_b_s_mean$Season_Year, 11, 14),
                                   match(
                                     substr(db_b_s_mean$Season_Year, 1, 9),
                                     c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                   )), ]
  rownames(db_b_s_mean) <- 1:nrow(db_b_s_mean)
  db_b_s_mean[is.na(db_b_s_mean)] <- NA
  
  db_e_s_median <- aggregate(
    db_e_s[3:15],
    by = list(db_e_s$Station, db_e_s$Season_Year),
    FUN = median,
    na.rm = TRUE
  )
  colnames(db_e_s_median)[1:2] <- c("Station", "Season_Year")
  db_e_s_median <- db_e_s_median[order(
    db_e_s_median$Station,
    substr(db_e_s_median$Season_Year, 11, 14),
    match(
      substr(db_e_s_median$Season_Year, 1, 9),
      c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
    )
  ), ]
  rownames(db_e_s_median) <- 1:nrow(db_e_s_median)
  db_e_s_median[is.na(db_e_s_median)] <- NA
  
  db_b_s_median <- aggregate(
    db_b_s[3:15],
    by = list(db_b_s$Station, db_b_s$Season_Year),
    FUN = median,
    na.rm = TRUE
  )
  colnames(db_b_s_median)[1:2] <- c("Station", "Season_Year")
  db_b_s_median <- db_b_s_median[order(
    db_b_s_median$Station,
    substr(db_b_s_median$Season_Year, 11, 14),
    match(
      substr(db_b_s_median$Season_Year, 1, 9),
      c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
    )
  ), ]
  rownames(db_b_s_median) <- 1:nrow(db_b_s_median)
  db_b_s_median[is.na(db_b_s_median)] <- NA
  
  db_e_s_sd <- aggregate(
    db_e_s[3:15],
    by = list(db_e_s$Station, db_e_s$Season_Year),
    FUN = sd,
    na.rm = TRUE
  )
  colnames(db_e_s_sd)[1:2] <- c("Station", "Season_Year")
  db_e_s_sd <- db_e_s_sd[order(db_e_s_sd$Station,
                               substr(db_e_s_sd$Season_Year, 11, 14),
                               match(
                                 substr(db_e_s_sd$Season_Year, 1, 9),
                                 c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                               )), ]
  rownames(db_e_s_sd) <- 1:nrow(db_e_s_sd)
  db_e_s_sd[is.na(db_e_s_sd)] <- NA
  
  db_b_s_sd <- aggregate(
    db_b_s[3:15],
    by = list(db_b_s$Station, db_b_s$Season_Year),
    FUN = sd,
    na.rm = TRUE
  )
  colnames(db_b_s_sd)[1:2] <- c("Station", "Season_Year")
  db_b_s_sd <- db_b_s_sd[order(db_b_s_sd$Station,
                               substr(db_b_s_sd$Season_Year, 11, 14),
                               match(
                                 substr(db_b_s_sd$Season_Year, 1, 9),
                                 c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                               )), ]
  rownames(db_b_s_sd) <- 1:nrow(db_b_s_sd)
  db_b_s_sd[is.na(db_b_s_sd)] <- NA
  
  db_e_s_cnt <- aggregate(db_e_s[3:15],
                          by = list(db_e_s$Station, db_e_s$Season_Year),
                          FUN = cnt_int)
  colnames(db_e_s_cnt)[1:2] <- c("Station", "Season_Year")
  db_e_s_cnt <- db_e_s_cnt[order(db_e_s_cnt$Station,
                                 substr(db_e_s_cnt$Season_Year, 11, 14),
                                 match(
                                   substr(db_e_s_cnt$Season_Year, 1, 9),
                                   c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                 )), ]
  rownames(db_e_s_cnt) <- 1:nrow(db_e_s_cnt)
  db_e_s_cnt[is.na(db_e_s_cnt)] <- NA
  
  db_b_s_cnt <- aggregate(db_b_s[3:15],
                          by = list(db_b_s$Station, db_b_s$Season_Year),
                          FUN = cnt_int)
  colnames(db_b_s_cnt)[1:2] <- c("Station", "Season_Year")
  db_b_s_cnt <- db_b_s_cnt[order(db_b_s_cnt$Station,
                                 substr(db_b_s_cnt$Season_Year, 11, 14),
                                 match(
                                   substr(db_b_s_cnt$Season_Year, 1, 9),
                                   c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                 )), ]
  rownames(db_b_s_cnt) <- 1:nrow(db_b_s_cnt)
  db_b_s_cnt[is.na(db_b_s_cnt)] <- NA
  
  colnames(db_e_s_mean)[3:15] <- paste0(colnames(db_e_s_mean)[3:15], "_Mean_355")
  colnames(db_b_s_mean)[3:15] <- paste0(colnames(db_b_s_mean)[3:15], "_Mean_355")
  
  colnames(db_e_s_median)[3:15] <- paste0(colnames(db_e_s_median)[3:15], "_Median_355")
  colnames(db_b_s_median)[3:15] <- paste0(colnames(db_b_s_median)[3:15], "_Median_355")
  
  colnames(db_e_s_sd)[3:15] <- paste0(colnames(db_e_s_sd)[3:15], "_StDev_355")
  colnames(db_b_s_sd)[3:15] <- paste0(colnames(db_b_s_sd)[3:15], "_StDev_355")
  
  colnames(db_e_s_cnt)[3:15] <- paste0(colnames(db_e_s_cnt)[3:15], "_NumberValues_355")
  colnames(db_b_s_cnt)[3:15] <- paste0(colnames(db_b_s_cnt)[3:15], "_NumberValues_355")
  
  db_b_s_355 <- cbind(db_b_s_mean, db_b_s_median[3:15], db_b_s_sd[3:15], db_b_s_cnt[3:15])
  db_e_s_355 <- cbind(db_e_s_mean, db_e_s_median[3:15], db_e_s_sd[3:15], db_e_s_cnt[3:15])
  
  ################################# Normal Monthly Averages #################################
  
  db_e_nm <- db_e[, c(2, 6, 8:20)]
  db_b_nm <- db_b[, c(2, 6, 8:20)]
  
  wgs_e <- aggregate(
    db_e_m_cnt[4:16],
    by = list(db_e_m_cnt$Station, db_e_m_cnt$Month),
    FUN = ws
  )
  wgs_b <- aggregate(
    db_b_m_cnt[4:16],
    by = list(db_b_m_cnt$Station, db_b_m_cnt$Month),
    FUN = ws
  )
  
  colnames(wgs_e)[1:2] <- c("Station", "Month")
  colnames(wgs_b)[1:2] <- c("Station", "Month")
  
  wgs_e <- wgs_e[order(wgs_e$Station, wgs_e$Month), ]
  wgs_b <- wgs_b[order(wgs_b$Station, wgs_b$Month), ]
  
  db_e_nm_mean <- aggregate(
    db_e_m_mean[4:16],
    by = list(db_e_m_mean$Station, db_e_m_mean$Month),
    FUN = mean,
    na.rm = TRUE
  )
  db_b_nm_mean <- aggregate(
    db_b_m_mean[4:16],
    by = list(db_b_m_mean$Station, db_b_m_mean$Month),
    FUN = mean,
    na.rm = TRUE
  )
  
  colnames(db_e_nm_mean)[1:2] <- c("Station", "Month")
  colnames(db_b_nm_mean)[1:2] <- c("Station", "Month")
  db_e_nm_mean[is.na(db_e_nm_mean)] <- NA
  db_b_nm_mean[is.na(db_b_nm_mean)] <- NA
  db_e_nm_mean <- db_e_nm_mean[order(db_e_nm_mean$Station, db_e_nm_mean$Month), ]
  db_b_nm_mean <- db_b_nm_mean[order(db_b_nm_mean$Station, db_b_nm_mean$Month), ]
  
  db_e_nm_c <- aggregate(db_e_nm[3:15],
                         by = list(db_e_nm$Station, db_e_nm$Month),
                         FUN = c)
  db_b_nm_c <- aggregate(db_b_nm[3:15],
                         by = list(db_b_nm$Station, db_b_nm$Month),
                         FUN = c)
  colnames(db_e_nm_c) <- colnames(db_e_nm)
  colnames(db_b_nm_c) <- colnames(db_b_nm)
  db_e_nm_c <- db_e_nm_c[order(db_e_nm_c$Station, db_e_nm_c$Month), ]
  db_b_nm_c <- db_b_nm_c[order(db_b_nm_c$Station, db_b_nm_c$Month), ]
  
  db_e_nm_median <- data.frame(matrix(NA, nrow = nrow(db_e_nm_c), ncol = 15))
  colnames(db_e_nm_median) <- colnames(db_e_nm_c)
  db_e_nm_median[, 1:2] <- db_e_nm_c[, 1:2]
  db_b_nm_median <- data.frame(matrix(NA, nrow = nrow(db_b_nm_c), ncol = 15))
  colnames(db_b_nm_median) <- colnames(db_b_nm_c)
  db_b_nm_median[, 1:2] <- db_b_nm_c[, 1:2]
  db_e_nm_sd <- data.frame(matrix(NA, nrow = nrow(db_e_nm_c), ncol = 15))
  colnames(db_e_nm_sd) <- colnames(db_e_nm_c)
  db_e_nm_sd[, 1:2] <- db_e_nm_c[, 1:2]
  db_b_nm_sd <- data.frame(matrix(NA, nrow = nrow(db_b_nm_c), ncol = 15))
  colnames(db_b_nm_sd) <- colnames(db_b_nm_c)
  db_b_nm_sd[, 1:2] <- db_b_nm_c[, 1:2]
  
  for (j in 3:15) {
    tryCatch({
      db_e_nm_median[, j] <- mapply(w_median_int, db_e_nm_c[, j], wgs_e[, j])
    }, error = function(e) {
      db_e_nm_median[, j] <<- median(mapply(w_median_int, db_e_nm_c[, j], wgs_e[, j]))
    })
    tryCatch({
      db_b_nm_median[, j] <- mapply(w_median_int, db_b_nm_c[, j], wgs_b[, j])
    }, error = function(e) {
      db_b_nm_median[, j] <<- median(mapply(w_median_int, db_b_nm_c[, j], wgs_b[, j]))
    })
    tryCatch({
      db_e_nm_sd[, j] <- mapply(w_sd_int, db_e_nm_c[, j], wgs_e[, j])
    }, error = function(e) {
      db_e_nm_sd[, j] <<- sd(mapply(w_sd_int, db_e_nm_c[, j], wgs_e[, j]))
    })
    tryCatch({
      db_b_nm_sd[, j] <- mapply(w_sd_int, db_b_nm_c[, j], wgs_b[, j])
    }, error = function(e){
      db_b_nm_sd[, j] <<- sd(mapply(w_sd_int, db_b_nm_c[, j], wgs_b[, j]))
    })
  }
  
  db_e_nm_cnt <- aggregate(db_e_m[4:16],
                           by = list(db_e_m$Station, db_e_m$Month),
                           FUN = cnt_int)
  db_b_nm_cnt <- aggregate(db_b_m[4:16],
                           by = list(db_b_m$Station, db_b_m$Month),
                           FUN = cnt_int)
  
  colnames(db_e_nm_cnt)[1:2] <- c("Station", "Month")
  colnames(db_b_nm_cnt)[1:2] <- c("Station", "Month")
  db_e_nm_cnt <- db_e_nm_cnt[order(db_e_nm_cnt$Station, db_e_nm_cnt$Month), ]
  db_b_nm_cnt <- db_b_nm_cnt[order(db_b_nm_cnt$Station, db_b_nm_cnt$Month), ]
  
  colnames(db_e_nm_mean)[3:15] <- paste0(colnames(db_e_nm_mean)[3:15], "_Mean_355")
  colnames(db_b_nm_mean)[3:15] <- paste0(colnames(db_b_nm_mean)[3:15], "_Mean_355")
  
  colnames(db_e_nm_median)[3:15] <- paste0(colnames(db_e_nm_median)[3:15], "_Median_355")
  colnames(db_b_nm_median)[3:15] <- paste0(colnames(db_b_nm_median)[3:15], "_Median_355")
  
  colnames(db_e_nm_sd)[3:15] <- paste0(colnames(db_e_nm_sd)[3:15], "_StDev_355")
  colnames(db_b_nm_sd)[3:15] <- paste0(colnames(db_b_nm_sd)[3:15], "_StDev_355")
  
  colnames(db_e_nm_cnt)[3:15] <- paste0(colnames(db_e_nm_cnt)[3:15], "_NumberValues_355")
  colnames(db_b_nm_cnt)[3:15] <- paste0(colnames(db_b_nm_cnt)[3:15], "_NumberValues_355")
  
  rownames(db_b_nm_mean) <- 1:nrow(db_b_nm_mean)
  rownames(db_b_nm_median) <- 1:nrow(db_b_nm_mean)
  rownames(db_b_nm_sd) <- 1:nrow(db_b_nm_mean)
  rownames(db_b_nm_cnt) <- 1:nrow(db_b_nm_mean)
  
  rownames(db_e_nm_mean) <- 1:nrow(db_e_nm_mean)
  rownames(db_e_nm_median) <- 1:nrow(db_e_nm_mean)
  rownames(db_e_nm_sd) <- 1:nrow(db_e_nm_mean)
  rownames(db_e_nm_cnt) <- 1:nrow(db_e_nm_mean)
  
  db_b_nm_355 <- cbind(db_b_nm_mean, db_b_nm_median[3:15], db_b_nm_sd[3:15], db_b_nm_cnt[3:15])
  db_e_nm_355 <- cbind(db_e_nm_mean, db_e_nm_median[3:15], db_e_nm_sd[3:15], db_e_nm_cnt[3:15])
  
  ################################# Normal Seasonal Averages #################################
  
  db_e_ns <- db_e[, c(2, 6, 8:20)]
  db_b_ns <- db_b[, c(2, 6, 8:20)]
  
  db_e_ns$Season <- mapply(seas1, db_e_ns[, 2])
  db_b_ns$Season <- mapply(seas1, db_b_ns[, 2])
  
  db_e_ns <- db_e_ns[, c(1, 16, 3:15)]
  db_b_ns <- db_b_ns[, c(1, 16, 3:15)]
  
  db_e_s_mean$Season <- substr(db_e_s_mean$Season_Year, 1, 9)
  db_b_s_mean$Season <- substr(db_b_s_mean$Season_Year, 1, 9)
  db_e_s_cnt$Season <- substr(db_e_s_cnt$Season_Year, 1, 9)
  db_b_s_cnt$Season <- substr(db_b_s_cnt$Season_Year, 1, 9)
  
  wgs_e <- aggregate(
    db_e_s_cnt[3:15],
    by = list(db_e_s_cnt$Station, db_e_s_cnt$Season),
    FUN = ws
  )
  wgs_b <- aggregate(
    db_b_s_cnt[3:15],
    by = list(db_b_s_cnt$Station, db_b_s_cnt$Season),
    FUN = ws
  )
  
  colnames(wgs_e)[1:2] <- c("Station", "Season")
  colnames(wgs_b)[1:2] <- c("Station", "Season")
  
  wgs_e <- wgs_e[order(wgs_e$Station, match(
    wgs_e$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  wgs_b <- wgs_b[order(wgs_b$Station, match(
    wgs_b$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  
  rownames(wgs_e) <- 1:nrow(wgs_e)
  rownames(wgs_b) <- 1:nrow(wgs_b)
  
  db_e_ns_mean <- aggregate(
    db_e_s_mean[3:15],
    by = list(db_e_s_mean$Station, db_e_s_mean$Season),
    FUN = mean,
    na.rm = TRUE
  )
  db_b_ns_mean <- aggregate(
    db_b_s_mean[3:15],
    by = list(db_b_s_mean$Station, db_b_s_mean$Season),
    FUN = mean,
    na.rm = TRUE
  )
  
  colnames(db_e_ns_mean)[1:2] <- c("Station", "Season")
  colnames(db_b_ns_mean)[1:2] <- c("Station", "Season")
  db_e_ns_mean[is.na(db_e_ns_mean)] <- NA
  db_b_ns_mean[is.na(db_b_ns_mean)] <- NA
  db_e_ns_mean <- db_e_ns_mean[order(db_e_ns_mean$Station, match(
    db_e_ns_mean$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  db_b_ns_mean <- db_b_ns_mean[order(db_b_ns_mean$Station, match(
    db_b_ns_mean$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  rownames(db_e_ns_mean) <- 1:nrow(db_e_ns_mean)
  rownames(db_b_ns_mean) <- 1:nrow(db_b_ns_mean)
  
  db_e_ns_c <- aggregate(db_e_ns[3:15],
                         by = list(db_e_ns$Station, db_e_ns$Season),
                         FUN = c)
  db_b_ns_c <- aggregate(db_b_ns[3:15],
                         by = list(db_b_ns$Station, db_b_ns$Season),
                         FUN = c)
  colnames(db_e_ns_c) <- colnames(db_e_ns)
  colnames(db_b_ns_c) <- colnames(db_b_ns)
  db_e_ns_c <- db_e_ns_c[order(db_e_ns_c$Station, match(
    db_e_ns_c$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  db_b_ns_c <- db_b_ns_c[order(db_b_ns_c$Station, match(
    db_b_ns_c$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  rownames(db_e_ns_c) <- 1:nrow(db_e_ns_c)
  rownames(db_b_ns_c) <- 1:nrow(db_b_ns_c)
  
  db_e_ns_median <- data.frame(matrix(NA, nrow = nrow(db_e_ns_c), ncol = 15))
  colnames(db_e_ns_median) <- colnames(db_e_ns_c)
  db_e_ns_median[, 1:2] <- db_e_ns_c[, 1:2]
  db_b_ns_median <- data.frame(matrix(NA, nrow = nrow(db_b_ns_c), ncol = 15))
  colnames(db_b_ns_median) <- colnames(db_b_ns_c)
  db_b_ns_median[, 1:2] <- db_b_ns_c[, 1:2]
  db_e_ns_sd <- data.frame(matrix(NA, nrow = nrow(db_e_ns_c), ncol = 15))
  colnames(db_e_ns_sd) <- colnames(db_e_ns_c)
  db_e_ns_sd[, 1:2] <- db_e_ns_c[, 1:2]
  db_b_ns_sd <- data.frame(matrix(NA, nrow = nrow(db_b_ns_c), ncol = 15))
  colnames(db_b_ns_sd) <- colnames(db_b_ns_c)
  db_b_ns_sd[, 1:2] <- db_b_ns_c[, 1:2]
  
  for (j in 3:15) {
    tryCatch({
      db_e_ns_median[, j] <- mapply(w_median_int, db_e_ns_c[, j], wgs_e[, j])
    }, error = function(e) {
      db_e_ns_median[, j] <<- median(mapply(w_median_int, db_e_ns_c[, j], wgs_e[, j]))
    })
    tryCatch({
      db_b_ns_median[, j] <- mapply(w_median_int, db_b_ns_c[, j], wgs_b[, j])
    }, error = function(e) {
      db_b_ns_median[, j] <<- median(mapply(w_median_int, db_b_ns_c[, j], wgs_b[, j]))
    })
    tryCatch({
      db_e_ns_sd[, j] <- mapply(w_sd_int, db_e_ns_c[, j], wgs_e[, j])
    }, error = function(e) {
      db_e_ns_sd[, j] <<- sd(mapply(w_sd_int, db_e_ns_c[, j], wgs_e[, j]))
    })
    tryCatch({
      db_b_ns_sd[, j] <- mapply(w_sd_int, db_b_ns_c[, j], wgs_b[, j])
    }, error = function(e) {
      db_b_ns_sd[, j] <<- sd(mapply(w_sd_int, db_b_ns_c[, j], wgs_b[, j]))
    })
  }
  
  db_e_ns_cnt <- aggregate(db_e_ns[3:15],
                           by = list(db_e_ns$Station, db_e_ns$Season),
                           FUN = cnt_int)
  db_b_ns_cnt <- aggregate(db_b_ns[3:15],
                           by = list(db_b_ns$Station, db_b_ns$Season),
                           FUN = cnt_int)
  
  colnames(db_e_ns_cnt)[1:2] <- c("Station", "Season")
  colnames(db_b_ns_cnt)[1:2] <- c("Station", "Season")
  db_e_ns_cnt <- db_e_ns_cnt[order(db_e_ns_cnt$Station, match(
    db_e_ns_cnt$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  db_b_ns_cnt <- db_b_ns_cnt[order(db_b_ns_cnt$Station, match(
    db_b_ns_cnt$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  
  colnames(db_e_ns_median)[3:15] <- paste0(colnames(db_e_ns_median)[3:15], "_Median_355")
  colnames(db_b_ns_median)[3:15] <- paste0(colnames(db_b_ns_median)[3:15], "_Median_355")
  
  colnames(db_e_ns_sd)[3:15] <- paste0(colnames(db_e_ns_sd)[3:15], "_StDev_355")
  colnames(db_b_ns_sd)[3:15] <- paste0(colnames(db_b_ns_sd)[3:15], "_StDev_355")
  
  colnames(db_e_ns_cnt)[3:15] <- paste0(colnames(db_e_ns_cnt)[3:15], "_NumberValues_355")
  colnames(db_b_ns_cnt)[3:15] <- paste0(colnames(db_b_ns_cnt)[3:15], "_NumberValues_355")
  
  db_b_ns_355 <- cbind(db_b_ns_mean, db_b_ns_median[3:15], db_b_ns_sd[3:15], db_b_ns_cnt[3:15])
  db_e_ns_355 <- cbind(db_e_ns_mean, db_e_ns_median[3:15], db_e_ns_sd[3:15], db_e_ns_cnt[3:15])
  
  #################################################################################
  #                                                                               #
  #                               PBL STATISTICS                                  #
  #                                                                               #
  #################################################################################
  
  ################################# Monthly averages #################################
  
  db_pbl_m <- db_pbl[, c(1:3, 7)]
  
  db_pbl_m_mean <- aggregate(
    db_pbl_m[4],
    by = list(db_pbl_m$Station, db_pbl_m$Year, db_pbl_m$Month),
    FUN = mean,
    na.rm = TRUE
  )
  colnames(db_pbl_m_mean)[1:3] <- c("Station", "Year", "Month")
  
  db_pbl_m_mean <- db_pbl_m_mean[order(db_pbl_m_mean$Station,
                                       db_pbl_m_mean$Year,
                                       db_pbl_m_mean$Month), ]
  db_pbl_m_mean[is.na(db_pbl_m_mean)] <- NA
  rownames(db_pbl_m_mean) <- 1:nrow(db_pbl_m_mean)
  
  db_pbl_m_cnt <- aggregate(
    db_pbl_m[4],
    by = list(db_pbl_m$Station, db_pbl_m$Year, db_pbl_m$Month),
    FUN = cnt_int
  )
  colnames(db_pbl_m_cnt)[1:3] <- c("Station", "Year", "Month")
  db_pbl_m_cnt <- db_pbl_m_cnt[order(db_pbl_m_cnt$Station,
                                     db_pbl_m_cnt$Year,
                                     db_pbl_m_cnt$Month), ]
  db_pbl_m_cnt[is.na(db_pbl_m_cnt)] <- NA
  rownames(db_pbl_m_cnt) <- 1:nrow(db_pbl_m_cnt)
  
  ################################# Annual averages #################################
  
  db_pbl_y <- db_pbl[, c(1, 2, 7)]
  
  wgs_pbl <- aggregate(
    db_pbl_m_cnt[4],
    by = list(db_pbl_m_cnt$Station, db_pbl_m_cnt$Year),
    FUN = ws
  )
  colnames(wgs_pbl)[1:2] <- c("Station", "Year")
  
  wgs_pbl <- wgs_pbl[order(wgs_pbl$Station, wgs_pbl$Year), ]
  
  db_pbl_y_mean <- aggregate(
    db_pbl_m_mean[4],
    by = list(db_pbl_m_mean$Station, db_pbl_m_mean$Year),
    FUN = mean,
    na.rm = TRUE
  )
  
  colnames(db_pbl_y_mean)[1:2] <- c("Station", "Year")
  db_pbl_y_mean[is.na(db_pbl_y_mean)] <- NA
  db_pbl_y_mean <- db_pbl_y_mean[order(db_pbl_y_mean$Station, db_pbl_y_mean$Year), ]
  
  db_pbl_y_c <- aggregate(db_pbl_y[3],
                          by = list(db_pbl_y$Station, db_pbl_y$Year),
                          FUN = c)
  colnames(db_pbl_y_c) <- colnames(db_pbl_y)
  db_pbl_y_c <- db_pbl_y_c[order(db_pbl_y_c$Station, db_pbl_y_c$Year), ]
  
  db_pbl_y_median <- data.frame(matrix(NA, nrow = nrow(db_pbl_y_c), ncol = 3))
  colnames(db_pbl_y_median) <- colnames(db_pbl_y_c)
  db_pbl_y_median[, 1:2] <- db_pbl_y_c[, 1:2]
  
  tryCatch({
    db_pbl_y_median[, 3] <- mapply(w_median_int, db_pbl_y_c[, 3], wgs_pbl[, 3])
  }, error = function(e) {
    tmp <- mapply(w_median_int, db_pbl_y_c[, 3], wgs_pbl[, 3])
    db_pbl_y_median[, 3] <<- median(tmp[!is.na(tmp)])
  })
  
  db_pbl_y_sd <- data.frame(matrix(NA, nrow = nrow(db_pbl_y_c), ncol = 3))
  colnames(db_pbl_y_sd) <- colnames(db_pbl_y_c)
  db_pbl_y_sd[, 1:2] <- db_pbl_y_c[, 1:2]
  
  tryCatch({
    db_pbl_y_sd[, 3] <- mapply(w_sd_int, db_pbl_y_c[, 3], wgs_pbl[, 3])
  }, error = function(e) {
    db_pbl_y_sd[, 3] <- w_sd_int(db_pbl_y_c[, 3], wgs_pbl[, 3])
  })
  
  db_pbl_y_cnt <- aggregate(db_pbl_m[4],
                            by = list(db_pbl_m$Station, db_pbl_m$Year),
                            FUN = cnt_int)
  
  colnames(db_pbl_y_cnt)[1:2] <- c("Station", "Year")
  db_pbl_y_cnt <- db_pbl_y_cnt[order(db_pbl_y_cnt$Station, db_pbl_y_cnt$Year), ]
  
  colnames(db_pbl_y_mean)[3] <- paste0(colnames(db_pbl_y_mean)[3], "_Mean")
  colnames(db_pbl_y_median)[3] <- paste0(colnames(db_pbl_y_median)[3], "_Median")
  colnames(db_pbl_y_sd)[3] <- paste0(colnames(db_pbl_y_sd)[3], "_StDev")
  colnames(db_pbl_y_cnt)[3] <- paste0(colnames(db_pbl_y_cnt)[3], "_NumberValues")

  rownames(db_pbl_y_mean) <- 1:nrow(db_pbl_y_mean)
  rownames(db_pbl_y_median) <- 1:nrow(db_pbl_y_mean)
  rownames(db_pbl_y_sd) <- 1:nrow(db_pbl_y_mean)
  rownames(db_pbl_y_cnt) <- 1:nrow(db_pbl_y_mean)
  
  db_pbl_y_tot <- cbind(db_pbl_y_mean,
                        db_pbl_y_median[3],
                        db_pbl_y_sd[3],
                        db_pbl_y_cnt[3])
  
  ################################# Seasonal Averages #################################
  
  db_pbl_s <- db_pbl[, c(1:3, 7)]
  
  db_pbl_s$Season_Year <- mapply(seas, db_pbl_s[, 3], db_pbl_s[, 2])
  
  db_pbl_s <- db_pbl_s[, c(1, 5, 4)]
  
  db_pbl_s_mean <- aggregate(
    db_pbl_s[3],
    by = list(db_pbl_s$Station, db_pbl_s$Season_Year),
    FUN = mean,
    na.rm = TRUE
  )
  colnames(db_pbl_s_mean)[1:2] <- c("Station", "Season_Year")
  db_pbl_s_mean <- db_pbl_s_mean[order(
    db_pbl_s_mean$Station,
    substr(db_pbl_s_mean$Season_Year, 11, 14),
    match(
      substr(db_pbl_s_mean$Season_Year, 1, 9),
      c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
    )
  ), ]
  rownames(db_pbl_s_mean) <- 1:nrow(db_pbl_s_mean)
  db_pbl_s_mean[is.na(db_pbl_s_mean)] <- NA
  
  db_pbl_s_median <- aggregate(
    db_pbl_s[3],
    by = list(db_pbl_s$Station, db_pbl_s$Season_Year),
    FUN = median,
    na.rm = TRUE
  )
  colnames(db_pbl_s_median)[1:2] <- c("Station", "Season_Year")
  db_pbl_s_median <- db_pbl_s_median[order(
    db_pbl_s_median$Station,
    substr(db_pbl_s_median$Season_Year, 11, 14),
    match(
      substr(db_pbl_s_median$Season_Year, 1, 9),
      c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
    )
  ), ]
  rownames(db_pbl_s_median) <- 1:nrow(db_pbl_s_median)
  db_pbl_s_median[is.na(db_pbl_s_median)] <- NA
  
  db_pbl_s_sd <- aggregate(
    db_pbl_s[3],
    by = list(db_pbl_s$Station, db_pbl_s$Season_Year),
    FUN = sd,
    na.rm = TRUE
  )
  colnames(db_pbl_s_sd)[1:2] <- c("Station", "Season_Year")
  db_pbl_s_sd <- db_pbl_s_sd[order(db_pbl_s_sd$Station,
                                   substr(db_pbl_s_sd$Season_Year, 11, 14),
                                   match(
                                     substr(db_pbl_s_sd$Season_Year, 1, 9),
                                     c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                   )), ]
  rownames(db_pbl_s_sd) <- 1:nrow(db_pbl_s_sd)
  db_pbl_s_sd[is.na(db_pbl_s_sd)] <- NA
  
  db_pbl_s_cnt <- aggregate(
    db_pbl_s[3],
    by = list(db_pbl_s$Station, db_pbl_s$Season_Year),
    FUN = cnt_int
  )
  colnames(db_pbl_s_cnt)[1:2] <- c("Station", "Season_Year")
  db_pbl_s_cnt <- db_pbl_s_cnt[order(
    db_pbl_s_cnt$Station,
    substr(db_pbl_s_cnt$Season_Year, 11, 14),
    match(
      substr(db_pbl_s_cnt$Season_Year, 1, 9),
      c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
    )
  ), ]
  rownames(db_pbl_s_cnt) <- 1:nrow(db_pbl_s_cnt)
  db_pbl_s_cnt[is.na(db_pbl_s_cnt)] <- NA
  
  colnames(db_pbl_s_mean)[3] <- paste0(colnames(db_pbl_s_mean)[3], "_Mean")
  colnames(db_pbl_s_median)[3] <- paste0(colnames(db_pbl_s_median)[3], "_Median")
  colnames(db_pbl_s_sd)[3] <- paste0(colnames(db_pbl_s_sd)[3], "_StDev")
  colnames(db_pbl_s_cnt)[3] <- paste0(colnames(db_pbl_s_cnt)[3], "_NumberValues_355")
  
  db_pbl_s_tot <- cbind(db_pbl_s_mean,
                        db_pbl_s_median[3],
                        db_pbl_s_sd[3],
                        db_pbl_s_cnt[3])
  
  ################################# Normal Monthly Averages #################################
  
  db_pbl_nm <- db_pbl[, c(1, 3, 7)]
  
  wgs_pbl <- aggregate(
    db_pbl_m_cnt[4],
    by = list(db_pbl_m_cnt$Station, db_pbl_m_cnt$Month),
    FUN = ws
  )
  colnames(wgs_pbl)[1:2] <- c("Station", "Month")
  
  wgs_pbl <- wgs_pbl[order(wgs_pbl$Station, wgs_pbl$Month), ]
  
  db_pbl_nm_mean <- aggregate(
    db_pbl_m_mean[4],
    by = list(db_pbl_m_mean$Station, db_pbl_m_mean$Month),
    FUN = mean,
    na.rm = TRUE
  )
  
  colnames(db_pbl_nm_mean)[1:2] <- c("Station", "Month")
  db_pbl_nm_mean[is.na(db_pbl_nm_mean)] <- NA
  db_pbl_nm_mean <- db_pbl_nm_mean[order(db_pbl_nm_mean$Station, db_pbl_nm_mean$Month), ]
  
  db_pbl_nm_c <- aggregate(db_pbl_nm[3],
                           by = list(db_pbl_nm$Station, db_pbl_nm$Month),
                           FUN = c)
  colnames(db_pbl_nm_c) <- colnames(db_pbl_nm)
  db_pbl_nm_c <- db_pbl_nm_c[order(db_pbl_nm_c$Station, db_pbl_nm_c$Month), ]
  
  db_pbl_nm_median <- data.frame(matrix(NA, nrow = nrow(db_pbl_nm_c), ncol = 3))
  colnames(db_pbl_nm_median) <- colnames(db_pbl_nm_c)
  db_pbl_nm_median[, 1:2] <- db_pbl_nm_c[, 1:2]
  
  tryCatch({
    db_pbl_nm_median[, 3] <- mapply(w_median_int, db_pbl_nm_c[, 3], wgs_pbl[, 3])
  }, error = function(e) {
    tmp <- mapply(w_median_int, db_pbl_nm_c[, 3], wgs_pbl[, 3])
    db_pbl_nm_median[, 3] <<- median(tmp[!is.na(tmp)])
  })
  
  db_pbl_nm_sd <- data.frame(matrix(NA, nrow = nrow(db_pbl_nm_c), ncol = 3))
  colnames(db_pbl_nm_sd) <- colnames(db_pbl_nm_c)
  db_pbl_nm_sd[, 1:2] <- db_pbl_nm_c[, 1:2]
  
  tryCatch({
    db_pbl_nm_sd[, 3] <- mapply(w_sd_int, db_pbl_nm_c[, 3], wgs_pbl[, 3])
  }, error = function(e) {
    db_pbl_nm_sd[, 3] <<- w_sd_int(db_pbl_nm_c[, 3], wgs_pbl[, 3])
  })
  
  db_pbl_nm_cnt <- aggregate(db_pbl_m[4],
                             by = list(db_pbl_m$Station, db_pbl_m$Month),
                             FUN = cnt_int)
  
  colnames(db_pbl_nm_cnt)[1:2] <- c("Station", "Month")
  db_pbl_nm_cnt <- db_pbl_nm_cnt[order(db_pbl_nm_cnt$Station, db_pbl_nm_cnt$Month), ]
  
  colnames(db_pbl_nm_mean)[3] <- paste0(colnames(db_pbl_nm_mean)[3], "_Mean")
  colnames(db_pbl_nm_median)[3] <- paste0(colnames(db_pbl_nm_median)[3], "_Median")
  colnames(db_pbl_nm_sd)[3] <- paste0(colnames(db_pbl_nm_sd)[3], "_StDev")
  colnames(db_pbl_nm_cnt)[3] <- paste0(colnames(db_pbl_nm_cnt)[3], "_NumberValues")
  
  rownames(db_pbl_nm_mean) <- 1:nrow(db_pbl_nm_mean)
  rownames(db_pbl_nm_median) <- 1:nrow(db_pbl_nm_mean)
  rownames(db_pbl_nm_sd) <- 1:nrow(db_pbl_nm_mean)
  rownames(db_pbl_nm_cnt) <- 1:nrow(db_pbl_nm_mean)
  
  db_pbl_nm_tot <- cbind(db_pbl_nm_mean,
                         db_pbl_nm_median[3],
                         db_pbl_nm_sd[3],
                         db_pbl_nm_cnt[3])
  
  ################################# Normal Seasonal Averages #################################
  
  db_pbl_ns <- db_pbl[, c(1, 3, 7)]
  
  db_pbl_ns$Season <- mapply(seas1, db_pbl_ns[, 2])
  
  db_pbl_ns <- db_pbl_ns[, c(1, 4, 3)]
  
  db_pbl_s_mean$Season <- substr(db_pbl_s_mean$Season_Year, 1, 9)
  db_pbl_s_cnt$Season <- substr(db_pbl_s_cnt$Season_Year, 1, 9)
  
  wgs_pbl <- aggregate(
    db_pbl_s_cnt[3],
    by = list(db_pbl_s_cnt$Station, db_pbl_s_cnt$Season),
    FUN = ws
  )
  colnames(wgs_pbl)[1:2] <- c("Station", "Season")
  
  wgs_pbl <- wgs_pbl[order(wgs_pbl$Station, match(
    wgs_pbl$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  rownames(wgs_pbl) <- 1:nrow(wgs_pbl)
  
  db_pbl_ns_mean <- aggregate(
    db_pbl_s_mean[3],
    by = list(db_pbl_s_mean$Station, db_pbl_s_mean$Season),
    FUN = mean,
    na.rm = TRUE
  )
  
  colnames(db_pbl_ns_mean)[1:2] <- c("Station", "Season")
  db_pbl_ns_mean[is.na(db_pbl_ns_mean)] <- NA
  db_pbl_ns_mean <- db_pbl_ns_mean[order(db_pbl_ns_mean$Station, match(
    db_pbl_ns_mean$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  rownames(db_pbl_ns_mean) <- 1:nrow(db_pbl_ns_mean)
  
  db_pbl_ns_c <- aggregate(db_pbl_ns[3],
                           by = list(db_pbl_ns$Station, db_pbl_ns$Season),
                           FUN = c)
  colnames(db_pbl_ns_c) <- colnames(db_pbl_ns)
  db_pbl_ns_c <- db_pbl_ns_c[order(db_pbl_ns_c$Station, match(
    db_pbl_ns_c$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  rownames(db_pbl_ns_c) <- 1:nrow(db_pbl_ns_c)
  
  db_pbl_ns_median <- data.frame(matrix(NA, nrow = nrow(db_pbl_ns_c), ncol = 3))
  colnames(db_pbl_ns_median) <- colnames(db_pbl_ns_c)
  db_pbl_ns_median[, 1:2] <- db_pbl_ns_c[, 1:2]
  
  tryCatch({
    db_pbl_ns_median[, 3] <- mapply(w_median_int, db_pbl_ns_c[, 3], wgs_pbl[, 3])
  }, error = function(e) {
    tmp <- mapply(w_median_int, db_pbl_ns_c[, 3], wgs_pbl[, 3])
    db_pbl_ns_median[, 3] <<- median(tmp[!is.na(tmp)])
  })
  
  db_pbl_ns_sd <- data.frame(matrix(NA, nrow = nrow(db_pbl_ns_c), ncol = 3))
  colnames(db_pbl_ns_sd) <- colnames(db_pbl_ns_c)
  db_pbl_ns_sd[, 1:2] <- db_pbl_ns_c[, 1:2]
  
  tryCatch({
    db_pbl_ns_sd[, 3] <- mapply(w_sd_int, db_pbl_ns_c[, 3], wgs_pbl[, 3])
  }, error = function(e) {
    db_pbl_ns_sd[, 3] <<- w_sd_int(db_pbl_ns_c[, 3], wgs_pbl[, 3])
  })
  
  db_pbl_ns_cnt <- aggregate(db_pbl_ns[3],
                             by = list(db_pbl_ns$Station, db_pbl_ns$Season),
                             FUN = cnt_int)
  colnames(db_pbl_ns_cnt)[1:2] <- c("Station", "Season")
  db_pbl_ns_cnt <- db_pbl_ns_cnt[order(db_pbl_ns_cnt$Station, match(
    db_pbl_ns_cnt$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  
  colnames(db_pbl_ns_median)[3] <- paste0(colnames(db_pbl_ns_median)[3], "_Median")
  colnames(db_pbl_ns_sd)[3] <- paste0(colnames(db_pbl_ns_sd)[3], "_StDev")
  colnames(db_pbl_ns_cnt)[3] <- paste0(colnames(db_pbl_ns_cnt)[3], "_NumberValues")
  
  db_pbl_ns_tot <- cbind(db_pbl_ns_mean,
                         db_pbl_ns_median[3],
                         db_pbl_ns_sd[3],
                         db_pbl_ns_cnt[3])
  
  #################################################################################
  #                                                                               #
  #                                   PartDep_355                                 #
  #                                                                               #
  #################################################################################
  
  db_pd_355 <- db_pd[db_pd$Wavelength == "0355", ]
  
  ################################# Monthly averages #################################
  
  db_pd_m <- db_pd_355[, c(1, 3:4, 8:13)]
  
  db_pd_m_mean <- aggregate(
    db_pd_m[4:9],
    by = list(db_pd_m$Station, db_pd_m$Year, db_pd_m$Month),
    FUN = mean,
    na.rm = TRUE
  )
  colnames(db_pd_m_mean)[1:3] <- c("Station", "Year", "Month")
  
  db_pd_m_mean <- db_pd_m_mean[order(db_pd_m_mean$Station,
                                     db_pd_m_mean$Year,
                                     db_pd_m_mean$Month), ]
  db_pd_m_mean[is.na(db_pd_m_mean)] <- NA
  rownames(db_pd_m_mean) <- 1:nrow(db_pd_m_mean)
  
  db_pd_m_cnt <- aggregate(
    db_pd_m[4:9],
    by = list(db_pd_m$Station, db_pd_m$Year, db_pd_m$Month),
    FUN = cnt_int
  )
  colnames(db_pd_m_cnt)[1:3] <- c("Station", "Year", "Month")
  
  db_pd_m_cnt <- db_pd_m_cnt[order(db_pd_m_cnt$Station, db_pd_m_cnt$Year, db_pd_m_cnt$Month), ]
  db_pd_m_cnt[is.na(db_pd_m_cnt)] <- NA
  rownames(db_pd_m_cnt) <- 1:nrow(db_pd_m_cnt)
  
  ################################# Annual averages #################################
  
  db_pd_y <- db_pd_355[, c(1, 3, 8:13)]
  
  wgs_pd <- aggregate(
    db_pd_m_cnt[4:9],
    by = list(db_pd_m_cnt$Station, db_pd_m_cnt$Year),
    FUN = ws
  )
  colnames(wgs_pd)[1:2] <- c("Station", "Year")
  
  wgs_pd <- wgs_pd[order(wgs_pd$Station, wgs_pd$Year), ]
  
  db_pd_y_mean <- aggregate(
    db_pd_m_mean[4:9],
    by = list(db_pd_m_mean$Station, db_pd_m_mean$Year),
    FUN = mean,
    na.rm = TRUE
  )
  colnames(db_pd_y_mean)[1:2] <- c("Station", "Year")
  db_pd_y_mean[is.na(db_pd_y_mean)] <- NA
  db_pd_y_mean <- db_pd_y_mean[order(db_pd_y_mean$Station, db_pd_y_mean$Year), ]
  
  db_pd_y_c <- aggregate(db_pd_y[3:8],
                         by = list(db_pd_y$Station, db_pd_y$Year),
                         FUN = c)
  colnames(db_pd_y_c) <- colnames(db_pd_y)
  db_pd_y_c <- db_pd_y_c[order(db_pd_y_c$Station, db_pd_y_c$Year), ]
  
  db_pd_y_median <- data.frame(matrix(NA, nrow = nrow(db_pd_y_c), ncol = 8))
  colnames(db_pd_y_median) <- colnames(db_pd_y_c)
  db_pd_y_median[, 1:2] <- db_pd_y_c[, 1:2]
  
  db_pd_y_sd <- data.frame(matrix(NA, nrow = nrow(db_pd_y_c), ncol = 8))
  colnames(db_pd_y_sd) <- colnames(db_pd_y_c)
  db_pd_y_sd[, 1:2] <- db_pd_y_c[, 1:2]
  
  for (j in 3:8) {
    db_pd_y_median <- assign_with_fallback(db_pd_y_median, db_pd_y_c, wgs_pd, j, w_median_int)
    db_pd_y_sd <- assign_with_fallback(db_pd_y_sd, db_pd_y_c, wgs_pd, j, w_sd_int)
  }
  
  db_pd_y_cnt <- aggregate(db_pd_m[4:9],
                           by = list(db_pd_m$Station, db_pd_m$Year),
                           FUN = cnt_int)
  
  colnames(db_pd_y_cnt)[1:2] <- c("Station", "Year")
  db_pd_y_cnt <- db_pd_y_cnt[order(db_pd_y_cnt$Station, db_pd_y_cnt$Year), ]
  
  colnames(db_pd_y_mean)[3:8] = paste0(colnames(db_pd_y_mean)[3:8], "_Mean_355")
  colnames(db_pd_y_median)[3:8] = paste0(colnames(db_pd_y_median)[3:8], "_Median_355")
  colnames(db_pd_y_sd)[3:8] = paste0(colnames(db_pd_y_sd)[3:8], "_StDev_355")
  colnames(db_pd_y_cnt)[3:8] = paste0(colnames(db_pd_y_cnt)[3:8], "_NumberValues_355")
  
  rownames(db_pd_y_mean) <- 1:nrow(db_pd_y_mean)
  rownames(db_pd_y_median) <- 1:nrow(db_pd_y_mean)
  rownames(db_pd_y_sd) <- 1:nrow(db_pd_y_mean)
  rownames(db_pd_y_cnt) <- 1:nrow(db_pd_y_mean)
  
  db_pd_y_355 <- cbind(db_pd_y_mean, db_pd_y_median[3:8], db_pd_y_sd[3:8], db_pd_y_cnt[3:8])
  
  ################################# Seasonal Averages #################################
  
  db_pd_s <- db_pd_355[, c(1, 3:4, 8:13)]
  
  db_pd_s$Season_Year <- mapply(seas, db_pd_s[, 3], db_pd_s[, 2])
  
  db_pd_s <- db_pd_s[, c(1, 10, 4:9)]
  
  db_pd_s_mean <- aggregate(
    db_pd_s[3:8],
    by = list(db_pd_s$Station, db_pd_s$Season_Year),
    FUN = mean,
    na.rm = TRUE
  )
  colnames(db_pd_s_mean)[1:2] <- c("Station", "Season_Year")
  db_pd_s_mean <- db_pd_s_mean[order(
    db_pd_s_mean$Station,
    substr(db_pd_s_mean$Season_Year, 11, 14),
    match(
      substr(db_pd_s_mean$Season_Year, 1, 9),
      c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
    )
  ), ]
  rownames(db_pd_s_mean) <- 1:nrow(db_pd_s_mean)
  db_pd_s_mean[is.na(db_pd_s_mean)] <- NA
  
  db_pd_s_median <- aggregate(
    db_pd_s[3:8],
    by = list(db_pd_s$Station, db_pd_s$Season_Year),
    FUN = median,
    na.rm = TRUE
  )
  colnames(db_pd_s_median)[1:2] <- c("Station", "Season_Year")
  db_pd_s_median <- db_pd_s_median[order(
    db_pd_s_median$Station,
    substr(db_pd_s_median$Season_Year, 11, 14),
    match(
      substr(db_pd_s_median$Season_Year, 1, 9),
      c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
    )
  ), ]
  rownames(db_pd_s_median) <- 1:nrow(db_pd_s_median)
  db_pd_s_median[is.na(db_pd_s_median)] <- NA
  
  db_pd_s_sd <- aggregate(
    db_pd_s[3:8],
    by = list(db_pd_s$Station, db_pd_s$Season_Year),
    FUN = sd,
    na.rm = TRUE
  )
  colnames(db_pd_s_sd)[1:2] <- c("Station", "Season_Year")
  db_pd_s_sd <- db_pd_s_sd[order(db_pd_s_sd$Station,
                                 substr(db_pd_s_sd$Season_Year, 11, 14),
                                 match(
                                   substr(db_pd_s_sd$Season_Year, 1, 9),
                                   c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                 )), ]
  rownames(db_pd_s_sd) <- 1:nrow(db_pd_s_sd)
  db_pd_s_sd[is.na(db_pd_s_sd)] <- NA
  
  db_pd_s_cnt <- aggregate(db_pd_s[3:8],
                           by = list(db_pd_s$Station, db_pd_s$Season_Year),
                           FUN = cnt_int)
  colnames(db_pd_s_cnt)[1:2] <- c("Station", "Season_Year")
  db_pd_s_cnt <- db_pd_s_cnt[order(db_pd_s_cnt$Station,
                                   substr(db_pd_s_cnt$Season_Year, 11, 14),
                                   match(
                                     substr(db_pd_s_cnt$Season_Year, 1, 9),
                                     c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
                                   )), ]
  rownames(db_pd_s_cnt) <- 1:nrow(db_pd_s_cnt)
  db_pd_s_cnt[is.na(db_pd_s_cnt)] <- NA
  
  colnames(db_pd_s_mean)[3:8] = paste0(colnames(db_pd_s_mean)[3:8], "_Mean_355")
  colnames(db_pd_s_median)[3:8] = paste0(colnames(db_pd_s_median)[3:8], "_Median_355")
  colnames(db_pd_s_sd)[3:8] = paste0(colnames(db_pd_s_sd)[3:8], "_StDev_355")
  colnames(db_pd_s_cnt)[3:8] = paste0(colnames(db_pd_s_cnt)[3:8], "_NumberValues_355")
  
  db_pd_s_355 <- cbind(db_pd_s_mean, db_pd_s_median[3:8], db_pd_s_sd[3:8], db_pd_s_cnt[3:8])
  
  ################################# Normal Monthly Averages #################################
  
  db_pd_nm <- db_pd_355[, c(1, 4, 8:13)]
  
  wgs_pd <- aggregate(
    db_pd_m_cnt[4:9],
    by = list(db_pd_m_cnt$Station, db_pd_m_cnt$Month),
    FUN = ws
  )
  colnames(wgs_pd)[1:2] <- c("Station", "Month")
  
  wgs_pd <- wgs_pd[order(wgs_pd$Station, wgs_pd$Month), ]
  
  db_pd_nm_mean <- aggregate(
    db_pd_m_mean[4:9],
    by = list(db_pd_m_mean$Station, db_pd_m_mean$Month),
    FUN = mean,
    na.rm = TRUE
  )
  
  colnames(db_pd_nm_mean)[1:2] <- c("Station", "Month")
  db_pd_nm_mean[is.na(db_pd_nm_mean)] <- NA
  db_pd_nm_mean <- db_pd_nm_mean[order(db_pd_nm_mean$Station, db_pd_nm_mean$Month), ]
  
  db_pd_nm_c <- aggregate(db_pd_nm[3:8],
                          by = list(db_pd_nm$Station, db_pd_nm$Month),
                          FUN = c)
  colnames(db_pd_nm_c) <- colnames(db_pd_nm)
  db_pd_nm_c <- db_pd_nm_c[order(db_pd_nm_c$Station, db_pd_nm_c$Month), ]
  
  db_pd_nm_median <- data.frame(matrix(NA, nrow = nrow(db_pd_nm_c), ncol = 8))
  colnames(db_pd_nm_median) <- colnames(db_pd_nm_c)
  db_pd_nm_median[, 1:2] <- db_pd_nm_c[, 1:2]
  
  db_pd_nm_sd <- data.frame(matrix(NA, nrow = nrow(db_pd_nm_c), ncol = 8))
  colnames(db_pd_nm_sd) <- colnames(db_pd_nm_c)
  db_pd_nm_sd[, 1:2] <- db_pd_nm_c[, 1:2]
  
  for (j in 3:8) {
    tryCatch({
      db_pd_nm_median[, j] <- mapply(w_median_int, db_pd_nm_c[, j], wgs_pd[, j])
    }, error = function(e) {
      tmp <- mapply(w_median_int, db_pd_nm_c[, j], wgs_pd[, j])
      db_pd_nm_median[, j] <<- median(tmp[!is.na(tmp)])
    })
    tryCatch({
      db_pd_nm_sd[, j] <- mapply(w_sd_int, db_pd_nm_c[, j], wgs_pd[, j])
    }, error = function(e) {
      db_pd_nm_sd[, j] <<- w_sd_int(db_pd_nm_c[, j], wgs_pd[, j])
    })
  }
  
  db_pd_nm_cnt <- aggregate(db_pd_m[4:9],
                            by = list(db_pd_m$Station, db_pd_m$Month),
                            FUN = cnt_int)
  
  colnames(db_pd_nm_cnt)[1:2] <- c("Station", "Month")
  db_pd_nm_cnt <- db_pd_nm_cnt[order(db_pd_nm_cnt$Station, db_pd_nm_cnt$Month), ]
  
  colnames(db_pd_nm_mean)[3:8] = paste0(colnames(db_pd_nm_mean)[3:8], "_Mean_355")
  colnames(db_pd_nm_median)[3:8] = paste0(colnames(db_pd_nm_median)[3:8], "_Median_355")
  colnames(db_pd_nm_sd)[3:8] = paste0(colnames(db_pd_nm_sd)[3:8], "_StDev_355")
  colnames(db_pd_nm_cnt)[3:8] = paste0(colnames(db_pd_nm_cnt)[3:8], "_NumberValues_355")
  
  rownames(db_pd_nm_mean) <- 1:nrow(db_pd_nm_mean)
  rownames(db_pd_nm_median) <- 1:nrow(db_pd_nm_mean)
  rownames(db_pd_nm_sd) <- 1:nrow(db_pd_nm_mean)
  rownames(db_pd_nm_cnt) <- 1:nrow(db_pd_nm_mean)
  
  db_pd_nm_355 <- cbind(db_pd_nm_mean,
                        db_pd_nm_median[3:8],
                        db_pd_nm_sd[3:8],
                        db_pd_nm_cnt[3:8])
  
  ################################# Normal Seasonal Averages #################################
  
  db_pd_ns <- db_pd_355[, c(1, 4, 8:13)]
  
  db_pd_ns$Season <- mapply(seas1, db_pd_ns[, 2])
  
  db_pd_ns <- db_pd_ns[, c(1, 9, 3:8)]
  
  db_pd_s_mean$Season <- substr(db_pd_s_mean$Season_Year, 1, 9)
  db_pd_s_cnt$Season <- substr(db_pd_s_cnt$Season_Year, 1, 9)
  
  wgs_pd <- aggregate(
    db_pd_s_cnt[3:8],
    by = list(db_pd_s_cnt$Station, db_pd_s_cnt$Season),
    FUN = ws
  )
  
  colnames(wgs_pd)[1:2] <- c("Station", "Season")
  
  wgs_pd <- wgs_pd[order(wgs_pd$Station, match(
    wgs_pd$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  rownames(wgs_pd) <- 1:nrow(wgs_pd)
  
  db_pd_ns_mean <- aggregate(
    db_pd_s_mean[3:8],
    by = list(db_pd_s_mean$Station, db_pd_s_mean$Season),
    FUN = mean,
    na.rm = TRUE
  )
  colnames(db_pd_ns_mean)[1:2] <- c("Station", "Season")
  db_pd_ns_mean[is.na(db_pd_ns_mean)] <- NA
  db_pd_ns_mean <- db_pd_ns_mean[order(db_pd_ns_mean$Station, match(
    db_pd_ns_mean$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  rownames(db_pd_ns_mean) <- 1:nrow(db_pd_ns_mean)
  
  db_pd_ns_c <- aggregate(db_pd_ns[3:8],
                          by = list(db_pd_ns$Station, db_pd_ns$Season),
                          FUN = c)
  colnames(db_pd_ns_c) <- colnames(db_pd_ns)
  db_pd_ns_c <- db_pd_ns_c[order(db_pd_ns_c$Station, match(
    db_pd_ns_c$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  rownames(db_pd_ns_c) <- 1:nrow(db_pd_ns_c)
  
  db_pd_ns_median <- data.frame(matrix(NA, nrow = nrow(db_pd_ns_c), ncol = 8))
  colnames(db_pd_ns_median) <- colnames(db_pd_ns_c)
  db_pd_ns_median[, 1:2] <- db_pd_ns_c[, 1:2]
  
  db_pd_ns_sd <- data.frame(matrix(NA, nrow = nrow(db_pd_ns_c), ncol = 8))
  colnames(db_pd_ns_sd) <- colnames(db_pd_ns_c)
  db_pd_ns_sd[, 1:2] <- db_pd_ns_c[, 1:2]
  
  for (j in 3:8) {
    tryCatch({
      db_pd_ns_median[, j] <- mapply(w_median_int, db_pd_ns_c[, j], wgs_pd[, j])
    }, error = function(e) {
      tmp <- mapply(w_median_int, db_pd_ns_c[, j], wgs_pd[, j])
      db_pd_ns_median[, j] <<- median(tmp[!is.na(tmp)])
    })
    tryCatch({
      db_pd_ns_sd[, j] <- mapply(w_sd_int, db_pd_ns_c[, j], wgs_pd[, j])
    }, error = function(e) {
      db_pd_ns_sd[, j] <<- w_sd_int(db_pd_ns_c[, j], wgs_pd[, j])
    })
  }
  
  db_pd_ns_cnt <- aggregate(db_pd_ns[3:8],
                            by = list(db_pd_ns$Station, db_pd_ns$Season),
                            FUN = cnt_int)
  
  colnames(db_pd_ns_cnt)[1:2] <- c("Station", "Season")
  db_pd_ns_cnt <- db_pd_ns_cnt[order(db_pd_ns_cnt$Station, match(
    db_pd_ns_cnt$Season,
    c("MarAprMay", "JunJulAug", "SepOctNov", "DecJanFeb")
  )), ]
  
  colnames(db_pd_ns_median)[3:8] = paste0(colnames(db_pd_ns_median)[3:8], "_Median_355")
  colnames(db_pd_ns_sd)[3:8] = paste0(colnames(db_pd_ns_sd)[3:8], "_StDev_355")
  colnames(db_pd_ns_cnt)[3:8] = paste0(colnames(db_pd_ns_cnt)[3:8], "_NumberValues_355")
  
  db_pd_ns_355 <- cbind(db_pd_ns_mean,
                        db_pd_ns_median[3:8],
                        db_pd_ns_sd[3:8],
                        db_pd_ns_cnt[3:8])
  
}