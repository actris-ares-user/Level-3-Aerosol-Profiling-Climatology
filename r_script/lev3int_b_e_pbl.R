# Preallocation of data frames
db_int_b <- data.frame(matrix(NA, nrow = nrow(lev2db[lev2db$VarBool == "0", ]), ncol = 20))
colnames(db_int_b) <- c(
  colnames(lev2db)[c(1:7)],
  "H63_Bs",
  "TotalIntBs",
  "Error_TotalIntBs",
  "PBLIntBs",
  "Error_PBLIntBs",
  "FTIntBs",
  "Error_FTIntBs",
  "TotalCenterMass",
  "Error_TotalCenterMass",
  "PBLCenterMass",
  "Error_PBLCenterMass",
  "FTCenterMass",
  "Error_FTCenterMass"
)

db_pd <- data.frame(matrix(NA, nrow = nrow(lev2db), ncol = 15))
colnames(db_pd) <- c(
  colnames(lev2db)[c(1:9)],
  "TotalPartDep",
  "Error_TotalPartDep",
  "PBLPartDep",
  "Error_PBLPartDep",
  "FTPartDep",
  "Error_FTPartDep"
)

db_int_e <- data.frame(matrix(NA, nrow = nrow(lev2db[lev2db$Type == "e", ]), ncol = 20))
colnames(db_int_e) <- c(
  colnames(lev2db)[c(1:7)],
  "H63",
  "TotalAOD",
  "Error_TotalAOD",
  "PBLAOD",
  "Error_PBLAOD",
  "FTAOD",
  "Error_FTAOD",
  "TotalLidarRatio",
  "Error_TotalLidarRatio",
  "PBLLidarRatio",
  "Error_PBLLidarRatio",
  "FTLidarRatio",
  "Error_FTLidarRatio"
)

db_pbl <- data.frame(matrix(NA, nrow = nrow(lev2db), ncol = 7))
db_pbl[, 1:6] <- lev2db[, c(2, 5:9)]
colnames(db_pbl) <- c(colnames(lev2db)[c(2, 5:9)], "PBL")


for (i in 1:nrow(lev2db)) {
  file1 <- try(nc_open(paste0("./New/", substr(lev2db$File_name[i], 20, 22), "/", lev2db$File_name[i])), silent = TRUE)
  if (inherits(file1, "try-error")) next
  
  lydb <- lev2db_layers[lev2db_layers$File_name == lev2db$File_name[i], ]
  lydb <- lydb[!is.na(lydb$Top_Layer), ]

  alt <- ncvar_get(file1, "altitude")
  
  # check if the altitude was reported in Km or m
  if(sum(alt < 12, na.rm = TRUE) > sum(!is.na(alt)) / 2){
    alt <- alt * 1000
  }
  
  l1 <- which(!is.na(alt) & alt <= 12000)
  alt <- alt[l1]
  aslv <- ncvar_get(file1, "station_altitude")
  
  ########################### Only for _b files ###########################
  if (lev2db$VarBool[i] == "0"){
    bs <- try(ncvar_get(file1, "backscatter"), silent = TRUE)
    err_bs <- try(ncvar_get(file1, "error_backscatter"), silent = TRUE)
    
    if (length(class(bs)) > 1) {
      bs <- try(rowMeans(bs, na.rm = TRUE), silent = TRUE)
      err_bs <- try(rowMeans(err_bs, na.rm = TRUE), silent = TRUE)
    }
    if (class(bs) == "try-error") bs <- rep(NA, length(alt))
    if (class(err_bs) == "try-error") err_bs <- rep(NA, length(alt))
    
    bs <- bs[l1]
    err_bs <- err_bs[l1]
    
    l2 <- which(!is.na(bs) & !is.na(err_bs) & err_bs > 0 & abs(bs) <= 0.0001 & bs + err_bs >= 0)
    bs <- bs[l2]
    err_bs <- err_bs[l2]
    alt_bs <- alt[l2]
    m_bs <- length(alt_bs)
    
    if (nrow(lydb) > 0) {
      pbl <- min(lydb$Top_Layer)
    } else {
      pbl <- try(ncvar_get(file1, "aerosollayerheight"), silent = TRUE)
      if (class(pbl) == "try-error") {
        pbl <- NA
      } else {
        pbl <- as.numeric(pbl)
        if (length(pbl) > 1) {
          pbl <- mean(pbl, na.rm = TRUE)
        }
      }
    }
    
    db_int_b[i, 1:7] <- lev2db[i, 1:7]
    
    tot_int_bs <- NA
    h63 <- NA
    pbl_int_bs <- NA
    ft_int_bs <- NA
    tot_int_bs_err <- NA
    pbl_int_bs_err <- NA
    ft_int_bs_err <- NA
    tot_com <- NA
    tot_com_err <- NA
    pbl_com <- NA
    pbl_com_err <- NA
    ft_com <- NA
    ft_com_err <- NA
    
    if (m_bs > 0)
    {
      prod <- alt_bs * bs
      prod_err <- alt_bs * err_bs
      int_bs <- (alt_bs[1] - aslv) * bs[1]
      num_com <- (alt_bs[1] - aslv) * prod[1]
      int_bs_err <- (alt_bs[1] - aslv) * err_bs[1]
      num_com_err <- (alt_bs[1] - aslv) * prod_err[1]
      p_int_bs <- numeric(m_bs)
      p_int_bs[1] <- int_bs
      p_int_bs_err <- numeric(m_bs)
      p_int_bs_err <- int_bs_err
      p_num_com <- numeric(m_bs)
      p_num_com[1] <- num_com
      p_num_com_err <- numeric(m_bs)
      p_num_com_err[1] <- num_com_err
      
      if (m_bs > 1) {
        for (g in 2:m_bs) {
          int_bs <- (bs[g] + bs[g - 1]) * (alt_bs[g] - alt_bs[g - 1]) / 2
          num_com <- (prod[g] + prod[g - 1]) * (alt_bs[g] - alt_bs[g - 1]) / 2
          int_bs_err <- (err_bs[g] + err_bs[g - 1]) * (alt_bs[g] - alt_bs[g - 1]) / 2
          num_com_err <- (prod_err[g] + prod_err[g - 1]) * (alt_bs[g] - alt_bs[g - 1]) / 2
          p_int_bs[g] <- p_int_bs[g - 1] + int_bs
          p_num_com[g] <- p_num_com[g - 1] + num_com
          p_int_bs_err[g] <- p_int_bs_err[g - 1] + int_bs_err
          p_num_com_err[g] <- p_num_com_err[g - 1] + num_com_err
        }
      }
      
      tot_int_bs <- p_int_bs[m_bs]
      tot_com <- p_num_com[m_bs] / p_int_bs[m_bs]
      tot_int_bs_err <- p_int_bs_err[m_bs]
      tot_com_err <- abs(tot_com) * ((p_num_com_err[m_bs] / p_num_com[m_bs]) + (tot_int_bs_err / tot_int_bs))
      ind63 <- which(abs(p_int_bs) > 0.63 * abs(tot_int_bs))
      
      if (length(ind63) > 0) {
        h63 <- alt_bs[min(ind63)]
      }
      
      if (!is.na(pbl)) {
        ind_pbl_bs <- which(alt_bs <= pbl)
        n_bs <- length(ind_pbl_bs)
        if (n_bs == 0) {
          pbl_int_bs <- NA
          pbl_int_bs_err <- NA
          ft_int_bs <- tot_int_bs
          ft_int_bs_err <- tot_int_bs_err
          pbl_com <- NA
          pbl_com_err <- NA
          ft_com <- tot_com
          ft_com_err <- tot_com_err
        } else {
          pbl_int_bs <- p_int_bs[n_bs]
          ft_int_bs <- tot_int_bs - pbl_int_bs
          pbl_int_bs_err <- p_int_bs_err[n_bs]
          ft_int_bs_err <- tot_int_bs_err - pbl_int_bs_err
          pbl_com <- p_num_com[n_bs] / p_int_bs[n_bs]
          pbl_com_err <- pbl_com * ((p_num_com_err[n_bs] / p_num_com[n_bs]) + (p_int_bs_err[n_bs] / p_int_bs[n_bs]))
          ft_com <- tot_com - pbl_com
          ft_com_err <- tot_com_err - pbl_com_err
        }
      }
    } 
    
    db_int_b[i, 8:20] <- c(h63, tot_int_bs, tot_int_bs_err, pbl_int_bs, pbl_int_bs_err, ft_int_bs, ft_int_bs_err, tot_com, tot_com_err, pbl_com, pbl_com_err, ft_com, ft_com_err)
  }
  
  ########################### Only for _e files ###########################
  if (lev2db$Type[i] == "e"){
    ext <- try(ncvar_get(file1, "extinction"), silent = TRUE)
    if (class(ext) == "try-error") ext <- rep(NA, length(alt))
    
    err_ext <- try(ncvar_get(file1, "error_extinction"), silent = TRUE)
    if (class(err_ext) == "try-error") err_ext <- rep(NA, length(alt))
    
    s0 <- try(ncvar_get(file1, "lidarratio"), silent = TRUE)
    if (class(s0) == "try-error") s0 <- rep(NA, length(alt))
    
    err_s0 <- try(ncvar_get(file1, "error_lidarratio"), silent = TRUE)
    if (class(err_s0) == "try-error") err_s0 <- rep(NA, length(alt)) 
    
    ext <- ext[l1]
    err_ext <- err_ext[l1]
    s0 <- s0[l1]
    err_s0 <- err_s0[l1]
    l3 <- which(!is.na(ext) & abs(ext) <= 0.01 & err_ext + ext >= 0)
    ext <- ext[l3]
    err_ext <- err_ext[l3]
    alt_ext <- alt[l3]
    
    l5 <- which(!is.na(s0) & s0 >= -100 & s0 <= 200 & s0 + err_s0 >= 0)
    s0 <- s0[l5]
    err_s0 <- err_s0[l5]
    alt_s0 <- alt[l5]
    
    if (nrow(lydb) > 0) {
      pbl <- min(lydb$Top_Layer)
    } else {
      pbl <- try(ncvar_get(file1, "aerosollayerheight"), silent = TRUE)
      if (class(pbl) == "try-error") {
        pbl <- NA
      } else {
        pbl <- as.numeric(pbl)
      }
    }
    
    db_int_e[i, 1:7] <- lev2db[i, 1:7]
    
    tot_aod <- NA
    h63 <- NA
    pbl_aod <- NA
    ft_aod <- NA
    tot_aod_err <- NA
    pbl_aod_err <- NA
    ft_aod_err <- NA
    tot_s0 <- NA
    tot_s0_err <- NA
    pbl_s0 <- NA
    pbl_s0_err <- NA
    ft_s0 <- NA
    ft_s0_err <- NA
    
    if (length(alt_ext) > 0) {
      aod <- numeric(length(alt_ext))
      aod_err <- numeric(length(alt_ext))
      aod[1] <- (alt_ext[1] - aslv) * ext[1]
      aod_err[1] <- (alt_ext[1] - aslv) * err_ext[1]
      p_aod <- numeric(length(alt_ext))
      p_aod_err <- numeric(length(alt_ext))
      p_aod[1] <- aod[1]
      p_aod_err[1] <- aod_err[1]
      
      if (length(alt_ext) > 1) {
        for (h in 2:length(alt_ext)) {
          aod[h] <- (ext[h] + ext[h - 1]) * (alt_ext[h] - alt_ext[h - 1]) / 2
          aod_err[h] <- (err_ext[h] + err_ext[h - 1]) * (alt_ext[h] - alt_ext[h - 1]) / 2
          p_aod[h] <- p_aod[h - 1] + aod[h]
          p_aod_err[h] <- p_aod_err[h - 1] + aod_err[h]
        }
      }
      
      tot_aod <- p_aod[length(alt_ext)]
      tot_aod_err <- p_aod_err[length(alt_ext)]
      ind63 <- which(abs(p_aod) > 0.63 * abs(tot_aod))
      if (length(ind63) > 0) {
        h63 <- alt_ext[min(ind63)]
      }
      if (!is.na(pbl)) {
        ind_pbl_ext <- which(alt_ext <= pbl)
        if (length(ind_pbl_ext) == 0) {
          pbl_aod <- NA
          pbl_aod_err <- NA
          ft_aod <- tot_aod
          ft_aod_err <- tot_aod_err
        }
        else
        {
          pbl_aod <- p_aod[length(ind_pbl_ext)]
          ft_aod <- tot_aod - pbl_aod
          pbl_aod_err <- p_aod_err[length(ind_pbl_ext)]
          ft_aod_err <- tot_aod_err - pbl_aod_err
        }
      }
      
    }
    
    if (length(alt_s0) > 0) {
      tot_s0 <- mean(s0, na.rm = TRUE)  
      tot_s0_err <- mean(err_s0, na.rm = TRUE)  
      
      if (!is.na(pbl)) {
        ind_pbl_s0 <- which(alt_s0 <= pbl)
        if (length(ind_pbl_s0) == 0) {
          pbl_s0 <- NA
          pbl_s0_err <- NA
          ft_s0 <- mean(s0, na.rm = TRUE)
          ft_s0_err <- mean(err_s0, na.rm = TRUE)
        } else {
          pbl_s0 <- mean(s0[ind_pbl_s0], na.rm = TRUE)
          pbl_s0_err <- mean(err_s0[ind_pbl_s0], na.rm = TRUE)
          ft_s0 <- mean(s0[-ind_pbl_s0], na.rm = TRUE)
          ft_s0_err <- mean(err_s0[-ind_pbl_s0], na.rm = TRUE)
        }
      }
    } 
    
    db_int_e[i, 8:20] <- c(h63, tot_aod, tot_aod_err, pbl_aod, pbl_aod_err, ft_aod, ft_aod_err, tot_s0, tot_s0_err, pbl_s0, pbl_s0_err, ft_s0, ft_s0_err)
  }
  
  ########################### For each files ###########################
  pd <- try(ncvar_get(file1, "particledepolarization"), silent = TRUE)
  if (class(pd) == "try-error") pd <- rep(NA, length(alt))
  
  err_pd <- try(ncvar_get(file1, "error_particledepolarization"), silent = TRUE)
  if (class(err_pd) == "try-error") err_pd <- rep(NA, length(alt))
  
  pd <- pd[l1]
  err_pd <- err_pd[l1]
  
  l4 <- which(!is.na(pd) & !is.na(err_pd) & err_pd > 0 & pd + err_pd >= 0 & pd - err_pd <= 1)
  pd <- pd[l4]
  err_pd <- err_pd[l4]
  alt_pd <- alt[l4]
  m_pd <- length(alt_pd)
  
  if (nrow(lydb) > 0) {
    pbl <- min(lydb$Top_Layer)
  } else {
    pbl <- try(ncvar_get(file1, "aerosollayerheight"), silent = TRUE)
    if (class(pbl) == "try-error") {
      pbl <- NA
    } else {
      pbl <- as.numeric(pbl)
      if (length(pbl) > 1) {
        pbl <- mean(pbl, na.rm = TRUE)
      }
    }
  }
  
  tot_pd <- NA
  tot_pd_err <- NA
  pbl_pd <- NA
  pbl_pd_err <- NA
  ft_pd <- NA
  ft_pd_err <- NA
  
  if (m_pd > 0) {
    tot_pd <- mean(pd, na.rm = TRUE)
    tot_pd_err <- mean(err_pd, na.rm = TRUE)
    if (!is.na(pbl)) {
      ind_pbl_pd <- which(alt_pd <= pbl)
      n_pd <- length(ind_pbl_pd)
      if (n_pd == 0) {
        pbl_pd <- NA
        pbl_pd_err <- NA
        ft_pd <- tot_pd
        ft_pd_err <- tot_pd_err
      } else {
        pbl_pd <- mean(pd[ind_pbl_pd], na.rm = TRUE)
        pbl_pd_err <- mean(err_pd[ind_pbl_pd], na.rm = TRUE)
        ft_pd <- mean(pd[-ind_pbl_pd], na.rm = TRUE)
        ft_pd_err <- mean(err_pd[-ind_pbl_pd], na.rm = TRUE)
      }
    }
  }
  
  db_pd[i, 1:15] <- c(lev2db[i, 1:9], tot_pd, tot_pd_err, pbl_pd, pbl_pd_err, ft_pd, ft_pd_err)
  
  # Calculating PBL
  if (nrow(lydb) > 0) {
    pbl <- min(lydb$Top_Layer)
  } else {
    pbl <- try(ncvar_get(file1, "aerosollayerheight"), silent = TRUE)
    if (class(pbl) == "try-error") {
      pbl <- NA
    } else {
      pbl <- as.numeric(pbl)
      if (length(pbl) > 1) {
        pbl <- mean(pbl, na.rm = TRUE)
      }
    }
  }
  db_pbl[i, 7] <- pbl
  
  print(i)
  nc_close(file1)
}

# Initializing and saving databases
db_int_b[is.na(db_int_b)] <- NA
db_int_b <- db_int_b[!apply(is.na(db_int_b), 1, all), ]  # Remove all NA rows
rownames(db_int_b) <- 1:nrow(db_int_b)

db_pd[is.na(db_pd)] <- NA

db_int_e[is.na(db_int_e)] <- NA
db_int_e <- db_int_e[!apply(is.na(db_int_e), 1, all), ]  # Remove all NA rows
if (nrow(db_int_e) > 0) {
  rownames(db_int_e) <- 1:nrow(db_int_e)
}

# Data aggregation
db_pd <- aggregate(
  db_pd[10:15],
  by = list(
    db_pd$Station,
    db_pd$Wavelength,
    db_pd$Year,
    db_pd$Month,
    db_pd$Day,
    db_pd$Hour,
    db_pd$Minutes
  ),
  FUN = mean,
  na.rm = TRUE
)
db_pd[is.na(db_pd)] <- NA
colnames(db_pd)[1:7] <- colnames(lev2db)[c(2, 4:9)]
db_pd <- db_pd[order(db_pd$Station, db_pd$Year, db_pd$Month, db_pd$Day, db_pd$Hour, db_pd$Minutes), ]
rownames(db_pd) <- 1:nrow(db_pd)

db_pbl <- aggregate(
  db_pbl$PBL,
  by = list(
    db_pbl$Station,
    db_pbl$Year,
    db_pbl$Month,
    db_pbl$Day,
    db_pbl$Hour,
    db_pbl$Minutes
  ),
  FUN = mean,
  na.rm = TRUE
)
colnames(db_pbl) <- c(colnames(lev2db)[c(2, 5:9)], "PBL")

# Data sorting
db_pbl <- db_pbl[order(db_pbl$Station, db_pbl$Year, db_pbl$Month, db_pbl$Day, db_pbl$Hour, db_pbl$Minutes), ]

# Resetting row names
rownames(db_pbl) <- 1:nrow(db_pbl)

# Database filtering
db_int_e_355 <- db_int_e[db_int_e$Wavelength == "0355" & !is.na(db_int_e$TotalAOD), 1:16]

# Dataframe pre-allocation
db_angstrom <- data.frame(matrix(NA, nrow = nrow(db_int_e_355), ncol = 10))
db_angstrom[, 1:4] <- db_int_e_355[, c(2, 5:7)]
colnames(db_angstrom) <- c(
  colnames(db_int_e_355)[c(2, 5:7)],
  "TotalAng",
  "Error_TotalAng",
  "PBLAng",
  "Error_PBLAng",
  "FTAng",
  "Error_FTAng"
)

# Angstrom calculation
for (i in seq_len(nrow(db_int_e_355))) {
  db1 <- db_int_e[db_int_e$Station == db_int_e_355$Station[i] &
                    db_int_e$Wavelength == "0532" &
                    db_int_e$Year == db_int_e_355$Year[i] &
                    db_int_e$Month == db_int_e_355$Month[i] &
                    db_int_e$Day == db_int_e_355$Day[i], ]
  db1 <- db1[!is.na(db1$TotalAOD), ]
  
  if (nrow(db1) > 0) {
    tot_aod1 <- db_int_e_355$TotalAOD[i]
    err_tot_aod1 <- db_int_e_355$Error_TotalAOD[i]
    tot_aod2 <- db1$TotalAOD[1]
    err_tot_aod2 <- db1$Error_TotalAOD[1]
    pbl_aod1 <- db_int_e_355$PBLAOD[i]
    err_pbl_aod1 <- db_int_e_355$Error_PBLAOD[i]
    pbl_aod2 <- db1$PBLAOD[1]
    err_pbl_aod2 <- db1$Error_PBLAOD[1]
    ft_aod1 <- db_int_e_355$FTAOD[i]
    err_ft_aod1 <- db_int_e_355$Error_FTAOD[i]
    ft_aod2 <- db1$FTAOD[1]
    err_ft_aod2 <- db1$Error_FTAOD[1]
    
    db_angstrom$TotalAng[i] <- ang(tot_aod1, tot_aod2)
    db_angstrom$Error_TotalAng[i] <- ang_err(tot_aod1, tot_aod2, err_tot_aod1, err_tot_aod2)
    db_angstrom$PBLAng[i] <- ang(pbl_aod1, pbl_aod2)
    db_angstrom$Error_PBLAng[i] <- ang_err(pbl_aod1, pbl_aod2, err_pbl_aod1, err_pbl_aod2)
    db_angstrom$FTAng[i] <- ang(ft_aod1, ft_aod2)
    db_angstrom$Error_FTAng[i] <- ang_err(ft_aod1, ft_aod2, err_ft_aod1, err_ft_aod2)
    
  }
  print(i)
}

# Cleaning up NA and infinite values
db_angstrom[is.na(db_angstrom)] <- NA
db_angstrom$TotalAng[db_angstrom$TotalAng == Inf | db_angstrom$TotalAng == -Inf] <- NA
db_angstrom$Error_TotalAng[db_angstrom$Error_TotalAng == Inf | db_angstrom$Error_TotalAng == -Inf] <- NA
db_angstrom$PBLAng[db_angstrom$PBLAng == Inf | db_angstrom$PBLAng == -Inf] <- NA
db_angstrom$Error_PBLAng[db_angstrom$Error_PBLAng == Inf | db_angstrom$Error_PBLAng == -Inf] <- NA
db_angstrom$FTAng[db_angstrom$FTAng == Inf | db_angstrom$FTAng == -Inf] <- NA
db_angstrom$Error_FTAng[db_angstrom$Error_FTAng == Inf | db_angstrom$Error_FTAng == -Inf] <- NA
