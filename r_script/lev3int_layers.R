# Initializing the DataFrame with NA
db_int_layers <- data.frame(matrix(NA, nrow = 1, ncol = 27))
colnames(db_int_layers) <- c(
  colnames(lev2db_layers),
  "LidarRatio", "Error_LidarRatio",
  "ParticleDep", "Error_ParticleDep",
  "AOD", "Error_AOD",
  "Extinction", "Error_Extinction",
  "CenterMass", "Error_CenterMass",
  "IntBs", "Error_IntBs",
  "Backscatter", "Error_Backscatter"
)

for (i in 1:nrow(lev2db)) {
  file1 <- nc_open(paste0(
    "./New/",
    substr(lev2db$File_name[i], 20, 22),
    "/",
    lev2db$File_name[i]
  ))
  
  lydb <- lev2db_layers[lev2db_layers$File_name == lev2db$File_name[i], ]
  lydb <- lydb[!is.na(lydb$Base_Layer), ]
  n_layers <- nrow(lydb)
  
  if (n_layers > 0) {
    alt <- ncvar_get(file1, "altitude")
    
    bs <- get_nc_var(file1, "backscatter")
    err_bs <- get_nc_var(file1, "error_backscatter")
    
    pd <- get_nc_var(file1, "particledepolarization")
    err_pd <- get_nc_var(file1, "error_particledepolarization")
    
    ext <- get_nc_var(file1, "extinction")
    err_ext <- get_nc_var(file1, "error_extinction")
    
    s0 <- get_nc_var(file1, "lidarratio")
    err_s0 <- get_nc_var(file1, "error_lidarratio")
  
    for (j in 1:n_layers) {
      bl <- lydb$Base_Layer[j]
      tl <- lydb$Top_Layer[j]
      ind1 <- which.min(abs(alt - bl))
      ind2 <- which.min(abs(alt - tl))
      ind <- ind1:ind2
      alt1 <- alt[ind]
      bs1 <- bs[ind]
      err_bs1 <- err_bs[ind]
      ext1 <- ext[ind]
      err_ext1 <- err_ext[ind]
      pd1 <- pd[ind]
      err_pd1 <- err_pd[ind]
      s01 <- s0[ind]
      err_s01 <- err_s0[ind]
      
      l1 <- which(!is.na(bs1) & !is.na(err_bs1) & err_bs1 > 0 & (bs1 + err_bs1) > 0)
      l2 <- which(!is.na(ext1) & !is.na(err_ext1) & err_ext1 > 0 & (ext1 + err_ext1) > 0)
      l3 <- which(!is.na(s01) & !is.na(err_s01) & err_s01 > 0 & (s01 + err_s01) > 0)
      
      bs1 <- bs1[l1]
      err_bs1 <- err_bs1[l1]
      ext1 <- ext1[l2]
      err_ext1 <- err_ext1[l2]
      s01 <- s01[l3]
      err_s01 <- err_s01[l3]
      
      tot_com <- NA
      tot_com_err <- NA
      tot_int_bs <- NA
      tot_int_bs_err <- NA
      wm_bs <- NA
      wm_bs_err <- NA
      tot_aod <- NA
      tot_aod_err <- NA
      wm_ext <- NA
      wm_ext_err <- NA
      wm_s <- NA
      wm_s_err <- NA
      
      if (length(l1) > 0 & lydb[j, 10] < 0.5) {
        alt11 <- alt1[l1]
        prod <- alt11 * bs1
        prod_err <- alt11 * err_bs1
        int_bs <- (alt11[1] - bl) * bs1[1]
        num_com <- (alt11[1] - bl) * prod[1]
        int_bs_err <- (alt11[1] - bl) * err_bs1[1]
        num_com_err <- (alt11[1] - bl) * prod_err[1]
        p_int_bs <- numeric(length(l1))
        p_int_bs[1] <- int_bs
        p_int_bs_err <- numeric(length(l1))
        p_int_bs_err <- int_bs_err
        p_num_com <- numeric(length(l1))
        p_num_com[1] <- num_com
        p_num_com_err <- numeric(length(l1))
        p_num_com_err[1] <- num_com_err
        
        if (length(l1) > 1) {
          for (g in 2:length(l1)) {
            int_bs <- (bs1[g] + bs1[g - 1]) * (alt11[g] - alt11[g - 1]) / 2
            num_com <- (prod[g] + prod[g - 1]) * (alt11[g] - alt11[g - 1]) / 2
            int_bs_err <- (err_bs1[g] + err_bs1[g - 1]) * (alt11[g] - alt11[g - 1]) / 2
            num_com_err <- (prod_err[g] + prod_err[g - 1]) * (alt11[g] - alt11[g - 1]) / 2
            p_int_bs[g] <- p_int_bs[g - 1] + int_bs
            p_num_com[g] <- p_num_com[g - 1] + num_com
            p_int_bs_err[g] <- p_int_bs_err[g - 1] + int_bs_err
            p_num_com_err[g] <- p_num_com_err[g - 1] + num_com_err
          }
        }
        
        tot_int_bs <- p_int_bs[length(l1)]
        tot_com <- p_num_com[length(l1)] / p_int_bs[length(l1)]
        tot_int_bs_err <- p_int_bs_err[length(l1)]
        tot_com_err <- abs(tot_com) * ((p_num_com_err[length(l1)] / p_num_com[length(l1)]) + (tot_int_bs_err / tot_int_bs))
        
        w_bs <- abs(bs1 / err_bs1)
        w_bs <- w_bs / sum(w_bs, na.rm = TRUE)
        wm_bs <- weighted.mean(bs1, w_bs)
        wm_bs_err <- mean(err_bs1, na.rm = TRUE)
      }
      
      if (length(l2) > 0) {
        alt12 <- alt1[l2]
        aod <- numeric(length(l2))
        aod_err <- numeric(length(l2))
        aod[1] <- (alt12[1] - bl) * ext1[1]
        aod_err[1] <- (alt12[1] - bl) * err_ext1[1]
        p_aod <- numeric(length(l2))
        p_aod_err <- numeric(length(l2))
        p_aod[1] <- aod[1]
        p_aod_err[1] <- aod_err[1]
        
        if (length(l2) > 1) {
          for (h in 2:length(l2)) {
            aod[h] <- (ext1[h] + ext1[h - 1]) * (alt12[h] - alt12[h - 1]) / 2
            aod_err[h] <- (err_ext1[h] + err_ext1[h - 1]) * (alt12[h] - alt12[h - 1]) / 2
            p_aod[h] <- p_aod[h - 1] + aod[h]
            p_aod_err[h] <- p_aod_err[h - 1] + aod_err[h]
          }
        }
        tot_aod <- p_aod[length(l2)]
        tot_aod_err <- p_aod_err[length(l2)]
        
        w_ext <- abs(ext1 / err_ext1)
        w_ext <- w_ext / sum(w_ext, na.rm = TRUE)
        wm_ext <- weighted.mean(ext1, w_ext)
        wm_ext_err <- mean(err_ext1, na.rm = TRUE)
      }
      
      if (length(l3) > 0) {
        w_s <- abs(s01 / err_s01)
        w_s <- w_s / sum(w_s, na.rm = TRUE)
        wm_s <- weighted.mean(s01, w_s)
        wm_s_err <- mean(err_s01, na.rm = TRUE)
      }
      
      ind_pd <- which(
        !is.na(pd1) & !is.na(err_pd1) & (pd1 + err_pd1) >= 0 & (pd1 - err_pd1) <= 1 &
          err_pd1 > 0 & abs(pd1) < 1.1
      )
      
      pd1 <- pd1[ind_pd]
      err_pd1 <- err_pd1[ind_pd]
      w_pd <- abs(pd1 / err_pd1)
      w_pd <- w_pd / sum(w_pd, na.rm = TRUE)
      
      lydb[j, 14] <- wm_s
      lydb[j, 15] <- wm_s_err
      lydb[j, 16] <- weighted.mean(pd1, w_pd)
      lydb[j, 17] <- mean(err_pd1, na.rm = TRUE)
      lydb[j, 18] <- tot_aod
      lydb[j, 19] <- tot_aod_err
      lydb[j, 20] <- wm_ext
      lydb[j, 21] <- wm_ext_err
      lydb[j, 22] <- tot_com
      lydb[j, 23] <- tot_com_err
      lydb[j, 24] <- tot_int_bs
      lydb[j, 25] <- tot_int_bs_err
      lydb[j, 26] <- wm_bs
      lydb[j, 27] <- wm_bs_err
    }
    
    # Add columns to lydb and join them to db_int_layers
    colnames(lydb) <- c(
      colnames(lev2db_layers),
      "LidarRatio", "Error_LidarRatio",
      "ParticleDep", "Error_ParticleDep",
      "AOD", "Error_AOD",
      "Extinction", "Error_Extinction",
      "CenterMass", "Error_CenterMass",
      "IntBs", "Error_IntBs",
      "Backscatter", "Error_Backscatter"
    )
    db_int_layers <- rbind(db_int_layers, lydb)
  }
  
  # Closing the NetCDF file and printing the index
  print(i)
  nc_close(file1)
}

# Removing the first line initialized with NA
db_int_layers <- db_int_layers[-1, ]

# Set the NA values to NA (redundant, but kept for clarity)
db_int_layers[is.na(db_int_layers)] <- NA

# Reset row names
rownames(db_int_layers) <- 1:nrow(db_int_layers)

# Correction of CenterMass values
ind1 <- which(db_int_layers$CenterMass < db_int_layers$Base_Layer)
db_int_layers$CenterMass[ind1] <- db_int_layers$Base_Layer[ind1]
ind2 <- which(db_int_layers$CenterMass > db_int_layers$Top_Layer)
db_int_layers$CenterMass[ind2] <- db_int_layers$Top_Layer[ind2] 
