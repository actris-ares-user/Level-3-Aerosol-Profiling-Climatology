ll <- "./Layers/"
sts <- intersect(unique(lev2db$Station), list.files(ll))
layers_files <- NULL

for (i in 1:length(sts)) {
  lf <- list.files(paste0(ll, sts[i]))
  file0 <- nc_open(paste0(ll, sts[i], "/", lf[1]))  
  
  iptf <- ncvar_get(file0, "input_file")
  gp <- ncvar_get(file0, "geometrical_properties")
  n_layers <- dim(gp)[2]
  n_time <- dim(gp)[3]
  k <- n_layers * n_time
  
  if (k > 0 & !is.na(k)) {
    m0 <- data.frame(matrix(NA, nrow = k, ncol = 8))
    m0[, 1] <- rep(sts[i], times = k)
      
    for (j in 1:n_time) {
      s1 <- n_layers * (j - 1) + 1
      s2 <- n_layers * j
      
      yy <- substr(iptf[j], 36, 39)
      mnth <- substr(iptf[j], 40, 41)
      dd <- substr(iptf[j], 42, 43)
      hh <- substr(iptf[j], 44, 45)
      mnts <- substr(iptf[j], 46, 47)
      
      m0[s1:s2, 2] <- rep(yy, times = n_layers)
      m0[s1:s2, 3] <- rep(mnth, times = n_layers)
      m0[s1:s2, 4] <- rep(dd, times = n_layers)
      m0[s1:s2, 5] <- rep(hh, times = n_layers)
      m0[s1:s2, 6] <- rep(mnts, times = n_layers)
      
      for (l in 1:n_layers) {
        m0[(s1 + l - 1), 7] <- gp[2, l, j]
        m0[(s1 + l - 1), 8] <- gp[4, l, j]
      }
    }
  }
  
  if (i == 1) {
    layers_files <- m0
  } else {
    layers_files <- rbind(layers_files, m0)
  }
  
  nc_close(file0)
  print(i)
}

colnames(layers_files) <- c("Station",
                           "Year",
                           "Month",
                           "Day",
                           "Hour",
                           "Minutes",
                           "Base_Layer",
                           "Top_Layer")
layers_files$Base_Layer[layers_files$Base_Layer > 9e36] <- NA
layers_files$Top_Layer[layers_files$Top_Layer > 9e36] <- NA
layers_files[, 7:8] <- layers_files[, 7:8] * 1000
layers_files <- layers_files[!is.na(layers_files$Top_Layer), ]
lev2db_layers <- merge(
  lev2db,
  layers_files,
  by = c("Station", "Year", "Month", "Day", "Hour", "Minutes"),
  all = TRUE
)

layers_base_top <- layers_files