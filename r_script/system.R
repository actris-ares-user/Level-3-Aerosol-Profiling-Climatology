loc2 <- unique(lev2db$Station)
syst = matrix("", nrow = length(loc2), ncol = (release[2] - release[1] + 1))

for (i in 1:length(loc2)) {
  for (j in 1:(release[2] - release[1] + 1)) {
    db1 <- lev2db[lev2db$Station == loc2[i] & lev2db$Year == j + 1999, ]
    
    if (nrow(db1) > 0) {
      s <- NULL
      for (k in 1:nrow(db1)) {
        file0 <- nc_open(paste0("./New/", loc2[i], "/", db1[k, 1]))
        s <- c(s, ncatt_get(file0, 0, attname = "system")$value) 
        nc_close(file0)
      }
      s <- unique(s)
      syst[i, j] <- paste(s, collapse = " ; ")
      
    }
  }
}
syst_df <- data.frame(syst)
rownames(syst_df) <- loc2
colnames(syst_df) <- as.character(seq(from = release[1], to = release[2], by = 1))
