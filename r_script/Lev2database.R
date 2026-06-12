sel_files <- basename(list.files("./New", recursive = TRUE, full.names = TRUE))

lev2db <- setNames(
  data.frame(matrix(NA, nrow = length(sel_files), ncol = 10)),
  c(
    "File_name",
    "Station",
    "Type",
    "Wavelength",
    "Year",
    "Month",
    "Day",
    "Hour",
    "Minutes",
    "VarBool"
  )
)

lev2db$File_name = sel_files
lev2db$Station = substr(sel_files, 20, 22)
lev2db$Type = substr(sel_files, 30, 30)
lev2db$Wavelength = substr(sel_files, 31, 34)
lev2db$Year = substr(sel_files, 36, 39)
lev2db$Month = substr(sel_files, 40, 41)
lev2db$Day = substr(sel_files, 42, 43)
lev2db$Hour = substr(sel_files, 44, 45)
lev2db$Minutes = substr(sel_files, 46, 47)

climatol = read.table("Climatol2.log")[-c(1:2, (nrow(read.table("Climatol2.log")) - 1):nrow(read.table("Climatol2.log"))), 1]  # Sergio's code: climatol = read.table("Climatol2.log")[-c(1:2, 35048:35049), 1]
calipso = read.table("Calipso2.log")[-c(1:2, (nrow(read.table("Calipso2.log")) - 1):nrow(read.table("Calipso2.log"))), 1]  # Sergio's code: calipso = read.table("Calipso2.log")[-c(1:2, 8941:8942), 1]

all_files = sort(union(climatol, calipso))

for (i in 1:length(sel_files)) {
  ct = 0
  if (lev2db$Type[i] == "e") {
    d_b = lev2db[lev2db$Station == lev2db$Station[i] &
                   lev2db$Type == "b" &
                   lev2db$Wavelength == lev2db$Wavelength[i] &
                   lev2db$Year == lev2db$Year[i] &
                   lev2db$Month == lev2db$Month[i] &
                   lev2db$Day == lev2db$Day[i] &
                   lev2db$Hour == lev2db$Hour[i] & lev2db$Minutes == lev2db$Minutes[i], ]
    if (nrow(d_b) > 0) {
      ct = ct + 1
    }
  }
  lev2db[i, 10] = ct
}

lev2db$PartDepFlag = rep(NA, times = nrow(lev2db))
lev2db$PartDepFlag2 = rep(NA, times = nrow(lev2db))
lev2db$PdF = rep(0, times = nrow(lev2db))
vtt = NULL

for (i in 1:nrow(lev2db)) {
  # PartDepFlag code
  file1 <- nc_open(paste0(
    "./New/",
    substr(lev2db$File_name[i], 20, 22),
    "/",
    lev2db$File_name[i]
  ))
  pd_flag <- 1
  pd <- try(ncvar_get(file1, "particledepolarization"), silent = TRUE)
  
  if (class(pd) == "try-error") {
    pd_flag <- 0
  }
  lev2db$PartDepFlag[i] <- pd_flag
  nc_close(file1)

  # PartDepFlag2 code
  ct = 0
  if (lev2db$Type[i] == "e" & lev2db$PartDepFlag[i] == 1) {
    d_b <- lev2db[lev2db$Station == lev2db$Station[i] &
                   lev2db$Type == "b" &
                   lev2db$Wavelength == lev2db$Wavelength[i] &
                   lev2db$Year == lev2db$Year[i] &
                   lev2db$Month == lev2db$Month[i] &
                   lev2db$Day == lev2db$Day[i] &
                   lev2db$Hour == lev2db$Hour[i] & lev2db$Minutes == lev2db$Minutes[i], ]
    if (nrow(d_b) > 0) {
      ct <- ct + 1
    }
  }
  lev2db$PartDepFlag2[i] <- ct

  # PdF code
  if (lev2db$Type[i] == "e" & lev2db$PartDepFlag[i] == 1 & lev2db$PartDepFlag2[i] == 0) {
    lev2db$PdF[i] <- 1
  }
  
  # Filtering dataframe code
  if (lev2db$File_name[i] %in% all_files) {
    vtt <- c(vtt, i)
  }
}

lev2db <- lev2db[vtt, ]
lev2db <- lev2db[, -which(names(lev2db) %in% c("PartDepFlag", "PartDepFlag2"))]
lev2db <- lev2db[(lev2db$Station == "laq" & lev2db$Wavelength == "0351") |
                  (lev2db$Station != "laq" & lev2db$Wavelength %in% c("0355", "0532", "1064")), ]
