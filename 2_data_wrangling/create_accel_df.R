rm(list=ls())
gc()

library(tidyverse)

output_dir = ""
data_dir = ""

bins_df = read_csv("", show_col_types = FALSE) # read in bins for IG
binfile_dirs = list.dirs(data_dir, recursive = FALSE)
dirs_missing = c()
accel_df = tibble()

#### IG functions

calc_IG = function(epoch_metric, bins_name, bins = bins_df){
  bins$ENMO = bins$ENMO / 1000 # scaling bins to g rather than mg as that's the format of the epochs
  bins$MAD = bins$MAD / 1000
  bins$AGSA = bins$AGSA / 1000
  bins$AGSA_naive = bins$AGSA_naive / 1000
  bins = pull(bins[bins_name]) # select specific set of bins
  binned = cut(epoch_metric, breaks = bins)
  binned = table(binned)
  binned = as.numeric(binned) * 5 / 60 # converting to minutes (not sure why GGIR does this)
  x_ig = zoo::rollmean(bins, k = 2)
  y_ig = binned
  igout = g.intensitygradient(x_ig, y_ig)
  return(igout)
}


# from GGIR
g.intensitygradient = function(x,y) {
  # x: numeric vector of bin mid-points
  # y: numeric vector of time spent in bins
  y = ifelse(test = y <=0,yes = NA,no = y)
  ly = log(y)
  lx = log(x)
  y_intercept = NA
  gradient = NA
  rsquared = NA
  if (length(which(is.na(lx) == FALSE)) > 1 & length(which(is.na(ly) == FALSE)) > 1) {
    if (sd(lx,na.rm = TRUE) != 0 & sd(ly,na.rm = TRUE) != 0) {
      fitsum = summary(stats::lm(ly ~ lx))
      # y_intercept = stats::coef(fitsum)[1,1]
      gradient = stats::coef(fitsum)[2,1]
      rsquared = fitsum$r.squared
    }
  }
  return(list("gradient" = gradient, "rsquared" = rsquared))
}

# processing
file_n = 1
for (dir in binfile_dirs){
  print(paste0("File ", file_n, "/", length(binfile_dirs), ": ", basename(dir)))
  epochs_file = list.files(dir, pattern = "*_epochs_5s.rds", full.names = TRUE)
  MPI_file = list.files(dir, pattern = "*_MPI.rds", full.names = TRUE)
  if (length(MPI_file) + length(epochs_file) == 2){
    MPI = readRDS(MPI_file)
    epochs = readRDS(epochs_file)
    for (day in unique(epochs$day_number)){
      epochs_day = filter(epochs, day_number == day)
      
      duration_hours = nrow(epochs_day) * 5 / 60**2
      non_wear_hours = nrow(filter(epochs_day,non_wear == 1)) * 5 / 60**2
      
      IG_AGSA = calc_IG(epochs_day$AGSA, "AGSA")
      IG_ENMO = calc_IG(epochs_day$ENMO, "ENMO")
      IG_MAD = calc_IG(epochs_day$MAD, "MAD")
      IG_MIMS = calc_IG(epochs_day$MIMS, "MIMS")
      IG_CountsVM = calc_IG(epochs_day$CountsVM, "CountsVM")
      
      row_for_df = list(
        "BinfileName" = MPI$file_data$BinfileName,
        "auto_calibration_error" = MPI$auto_calibration$error,
        "day_number_file" = day,
        "date" = date(as.POSIXct(epochs_day$timestamp[1])),
        "weekday" = wday(as.POSIXct(epochs_day$timestamp[1])),
        "day_of_year" = yday(as.POSIXct(epochs_day$timestamp[1])),
        "week_of_year" = floor(yday(as.POSIXct(epochs_day$timestamp[1]))/7) + 1,
        "season_spring" = sin(2 * pi * yday(as.POSIXct(epochs_day$timestamp[1])) / 365.24),
        "season_winter" = cos(2 * pi * yday(as.POSIXct(epochs_day$timestamp[1])) / 365.24),
        "duration_hours" = round(duration_hours, 2),
        "non_wear_hours" = round(non_wear_hours, 2),
        "valid_wear_hours" = round(duration_hours - non_wear_hours, 2),
        "avg_acc_AGSA" = mean(epochs_day$AGSA) * 1000,
        "avg_acc_ENMO" = mean(epochs_day$ENMO) * 1000,
        "avg_acc_MAD" = mean(epochs_day$MAD) * 1000,
        "avg_acc_MIMS" = mean(epochs_day$MIMS),
        "avg_acc_CountsVM" = mean(epochs_day$CountsVM),
        "IG_AGSA" = IG_AGSA[[1]],
        "IG_rsq_AGSA" = IG_AGSA[[2]],
        "IG_ENMO" = IG_ENMO[[1]],
        "IG_rsq_ENMO" = IG_ENMO[[2]],
        "IG_MAD" = IG_MAD[[1]],
        "IG_rsq_MAD" = IG_MAD[[2]],
        "IG_MIMS" = IG_MIMS[[1]],
        "IG_rsq_MIMS" = IG_MIMS[[2]],
        "IG_CountsVM" = IG_CountsVM[[1]],
        "IG_rsq_CountsVM" = IG_CountsVM[[2]]
      )
      accel_df = bind_rows(accel_df, row_for_df)
    }
  } else {
    dirs_missing = c(dirs_missing, dir)
  }
  file_n = file_n + 1
}

write_rds(accel_df, paste0(output_dir, "accel_df.rds"))
write_rds(dirs_missing, paste0(output_dir, "dirs_missing.rds"))
rm(list=ls())
gc()