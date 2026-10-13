rm(list = ls())

library(tidyverse)
library(ggplot2)
library(patchwork)

outputdir = "" # change to path where output will be generated, just like GGIR
geneacoredir = ""

metafiles = list.files(outputdir)


##### DOING JUST ONE DAY PER FILE

validation_df = data.frame()

for (metafile in metafiles) {
# for (metafile in metafiles[1]) {
  gc_dir_name = paste0(geneacoredir, sub(".bin.RData", "", sub("meta_", "", metafile)))
  gc_epochs = list.files(gc_dir_name, pattern = "*_epochs_5s.rds", full.names = TRUE)
  gc_epochs = readRDS(gc_epochs)
  gc_epochs = gc_epochs[which(gc_epochs$day_number == 2),]
  
  load(paste0(outputdir, metafile))
  ggir_epochs = M$metashort
  rm(GGIRversion); rm(I); rm(C); rm(M)
  
  gc_epochs = gc_epochs[20:(nrow(gc_epochs)-20),]
  ggir_epochs = ggir_epochs[as.POSIXct(ggir_epochs$timestamp, format = "%Y-%m-%dT%H:%M:%S") >= as.POSIXct(gc_epochs$timestamp[1]),]
  ggir_epochs = ggir_epochs[as.POSIXct(ggir_epochs$timestamp, format = "%Y-%m-%dT%H:%M:%S") <= as.POSIXct(gc_epochs$timestamp[nrow(gc_epochs)]),]
  
  if (nrow(ggir_epochs) > nrow(gc_epochs)){
    ggir_epochs = ggir_epochs[1:nrow(gc_epochs),]
  } else if (nrow(ggir_epochs) < nrow(gc_epochs)){
    gc_epochs = gc_epochs[1:nrow(ggir_epochs),]
  }
  
  epochs = ggir_epochs
  epochs$timestamp = as.POSIXct(epochs$timestamp, format = "%Y-%m-%dT%H:%M:%S")
  # epochs$non_wear = epochs[which(gc_epochs$non_wear == 0),]
  epochs$MIMS = epochs$LFENMO
  epochs$Counts = epochs$NeishabouriCount_vm
  epochs$gc_ENMO = gc_epochs$ENMO
  epochs$gc_ENMOa = gc_epochs$AGSA
  epochs$gc_MAD = gc_epochs$MAD
  epochs$gc_MIMS = gc_epochs$MIMS
  epochs$gc_Counts = gc_epochs$CountsVM
  epochs$diff_ENMO = epochs$ENMO - epochs$gc_ENMO
  epochs$diff_ENMOa = epochs$ENMOa - epochs$gc_ENMOa
  epochs$diff_MAD = epochs$MAD - epochs$gc_MAD
  epochs$diff_MIMS = epochs$MIMS - epochs$gc_MIMS
  epochs$diff_Counts = epochs$Counts - epochs$gc_Counts
  epochs$order = 1:nrow(epochs)
  rm(gc_epochs); rm(ggir_epochs)
  
  ### PLOTTING
  seed = sample(0:19, 1)
  subset_size = floor(nrow(epochs)/20)
  p1 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, ENMO)) +
    geom_line(aes(order, gc_ENMO), color = "yellow", alpha = 0.7)
  p2 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, diff_ENMO))
  p1 / p2
  ggsave(
    filename = paste0(basename(gc_dir_name), " ENMO.pdf"),
    plot = last_plot(),
    device = "pdf",
    path = "",
    scale = 1,
    width = 2000,
    height = 2000,
    units = "px"
  )
  
  p1 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, ENMOa)) +
    geom_line(aes(order, gc_ENMOa), color = "yellow", alpha = 0.7)
  p2 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, diff_ENMOa))
  p1 / p2
  ggsave(
    filename = paste0(basename(gc_dir_name), " ENMOa.pdf"),
    plot = last_plot(),
    device = "pdf",
    path = "",
    scale = 1,
    width = 2000,
    height = 2000,
    units = "px"
  )
  
  p1 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, MAD)) +
    geom_line(aes(order, gc_MAD), color = "yellow", alpha = 0.7)
  p2 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, diff_MAD))
  p1 / p2
  ggsave(
    filename = paste0(basename(gc_dir_name), " MAD.pdf"),
    plot = last_plot(),
    device = "pdf",
    path = "",
    scale = 1,
    width = 2000,
    height = 2000,
    units = "px"
  )
  
  p1 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, MIMS)) +
    geom_line(aes(order, gc_MIMS), color = "yellow", alpha = 0.7)
  p2 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, diff_MIMS))
  p1 / p2
  ggsave(
    filename = paste0(basename(gc_dir_name), " MIMS.pdf"),
    plot = last_plot(),
    device = "pdf",
    path = "",
    scale = 1,
    width = 2000,
    height = 2000,
    units = "px"
  )
  
  p1 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, Counts)) +
    geom_line(aes(order, gc_Counts), color = "yellow", alpha = 0.7)
  p2 = ggplot(epochs[(1+seed*subset_size):(200+seed*subset_size),]) +
    geom_line(aes(order, diff_Counts))
  p1 / p2
  ggsave(
    filename = paste0(basename(gc_dir_name), " Counts.pdf"),
    plot = last_plot(),
    device = "pdf",
    path = "",
    scale = 1,
    width = 2000,
    height = 2000,
    units = "px"
  )
  
  ### STATS
  validation_df = rbind(
    validation_df,
    list(
      "filename" = paste0(basename(gc_dir_name), ".bin"),
      "diff_ENMO_q0" = format(round(quantile(epochs$diff_ENMO), 6), scientific = F)[[1]], 
      "diff_ENMO_q25" = format(round(quantile(epochs$diff_ENMO), 6), scientific = F)[[2]], 
      "diff_ENMO_q50" = format(round(quantile(epochs$diff_ENMO), 6), scientific = F)[[3]], 
      "diff_ENMO_q75" = format(round(quantile(epochs$diff_ENMO), 6), scientific = F)[[4]], 
      "diff_ENMO_q100" = format(round(quantile(epochs$diff_ENMO), 6), scientific = F)[[5]],
      "diff_ENMO_mean" = format(round(mean(epochs$diff_ENMO), 6), scientific = F),
      "abs_diff_ENMO_mean" = format(round(mean(abs(epochs$diff_ENMO)), 6), scientific = F),
      "diff_ENMOa_q0" = format(round(quantile(epochs$diff_ENMOa), 6), scientific = F)[[1]], 
      "diff_ENMOa_q25" = format(round(quantile(epochs$diff_ENMOa), 6), scientific = F)[[2]], 
      "diff_ENMOa_q50" = format(round(quantile(epochs$diff_ENMOa), 6), scientific = F)[[3]], 
      "diff_ENMOa_q75" = format(round(quantile(epochs$diff_ENMOa), 6), scientific = F)[[4]], 
      "diff_ENMOa_q100" = format(round(quantile(epochs$diff_ENMOa), 6), scientific = F)[[5]],
      "diff_ENMOa_mean" = format(round(mean(epochs$diff_ENMOa), 6), scientific = F),
      "abs_diff_ENMOa_mean" = format(round(mean(abs(epochs$diff_ENMOa)), 6), scientific = F),
      "diff_MAD_q0" = format(round(quantile(epochs$diff_MAD), 6), scientific = F)[[1]], 
      "diff_MAD_q25" = format(round(quantile(epochs$diff_MAD), 6), scientific = F)[[2]], 
      "diff_MAD_q50" = format(round(quantile(epochs$diff_MAD), 6), scientific = F)[[3]], 
      "diff_MAD_q75" = format(round(quantile(epochs$diff_MAD), 6), scientific = F)[[4]], 
      "diff_MAD_q100" = format(round(quantile(epochs$diff_MAD), 6), scientific = F)[[5]],
      "diff_MAD_mean" = format(round(mean(epochs$diff_MAD), 6), scientific = F),
      "abs_diff_MAD_mean" = format(round(mean(abs(epochs$diff_MAD)), 6), scientific = F),
      "diff_MIMS_q0" = format(round(quantile(epochs$diff_MIMS), 6), scientific = F)[[1]], 
      "diff_MIMS_q25" = format(round(quantile(epochs$diff_MIMS), 6), scientific = F)[[2]], 
      "diff_MIMS_q50" = format(round(quantile(epochs$diff_MIMS), 6), scientific = F)[[3]], 
      "diff_MIMS_q75" = format(round(quantile(epochs$diff_MIMS), 6), scientific = F)[[4]], 
      "diff_MIMS_q100" = format(round(quantile(epochs$diff_MIMS), 6), scientific = F)[[5]],
      "diff_MIMS_mean" = format(round(mean(epochs$diff_MIMS), 6), scientific = F),
      "abs_diff_MIMS_mean" = format(round(mean(abs(epochs$diff_MIMS)), 6), scientific = F),
      "diff_Counts_q0" = format(round(quantile(epochs$diff_Counts), 6), scientific = F)[[1]], 
      "diff_Counts_q25" = format(round(quantile(epochs$diff_Counts), 6), scientific = F)[[2]], 
      "diff_Counts_q50" = format(round(quantile(epochs$diff_Counts), 6), scientific = F)[[3]], 
      "diff_Counts_q75" = format(round(quantile(epochs$diff_Counts), 6), scientific = F)[[4]], 
      "diff_Counts_q100" = format(round(quantile(epochs$diff_Counts), 6), scientific = F)[[5]],
      "diff_Counts_mean" = format(round(mean(epochs$diff_Counts), 6), scientific = F),
      "abs_diff_Counts_mean" = format(round(mean(abs(epochs$diff_Counts)), 6), scientific = F)
    )
  )
}

write_csv(validation_df, "")
