rm(list = ls())

library(tidyverse)
library(rms)
library(ggplot2)
library(viridis)

palette <- viridis(6)

metafiles = list.files("", full.names = TRUE)
alldata_df = data.frame()
for (metafile in metafiles){
  load(metafile)
  input_df = IMP$metashort
  input_df$non_wear = IMP$r5long[,1]
  input_df$ID = SUM$summary$ID
  alldata_df = rbind(alldata_df, input_df)
  rm(GGIRversion); rm(IMP); rm(input_df); rm(SUM)
}
rm(metafiles)
rm(metafile)

alldata_df <- alldata_df[which(alldata_df$non_wear == 0),]
alldata_df$ENMO = alldata_df$ENMO * 1000
alldata_df$ENMOa = alldata_df$ENMOa * 1000

# some functions seem to present problems when feeding a df to datadist
# works better with individual vectors
enmo <- alldata_df$ENMO
enmoa <- alldata_df$ENMOa

ddist <- datadist(enmo, enmoa) # create datadist with vectors
options(datadist = "ddist") # tell rms to use ddist

# fit models
enmoa_fit_lm <- lm(enmoa ~ 0 + enmo) # no intercept linreg
enmoa_fit_0 <- ols(enmoa ~ enmo) # create fit ols object
enmoa_fit_3 <- ols(enmoa ~ rcs(enmo, c(1, 4000^(1/2), 4000))) # create fit ols object
enmoa_fit_4 <- ols(enmoa ~ rcs(enmo, c(1, 4000^(1/3), 4000^(2/3), 4000))) # create fit ols object
enmoa_fit_5 <- ols(enmoa ~ rcs(enmo, c(1, 4000^(1/4), 4000^(2/4), 4000^(3/4), 4000))) # create fit ols object
enmoa_fit_6 <- ols(enmoa ~ rcs(enmo, c(1, 4000^(1/5), 4000^(2/5), 4000^(3/5), 4000^(4/5), 4000))) # create fit ols object
enmoa_fit_7 <- ols(enmoa ~ rcs(enmo, c(1, 4000^(1/6), 4000^(2/6), 4000^(3/6), 4000^(4/6), 4000^(5/6), 4000))) # create fit ols object

# evaluation
enmoa_aics <- c(AIC(enmoa_fit_0), AIC(enmoa_fit_3), AIC(enmoa_fit_4), AIC(enmoa_fit_5), AIC(enmoa_fit_6), AIC(enmoa_fit_7))
enmoa_bics <- c(BIC(enmoa_fit_0), BIC(enmoa_fit_3), BIC(enmoa_fit_4), BIC(enmoa_fit_5), BIC(enmoa_fit_6), BIC(enmoa_fit_7))
enmoa_rsq <- c(enmoa_fit_0$stats["R2"], enmoa_fit_3$stats["R2"], enmoa_fit_4$stats["R2"], enmoa_fit_5$stats["R2"], enmoa_fit_6$stats["R2"], enmoa_fit_7$stats["R2"]) 
# fit_7 best

#plotting
print("plotting")

alldata_df$enmoa_fitted_0 <- enmoa_fit_0$fitted.values
alldata_df$enmoa_fitted_3 <- enmoa_fit_3$fitted.values
alldata_df$enmoa_fitted_4 <- enmoa_fit_4$fitted.values
alldata_df$enmoa_fitted_5 <- enmoa_fit_5$fitted.values
alldata_df$enmoa_fitted_6 <- enmoa_fit_6$fitted.values
alldata_df$enmoa_fitted_7 <- enmoa_fit_7$fitted.values

ggplot(alldata_df) + 
  geom_point(aes(x=ENMO, y=enmoa), alpha=1/10) +
  geom_line(aes(x=ENMO, y=enmoa_fitted_0), color=palette[1]) +
  geom_line(aes(x=ENMO, y=enmoa_fitted_3), color=palette[2]) +
  geom_line(aes(x=ENMO, y=enmoa_fitted_4), color=palette[3]) +
  geom_line(aes(x=ENMO, y=enmoa_fitted_5), color=palette[4]) +
  geom_line(aes(x=ENMO, y=enmoa_fitted_6), color=palette[5]) +
  geom_line(aes(x=ENMO, y=enmoa_fitted_7), color=palette[6]) +
  ggtitle("enmoa")

print("saving plot")
ggsave(
  "enmoa_models.png",
  plot = last_plot(),
  device = NULL,
  path = "",
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  limitsize = TRUE,
)

ggplot(alldata_df[seq(1, nrow(alldata_df), by = 10),]) + 
  geom_point(aes(x=ENMO,y=ENMOa), alpha=1) +
  geom_line(
    aes(x=ENMO, y=enmoa_fitted_7),
    color="grey",
    linewidth = 1
    ) +
  theme_minimal() +
  ggtitle("enmoa")

ggsave(
  "enmoa_modelfit.png",
  plot = last_plot(),
  device = NULL,
  path = "",
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  limitsize = TRUE,
)

# predict bins
enmoa_p <- Predict(enmoa_fit_7, enmo = seq(0,4000,by=25))

# BINS
# GGIR expects mg bins for g data. mad bins here are already in mg, but need to create
# milli-MIMS bins, and milli-counts bins
# coercing first value to zero, and double the modelled value for enmo=4000 for top value
enmo_bins <- c(seq(0, 4000, by = 25), 8000)
enmoa_bins <- c(0, enmoa_p$yhat[-1], tail(enmoa_p$yhat, 1)*2)

#do naive bins
cdf <- ecdf(alldata_df$ENMO)

enmoa_sf <- quantile(alldata_df$ENMOa, cdf(4000), names = FALSE) / 4000

enmoa_naive_bins <- round(enmo_bins * enmoa_sf, 0)

bins = read_csv("")

bins_update = data.frame(enmo_bins)
bins_update$ENMO = bins$enmo_bins
bins_update$MAD = round(bins$mad_bins, 0)
bins_update$MIMS = round(bins$mims_bins / 1000, 3)
bins_update$CountsVM = round(bins$counts_bins / 1000, 0)
bins_update$AGSA = round(enmoa_bins, 0)
bins_update$AGSA_naive = bins$enmo_bins
bins_update = select(
  bins_update,
  ENMO, MAD, MIMS, CountsVM, AGSA, AGSA_naive
)

# write csv
write_csv(bins_update, "")
