rm(list = ls())

library(psych)
library(irr)
library(ggplot2)

df <- read.csv("")

#correlations
spearman <- cor(df$enmo_bins, df$agsa_bins, method = "spearman")

# Kappa
kappa <- cohen.kappa(data.frame(df$enmo_bins, df$agsa_bins), levels=1:161)$weighted.kappa

# mean diff
mean_diff = mean(df$agsa_bins - df$enmo_bins)

# median diff
median_diff = median(df$agsa_bins - df$enmo_bins)
