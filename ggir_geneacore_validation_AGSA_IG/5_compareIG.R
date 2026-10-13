rm(list = ls())

library(psych)
library(irr)
library(blandr)
library(ggplot2)
library(yardstick)

df_avg_ig_short <- read.csv("")

#correlations
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$agsa_avg_ig, method = "pearson")

cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$agsa_avg_ig, method = "spearman")

# ICC
agsa_icc<- icc(data.frame(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$agsa_avg_ig),
                 model = "twoway", type = "consistency", unit = "single")

#BA-stuff
agsa_ba <- blandr.statistics(df_avg_ig_short$agsa_avg_ig, df_avg_ig_short$enmo_avg_ig)
# blandr.draw(df_avg_ig_short$agsa_avg_ig, df_avg_ig_short$enmo_avg_ig, ciDisplay = FALSE, ciShading = FALSE)

# descriptives
quantile(df_avg_ig_short$agsa_avg_ig, prob=c(.25,.5,.75), type=1)

# MPE
mpe(df_avg_ig_short, enmo_avg_ig, agsa_avg_ig)
