rm(list = ls())
library(GGIR)
library(dplyr)

bins <- read.csv("")

# GGIR part 2
GGIR(
  mode = c(2),
  datadir = "",
  outputdir = "",
  iglevels = bins$ENMO,
  epochvalues2csv = TRUE,
  overwrite = TRUE
)

GGIR(
  mode = c(2),
  datadir = "",
  outputdir = "",
  iglevels = bins$AGSA,
  overwrite = TRUE
)

# bin ranks for classification accuracy
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

alldata_df$enmo_bins = 0
alldata_df$agsa_bins = 0

rank = 1
for (bin in bins$ENMO){
  alldata_df[which(alldata_df$ENMO >= bin), "enmo_bins"] = rank
  rank = rank + 1
}

rank = 1
for (bin in bins$AGSA){
  alldata_df[which(alldata_df$ENMOa >= bin), "agsa_bins"] = rank
  rank = rank + 1
}

alldata_df = select(alldata_df, enmo_bins, agsa_bins)

write.csv(alldata_df, "")

# Participant avg IGs for agreement analyses
df_in <- read.csv("")

id <- df_in$ID

enmo_ig <- df_in$ig_gradient_ENMO_0.24hr
enmo_int <- df_in$ig_intercept_ENMO_0.24hr
enmo_rsq <- df_in$ig_rsquared_ENMO_0.24hr

df_in <- read.csv("")

agsa_ig <- df_in$ig_gradient_ENMOa_0.24hr
agsa_int <- df_in$ig_intercept_ENMOa_0.24hr
agsa_rsq <- df_in$ig_rsquared_ENMOa_0.24hr

df_ig <- data.frame(id,
                    enmo_ig,
                    agsa_ig
                    )

df_ig <- df_ig[complete.cases(df_ig),]

include <- character()
exclude <- character()

for (id in unique(df_ig$id)){
  if(nrow(df_ig[which(df_ig$id == id),]) > 2){
    include <- c(include, id)
  } else{
    exclude <- c(exclude, id)
  }
}

enmo_avg_ig <- c()
agsa_avg_ig <- c()

for (id in include){
  enmo_avg_ig <- c(enmo_avg_ig, mean(df_ig[which(df_ig$id == id), "enmo_ig"]))
  agsa_avg_ig <- c(agsa_avg_ig, mean(df_ig[which(df_ig$id == id), "agsa_ig"]))
}

df_ig_avg_short <- data.frame(include,
                              enmo_avg_ig,
                              agsa_avg_ig
)

write.csv(df_ig_avg_short, "")
