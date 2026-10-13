rm(list = ls())
gc()

# needs GGIR version < 3.2.6 for add_MIMS() to work. not sure exactly. 3.1.0 works

source(".../addMIMS_func.R") # change to path of addMIMS_func.R script
datadir = "" # change to path of raw data directory, just like GGIR
outputdir = "" # change to path where output will be generated, just like GGIR

# geneacore
setwd("")
sapply(list.files()[-which(list.files() %in% c("mims_ig.R"))], source)

data_folder <- ""
subset <- FALSE

# NOTE: one of the internal functions borrowed from GGIR
# apparently doesn't handle tilde-expansion in paths, so I've added path.expand()
# to expand to full paths ie. "~/username/data/" becomes "/Users/username/data/"
# This works, but I don't know which conditions might break it (ie. some non-standard home directory)
# so recommend using full paths in the first place, ie. don't use ~ in path.
# Script will now print outputdir and datadir paths after expansion to allow debugging

dynrange = c(-8, 8) # for MIMSunit function. Don't change.

# Run GGIR part 1
# don't change parameters
GGIR(
  mode = c(1,2),
  datadir = datadir,
  outputdir = outputdir,
  # f1 = 2,
  maxdur = 9,
  windowsizes = c(5, 900, 3600),
  overwrite = TRUE,
  do.cal = TRUE,
  do.imp = FALSE,
  do.enmo = TRUE, # generate ENMO
  do.enmoa = TRUE, # generate ENMOa / AGSA
  do.mad = TRUE, # generate MAD
  do.neishabouricounts = TRUE, #generate counts,
  maxNcores = 4,
)

# Add MIMS to metafiles output from GGIR part 1
# MIMS vector in metafile output will be called "LFENMO" to trick GGIR part 2 later
addMIMS(
  datadir = datadir,
  outputdir = outputdir,
  dynrange = dynrange,
  desiredtz = "",
  configtz = "",
  overwrite = FALSE
)

geneacore_epochs(
  data_folder = data_folder,
  CutTime24Hr = "00:00",
  output_epochs = TRUE,
  epoch_duration = 5,
  output_events = FALSE,
  output_steps = FALSE,
  output_csv = FALSE,
  timer = TRUE,
  dynamic_range = dynrange,
  subset = subset
)

