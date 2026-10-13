rm(list = ls())
gc()

setwd("")
sapply(list.files()[-which(list.files() == "mims_ig.R")], source)

data_folder <- ""

#create output sink
con_out_file <- file(
  paste0("", ".txt"),
  open = "a"
)
sink(con_out_file, append = TRUE, type = "output", split = TRUE)

print("")
print("")
print("NEW RUN")
print(paste("START TIME : ", Sys.time()))

geneacore_epochs(
  data_folder = data_folder,
  CutTime24Hr = "00:00",
  output_epochs = TRUE,
  epoch_duration = 5,
  output_events = FALSE,
  output_steps = FALSE,
  output_csv = FALSE,
  timer = TRUE,
  dynamic_range = c(-8, 8),
)

print(paste("END TIME : ", Sys.time()))

# close output sink
sink()
close(con_out_file)
