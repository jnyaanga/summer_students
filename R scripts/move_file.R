library(tidyverse)
library(here)
library(glue)

files <- list.files(here("F"), full.names = T)

all <- list()
for (i in 1:9) {
  name <- paste("p",i, sep="0")
  all[[i]] <- as.list(stringr::str_subset(files, name))
  
  hour <- paste("H", i, sep = "0")
  #filesstrings::move_files(unlist(all[[i]]),here("F",hour))
}


rB <- stringr::str_subset(files, "_B")
rC <- stringr::str_subset(files, "_C")
rD <- stringr::str_subset(files, "_D")
rE <- stringr::str_subset(files, "_E")
rG <- stringr::str_subset(files, "_G")

#dir.create("A")
#filesstrings::move_files(rG, "G")
