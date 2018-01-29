setwd("~/Mimic/resultats/")

# changer le Path scripts

dest_r <- "~/Mimic/scripts/R/toolbox/"
#dest_r <- "~/Desktop/Mimic/scripts/R/toolbox/"

# changer le Path data

dest_data <- "~/Mimic/data/clean/mimic2/"
# dest_data <- "~/Desktop/Mimic/data/"

path_result <-"~/Mimic/resultats/"


source(paste0(dest_r,"packages.R"))
source(paste0(dest_r,"fonctions.R"))
source(paste0(dest_r,"models.R"))
source(paste0(dest_r,"cross_validation.R"))
source(paste0(dest_r,"utils.r"))
source(paste0(dest_r,"text_cleaning.R"))
source(paste0(dest_r,"to.balanced.R"))
source(paste0(dest_r,"text_mining.R"))
