
# setwd(dir = "~/Recherche/Mimic-II/shiny")

source(file = "packages.R")

#######################################################
# -------- Lecture 
#######################################################

options(shiny.sanitize.errors = TRUE)

informations <- readRDS(file = "data/df_information.rds") 
recherche    <- readRDS(file = "data/episode_periode_5.rds")
df_long.n    <- readRDS("data/numerics.rds")
carac        <- read.csv(file = "data/caracteristiques_patients.csv",
                         header = T,sep=",")
carac1       <- read.csv(file = "data/caracteristiques_periodes.csv",
                         header = T,sep=",")

projectName = " AHE prediction "

image = img(src ="cress.png", height = 100)

list_patients.n <- drop.levels(sort(unique(df_long.n$id)))
variables.n <- sort(colnames(df_long.n))

#######################################################
# -------- Panels
#######################################################

dashboard = T
visualise = T
contact = T
donnees = T
modelisation = T

# DONE 

print("config.R done.")


















