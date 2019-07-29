setwd("~/Recherche/Mimic-II//scripts/R/SQL")

# sudo apt-get install libpq-dev
library(RPostgreSQL)

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {"yessersbim75"}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

###############################################
# creates a connection to the postgres MIMIC2
###############################################

# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "MIMIC2",
                 host = "localhost", port = 5432,
                 user = "menyssa", password = pw)
rm(pw) # removes the password
