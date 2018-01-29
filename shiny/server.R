##################
#### Server.R ####
##################

shinyServer(function(input, output) {
  
  if (dashboard == T )
    source('panels/dashboard/dashboard.r', local=T)
  
  if(donnees == T)
    source('panels/donnees/donnees.r', local=T)
  
  if (visualise == T )
    source('panels/visualise/visualise.r', local=T)
  
})
