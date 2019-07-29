rm(list=ls())

source(file  = "config.r")
source (file = "packages.R")
source(file  = "utils.R")

#####################################
#### Interface utilisateur Shiny ####
#####################################

navBarPageARGS = list(projectName)

if (dashboard==T){
  source('panels/dashboard/dashboard_ui.r',local=T)
  navBarPageARGS = append(navBarPageARGS, list(dashboard_panel))
}
if (donnees==T){
  source('panels/donnees/donnees_ui.r',local=T)
  navBarPageARGS = append(navBarPageARGS, list(donnees_panel))
}
if (visualise==T){
  source('panels/visualise/visualise_ui.r',local=T)
  navBarPageARGS = append(navBarPageARGS, list(visualise_panel))
}
if (modelisation==T){
  source('panels/modelisation/model_ui.r',local=T)
  navBarPageARGS = append(navBarPageARGS, list(model_panel))
}
if (contact==T){
  source('panels/contact/contact_ui.r',local=T)
  navBarPageARGS = append(navBarPageARGS, list(contact_panel))
}

navbarPagePerso = do.call(navbarPage, navBarPageARGS)

shinyUI(
  fluidPage(theme = shinytheme("flatly"),
            br(),
            #             div (style=" float:left; display:inline",img(src ="cress.png", height = 100)), 
            #             div(style="float:right; display:inline",img(src ="p7.png", height = 120)),
            div(style="text-align: center;",
                h3("Prediction of hypotensive episodes from longitudinal
                  high-frequency data collected in ICU patients")),
            
            br(),
            #div(style="background-color:SteelBlue; height:50px; 
            #overflow:scroll; overflow-x: scroll; overflow-y: hidden;"),
            tags$head(
              tags$style(HTML(
                ".navbar .navbar-nav {
                margin-left: 20%"))),
            tags$head(
              tags$style(HTML(
                ".navbar-default {
                background-color: rgb(126,150, 161);
                border-color: transparent;}"))),
            
            navbarPagePerso
            
  ))


