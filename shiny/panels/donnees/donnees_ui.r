donnees_panel <- tabPanel("Monitoring", icon = icon("heartbeat"),
                          sidebarLayout(
                            sidebarPanel(
                              align = "center",
                              br(),
                              br(),
                              helpText("Select ID patient"),
                              selectInput("idn", " ",
                                          choices = list_patients.n ,selected = 397),
                              br(),
                              br(),
                              helpText("Patient Characteristics"),
                              div(style="margin-bottom:20px; color: #808080;",
                                  tags$style(type="text/css",
                                             ".shiny-output-error { visibility: hidden; }",
                                             ".shiny-output-error:before { visibility: hidden; }"
                                  )),
                                  uiOutput('data_box1'),
                                  uiOutput('data_box2'),
                                  uiOutput('data_box3'),
                                  uiOutput('data_box4'),
                                  br()
                              #helpText("Le bouton ci-dessous permet de 
                              #télécharger les graphiques associés au patient"),
                              #br()
                              #helpText("Download visual"),
                              #downloadButton('visualise_downloadPlot','Download')
                              #align = "center"
                            ),
                          mainPanel(
                            div(style="margin-bottom:20px; color: #808080;",
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"
                                )),
                            div(class="panel panel-primary",
                                div(class="panel-heading",
                                    h4("Evolution of patient's time series during ICU stay"))),
                                
                                dygraphOutput("data_plot1",height = 700),
                                br(),
                                br()
                                )
                          )
)

