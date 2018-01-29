visualise_panel <- tabPanel( "AHE Visualisation", icon = icon("area-chart"), align = "center",
                             fluidRow(
                               helpText("Select ID patient"),
                               selectInput("visual_idn", " ",
                                           choices = list_patients.n ,selected = 397),
                               br()
                             ),
                             fluidRow(
                               
                               div(class="panel panel-primary",
                                   div(class="panel-heading",
                                       h4("AHE presence during ICU stay"))),
                               p("This graph shows the evolution of the mean blod pressure for a patient.
                                        A blue zone means the presence of a hypotensive episode in
                                         the prediction period (last 20 min of the 90 min period )"),
                               dygraphOutput("visualise_plot1",height = 700)
                             )
)
