dashboard_panel <- tabPanel("Resume",icon = icon("dashboard"),
                            fluidRow(
                              column(6,
                                     fluidRow(
                                       column(6,uiOutput('dashboard_box1')),
                                       column(6,uiOutput('dashboard_box2'))
                                     ),
                                     div(class="panel panel-primary",
                                         div(class="panel-heading",
                                             h4("Study patient flowchart")),
                                         img(src = "flowchart.png",height = 700))),
                              column(6,
                                     div(class="panel panel-primary",
                                         div(class="panel-heading",
                                             h4("Characteristics of the study population ")),
                                         uiOutput("mytable"),
                                         p("There are 1152 patients included. Continuous variables are presented as Median [InterQuartile Range];
                                       Binary or categorical variables as count (%);
                                       SAPS II : Simplified Acute Physiology Score II ;
                                       SOFA : Sequential Organ Failure Assessment")),
                                     div(class="panel panel-primary",
                                         div(class="panel-heading",
                                             h4("Description of means numerics over
                                                study periods according to AHE
                                                periods status")),
                                         uiOutput("mytable1"),
                                         p("We defined 30025 periods of 90 minutes, 
                                           26720 periods without AHE and 2165 with AHE."))
                              )
                            )
)
