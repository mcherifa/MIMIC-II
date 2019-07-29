contact_panel <- tabPanel("Contact", icon = icon("info"),
                          align = "center",
                          br(),
                          br(),

                          p(strong(span("Application", style = "color:#000000")),
                            "développée dans le cadre de doctorat au sein de l'unité Inserm 1153"),
                          div (style=" float:center; display:inline",img(src ="cress.png", height = 150)), 
                          br(),
                          br(),
                          p("Elle permet de visualiser des données anonymes issues de la base MIMIC II
                             (telles que la fréquence cardiaque, la SpO2, la pression artérielle moyenne,…) ,
                             chez des patients monitorés en réanimation, et ce 
                             à intervalle de temps régulier."),
                          br(),
                          p("Cette application permet d'avoir une ",
                            strong(span(" vision dynamique", style = "color:#000000")),
                            " des données."),
                          br(),
                          br(),
                          br(),
                          br(),
                          img(src = "moi.png", height = 140),
                          br(),
                          p("Contact :"),
                          p(HTML(paste0("<a href='", "mailto:cmenyssa@live.fr",
                                        "' target='_blank'>menyssa.cherifa@inserm.fr</a>"))),
                          br(),
                          br(),
                          br(),
                          br(),
                          div(id="footer",class="flex-row flex-between",
                              div(em("Application optimisée pour le navigateur internet ",
                                     a("Chrome",href = "https://www.google.fr/chrome/browser/desktop/",target="_blank")," .")),
                              div("Copyright © ", a("Ményssa CHERIFA",href = "https://fr.linkedin.com/in/menyssacherifa", target="_blank"),
                                  ". All rights reserved."))
)




