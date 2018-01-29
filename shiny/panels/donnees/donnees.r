contenu.n <- reactive({
  
  fichier <- subset(df_long.n, id %in% input$idn)
  
  return(fichier)
})

contenu.serie.n <- reactive({
  
  data <- xts(contenu.n(),order.by = contenu.n()[,2] )[,2:7]
  
  return(data) 
})

info <- reactive({
  informations$id <- as.character(informations$id)
  data <- subset(informations, id %in% input$idn)
  
  return(data) 
})

output$data_box1 <- renderUI({
  c = unique(info()$age)
  return(html_tpl_dashboard_box(c,fat_text = "Age (years) ",
                                icon = 'fa fa-birthday-cake',bg_color = 'steelblue1'))
})

output$data_box2 <- renderUI({
  d = unique(info()$gender)
  if (d =="F"){  
    return(html_tpl_dashboard_box(d,fat_text = "Sex ",icon = 'fa fa-female',bg_color = 'steelblue1'))
  }else{
    return(html_tpl_dashboard_box(d,fat_text = "Sex ",icon = 'fa fa-male',bg_color = 'steelblue1'))
  }
  
})
output$data_box3 <- renderUI({
  e = unique(info()$sofa_first)
  return(html_tpl_dashboard_box(e,fat_text = "SOFA ",
                                icon = 'fa fa-hospital-o',bg_color = 'steelblue1'))
})
output$data_box4 <- renderUI({
  f = unique(info()$sapsi_first)
  return(html_tpl_dashboard_box(f,fat_text = "SAPS II ",
                                icon = 'fa fa-hospital-o',bg_color = 'steelblue1'))
})

output$tableau.n <- renderDataTable({
  inFile <- contenu.serie.n()
  if (is.null(inFile)){ h5("En attente de données ...", align = "center")}
  else{
    DT::datatable(inFile,rownames = F, style = 'bootstrap',
                  options = list(
                    lengthMenu = list(c(3,5,10), c('3', '5', '10')),
                    pageLength = 3,
                    language =
                      list(url = '//cdn.datatables.net/plug-ins/f2c75b7247b/i18n/French.json')))
    
  }
  return(inFile)
  
})

output$mainPanel_collecter.n <- renderUI({
  inFile <- contenu.serie.n()
  liste <- NULL
  
  if (is.null(inFile)) { 
    liste <- list(br(),br(),br(),br(),br(),br(),
                  h3("Aucune données disponible à ces dates ", align = "center"))
  } else {
    liste <- list(
      h5("Aperçu du fichier: "),
      dataTableOutput("tableau.n")) 
  }
  return(liste)
  
})


output$download_data.n <- downloadHandler(
  filename = function() {paste('data-', Sys.Date(), '.csv', sep='')},
  content = function(file) { 
    aux <- contenu.n()
    write.table(aux, file,sep = ";",col.names = T, dec = ".",
                qmethod ="double",row.names = T)
  }
)



output$data_plot1 <- renderDygraph({
  
  if(is.null(contenu.serie.n())) return(NULL)
  
  else{
    graphique <-  dygraph(contenu.serie.n() ) %>%
      dyRangeSelector()%>% 
      dyOptions(colors = RColorBrewer::brewer.pal(5, "Set1"), labelsUTC = T) %>% 
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE) 
    return(graphique)
  }
})


# contenu.serie.w <- reactive({
#   
# 	data <- xts(contenu.w(),order.by = contenu.w()[,1])[,1:ncol(contenu.w())]
# 
#   return(data)
# })
# 
# output$tableau.i <- renderDataTable({
#   inFile <- info()
#   if (is.null(inFile)){ h5("En attente de données ...", align = "center")}
#   else{
#     DT::datatable(inFile,rownames = F, style = 'bootstrap',
#                   options = list(
#                     lengthMenu = list(c(3,5,10), c('3', '5', '10')),
#                     pageLength = 3,
#                     language =
#                       list(url = '//cdn.datatables.net/plug-ins/f2c75b7247b/i18n/French.json')))
#     
#   }
#   return(inFile)
#   
# })
# 
# output$mainPanel_collecter.w <- renderUI({
#   inFile <- contenu.w()
#   liste <- NULL
#   
#   if (is.null(inFile)) { 
#     liste <- list(br(),br(),br(),br(),br(),br(),
#                   h3("Aucune données disponible à ces dates ", align = "center"))
#   } else {
#     liste <- list(
#       h5("Aperçu du fichier: Waveform "),
#       dataTableOutput("tableau.w"))
#   }
#   return(liste)
#   
# })


# output$download_data.w <- downloadHandler(
#   filename = function() {paste('data-', Sys.Date(), '.csv', sep='')},
#   content = function(file) { 
#     aux <- contenu.w()
#     write.table(aux, file,sep = ";",col.names = T, dec = ".",
#                 qmethod ="double",row.names = T)
#   }
# )


