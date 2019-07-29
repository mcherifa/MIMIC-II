output$dashboard_box1 <- renderUI({
  r = length(unique(informations$id))
  return(html_tpl_dashboard_box(paste0(r,""),fat_text='Numbers of patients',
                                'fa fa-hospital-o','steelblue1',"white"))

})

output$dashboard_box2 <- renderUI({
  a = 571
  return(html_tpl_dashboard_box(paste0(a,""),fat_text = "Patients with at least one AHE",
                                icon = 'fa fa-stethoscope',bg_color = 'steelblue1'))
})

output$tbt <- renderDataTable({
  inFile <- carac
  if (is.null(inFile)){ h5("En attente de données ...", align = "center")}
  else{
    DT::datatable(inFile,rownames = F, style = 'bootstrap')
                  # 
                  # options = list(
                  #   lengthMenu = list(c(3,5,10), c('3', '5', '10')),
                  #   pageLength = 3,
                  #   language =
                  #     list(url = '//cdn.datatables.net/plug-ins/f2c75b7247b/i18n/French.json')))
    
  }
  return(inFile)
  
})

output$mytable <- renderUI({
  inFile <- carac
  liste <- NULL
  
  if (is.null(inFile)) { 
    liste <- list(br(),br(),br(),br(),br(),br(),
                  h3("Aucune données disponible à ces dates ", align = "center"))
  } else {
    liste <- list(
      h5("Aperçu du fichier: "),
      dataTableOutput("tbt")) 
  }
  return(liste)
  
})

output$tbt1 <- renderDataTable({
  inFile <- carac1
  if (is.null(inFile)){ h5("En attente de données ...", align = "center")}
  else{
    DT::datatable(inFile,rownames = F, style = 'bootstrap')
                  # ,
                  # options = list(
                  #   lengthMenu = list(c(3,5,10), c('3', '5', '10')),
                  #   pageLength = 3,
                  #   language =
                  #     list(url = '//cdn.datatables.net/plug-ins/f2c75b7247b/i18n/French.json')))
                  # 
  }
  return(inFile)
  
})

output$mytable1 <- renderUI({
  inFile <- carac1
  liste <- NULL
  
  if (is.null(inFile)) { 
    liste <- list(br(),br(),br(),br(),br(),br(),
                  h3("Aucune données disponible à ces dates ", align = "center"))
  } else {
    liste <- list(
      h5("Aperçu du fichier: "),
      dataTableOutput("tbt1")) 
  }
  return(liste)
  
})













