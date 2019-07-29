# contenu_visual.n <- reactive({
#   
#   patient  <- input$visual_idn
#   data     <- subset(df_long.n, id %in% patient )
#   info     <- subset(informations, id %in% patient )
#   abpmean  <- xts(data,order.by = data[,2] )[,6]
#   
#   data$time_and_date <- as.POSIXct(strptime(data$time_and_date,
#                                             "%Y-%m-%d %H:%M:%S"))
#   premier.point      <- as.POSIXlt(data$time_and_date)[1]
#   dernier.point      <- as.POSIXlt(data$time_and_date)[nrow(data)]
#   difference         <- difftime(dernier.point, premier.point,units="hours")
#   nombre.periode     <- as.numeric(difference) * 60 / 90
#   timebreak          <- premier.point + (seq(0,nombre.periode) * 90 * 60)
#   data$periode       <- cut(data$time_and_date, breaks = timebreak, 
#                             labels = 1 : nombre.periode)
#   # periode et 1er time_and_date 
#   res = NULL
#   res <- subset(data, 
#                 subset = !duplicated(data[("periode")]),
#                 select = c("periode", "time_and_date")) %>% 
#     mutate(
#       periode = as.numeric(periode),
#       time_and_date_decale = c(time_and_date[-1],time_and_date[length(time_and_date)]+5400)
#     )
#   
#   final = inner_join(res, info, "periode")
#   
#   
#   return(final)
# })
# 
# abpmean <- reactive({
#   
#   patient  <- input$visual_idn
#   data     <- subset(df_long.n, id %in% patient )
#   info     <- subset(informations, id %in% patient )
#   abpmean  <- xts(data,order.by = data[,2] )[,6]
#   
#   return(abpmean)
# })

output$visualise_plot1 <- renderDygraph({
  
  if(is.null(contenu.serie.n())) return(NULL)
  
  else{
    patient  <- input$visual_idn
    data     <- subset(df_long.n, id %in% patient )
    info     <- subset(informations, id %in% patient )
    abpmean  <- xts(data,order.by = data[,2] )[,6]
    
    data$time_and_date <- as.POSIXct(strptime(data$time_and_date,
                                              "%Y-%m-%d %H:%M:%S"))
    premier.point      <- as.POSIXlt(data$time_and_date)[1]
    dernier.point      <- as.POSIXlt(data$time_and_date)[nrow(data)]
    difference         <- difftime(dernier.point, premier.point,units="hours")
    nombre.periode     <- as.numeric(difference) * 60 / 90
    timebreak          <- premier.point + (seq(0,nombre.periode) * 90 * 60)
    data$periode       <- cut(data$time_and_date, breaks = timebreak, 
                              labels = 1 : nombre.periode)
    # periode et 1er time_and_date 
    res = NULL
    res <- subset(data, 
                  subset = !duplicated(data[("periode")]),
                  select = c("periode", "time_and_date")) %>% 
      mutate(
        periode = as.numeric(periode),
        time_and_date_decale = c(time_and_date[-1],time_and_date[length(time_and_date)]+5400)
      )
    
    final = inner_join(res, info, "periode")
    
    q <- dygraph(abpmean,xlab = "Time",
                 ylab = "Mean blood pressure (mmHg)") %>%
      dyLimit(limit = 65,label = "Threshold = 65 mmHg",color = "red") %>%
      dyRangeSelector()%>% 
      dyOptions(labelsUTC = F,colors = "#009F98") %>%
      dyLegend(show="always", hideOnMouseOut = T) %>%
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = T)
    
    for ( i in 1: length(final$time_and_date)){
      q <-  dyShading(q,from = final$time_and_date[i],
                      to = final$time_and_date_decale[i] , 
                      axis = "x",
                      color= ifelse(final$event[i] == 0, "white", "#009F98"))
      if (final$event[i] != 0){
        q <- dyAnnotation(q, (final$time_and_date[i]+2700), 
                          text = "AHE",tooltip = "Vietnam",
                          width = 50, height = 30,tickHeight= 100)
      }
      
    }
    return(q)
  }
})
# 
# output$visualise_gplot <- renderPlot({
#   patient  <- input$visual_idn
#   data     <- subset(df_long.n, id %in% patient )
#   info     <- subset(informations, id %in% patient )
#   abpmean  <- xts(data,order.by = data[,2] )[,6]
#   
#   info <- info
#   info$couleur <- ifelse(info$event == 1,100,5)
#   info$event <- as.factor(info$event)
#   
#   p <- ggplot(info, aes(x = periode, y = couleur, fill = event)) + theme_bw() +
#     geom_bar(stat = "identity") + 
#     scale_color_manual(values=c("#9999CC", "#009F98"))+
#     guides(fill = F) + ylab("AHE ( Yes / No) ") +
#     xlab("Periods") +
#     scale_x_continuous(breaks = info$periode )+
#     scale_y_continuous(breaks =c(5,100) , labels = c("Yes", "No"))+
#     ggtitle("AHE per 90 min period")
#   return(p)
# })

# output$visualise_plot2 <- renderDygraph({
#   
#   if(is.null(contenu.serie.w())) return(NULL)
#   
#   else{
#     dygraph(contenu.serie.w()[,input$visualise_variable.w],xlab = "Time" )%>%
#       dyRangeSelector()%>% dyOptions(labelsUTC = T,colors = "#808080") %>% 
#       dyLegend(show="always",hideOnMouseOut = F) %>% 
#       dyHighlight(highlightCircleSize = 5, 
#                   highlightSeriesBackgroundAlpha = 0.2,
#                   hideOnMouseOut = FALSE)
#     
#   }
# })
# 
# fake <- reactive({
# 
#   aux <- contenu()
#   aux <- aux[colSums(!is.na(aux)) > 0]
# 
#   #   aux <- donnees
#   #   input = list ()
#   #   input$visualise_variable <- "FR"
#   #   input$visualise_variable2 <- "FC.x"
#   #   input$visualise_variable3 <- "SpO2.x"
# 
#   var1 <- input$visualise_variable
#   var2 <- input$visualise_variable_2
#   var3 <- input$visualise_variable_3
# 
#   date_heure <- paste0(aux$DATE," ",aux$HEURE)
#   datetimesseries <- as.POSIXct(as.character(date_heure), format="%d/%m/%Y %H:%M:%S",tz ="Europe/Paris")
# 
#   serie_fake1 <- na.omit(data.frame(
#     value = aux[,var1],
#     temps = datetimesseries))
# 
#   serie_fake2 <- na.omit(data.frame(
#     value = aux[,var2],
#     temps = datetimesseries))
# 
#   serie_fake3 <- na.omit(data.frame(
#     value = aux[,var3],
#     temps = datetimesseries))
# 
#   titre1 = paste0("Série temporelle : ", var1)
#   p1 <- ggplot(serie_fake1, aes(x = temps, y = value)) + geom_line() + theme_classic()+
#     ylab(label = var1) + ggtitle(titre1)
# 
#   titre2 = paste0("Série temporelle : ", var2)
#   p2 <- ggplot(serie_fake2, aes(x = temps, y = value)) + geom_line() + theme_classic()+
#     ylab(label = var2) +
#     ggtitle(titre2)
# 
#   titre3 = paste0("Série temporelle : ", var3)
#   p3 <- ggplot(serie_fake3, aes(x = temps, y = value)) + geom_line() + theme_classic()+
#     ylab(label = var3) +
#     ggtitle(titre3)
# 
#   a <- grid.arrange(p1,p2,p3)
# 
#   return(a)
# 
# })

# condition si champs pas renseigner
output$visualise_downloadPlot <- downloadHandler(
  
  filename = function() { paste(input$nom,"_",input$prenom,"_",input$nip, '.png', sep='') },
  content = function(file) {
    if(is.null(input$nom) | is.null(input$prenom) | is.null(input$nip)) { 
      return()
    }else{
      ggsave(file, plot = fake(), device = "png",width = 20  , height = 20, limitsize = F)}
  }
)



