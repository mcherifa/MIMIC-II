#######################################################
# Script pour les graphiques 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/afroinshape/AHE.git
#######################################################


source("/home/menyssa/Recherche/Mimic-II/scripts/R/toolbox/les_sources.R")

dest_data <- "~/Recherche/Mimic-II/data/clean/mimic2/"

library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)

#######################################
# DATA
#######################################

df_long <- readRDS(paste0(dest_data,"numerics.rds")) 
df_wide <- readRDS(paste0(dest_data,"fichier_wide_periode.rds")) 
df <- readRDS(paste0(dest_data,"fichier_long_periode.rds")) 


####
# Sous data 
###

temp = subset(df, select = c(id,event, abpsys , abpmean, abpdias))
melt.temp = reshape::melt(temp, id= c("id","event"))

####
# Theme
###

blancheur <- theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.title = element_blank(),
  legend.position="bottom",
  legend.background = element_rect(fill="gainsboro",
                                   size=0.5, 
                                   linetype="solid", 
                                   colour ="black"),
  # legend.position = "none",
  panel.background = element_rect(fill = "white",
                                  colour = "white"),
  axis.line = element_line(colour = "black")
)

blancheur_sans_legend <- theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none",
  panel.background = element_rect(fill = "white",
                                  colour = "white"),
  axis.line = element_line(colour = "black")
)

####
# Boxplot pour SAP MAP DAP en fonction AHE par période
###


c <- ggplot(melt.temp, aes(x=variable, y=value, fill=as.factor(event))) +
  geom_boxplot(outlier.shape = NA) + 
  blancheur + ylim(30,200) + 
  ylab("Arterial Pressure ( mmHg )") +
  scale_x_discrete(labels = c("Systolic Arterial Pressure", 
                              "Mean Arterial Pressure",
                              "Diastolic Arterial Pressure")) +
  xlab(" ") +
  scale_fill_manual(values=c("coral3", "grey"), 
                    name=" ",
                    breaks=c("0", "1"),
                    labels=c("Periods wihout AHE", "Periods with AHE"))
c

####
# plot ts pour tous les numerics de deux patients 
####
# 
# sensiplot <- ggplot(mdata, aes(x = seuil_prediction, y = value,colour=id))+
#   geom_line(aes(linetype=variable, color=type))+blancheur +
#   labs(title=" ", y="Value", x="Thresholds")+
#   scale_x_continuous(  expand = c(0, 0),limits=c(0,0.5)) +
#   scale_y_continuous(  expand = c(0, 0),limits=c(0,1))+
#   scale_color_manual(name="Preprocessing Methods", 
#                      labels = c("ARMA", "Haar", "Linear", "Statistical measures"), 
#                      values = c("ARMA"="black", "Haar"="grey", "Linear"="orange","Statistical measures"="red"))+
#   scale_linetype_discrete(name = "Measures", labels = c("Sensitivity", "Specificity"))


type_dash = c("solid", "dotted","dashed","dotdash","longdash")

patient = 10013
lol = as.POSIXct(strptime("2564-11-02 10:30:00", "%Y-%m-%d %H:%M:%S"))
seriea = subset(df_long, id %in% patient)
seriea = subset(seriea, time_and_date <="2564-11-02 11:57")
melt.serie = reshape::melt(data = seriea ,id=c("id", "time_and_date"))
melt.serie = subset(melt.serie, value > 0)

a <- ggplot(melt.serie, aes(x = time_and_date, y = value)) + 
  geom_line(aes(linetype=variable)) + 
  scale_x_datetime(expand = c(0, 0),
                  labels = date_format("%d-%m-%Y \n %H:%M:%S"), 
                   breaks = date_breaks("15 min")) +
  blancheur + xlab(" ") +
  scale_y_continuous(limits = c(0,250),
                     "Patient: 10013")+
                     # sec.axis = dup_axis( name = "bpm")) +
  scale_linetype_manual(values = type_dash,
                        breaks = c("hr", "spo2","abpsys", "abpdias","abpmean"),
                        labels = c("Heart Rate","Pulse oximetry","Systolic Arterial Pressure",
                                   "Diastolic Arterial Pressure", "Mean Arterial Pressure")) +
  ggplot2::annotate("rect", xmin = seriea[1,"time_and_date"], xmax = seriea[61,"time_and_date"],
                    ymin = 0, ymax = 205,alpha = .2) + 
  ggplot2::annotate(geom= "text", x = seriea[30,"time_and_date"], y = 170,
                    label = "Observation ", angle = 0,size = 5) +
  ggplot2::annotate("rect", xmin = seriea[71,"time_and_date"], xmax = seriea[91,"time_and_date"],
                    ymin = 0, ymax = 205, alpha = .2) + 
  ggplot2::annotate(geom= "text", x = seriea[80,"time_and_date"], y = 170,
                    label = "Prediction ", angle = 0,size = 5) +
  ggplot2::annotate(geom= "text", x = seriea[66,"time_and_date"], y = 170,
                    label = "Gap ", angle = 0,size = 5) 
  
patient = 10086
lol = as.POSIXct(strptime("2925-02-14 20:30:24", "%Y-%m-%d %H:%M:%S"))
serieb = subset(df_long, id %in% patient)
serieb = subset(serieb, time_and_date <= "2925-02-14 21:55:24")
melt.serie = reshape::melt(data = serieb,id=c("id", "time_and_date"))
melt.serie = subset(melt.serie, value > 0)

b <- ggplot(melt.serie, aes(x = time_and_date, y = value)) + 
  geom_line(aes(linetype=variable)) +
  blancheur_sans_legend +
  scale_x_datetime(expand = c(0, 0), labels = date_format("%d-%m-%Y \n %H:%M:%S"), breaks = date_breaks("15 min")) +
  #geom_hline(yintercept=65, linetype="dashed", color = "red", size=0.2) +
  xlab("Times (min)") +
  scale_y_continuous(limits = c(0,250),
                     "Patient: 10086") +
                     # sec.axis = dup_axis( name = "bpm")) +
  scale_linetype_manual(values = type_dash,
                        breaks = c("hr", "abpsys", "abpdias","abpmean"),
                        labels = c("Heart Rate", "Systolic Arterial Pressure",
                                   "Diastolic Arterial Pressure", "Mean Arterial Pressure")) +
  # ggplot2::annotate("segment", x =  serieb[61,"time_and_date"], xend = serieb[61,"time_and_date"],
  #                   y = 0, yend = 250,alpha = 0.8) + 
  # ggplot2::annotate("segment", x = serieb[71,"time_and_date"] , xend = serieb[71,"time_and_date"],
  #                   y = 0, yend = 250, alpha = .8) + 
  
  ggplot2::annotate("rect", xmin = serieb[1,"time_and_date"], xmax = serieb[61,"time_and_date"],
                    ymin = 0, ymax = 205,alpha = .2) + 
  ggplot2::annotate("rect", xmin = serieb[71,"time_and_date"], xmax = serieb[91,"time_and_date"],
                    ymin = 0, ymax = 205, alpha = .2) + 
  ggplot2::annotate(geom= "text", x = serieb[30,"time_and_date"], y = 170,
                    label = "Observation ", angle = 0,size = 5) +
  ggplot2::annotate(geom= "text", x = serieb[66,"time_and_date"], y = 170,
                    label = "Gap ", angle = 0,size = 5) +
  ggplot2::annotate(geom= "text", x = serieb[80,"time_and_date"], y = 170,
                    label = "Prediction ", angle = 0, size = 5) 

cairo_ps(file = "test.tif",  width = 10, height = 5,
         onefile = TRUE, fallback_resolution = 300)
ml <- grid.arrange(a, b, ncol=1)
dev.off()


# dev.copy2eps(ml)
# 
# x11() # on a Unix-alike
# ml <- grid.arrange(a, b, ncol=1)
# dev.copy(device = x11)
# #mtext("Copy 1", 3)
# dev.print(width = 6, height = 6, horizontal = FALSE) # prints it
# dev.off(dev.prev())
# dev.off()
# ggsave(file= "numerics.pdf",plot = ml)

# geom_hline(yintercept=65, linetype="dashed", color = "red", size=0.2,show.legend = "Th") +
# ggplot2::annotate(geom= "text", x = lol, y = 200, label = "Threshold \n for MAP", angle = 0,size = 2.5) +
# ggplot2::annotate(geom= "text", x = lol + 2, y = 195, label = "------", color = "red", angle = 0,size = 6) +
# ggplot2::annotate("rect", xmin = lol - 200, xmax = lol + 200, ymin = 190, ymax = 205,alpha = .2)

####
# Description des périodes des patients
####

recherche_episode <- df

periode_par_patient <- as.data.frame(table(recherche_episode$id))
periode_par_patient$breaks = cut(periode_par_patient$Freq, 
                                 breaks = c(seq(0,100,10),120,150,200,400),
                                 include.lowest=TRUE,
                                 right= TRUE)
# periode_par_patient$breaks <- as.factor(gsub(",", "-",periode_par_patient$breaks, perl=TRUE))         

a <- ggplot(data = subset(periode_par_patient, !is.na(breaks)),aes(x = breaks)) + 
  geom_bar(fill = "black") + 
  blancheur +ylim(0,300)+
  guides(fill=FALSE) +
  geom_text(stat='count',aes(label=..count..),vjust=-1) +
  labs(x = "90 minutes periods",y = "Numbers of patients")

ggsave("numbers_of_periods", plot = a, device = pdf)


#####
# Plot boxplot des numerics 
#####

# Visualize

b <- ggplot(df, aes(x  =  event, y = hr, fill = event)) +
  geom_boxplot(outlier.shape  =  NA) + 
  scale_fill_manual(values = c("#606060", "#C0C0C0")) + blancheur +
  ylab("Heart Rate ( bpm )")  + 
  scale_x_discrete(labels  =  c("No AHE", "AHE")) +
  labs(x  =  " ")

c <- ggplot(df, aes(x = event, y = spo2, fill = event)) +
  geom_boxplot(outlier.shape  =  NA) + 
  scale_fill_manual(values = c("#606060", "#C0C0C0")) + blancheur +
  ylab("SPO2 (%)")  + 
  scale_x_discrete(labels  =  c("No AHE", "AHE")) +
  labs(x  =  " ")


d <- ggplot(df, aes(x = event, y = abpmean, fill = event)) +
  geom_boxplot(outlier.shape  =  NA) + 
  scale_fill_manual(values = c("#606060", "#C0C0C0")) + blancheur +
  ylab("Mean Arterial Pressure\n (mmHg)")  + ylim(40,140) +
  scale_x_discrete(labels  =  c("No AHE", "AHE")) +
  labs(x  =  " ")

e <- ggplot(df, aes(x = event, y = abpdias, fill = event)) +
  geom_boxplot(outlier.shape  =  NA) + 
  scale_fill_manual(values = c("#606060", "#C0C0C0")) + blancheur +
  ylab("Diastolic Arterial Pressure \n (mmHg)") + ylim(30,100) +
  scale_x_discrete(labels  =  c("No AHE", "AHE")) +
  labs(x  =  " ")

f <- ggplot(df, aes(x = event, y = abpsys, fill = event)) +
  geom_boxplot(outlier.shape  =  NA) + 
  scale_fill_manual(values = c("#606060", "#C0C0C0")) + blancheur +
  ylab("Systolic Arterial  \n Pressure (mmHg)")  + ylim(50,170) +
  scale_x_discrete(labels  =  c("No AHE", "AHE")) +
  labs(x  =  " ")

representation <- grid.arrange(b, c, d, e, f, ncol = 2, nrow = 3)


