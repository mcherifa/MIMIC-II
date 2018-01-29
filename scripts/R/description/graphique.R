#######################################################
# Script combine all non parametric models for numerics 
#                 series and AHE event 
# 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/afroinshape/AHE.git
#######################################################


# setwd("~/Desktop/Mimic/resultats/")

# Path scripts
dest_r <- "/home/mcherifa/Mimic/scripts/R/toolbox/"
# dest_r <- "~/Desktop/Mimic/scripts/R/toolbox/"

# Path data 
dest_data <- "/home/mcherifa/Mimic/data/clean/"
# dest_data <- "~/Desktop/Mimic/data/"

source(paste0(dest_r,"packages.R"))
source(paste0(dest_r,"fonctions.R"))
source(paste0(dest_r,"models.R"))
source(paste0(dest_r,"cross_validation.R"))


#######################################
# DATA
#######################################

df_wide_resume 	<- readRDS(paste0(dest_data,"df_resume.rds"))
df_wide_hw 			<- readRDS(paste0(dest_data,"df_hw.rds"))
df_wide_fourier <- readRDS(paste0(dest_data,"df_fourier.rds"))
df_wide_haar 		<- readRDS(paste0(dest_data,"df_haar.rds"))
df_wide 				<- readRDS(paste0(dest_data,"fichier_wide_periode.rds"))

#######################################
# BOXPLOT POUR LES CO VARIABLES RESUME
#######################################

# THEME 
blancheur <- theme(
					panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
					legend.position = "none",
					panel.background = element_rect(fill = "white",
                                					colour = "white"),
          axis.line = element_line(colour = "black")
          )
                  
a <- ggplot(df_wide_resume, aes(x=event, y=m_hr, fill=event)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c("#606060", "#C0C0C0")) + blancheur +
  ylab("Heart Rate ( bpm )")  + 
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = " ")
  # xlab("Acute Hypotensif Episode ")

b <- ggplot(df_wide_resume, aes(x=event, y=m_sp, fill=event)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c("#606060", "#C0C0C0")) + 
 	blancheur +
  ylim(85,100) + 
  ylab("Spo2 ( % )") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = " ")
  # xlab("Acute Hypotensif Episode ") +

c <- ggplot(df_wide_resume, aes(x=event, y=m_dias, fill=event)) +
	geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c("#606060", "#C0C0C0")) + 
	blancheur + ylim(40,85) + 
  ylab("Diastolic Arterial \n Pressure ( mmHg )") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = " ")
   # xlab("Acute Hypotensif Episode ") +
  
d <- ggplot(df_wide_resume, aes(x=event, y=m_resp, fill=event)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c("#606060", "#C0C0C0")) + 
 	blancheur + ylim(0,40) + 
  ylab(" Respiratory Rate \n ( per minute )") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = " ")
   # xlab("Acute Hypotensif Episode ") +
  
e <- ggplot(df_wide_resume, aes(x=event, y=m_mean, fill=event)) +
 geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c("#606060", "#C0C0C0")) + 
 	blancheur + ylim(70,110) + 
	ylab("Mean Arterial \n Pressure ( mmHg )") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = " ")
   # xlab("Acute Hypotensif Episode ") +
   
f <- ggplot(df_wide_resume, aes(x=event, y=m_sys, fill=event)) +
 geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c("#606060", "#C0C0C0")) + 
  blancheur +  ylim(40,160) + 
  xlab("Acute Hypotensif Episode ") +
  ylab("Systolic Arterial \n Pressure ( mmHg )") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = " ") +
  xlab("Acute Hypotensif Episode ") 
  
g <- ggplot(df_wide_resume, aes(x=event, y=m_ratio, fill=event)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c("#606060", "#C0C0C0")) + 
  blancheur + ylim(0,1.5) + 
  xlab("Acute Hypotensif Episode ") +
  ylab("PAM / HR ratio") + 
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = " ") +
  xlab("Acute Hypotensif Episode ") 

ml <- grid.arrange(a, b, c, d, e, f, g, ncol=2, nrow = 4)
ggsave("/home/mcherifa/Mimic/resultats/boxplot_numerics_resume.pdf", ml)

#######################################
# plot ts pour les numerics 
#######################################

patient = 10013
serie = subset(df_long, id %in% patient)

a <- ggplot(serie, aes(x = time_and_date, y = abpmean)) + 
  geom_line()+   
  scale_x_datetime(labels = date_format("%d-%m-%Y \n %H:%M:%S"), breaks  = date_breaks("5 hours")) +
  geom_hline(yintercept=65, linetype="dashed", color = "red", size=0.5) +
  theme_classic()+ ylim(0,150) + 
  ylab(label = "MAP (mmHg)") +
  xlab("Time (hours)") 
a

patient = 10086
serie = subset(df_long, id %in% patient)
b<- ggplot(serie, aes(x = time_and_date, y = abpmean)) + 
  geom_line()+   
  scale_x_datetime(labels = date_format("%d-%m-%Y \n %H:%M:%S"), breaks  = date_breaks("5 hours")) +
  geom_hline(yintercept=65, linetype="dashed", color = "red", size=0.5) +
  theme_classic()+ ylim(0,150) + 
  ylab(label = "MAP (mmHg)") +
  xlab("Time (hours)") 
b

grid.arrange(a, b, ncol=1)


