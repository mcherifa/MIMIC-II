#######################################################
# Patients characteristics
# 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("/home/menyssa/Mimic/scripts/R/toolbox/les_sources.R")

df <- readRDS(paste0(dest_data,"fichier_long_periode.rds"))  %>% 
  subset(age < 100 & eevent == 0) 

n = length(unique(df$id))
df$event <- as.factor(df$event)

#######################################
# Lecture 
#######################################

############# Binary all

binary <- list()

# Sexe
temp <- unique(df[,c("id","gender")]) 
binary [[1]] <- rbind(table(temp$gender),
                      table(temp$gender) / n * 100)

# Au moins 1 fois durant le séjour : 
# Amine 
df$num <- as.numeric(as.character(df$amine))
num <- aggregate(num ~ id, data = df, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary [[2]] <- rbind( table(num_pat), 
                       table(num_pat) / n * 100)

# Curare 
df$num <- as.numeric(as.character(df$curare))
num <- aggregate(num ~ id, data = df, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary [[3]] <- rbind( table(num_pat), 
                       table(num_pat) / n * 100)

# Sédate 
df$num <- as.numeric(as.character(df$sedate))
num <- aggregate(num ~ id, data = df, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary [[4]] <- rbind( table(num_pat), 
                       table(num_pat) / n * 100)

# Ventilation
df$num <- as.numeric(as.character(df$venti))
num <- aggregate(num ~ id, data = df, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary [[5]] <- rbind( table(num_pat), 
                       table(num_pat) / n * 100)

names(binary) <- c("sexe","amine","curare","sedate","ventilation")

############# Binary / Event == 1

df_1 <- df[df$event =="1",]
binary_event_1 <- list()
n_event = length(unique(df_1$id))

# Sexe
temp <- unique(df_1[,c("id","gender")]) 
binary_event_1 [[1]] <- rbind(table(temp$gender),
                              table(temp$gender) / n_event * 100)

# Au moins 1 fois durant le séjour : 

# Amine 
df_1$num <- as.numeric(as.character(df_1$amine))
num <- aggregate(num ~ id, data = df_1, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary_event_1 [[2]] <- rbind( table(num_pat),
                               table(num_pat) / n_event * 100)

# Curare 
df_1$num <- as.numeric(as.character(df_1$curare))
num <- aggregate(num ~ id, data = df_1, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary_event_1 [[3]] <- rbind( table(num_pat),
                               table(num_pat) / n_event * 100)

# Sédate 
df_1$num <- as.numeric(as.character(df_1$sedate))
num <- aggregate(num ~ id, data = df_1, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary_event_1 [[4]] <- rbind( table(num_pat),
                               table(num_pat) / n_event * 100)

# Ventilation
df_1$num <- as.numeric(as.character(df_1$venti))
num <- aggregate(num ~ id, data = df_1, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary_event_1 [[5]] <- rbind( table(num_pat),
                               table(num_pat) / n_event * 100)

names(binary_event_1) <- c("sexe","amine","curare","sedate","ventilation")

############# Binary / Event == 0

df_0 <- df[df$event == "0",]

df_0 <- subset(df_0, !(id %in% n_event))

binary_event_0  <- list()

n_event_0 = length(unique(df_0$id))


# Sexe
temp <- unique(df_0[,c("id","gender")]) 
binary_event_0 [[1]] <- rbind(table(temp$gender),
                              table(temp$gender) / n_event_0 * 100)

# Au moins 1 fois durant le séjour : 

# Amine 
df_0$num <- as.numeric(as.character(df_0$amine))
num <- aggregate(num ~ id, data = df_0, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary_event_0 [[2]] <- rbind( table(num_pat),
                               table(num_pat) / n_event_0 * 100)

# Curare 
df_0$num <- as.numeric(as.character(df_0$curare))
num <- aggregate(num ~ id, data = df_0, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary_event_0 [[3]] <- rbind( table(num_pat),
                               table(num_pat) / n_event_0 * 100)

# Sédate 
df_0$num <- as.numeric(as.character(df_0$sedate))
num <- aggregate(num ~ id, data = df_0, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary_event_0 [[4]] <- rbind( table(num_pat),
                               table(num_pat) / n_event_0 * 100)

# Ventilation
df_0$num <- as.numeric(as.character(df_0$venti))
num <- aggregate(num ~ id, data = df_0, sum)
num_pat <- ifelse(num$num != 0, 1, 0)
binary_event_0 [[5]] <- rbind( table(num_pat),
                               table(num_pat) / n_event_0 * 100)

names(binary_event_0) <- c("sexe","amine","curare","sedate","ventilation")

############# Continue

RES <- NULL 
RES <-  descr(x="age", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1,test="wilcoxon")                                
RES <-  descr(x="sapsi_first", y = NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="sofa_first", y = NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="hr", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="abpsys", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="abpdias", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="abpmean", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES <-  descr(x="spo2", y=NULL, dat=df, RES, type="c", categ=1, desc="med", prec=1, test="wilcoxon")

RES1 <- NULL 
RES1 <-  descr(x="age", y="event", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="sapsi_first", y = "event", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="sofa_first", y = "event", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="hr", y="event", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="abpsys", y="event", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="abpdias", y="event", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="abpmean", y="event", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")
RES1 <-  descr(x="spo2", y="event", dat=df, RES1, type="c", categ=1, desc="med", prec=1, test="wilcoxon")


#######################################################
#-------- Description des périodes des patients 
#######################################################

blancheur <- theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none",
  panel.background = element_rect(fill = "white",
                                  colour = "white"),
  axis.line = element_line(colour = "black")
)

recherche_episode <- df
periode_par_patient <- as.data.frame(table(recherche_episode$id))
periode_par_patient$breaks = cut(periode_par_patient$Freq, 
                                 breaks = c(seq(0,100,10),120,150,200,400),
                                 include.lowest=TRUE)
a <- ggplot(data = subset(periode_par_patient, !is.na(breaks)),aes(x = breaks, fill = "red")) + 
  geom_bar() + theme_classic() + guides(fill=FALSE) +
  geom_text(stat='count',aes(label=..count..),vjust=-1) +
  labs(x = "Nombre de périodes de 90 min ",y = "Nombre de patients")


#######################################################
#-------- Plot boxplot des numerics 
#######################################################

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
  ylab("SPO2 (%)") + ylim(80,100)
  scale_x_discrete(labels  =  c("No AHE", "AHE")) +
  labs(x  =  " ")

d <- ggplot(df, aes(x = event, y = abpmean, fill = event)) +
  geom_boxplot(outlier.shape  =  NA) + 
  scale_fill_manual(values = c("#606060", "#C0C0C0")) + blancheur +
  ylab("Mean Blood Pressure\n (mmHg)")  + ylim(40,140) +
  scale_x_discrete(labels  =  c("No AHE", "AHE")) +
  labs(x  =  " ")

e <- ggplot(df, aes(x = event, y = abpdias, fill = event)) +
  geom_boxplot(outlier.shape  =  NA) + 
  scale_fill_manual(values = c("#606060", "#C0C0C0")) + blancheur +
  ylab("Diastolic Blood Pressure \n (mmHg)") + ylim(30,100) +
  scale_x_discrete(labels  =  c("No AHE", "AHE")) +
  labs(x  =  " ")

f <- ggplot(df, aes(x = event, y = abpsys, fill = event)) +
  geom_boxplot(outlier.shape  =  NA) + 
  scale_fill_manual(values = c("#606060", "#C0C0C0")) + blancheur +
  ylab("Systolic Blood  \n Pressure (mmHg)")  + ylim(50,170) +
  scale_x_discrete(labels  =  c("No AHE", "AHE")) +
  labs(x  =  " ")

representation <- grid.arrange(b, c, d, e, f, ncol = 2, nrow = 3)

#######################################################
#-------- Résultats 
#######################################################

# Table

RES <- as.data.frame(cbind(RES,RES1[,-1]))
colnames(RES) <- c("Variables","All patients","No event","Event","P_value")
RES$Variables <- toupper(RES$Variables)
RES$P_value <- as.numeric(as.character(RES$P_value))
RES$P_value <- ifelse( RES$P_value < 0.001, "< 0.001",RES$P_value)
write.table(RES,paste0(path_result,"caracteristiques_patients_continue.csv"),sep="\t",row.names=F,dec = ".")

binaire  <- data.frame(all = unlist(binary))
  # no_event = unlist(binary_event_0),
  # event = unlist(binary_event_1))

write.table(binaire,paste0(path_result,"caracteristiques_patients_facteurs.csv"),sep="\t",row.names=T,dec = ".")

# Graphique

ggsave(paste0(path_result,"Images/Nombre_periode_patient.pdf"),a,
       width = 10, height = 10)

ggsave(paste0(path_result,"Images/Numerics_ahe.pdf"), representation,
       width = 8.27, height = 12)

