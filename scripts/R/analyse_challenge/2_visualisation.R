##############################
# Analyse factorielle
##############################

library(FactoMineR)
library(factoextra)

load("data_test.RData")
load("data_train.RData")

####
#### Training 
####

# ACP
donnees_acp <- apply(data_train[,5:49], 2, scale)
res.pca <- PCA(donnees_acp, 
               scale.unit = TRUE,
               ncp = 5, graph = TRUE)
# Acute Hypotensive Episode
outcome <- factor(ifelse(data_train$outcome== "H1" | data_train$outcome == "H2",1,0), 
                     levels = c(0,1),
                     labels = c("No_AHE","AHE"))

pdf("outcome_train.pdf")
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = outcome , # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07","blue"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups")
dev.off()

# Sexe
# sex <- as.factor(data_train$sex)
# 
# pdf("sex.pdf")
# fviz_pca_ind(res.pca,
#              geom.ind = "point", # show points only (nbut not "text")
#              col.ind = sex , # color by groups
#              palette = c("#00AFBB", "#E7B800", "#FC4E07","blue"),
#              addEllipses = FALSE, # Concentration ellipses
#              legend.title = "Groups")
# dev.off()


####
#### Test
####

# ACP
donnees_acp <- apply(data_test[,3:47], 2, scale)
res.pca <- PCA(donnees_acp, 
               scale.unit = TRUE,
               ncp = 5, graph = TRUE)

# Acute Hypotensive Episode
outcome <- factor(ifelse(data_test$outcome== "H",1,0), 
       levels = c(0,1),
       labels = c("No_AHE","AHE"))

pdf("outcome_test.pdf")
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = outcome , # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07","blue"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups")
dev.off()






