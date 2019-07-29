#####################
# Produire les plots AUC sur 
# le training du super learner 
#####################

library("gridExtra")
library("ggplot2")
library("plyr")

load(file = "/home/menyssa/plot_auc.RData")

#####################
# Random Partial Sample
#####################

# On retire sample_mean
fit.plot = a_cv_5$graphique$data[-10,]
fit.plot = fit.plot[order(fit.plot$Ave, decreasing = T),]
# Réécriture
fit.plot$Algorithm <- plyr::mapvalues(fit.plot$Algorithm,"Discrete SL", "Discrete Super Learner")

graphique <- ggplot2::ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
  geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
  geom_point(size = 2.5, shape = 20, fill = "white") +
  xlim(0.5, 1) +
  xlab("CV-AUROC") +
  geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "black", size = 3 ) +
  theme_bw() +
  scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 12)) +
  ggtitle("Haar's wavelets preprocessing")

fit.plot = b_cv_5$graphique$data[-10,]
fit.plot = fit.plot[order(fit.plot$Ave, decreasing = T),]
fit.plot$Algorithm <- plyr::mapvalues(fit.plot$Algorithm,"Discrete SL", "Discrete Super Learner")

graphique1 <- ggplot2::ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
  geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
  geom_point(size = 2.5, shape = 20, fill = "white") +
  xlim(0.5, 1) +
  xlab("CV-AUROC") +
  geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "black", size = 3 ) +
  theme_bw() +
  scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 12)) +
  ggtitle("ARMA model preprocessing")


fit.plot = c_cv_5$graphique$data[-10,]
fit.plot = fit.plot[order(fit.plot$Ave, decreasing = T),]
fit.plot$Algorithm <- plyr::mapvalues(fit.plot$Algorithm,"Discrete SL", "Discrete Super Learner")

graphique2 <- ggplot2::ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
  geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
  geom_point(size = 2.5, shape = 20, fill = "white") +
  xlim(0.5, 1) +
  xlab("CV-AUROC") +
  geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "black", size = 3 ) +
  theme_bw() +
  scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 12)) +
  ggtitle("Linear regression preprocessing")


fit.plot = d_cv_5$graphique$data[-10,]
fit.plot = fit.plot[order(fit.plot$Ave, decreasing = T),]
fit.plot$Algorithm <- plyr::mapvalues(fit.plot$Algorithm,"Discrete SL", "Discrete Super Learner")

graphique3 <- ggplot2::ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
  geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
  geom_point(size = 2.5, shape = 20, fill = "white") +
  xlim(0.5, 1) +
  xlab("CV-AUROC") +
  geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "black", size = 3 ) +
  theme_bw() +
  scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 12)) +
  ggtitle("Statistical moments preprocessing")

ml_5 <- grid.arrange(graphique,graphique1,graphique2,graphique3, ncol=2, nrow=2)

tmp <-  cbind(c(rep("SUM",10),rep("LIN",10),rep("ARMA",10),rep("Haar",10)),
                rbind(d_cv_5$graphique$data,
                c_cv_5$graphique$data,
                b_cv_5$graphique$data,
                a_cv_5$graphique$data))
tmp$auc_ci <- paste0(round(tmp$Ave,3)," (",round(tmp$Min,3), ";",round(tmp$Max,3),")")
write.table(tmp,"cross_validation_random_partial_sample.csv",sep = ",",dec ='.')

#####################
# Full Sample
#####################

# ggsave(filename = "random_sample_auc.pdf", ml_5)

# rm(list = ls())

load(file = "/home/menyssa/full_sample_auc.RData")

fit.plot = 	 a_ma.cv$data
fit.plot = fit.plot[order(fit.plot$Ave, decreasing = T),]
fit.plot$Algorithm <- plyr::mapvalues(fit.plot$Algorithm,"Discrete SL", "Discrete Super Learner")

graphique <- ggplot2::ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
  geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
  geom_point(size = 2.5, shape = 20, fill = "white") +
  xlim(0.5, 1) +
  xlab("CV-AUROC") +
  geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "black", size = 3 ) +
  theme_bw() +
  scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 12)) +
  ggtitle("Haar's wavelets preprocessing")


fit.plot = 	 b_ma.cv$data
fit.plot = fit.plot[order(fit.plot$Ave, decreasing = T),]
fit.plot$Algorithm <- plyr::mapvalues(fit.plot$Algorithm,"Discrete SL", "Discrete Super Learner")

graphique1 <- ggplot2::ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
  geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
  geom_point(size = 2.5, shape = 20, fill = "white") +
  xlim(0.5, 1) +
  xlab("CV-AUROC") +
  geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "black", size = 3 ) +
  theme_bw() +
  scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 12)) +
  ggtitle("ARMA model preprocessing")


fit.plot = 	 c_ma.cv$data
fit.plot = fit.plot[order(fit.plot$Ave, decreasing = T),]
fit.plot$Algorithm <- plyr::mapvalues(fit.plot$Algorithm,"Discrete SL", "Discrete Super Learner")

graphique2 <- ggplot2::ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
  geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
  geom_point(size = 2.5, shape = 20, fill = "white") +
  xlim(0.5, 1) +
  xlab("CV-AUROC") +
  geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "black", size = 3 ) +
  theme_bw() +
  scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 12)) +
  ggtitle("Linear regression preprocessing")


fit.plot = 	 d_ma.cv$data
fit.plot = fit.plot[order(fit.plot$Ave, decreasing = T),]
fit.plot$Algorithm <- plyr::mapvalues(fit.plot$Algorithm,"Discrete SL", "Discrete Super Learner")

graphique3 <- ggplot2::ggplot(data = fit.plot, aes(x = Ave, y = Algorithm, color = Algorithm)) +
  geom_errorbarh(aes(xmin = Min, xmax = Max), colour = "black", height = .1) +
  geom_point(size = 2.5, shape = 20, fill = "white") +
  xlim(0.5, 1) +
  xlab("CV-AUROC") +
  geom_text(aes(label = round(Ave,2), x = Ave), vjust = -.6, hjust = .5, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Min,2), x = Min), vjust = .5, hjust = 1.2, show.legend = FALSE, colour = "black", size = 3 ) +
  geom_text(aes(label = round(Max,2), x = Max), vjust = .5, hjust = -.2, show.legend = FALSE, colour = "black", size = 3 ) +
  theme_bw() +
  scale_colour_hue(guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.title = element_text(face = "bold", size = 12)) +
  ggtitle("Statistical moments preprocessing")

ml_5 <- grid.arrange(graphique,graphique1,graphique2,graphique3, ncol=2, nrow=2)

tmp <-  cbind(c(rep("SUM",5),rep("LIN",5),rep("ARMA",5),rep("Haar",5)),
              rbind(d_ma.cv$data,
                    c_ma.cv$data,
                    b_ma.cv$data,
                    a_ma.cv$data))
tmp$auc_ci <- paste0(round(tmp$Ave,3)," (",round(tmp$Min,3), ";",round(tmp$Max,3),")")
write.table(tmp,"cross_validation_full_sample.csv",sep = ",",dec ='.')








