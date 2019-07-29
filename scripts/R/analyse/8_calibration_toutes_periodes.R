######################
### Calibration une periode par patient
######################

rm(list = ls())

library(caret)
library(SuperLearner)
library(plyr)
library(dplyr)
library(parallel)
library(doParallel)
library(ROCR)
library(pROC)
library(tidyverse)
library(reshape2)
library(nlme)


load(file = "toutes_periodes.Rdata")

#####						
### Méthode 1 avec médiane
#####
# Modele de Haar 
object = j
nb_cuts = 5

##### Prediction non calibrée
training <- data.frame(target = object$outcome_train,
											prediction = object$pred_train,
											pred_cut = cut(x = object$pred_train,
 																			breaks = quantile(object$pred_train,
                   										probs = seq(0, 1, length = (nb_cuts + 1))),
 																			labels = seq(1,nb_cuts,1))) %>%
 					na.omit() %>%
 					group_by(pred_cut) %>%
					summarise( event_pred = sum(prediction) / length(prediction), 
										 event_obs = sum(target) / length(target))
										 
										 
 																		 
val_interne <- data.frame(target = object$outcome_test,
												prediction = object$pred_test,
												pred_cut = cut(x = object$pred_test,
   																			breaks = quantile(object$pred_test,
                     										probs = seq(0, 1, length = (nb_cuts+1))),
   																			labels = seq(1,nb_cuts,1))) %>%
   					na.omit() %>%
   					group_by(pred_cut) %>%
						summarise( event_pred = sum(prediction) / length(prediction), 
											 event_obs = sum(target)/ length(target)) 												 
											 
val_externe <- data.frame(target = object$outcome_fab, 
												prediction = object$pred_fab,
												pred_cut = cut(x = object$pred_fab,
																		breaks = quantile(object$pred_fab,
                 										probs = seq(0,1, length = (nb_cuts+1))),
																		labels = seq(1,nb_cuts,1))) %>%
				na.omit() %>%
				group_by(pred_cut) %>%
				summarise( event_pred = sum(prediction) / length(prediction), 
									 event_obs = sum(target)/ length(target)) 
									 
data_calibration <- rbind(training, val_interne, val_externe) %>%
mutate(type = c(rep("Training",nb_cuts), 
								rep("Internal Validation", nb_cuts), 
								rep("External Validation", nb_cuts)))
									 
ggplot() + 
	geom_line(aes(c(0, 1), c(0, 1)), linetype = 2, color = 'grey50') + 
	geom_point(data = data_calibration, aes(x = event_pred, y = event_obs,colour = type),size = 3) +
	geom_line(data = data_calibration, aes(x = event_pred, y = event_obs,colour = type)) +
	theme(legend.position="top") +
	theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	xlab("Mean Probability per Bin") + ylab("True Positive Frequency per Bin")
	ggsave("calibration_plot_haar_toutes_periodes_non_calibree_m1.tiff", units="in", width=10, height=4, dpi=300, compression = 'lzw')

ggplot(data_calibration, aes(x = factor(pred_cut), event_pred, fill = type)) 	+
	geom_bar(stat = "identity", position = "dodge", aes(y = event_obs, group = type), fill = "darkgrey") +
	geom_bar(stat="identity", position = "dodge", alpha = 0.6)


##### Prediction calibrées

# Train

training <- data.frame(target = object$outcome_train,
										 	 prediction = object$pred_train)
										 
lr_model = glm(target ~ prediction, 
               data = training,
               family = binomial)
               
lr_probs = predict(lr_model, 
                   newdata = training[, 'prediction', drop = FALSE],
                   type = "response")

training <- data.frame(target = object$outcome_train,
											prediction = lr_probs,
											pred_cut = cut(x = lr_probs,
 																			breaks = quantile(lr_probs,
                   										probs = seq(0, 1, length = (nb_cuts+1))),
 																			labels = seq(1,nb_cuts,1))) %>%
 					na.omit() %>%
 					group_by(pred_cut) %>%
					summarise( event_pred = sum(prediction) / length(prediction), 
										 event_obs = sum(target) / length(target)) 
										 
# Val interne

val_interne <- data.frame(target = object$outcome_test,
												prediction = object$pred_test) 				
												
lr_model = glm(target ~ prediction, 
               data = val_interne,
               family = binomial)
               
lr_probs = predict(lr_model, 
                   newdata = val_interne[, 'prediction', drop = FALSE],
                   type = "response")				
                   
val_interne <- data.frame(target = object$outcome_test,
												prediction = lr_probs,
												pred_cut = cut(x = lr_probs,
   																			breaks = quantile(lr_probs,
                     										probs = seq(0, 1, length = (nb_cuts+1))),
   																			labels = seq(1,nb_cuts,1))) %>%
   					na.omit() %>%
   					group_by(pred_cut) %>%
						summarise( event_pred = sum(prediction) / length(prediction), 
											 event_obs = sum(target)/ length(target)) 								
                   															

# Val externe

val_externe <- data.frame(target = object$outcome_fab,
												prediction = object$pred_fab)
												
lr_model = glm(target ~ prediction, 
							data = val_externe,
							family = binomial)

lr_probs = predict(lr_model, 
                   newdata = val_externe[, 'prediction', drop = FALSE],
                   type = "response")				
									 
val_externe <- data.frame(target = object$outcome_fab,
												prediction = lr_probs,
												pred_cut = cut(x = lr_probs,
																		breaks = quantile(lr_probs,
                 										probs = seq(0,1, length = (nb_cuts+1))),
																		labels = seq(1,nb_cuts,1))) %>%
				na.omit() %>%
				group_by(pred_cut) %>%
				summarise( event_pred = sum(prediction) / length(prediction), 
									 event_obs = sum(target)/ length(target)) 



data_calibration <- rbind(training, val_interne, val_externe) %>%
mutate(type = c(rep("Training",nb_cuts), 
								rep("Internal Validation",nb_cuts), 
								rep("External Validation", nb_cuts)))

ggplot() + 
	geom_line(aes(c(0, 1), c(0, 1)), linetype = 2, color = 'grey50') + 
	geom_point(data = data_calibration, aes(x = event_pred, y = event_obs,colour = type),size = 3) +
	geom_line(data = data_calibration, aes(x = event_pred, y = event_obs,colour = type)) +
	theme(legend.position="top") +
	theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	xlab("Mean Probability per Bin") + ylab("True Positive Frequency per Bin")
	ggsave("calibration_plot_haar_toutes_periodes_calibree_m1.tiff", units="in", width=10, height=4, dpi=300, compression = 'lzw')

ggplot(data_calibration, aes(x = factor(pred_cut), event_pred, fill = type)) 	+
	geom_bar(stat = "identity", position = "dodge", aes(y = event_obs, group = type), fill = "darkgrey") +
	geom_bar(stat="identity", position = "dodge", alpha = 0.6)

###################
### Méthode 2 : meme bin Autre méthode de Calibration
###################

# Prediction Non calibrée
training <- data.frame(target = factor(object$outcome_train),
										 	 prediction = object$pred_train)

cal_training <- calibration(
  target ~ prediction, 
  data = training,
  class = 1,
  cuts = nb_cuts)$data
  
val_interne <- data.frame(target = factor(object$outcome_test),
										 	 prediction = object$pred_test)

cal_test <- calibration(
  target ~ prediction, 
  data = val_interne,
  class = 1,
  cuts = nb_cuts)$data

val_externe <- data.frame(target = factor(object$outcome_fab),
										 	 prediction = object$pred_fab)

cal_fab <- calibration(
  target ~ prediction, 
  data = val_externe,
  class = 1,
  cuts = nb_cuts)$data
  
# Plot global
data_non_calibree  <- rbind(cal_training, cal_test, cal_fab)
data_non_calibree$type = c(rep("Training",nb_cuts),
													 rep("Internal Validation", nb_cuts), 
 													 rep("External Validation", nb_cuts)) 													   
ggplot(data = data_non_calibree, 
				aes(x = midpoint/100, y = Percent/100, color = type)) + 
  geom_line() + 
  geom_point(data = data_non_calibree, aes(colour = type)) +
  geom_segment(aes(x = 0, y = 0, xend =1 , yend = 1), linetype = 2, color = 'grey50') +
  #geom_points(aes(ymin=Lower/100, ymax=Upper/100, shape = type)) 
  #geom_pointrange(aes(ymin=Lower/100, ymax=Upper/100, shape = type)) +
  	theme(legend.position="right",
 				legend.title=element_blank(),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(), 
				axis.line = element_line(colour = "black")) +
  ylab("Mean % Event Rate w Std Error") +
  xlab("Super Learner Predictions Range") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, .01))
ggsave("calibration_plot_haar_toutes_periodes_non_calibree_m2.tiff", units="in", width=10, height=7, dpi=300, compression = 'lzw')

##### Predictions calibrées
# Train
training <- data.frame(target = object$outcome_train,
										 	 prediction = object$pred_train)										 
lr_model = glm(target ~ prediction, 
               data = training,
               family = binomial)               
lr_probs = predict(lr_model, 
                   newdata = training[, 'prediction', drop = FALSE],
                   type = "response")                   
training <- data.frame(target = factor(object$outcome_train),
										 	 prediction = lr_probs)										 	 
cal_training <- calibration(
  target ~ prediction, 
  data = training,
  class = 1,
  cuts = nb_cuts)$data
val_interne <- data.frame(target = object$outcome_test,
												prediction = object$pred_test)
lr_model = glm(target ~ prediction, 
               data = val_interne,
               family = binomial)               
lr_probs = predict(lr_model, 
                   newdata = val_interne[, 'prediction', drop = FALSE],
                   type = "response")		                   
val_interne <- data.frame(target = factor(object$outcome_test),
													prediction = object$pred_test)
cal_test <- calibration(
  target ~ prediction, 
  data = val_interne,
  class = 1,
  cuts = nb_cuts)$data                 
val_externe <- data.frame(target = object$outcome_fab,
												prediction = object$pred_fab)												
lr_model = glm(target ~ prediction, 
							data = val_externe,
							family = binomial)
lr_probs = predict(lr_model, 
                   newdata = val_externe[, 'prediction', drop = FALSE],
                   type = "response")													 
val_externe <- data.frame(target = factor(object$outcome_fab),
													prediction = object$pred_fab)
cal_fab <- calibration(
  target ~ prediction, 
  data = val_externe,
  class = 1,
  cuts = nb_cuts)$data		     
# Plot global
data_calibree  <- rbind(cal_training, cal_test, cal_fab)
data_calibree$type = c(rep("Training",nb_cuts),
													 rep("Internal Validation", nb_cuts), 
 													 rep("External Validation", nb_cuts))

ggplot(data = data_calibree, 
				aes(x = midpoint/100, y = Percent/100, color = type)) + 
  geom_line() + 
  ylim(0,1.02) +
  geom_point(data = data_calibree, aes(colour = type),size = 3) +
  geom_segment(aes(x = 0, y = 0, xend =1 , yend = 1), linetype = 2, color = 'grey50') +
  #geom_pointrange(aes(ymin=Lower/100, ymax=Upper/100, shape = type)) +
 	theme(legend.position="right",
 				legend.title=element_blank(),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(), 
				axis.line = element_line(colour = "black")) +
  ylab("Mean % Event Rate w Std Error") +
  xlab("Super Learner Predictions Range") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.01))
	ggsave("calibration_plot_haar_toutes_periodes_calibree_m2.tiff", units="in", width=10, height=4, dpi=300, compression = 'lzw')

ggplot(data_calibration, aes(x = factor(pred_cut), event_pred, fill = type)) 	+
	geom_bar(stat = "identity", position = "dodge", aes(y = event_obs, group = type), fill = "darkgrey") +
	geom_bar(stat="identity", position = "dodge", alpha = 0.6)










