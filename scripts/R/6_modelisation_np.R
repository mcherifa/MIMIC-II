#######################################################
#-------- Modelisation non paramétrique 
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("/home/menyssa/Mimic/scripts/R/toolbox/les_sources.R")

# library(doMC)
# registerDoMC(cores = 5)

#######################################
# Lecture
#######################################

df <- readRDS(paste0(dest_data,"df_modelisation.rds")) %>%    
  subset(age < 100 & eevent == 0) %>%
  subset(select = c(-identif, -eevent, -care_unit)) %>%
  mutate(
    periode = as.numeric(as.character(periode)),
    gender  = factor(gender, levels = c("F","M"), labels = c("1","0")),
    event   = factor(event,  levels =  c(0,1), labels = c("0","1")),
    amine   = factor(amine,  levels =  c(0,1), labels = c("0","1")),
    curare  = factor(curare, levels = c(0,1), labels = c("0","1")),
    sedate  = factor(sedate, levels = c(0,1), labels = c("0","1")),
    venti   = factor(venti,  levels =  c(0,1), labels = c("0","1"))
  )

df <- df %>% 
  group_by(id) %>% 
  sample_n(size = 1)  

xfactors <- model.matrix(event ~ gender + amine + curare +
                           sedate + venti ,
                         data = df)[, -1]

xnum <- as.matrix(subset(df, select = c(-event, -id, -gender, -periode,
                                        -amine, -curare, -sedate, -venti )))

data   <- data.frame(xfactors, xnum)
data$y <- df$event


# Set classification column to factor

data$y <- as.factor(make.names(data$y))

#######################################
# Modelisation
#######################################

# Cross validation 

numFolds <- caret::trainControl(method = 'cv',
  number = 10,
  classProbs = TRUE,
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))

fit <- caret::train(y ~ . -y,
                    data = data,
                    method = 'nnet',
                    trControl = numFolds,
                    metric = "ROC",
                    maxit = 500,
                    tuneGrid=expand.grid(size=c(10), decay=c(0.1)))


options <- caret::trainControl( classProbs = TRUE,
                                verboseIter = TRUE,
                                summaryFunction = twoClassSummary)


fit <- caret::train(y ~ . -y, 
                    data = data, 
                    method = 'nnet', 
                    trControl = options,
                    metric = "ROC", 
                    maxit = 500,
                    tuneGrid = expand.grid(size=c(10), 
                                           decay=c(0.1)))

results1 <- predict(fit, newdata=data)
results2 <- predict(fit, newdata=df)

conf1 <- confusionMatrix(data$y,results1)
conf2 <- confusionMatrix(results2, testing$event)


# arbre de classification
# mtry = 28 
options <- caret::trainControl( classProbs = TRUE,
                                verboseIter = TRUE,
                                summaryFunction = twoClassSummary)
mod <- caret::train(event ~ ., 
                    data = training,
                    metric = "ROC", 
                    trControl = options,
                    method = "rf")
print(mod)
print(mod$finalModel)
varImpPlot(mod$finalModel)

#NOUVEAU DATA 
# probs <- predict(fit2, newdata=test, type='prob')
# Assemble output format: ID, prob.
# output <- data.frame(ID=test$ID)
# output <- cbind(output, TARGET=probs$X1)
# write.csv(output, file='output.csv', row.names=FALSE, quote=FALSE)

