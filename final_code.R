library(VIM)
library(caret)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(DMwR)
library(factoextra)
library(OptimalCutpoints)
library(cluster)
library(pROC)
library(tidyr)
library(randomForest)


# Read in file
market <- read.csv("/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/bank-additional/bank-additional-full.csv",
                   sep = ";")
# 21 variables and 41,188 observations
dim(market)

# There are 39,673 observations that have the value 999 for the pdays variable
# These are missing values
table(market$pdays)

# pdays variable identified as near zero variance
# this variable should be removed
nearZeroVar(market, names = TRUE)

# Remove the pdays variable from data
market <- market[, -which(colnames(market) == "pdays")]
colnames(market)

###### JOB ########
table(market$marital)
# set the "unknown" levels to missing
for (i in 1:nrow(market)){
  if(market$job[i] == "unknown"){
    market$job[i] <- NA
  }
}
# 330 missing values
length(which(is.na(market$job) == TRUE))

###### MARITAL ########
table(market$marital)
# set the "unknown" levels to missing
for (i in 1:nrow(market)){
  if(market$marital[i] == "unknown"){
    market$marital[i] <- NA
  }
}
# 80 missing values
length(which(is.na(market$marital) == TRUE))

###### EDUCATION ########
table(market$education)
# set the "unknown" levels to missing
for (i in 1:nrow(market)){
  if(market$education[i] == "unknown"){
    market$education[i] <- NA
  }
}
# 1,731 missing values
length(which(is.na(market$education) == TRUE))

###### DEFAULT ########
table(market$default)
# set the "unknown" levels to missing
for (i in 1:nrow(market)){
  if(market$default[i] == "unknown"){
    market$default[i] <- NA
  }
}
# 8,597 missing values
length(which(is.na(market$default) == TRUE))

###### HOUSING ########
table(market$housing)
# set the "unknown" levels to missing
for (i in 1:nrow(market)){
  if(market$housing[i] == "unknown"){
    market$housing[i] <- NA
  }
}
# 990 missing values
length(which(is.na(market$housing) == TRUE))

###### LOAN ########
table(market$loan)
# set the "unknown" levels to missing
for (i in 1:nrow(market)){
  if(market$loan[i] == "unknown"){
    market$loan[i] <- NA
  }
}
# 990 missing values
length(which(is.na(market$loan) == TRUE))

nearZeroVar(market, names = TRUE)

# Remove the default variable from data
market <- market[, -which(colnames(market) == "default" | colnames(market) == "duration")]
colnames(market)

# Use K Nearest Neighbors to impute the missing data
market2 <- VIM::kNN(market, variable = c("job", "marital", "education", "housing", "loan"), k = 10)
#saveRDS(market2, "/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/market2.rds")
#market2 <- readRDS("/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/market2.rds")

# Drop the boolean columns created from imputation
colnames(market2)
market2 <- market2[, -c(19:23)]

# Now there are no missisng values
market2[!complete.cases(market2),]

age_plot <- ggplot(data = market2, aes(x = age)) + 
  geom_histogram(bins = 30) + 
  labs(title = "Distribution of Age", x = "Age")

job_plot <- ggplot(data = market2, aes(x = job)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) + 
  labs(title = "Job", x = "Job Type")

marital_plot <- ggplot(data = market2, aes(x = marital)) + 
  geom_bar() + 
  labs(title = "Marital Status", x = "Marital Status")

education_plot <- ggplot(data = market2, aes(x = education)) +
  geom_bar()+
  labs(title = "Education", x = "Education Level") + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

housing_plot <- ggplot(data = market2, aes(x = housing)) + 
  geom_bar() + 
  labs(title = "Housing", x = "Has a Home Loan")

loan_plot <- ggplot(data = market2, aes(x = loan)) +
  geom_bar() +
  labs(title = "Personal Loan", x = "Has a Personal Loan")

contact_plot <- ggplot(data = market2, aes(x = contact)) +
  geom_bar() +
  labs(title = "Contact", x = "Type of Contact")

month_plot <- ggplot(data = market2, aes(x = month)) +
  geom_bar() +
  labs(title = "Month", x = "Month of Contact")

day_plot <- ggplot(data = market2, aes(x = day_of_week)) +
  geom_bar() +
  labs(title = "Day of Weed", x = "Day of Contact")

campaign_plot <- ggplot(data = market2, aes(x = campaign)) + 
  geom_histogram(bins = 25) + 
  labs(title = "Number of Contacts During Campaign", x = "# Contacts")

previous_plot <- ggplot(data = market2, aes(x = previous)) + 
  geom_histogram(bins = 8) + 
  labs(title = "Number of Contacts Prior to Campaign", x = "# Contacts")

poutcome_plot <- ggplot(data = market2, aes(x = poutcome)) + 
  geom_bar() + 
  labs(title = "Outcome of Previous Contact", x = "Outcome")

emp.var.rate_plot <- ggplot(data = market2, aes(x = emp.var.rate)) + 
  geom_histogram(bins = 10) + 
  labs(title = "Employment Variation Rate", x = "Rate")

cons.price.idx_plot <- ggplot(data = market2, aes(x = cons.price.idx)) + 
  geom_histogram(bins = 20) + 
  labs(title = "Consumer Price Index", x = "Index")

cons.conf.idx_plot <- ggplot(data = market2, aes(x = cons.conf.idx)) + 
  geom_histogram(bins = 10) + 
  labs(title = "Consumer Confidence Index", x = "Index")

euribor3m_plot <- ggplot(data = market2, aes(x = euribor3m)) + 
  geom_histogram(bins = 10) + 
  labs(title = "Euribor 3 Month Rate", x = "Rate")

nr.employed_plot <- ggplot(data = market2, aes(x = nr.employed)) + 
  geom_histogram(bins = 11) + 
  labs(title = "Number of Employees at Bank", x = "Number of Employees")

outcome_plot <- ggplot(data = market2, aes(x = y)) +
  geom_bar() +
  labs(title = "Did the customer subscribe to bank term deposit?", x = "Response")


# Change binary variables to levels of 0 and 1 instead of character levels
market2$housing <- ifelse(market2$housing == "no", 0, 1)
market2$loan <- ifelse(market2$loan == "no", 0, 1)
market2$contact <- ifelse(market2$contact == "telephone", 0, 1)

# Change outcome variable to a factor variable
market2$y <- as.factor(market2$y)
levels(market2$y)

# Subset only the numeric variables for correlation plot
numeric <- c("age", "housing", "loan", "contact", "campaign", "previous", "emp.var.rate",
             "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")
market2_num <- market2[, numeric]
correlation_df <- data.frame(market2_num, "outcome" = ifelse(market2$y == "yes", 1, 0))
correlation_df <- correlation_df[, -12]
head(correlation_df)
# Matrix of correlations, rounded to 4 decimal places
res <- round(cor(correlation_df), 4)
# Correlation plot
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)



# Create training and testing data
set.seed(869)
train_index <- sample(1:nrow(market2), 0.8 * nrow(market2))
test_index <- setdiff(1:nrow(market2), train_index)

pre_market_train <- market2[train_index, ]
market_test <- market2[test_index, ]

# Up-Sampling
set.seed(155)
market_train <- upSample(x = pre_market_train[, -which(colnames(pre_market_train) == "y")],
                            y = pre_market_train[, which(colnames(pre_market_train) == "y")])
colnames(market_train)[18] <- "y"

table(market_train$y)
dim(market_test)

# Logistic Regression Using Cross Validation
ctrl1 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 5,
                      savePredictions = TRUE,
                      classProbs = TRUE) 
set.seed(869)
logisticRegCV <- train(y ~ ., data = market_train,
                       method = "glm",
                       family = "binomial",
                       trControl = ctrl1)
saveRDS(logisticRegCV, "/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/logisticregcv.rds")
#logisticRegCV <- readRDS("/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/logisticregcv.rds")


# Create dataframe with predictions
cvLog_results <- data.frame(obs = market_test$y)
cvLog_up_results <- data.frame(obs = market_test$y)
# probabilities
cvLog_results$prob <- predict(logisticRegCV, market_test, type = "prob")[,2]
cvLog_up_results$prob <- predict(logisticRegCV_up, market_test, type = "prob")[,2]
# predictions
cvLog_results$pred <- predict(logisticRegCV, market_test)
cvLog_up_results$pred <- predict(logisticRegCV_up, market_test)

# Confusion matrix using original predictions
model1cm <- confusionMatrix(data = cvLog_results$pred, reference = cvLog_results$obs)
model1cm_up <- confusionMatrix(data = cvLog_up_results$pred, reference = cvLog_up_results$obs)
model1_accuracy <- model1cm$overall["Accuracy"]
model1_kappa <- model1cm$overall["Kappa"]
model1_sens <- model1cm$byClass["Sensitivity"]
model1_spec <- model1cm$byClass["Specificity"]

# New cutpoint
cutpoint1 <- summary(optimal.cutpoints(X = "prob", status = "obs", cvLog_results, 
                                     tag.healthy = "no", methods = "MaxKappa"))
final_cutpoint1 <- cutpoint1$MaxKappa$Global$optimal.cutoff$cutoff
final_cutpoint1
# Create new predictions using optimal cutpoint
cvLog_results$new_pred <- as.factor(ifelse(cvLog_results$prob > final_cutpoint1, "yes", "no"))
head(cvLog_results)
# New confusion matrix
model1cm2 <- confusionMatrix(data = cvLog_results$new_pred, reference = cvLog_results$obs)
model1_accuracy2 <- model1cm2$overall["Accuracy"]
model1_kappa2 <- model1cm2$overall["Kappa"]
model1_sens2 <- model1cm2$byClass["Sensitivity"]
model1_spec2 <- model1cm2$byClass["Specificity"]

########## Principal Component Analysis ###########

# economic variables
econ <- c("emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")
econ_df <- market2[, econ]

# Principal Component Analysis
econ_pca <- prcomp(econ_df, center = T, scale = T)
summary(econ_pca)

# Scree Plot
fviz_eig(econ_pca)

# Visualization -- Dimension Reduction
fviz_pca_ind(econ_pca, label = "none", habillage = market2$y,
             addEllipses = TRUE, ellipse.level = 0.95, palette = "Dark1", axes = c(1,2))

# Merge 2 columns of PCA variables with market data
colnames(market2)
pca_data <- data.frame(market2[, -c(13:17)], econ_pca$x[, 1:2])
pre_pca_train <- pca_data[train_index,]
pca_test <- pca_data[test_index, ]

pca_train <- upSample(x = pre_pca_train[, -which(colnames(pre_pca_train) == "y")],
                      y = pre_pca_train[, which(colnames(pre_pca_train) == "y")])
colnames(pca_train)[15] <- "y"
colnames(pca_train)

# Train logistic regression model with the new pca variables
set.seed(869)
logistic_pca <- train(y ~ ., data = pca_train,
                       method = "glm",
                       family = "binomial",
                       trControl = ctrl1)

saveRDS(logistic_pca, "/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/logistic_pca.rds")
#logistic_pca <- readRDS("/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/logistic_pca.rds")

# Create dataset with results
log.pca_results <- data.frame(obs = pca_test$y)
# Predicted probabilities
log.pca_results$prob <- predict(logistic_pca, newdata = pca_test, type = "prob")[,2]
# Predicted outcomes
log.pca_results$pred <- predict(logistic_pca, pca_test)
# Confusion matrix
model2cm <- confusionMatrix(data = log.pca_results$pred, reference = log.pca_results$obs)
model2_accuracy <- model2cm$overall["Accuracy"]
model2_kappa <- model2cm$overall["Kappa"]
model2_sens <- model2cm$byClass["Sensitivity"]
model2_spec <- model2cm$byClass["Specificity"]

# Find optimal cutpoint
cutpoint2 <- summary(optimal.cutpoints(X = "prob", status = "obs", log.pca_results, 
                                       tag.healthy = "no", methods = "MaxKappa"))
final_cutpoint2 <- cutpoint2$MaxKappa$Global$optimal.cutoff$cutoff
final_cutpoint2
# Re-label predictions based on new cutpoint
log.pca_results$new_pred <- as.factor(ifelse(log.pca_results$prob > final_cutpoint2, "yes", "no"))
# Confusion matrix with cutpoint
model2cm2 <- confusionMatrix(data = log.pca_results$new_pred, reference = log.pca_results$obs)
model2_accuracy2 <- model2cm2$overall["Accuracy"]
model2_kappa2 <- model2cm2$overall["Kappa"]
model2_sens2 <- model2cm2$byClass["Sensitivity"]
model2_spec2 <- model2cm2$byClass["Specificity"]



########## Penalized Logistic Regression ############

glmnGrid <- expand.grid(alpha = seq(0, 1, length = 11),
                        lambda = seq(0.01, 0.2, length = 10))
set.seed(869)
penalize.log <- train(x = data.matrix(market_train[, -which(colnames(market_train) == "y")]),
                      y = market_train[, which(colnames(market_train) == "y")],
                      method = "glmnet",
                      tuneGrid = glmnGrid,
                      preProc = c("center", "scale"),
                      family = "binomial",
                      trControl = ctrl1)
penalize.log$bestTune

saveRDS(penalize.log, "/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/penalize.log.rds")
#penalize.log <- readRDS("/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/penalize.log.rds")

# Create dataframe with predictions
pn.log_results <- data.frame(obs = market_test$y)
# probabilities
pn.log_results$prob <- predict(penalize.log, data.matrix(market_test), type = "prob")[,2]
# predictions
pn.log_results$pred <- predict(penalize.log, data.matrix(market_test))
# Confusion matrix using original predictions
model3cm <- confusionMatrix(data = pn.log_results$pred, reference = pn.log_results$obs)
model3_accuracy <- model2cm$overall["Accuracy"]
model3_kappa <- model2cm$overall["Kappa"]
model3_sens <- model2cm$byClass["Sensitivity"]
model3_spec <- model2cm$byClass["Specificity"]


# optimal cutpoint
cutpoint3 <- summary(optimal.cutpoints(X = "prob", status = "obs", pn.log_results, 
                                       tag.healthy = "no", methods = "MaxKappa"))
final_cutpoint3 <- cutpoint3$MaxKappa$Global$optimal.cutoff$cutoff
final_cutpoint3
# make new predictions using cutpoint
pn.log_results$new_pred <- as.factor(ifelse(pn.log_results$prob > final_cutpoint3, "yes", "no"))
# Confusion matrix with new predictions
model3cm2 <- confusionMatrix(data = pn.log_results$new_pred, reference = pn.log_results$obs)
model3_accuracy2 <- model2cm$overall["Accuracy"]
model3_kappa2 <- model2cm$overall["Kappa"]
model3_sens2 <- model2cm$byClass["Sensitivity"]
model3_spec2 <- model2cm$byClass["Specificity"]


# Random Forest Model
# #ctrlRF <- trainControl(method = "repeatedcv",
#                        classProbs = TRUE,
#                        number = 10,
#                        repeats=5,
#                        savePredictions = TRUE,
#                        search = 'random')
#set.seed(869)
# Random.Forest <- train(x = data.matrix(market_train[, -which(colnames(market_train) == "y")]),
#                        y = market_train[, which(colnames(market_train) == "y")],
#                        method = "rf",
#                        ntree = 500,
#                        tuneLenght = 5,
#                        importance = TRUE,
#                        metric = "Accuracy",
#                        trControl = ctrlRF)

Random.Forest <- readRDS("/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/Random.Forest.rds")
Random.Forest$bestTune
# Create dataframe with predictions
Random.Forest_results <- data.frame(obs = market_test$y)
# probabilities
Random.Forest_results$prob <- predict(Random.Forest, data.matrix(market_test), type = "prob")[,2]
# predictions
Random.Forest_results$pred <- predict(Random.Forest, data.matrix(market_test))
# Confusion matrix using original predictions
model4cm <- confusionMatrix(data = Random.Forest_results$pred, reference = Random.Forest_results$obs)
model4_accuracy <- model4cm$overall["Accuracy"]
model4_kappa <- model4cm$overall["Kappa"]
model4_sens <- model4cm$byClass["Sensitivity"]
model4_spec <- model4cm$byClass["Specificity"]
model4cm


# optimal cutpoint
cutpoint4 <- summary(optimal.cutpoints(X = "prob", status = "obs", Random.Forest_results, 
                                       tag.healthy = "no", methods = "MaxKappa"))
final_cutpoint4 <- cutpoint4$MaxKappa$Global$optimal.cutoff$cutoff
final_cutpoint4

# make new predictions using cutpoint
Random.Forest_results$new_pred <- as.factor(ifelse(Random.Forest_results$prob > final_cutpoint4, "yes", "no"))
# Confusion matrix with new predictions
confusionMatrix(data = Random.Forest_results$new_pred, reference = Random.Forest_results$obs)
model4cm2 <- confusionMatrix(data = Random.Forest_results$pred, reference = Random.Forest_results$obs)
model4_accuracy2 <- model4cm2$overall["Accuracy"]
model4_kappa2 <- model4cm2$overall["Kappa"]
model4_sens2 <- model4cm2$byClass["Sensitivity"]
model4_spec2 <- model4cm2$byClass["Specificity"]
model4cm2


# Neural Networks

# nnetGrid <- expand.grid(size = c(1:10), decay = c(0, .1, 1, 2))
# maxSize <- max(nnetGrid$size)
# 
# ctrlNN <- trainControl(method = "repeatedcv",
#                        summaryFunction = multiClassSummary,
#                        classProbs = TRUE,
#                        savePredictions = TRUE,
#                        search = 'grid')
# 
# set.seed(869)
# Neural.Network <- train(data.matrix(market_train[, -which(colnames(market_train) == "y")]),
#                         y = market_train[, which(colnames(market_train) == "y")],
#                         method = "nnet",
#                         metric = "Accuracy",
#                         preProc = c("center", "scale"),
#                         tuneGrid = nnetGrid,
#                         repeats = 10,
#                         trace = FALSE,
#                         maxit = 2000,
#                         MaxNWts = 10*(maxSize * (ncol(market_train) + 1) + maxSize + 1),
#                         allowParallel = FALSE,
#                         trControl = ctrlNN)
Neural.Network <- readRDS("/Users/rob.pruette/Documents/SMU Spring 2020/STAT 6309/Project1/Neural.Network.rds")
Neural.Network$bestTune
# Create dataframe with predictions
Neural.Network_results <- data.frame(obs = market_test$y)
# probabilities
Neural.Network_results$prob <- predict(Neural.Network, data.matrix(market_test), type = "prob")[,2]
# predictions
Neural.Network_results$pred <- predict(Neural.Network, data.matrix(market_test))
# Confusion matrix using original predictions
model5cm <- confusionMatrix(data = Neural.Network_results$pred, reference = Neural.Network_results$obs)
model5_accuracy <- model5cm$overall["Accuracy"]
model5_kappa <- model5cm$overall["Kappa"]
model5_sens <- model5cm$byClass["Sensitivity"]
model5_spec <- model5cm$byClass["Specificity"]
model5cm

# optimal cutpoint
cutpoint5 <- summary(optimal.cutpoints(X = "prob", status = "obs", Neural.Network_results, 
                                       tag.healthy = "no", methods = "MaxKappa"))
final_cutpoint5 <- cutpoint5$MaxKappa$Global$optimal.cutoff$cutoff
final_cutpoint5

# make new predictions using cutpoint
Neural.Network_results$new_pred <- as.factor(ifelse(Neural.Network_results$prob > final_cutpoint5, "yes", "no"))
# Confusion matrix with new predictions
model5cm2 <- confusionMatrix(data = Neural.Network_results$new_pred, reference = Neural.Network_results$obs)
model5_accuracy2 <- model5cm2$overall["Accuracy"]
model5_kappa2 <- model5cm2$overall["Kappa"]
model5_sens2 <- model5cm2$byClass["Sensitivity"]
model5_spec2 <- model5cm2$byClass["Specificity"]
model5cm2


# Calibration plots
cal_data <- data.frame(LogReg = predict(logisticRegCV, market_test, type = "prob")[,2])
cal_data$LogPCA <- predict(logistic_pca, pca_test, type = "prob")[,2]
cal_data$LogPenalized <- predict(penalize.log, data.matrix(market_test), type = "prob")[,2]
cal_data$RandomForest <- predict(Random.Forest, data.matrix(market_test), type = "prob")[,2]
cal_data$NeuralNetwork <- predict(Neural.Network, data.matrix(market_test), type = "prob")[,2]
cal_data$class <- market_test[, 18]

calibration_results <- calibration(class ~ LogReg + LogPCA + LogPenalized + RandomForest + NeuralNetwork, data = cal_data, cuts = 10, class = "yes")
xyplot(calibration_results, auto.key = list(columns = 3))

model1ROC <- roc(cvLog_results$obs, cvLog_results$prob)
plot(model1ROC, main ="CV Logistic Regression")
auc(model1ROC)
ci.auc(model1ROC)

model2ROC <- roc(log.pca_results$obs, log.pca_results$prob)
plot(model2ROC, main = "CV Logistic Regression with PCA Vars")
auc(model2ROC)
ci.auc(model2ROC)

model3ROC <- roc(pn.log_results$obs, pn.log_results$prob)
plot(model3ROC, main = "Penalized Logistic Regression")
auc(model3ROC)
ci.auc(model3ROC)

model4ROC <- roc(Random.Forest_results$obs, Random.Forest_results$prob)
plot(model4ROC, main = "Random Forest")
auc(model4ROC)
ci.auc(model4ROC)

model5ROC <- roc(Neural.Network_results$obs, Neural.Network_results$prob)
plot(model5ROC, main = "Neural Network")
auc(model5ROC)
ci.auc(model5ROC)

# Results dataframe
# Results dataframe
results_df <- data.frame(Model = c("CV Log Reg", "CV Log Reg OCP", "Log Reg PCA", "Log Reg PCA OCP", "Penlzd Log", "Penlzd Log OCP", "Random Forest", "Random Forest OCP", "Neural Network", "Neural Network OCP"))
results_df$Kappa <- c(model1_kappa, model1_kappa2, model2_kappa, model2_kappa2, model3_kappa, model3_kappa2, model4_kappa, model4_kappa2, model5_kappa, model5_kappa2)
results_df$Accuracy <- c(model1_accuracy, model1_accuracy2, model2_accuracy, model2_accuracy2, model3_accuracy, model3_accuracy2, model4_accuracy, model4_accuracy2, model5_accuracy, model5_accuracy2)
results_df$Sensitivity <- c(model1_sens, model1_sens2, model2_sens, model2_sens2, model3_sens, model3_sens2, model4_sens, model4_sens2, model5_sens, model5_sens2)
results_df$Specificity <- c(model1_spec, model1_spec2, model2_spec, model2_spec2, model3_spec, model3_spec2, model4_spec, model4_spec2, model5_spec, model5_spec2)
results_df$Cutpoint <- c("Default", "Optimal", "Default", "Optimal", "Default", "Optimal", "Default", "Optimal", "Default", "Optimal")
results_df

ggplot(data = results_df[c(2,4,6,8,10),], aes(x = reorder(Model, Kappa), y = Kappa))+
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = round(Kappa, 4)), position = position_dodge(width = 0.2), hjust = -0.25, size = 3) +
  coord_flip() + 
  theme_minimal() + 
  ylim(0,0.5) + 
  xlab("Model") + 
  theme(axis.text.y = element_text(angle = 20, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Kappa Statistic")

ggplot(data = results_df[c(2,4,6,8,10),], aes(x = reorder(Model, Accuracy), y = Accuracy))+
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = round(Accuracy, 4)), position = position_dodge(width = 0.2), hjust = -0.25, size = 3) +
  coord_flip() + 
  theme_minimal() + 
  ylim(0,1) + 
  xlab("Model") + 
  theme(axis.text.y = element_text(angle = 20, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Overall Accuracy")

ggplot(data = results_df[c(2,4,6,8,10),], aes(x = reorder(Model, Specificity), y = Specificity))+
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = round(Specificity, 4)), position = position_dodge(width = 0.2), hjust = -0.25, size = 3) +
  coord_flip() + 
  theme_minimal() + 
  ylim(0,1) + 
  xlab("Model") + 
  theme(axis.text.y = element_text(angle = 20, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Specificity")

ggplot(data = results_df[c(2,4,6,8,10),], aes(x = reorder(Model, Sensitivity), y = Sensitivity))+
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = round(Sensitivity, 4)), position = position_dodge(width = 0.2), hjust = -0.25, size = 3) +
  coord_flip() + 
  theme_minimal() + 
  ylim(0,1) + 
  xlab("Model") + 
  theme(axis.text.y = element_text(angle = 20, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Sensitivity")

gather.df <- gather(results_df, key = "statistic", value = "value", Accuracy, Sensitivity, Specificity, Kappa, Cutpoint)
gather.df2 <- gather.df[-which(gather.df$statistic == "Cutpoint"),]
#gather.df2 <- gather.df2[-which(gather.df2$Cutpoint == "Default"),]

model1_gather <- gather.df2[which(gather.df2$Model == "CV Log Reg" | gather.df2$Model == "CV Log Reg OCP"),]
model2_gather <- gather.df2[which(gather.df2$Model == "Log Reg PCA" | gather.df2$Model == "Log Reg PCA OCP"),]
model3_gather <- gather.df2[which(gather.df2$Model == "Penlzd Log" | gather.df2$Model == "Penlzd Log OCP"),]
model4_gather <- gather.df2[which(gather.df2$Model == "Random Forest" | gather.df2$Model == "Random Forest OCP"),]
model5_gather <- gather.df2[which(gather.df2$Model == "Neural Network" | gather.df2$Model == "Neural Network OCP"),]


ggplot(model5_gather, aes(fill = statistic, x = Model, y = as.numeric(value)))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) + 
  labs(title = "Neural Network", y = "Value")+
  theme(plot.title = element_text(hjust = 0.5))
