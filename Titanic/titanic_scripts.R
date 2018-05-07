# library("data.table") error occur when mice process
library("dplyr")                # data cleaning
library("ggplot2")              # visualization
library("ggthemes")             # visualization
library("mice")                 # missing value imputation
library("randomForest")         # randomForest
library("adabag")               # boosting
library("caret")                # CV
library("C50")                  # decision tree
library("irr")                  # Kappa value
library("e1071")                # SVM


train <- read.csv(file = "train.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv(file = "test.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# dplyr:bind_rows
# When row-binding, columns are matched by name, and any missing columns with be filled with NA.
full <- bind_rows(train, test)

#### Basic infos ####
# PassengerId: sequence number
# Survived: target variable, 0 for NO while 1 for YES !
# Pclass: passengers class, No Missing Data, from 1 to 3.
# Name: ?
# Sex: No Missing Data
# Age: 263 missing data, big problem !
# SibSp(of siblings / spouses aboard the Titanic): No Missing Data
# Parch(of parents / children aboard the Titanic): No Missing Data
# Ticket: ticket number
# Fare: 1 missing data
# Cabin: cabin number
# Embarked: 2 missing data(C = Cherbourg, Q = Queenstown, S = Southampton)

#### Sensible value imputation for Embarked ####
# full[full$Embarked == "",]  row number: 62, 830 with fare value is 80 !
embark_fare <- full %>%
    filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
    geom_boxplot() +
    geom_hline(aes(yintercept=80),
               colour='red', linetype='dashed', lwd=2) +
    scale_y_continuous(labels = scales::dollar) +
    theme_few()

full$Embarked[c(62, 830)] <- 'C'
rm(embark_fare)


#### Sensible value imputation for Fare ####
# Passenger on row 1044 has an NA Fare value
# full[is.na(full$Fare) == TRUE,]   row number: 1044
ggplot(full[full$Pclass == '3' & full$Embarked == 'S' & full$Age >= 55, ],
       aes(x = Fare)) +
    geom_density(fill = '#99d6ff', alpha=0.4) +
    geom_vline(aes(xintercept=median(Fare, na.rm=T)),
               colour='red', linetype='dashed', lwd=1) +
    scale_x_continuous(labels = scales::dollar) +
    theme_few()

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S' & full$Age >= 55, ]$Fare,
                          na.rm = TRUE) # 7.775



#### Predictive imputation for Age ####
# sum(is.na(full$Age)) : 263
factor_vars <- c('Survived','Pclass','Sex','Embarked')
full[factor_vars] <- lapply(full[factor_vars],
                            function(x) as.factor(x))
#-----------------------------------------------------------
#set.seed(42)
#mice_mod <- mice(full[, !names(full) %in%
#                          c('PassengerId','Name','Ticket','Cabin','Survived')], method='cart')
#mice_output <- complete(mice_mod)
#full$Age <- mice_output$Age
#rm(mice_mod,mice_output)
#-----------------------------------------------------------
# add-on column : family_size
full$family_size <- full$SibSp + full$Parch + 1
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + family_size,
                       data=full[!is.na(full$Age),],
                       method="anova")
rpart_output <- predict(predicted_age, full[is.na(full$Age),])

par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data',
     col='darkgreen', ylim=c(0,0.05))

full$Age[is.na(full$Age == 1)] <- rpart_output

hist(full$Age, freq=F, main='Age: After Imputation',
     col='lightgreen', ylim=c(0,0.05))
# compare Age distribution between original data and MICE output
sum(is.na(full$Age))
rm(predicted_age)
gc()

#### Prediction ####
# split data into a train data and a test data
train <- full[1:891,]
test <- full[892:1309,]

# CV
set.seed(9)
K = 9
folds <- createFolds(train$PassengerId, k = K)

# model column select #
# coming soon
# names(train)
# [3] "Pclass"
# [5] "Sex"
# [6] "Age"
# [12] "Embarked"
# [13] "family_size"

# model training
# -----------------------------------------------------------------------------------------
# C5.0
kappa_cv_c5 <- c()
for(i in 1:K){
    cv_train <- train[-folds[[i]], ]
    cv_test <- train[folds[[i]], ]
    model_cv_c5 <- C5.0(cv_train[ ,c(5, 6, 13)],
                        cv_train$Survived,
                        trials = 10,
                        control = C5.0Control(subset = TRUE,
                                              winnow = TRUE,
                                              noGlobalPruning = TRUE,
                                              fuzzyThreshold = TRUE))
    prediction_cv_c5 <- predict(model_cv_c5,
                                cv_test[ ,c(5, 6, 13)])
    cv_actual <- cv_test$Survived
    kappa_cv_c5[i] <- kappa2(data.frame(cv_actual, prediction_cv_c5))$value
    print(paste(i, ":", round(kappa_cv_c5[i], digits = 5)))
}
# -----------------------------------------------------------------------------------------
# randomforest
kappa_cv_rf <- c()
for(i in 1:K){
    cv_train <- train[-folds[[i]], ]
    cv_test <- train[folds[[i]], ]
    model_cv_rf <- randomForest(Survived ~ Pclass + Sex + Age + Embarked + family_size,
                                data = cv_train,
                                mtry = 3,
                                ntree = 200)
    prediction_cv_rf <- predict(model_cv_rf,
                                cv_test[ ,c(3, 5, 6, 12, 13)])
    cv_actual <- cv_test$Survived
    kappa_cv_rf[i] <- kappa2(data.frame(cv_actual, prediction_cv_rf))$value
    print(paste(i, ":", round(kappa_cv_rf[i], digits = 5)))
}
# -----------------------------------------------------------------------------------------
# AdaBoost.M1
kappa_cv_ad <- c()
for(i in 1:K){
    cv_train <- train[-folds[[i]], ]
    cv_test <- train[folds[[i]], ]
    model_cv_ad <- boosting(Survived ~ Pclass + Sex + Age + Embarked + family_size,
                            data = cv_train,
                            mfinal = 150,
                            coeflearn = 'Freund')
    prediction_cv_ad <- predict(model_cv_ad, cv_test)
    cv_actual <- cv_test$Survived
    kappa_cv_ad[i] <- kappa2(data.frame(cv_actual, prediction_cv_ad$class))$value
    print(paste(i, ":", round(kappa_cv_ad[i], digits = 5)))
}
# -----------------------------------------------------------------------------------------
# SVM
kappa_cv_svm <- c()
for(i in 1:K){
    cv_train <- train[-folds[[i]], ]
    cv_test <- train[folds[[i]], ]
    model_cv_svm <- svm(Survived ~ Pclass + Sex + Age + Fare + family_size,
                        data = cv_train,
                        #gamma = 0.2,
                        cost = 2)
    prediction_cv_svm <- predict(model_cv_svm, cv_test)
    cv_actual <- cv_test$Survived
    kappa_cv_svm[i] <- kappa2(data.frame(cv_actual, prediction_cv_svm))$value
    print(paste(i, ":", round(kappa_cv_svm[i], digits = 5)))
}
# -----------------------------------------------------------------------------------------
rm(i, K)
gc()
# -----------------------------------------------------------------------------------------
tbl_kappa <- data.frame(folds = 1:9,
                        C50 = kappa_cv_c5,
                        RandomForest = kappa_cv_rf,
                        #Adaboost = kappa_cv_ad,
                        SVM = kappa_cv_svm)
summary(tbl_kappa)


# -----------------------------------------------------------------------------------------
model_svm <- svm(Survived ~ Pclass + Sex + Age + Embarked + family_size,
                 data = train,
                 cost = 2)
prediction_svm <- predict(model_svm, test[, c(3,5,6,12,13)])
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_svm)
write.csv(solution, file = 'svm_mod_Solution_V12.csv', row.names = F)

N = sapply(1:200, function(i) svm(Survived ~ Pclass + Sex + Age + Fare + family_size,
                                   data = train, cost = i)$tot.nSV)
plot(N)

# ----------------------------------------------------------------------------------------
model_c5 <- C5.0(train[ ,c(3, 5, 6, 13)],
                 train$Survived,
                 trials = 10,
                 # weights = as.numeric(train$Pclass)*train$Fare,
                 control = C5.0Control(subset = TRUE,
                                       winnow = TRUE,
                                       noGlobalPruning = TRUE))
prediction_c5 <- predict(model_c5,
                         test[ ,c(3, 5, 6, 13)])
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_c5)
write.csv(solution, file = 'C5_mod_Solution_V26.csv', row.names = F)

# ----------------------------------------------------------------------------------------
par(mfrow=c(1,1))
# dev.off():break document generation systems like knitr\pandoc
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'rf_mod_Solution_V7.csv', row.names = F)
# -----------------------------------------------------------------------------------------
# boosting
ad_model <- boosting(Survived ~ Sex + Age + family_size,
                     data = train,
                     mfinal = 135,
                     coeflearn = 'Breiman')
prediction <- predict(ad_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction$class)
write.csv(solution, file = 'ad_mod_Solution_V16.csv', row.names = F)
# -----------------------------------------------------------------------------------------





# variable importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
    mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                           y = Importance, fill = Importance)) +
    geom_bar(stat='identity') +
    geom_text(aes(x = Variables, y = 0.5, label = Rank),
              hjust=0, vjust=0.55, size = 4, colour = 'yellow') +
    labs(x = 'Variables') +
    coord_flip() +
    theme_few()