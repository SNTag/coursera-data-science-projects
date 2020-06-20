## ----self_running, eval=FALSE, include = FALSE--------------------------------
## knitr::purl("./prediction-assignment-completecase.Rmd")
## source("./prediction-assignment-completecase.R")


## ----loading_data, echo = T, eval = T, warning = F, message = F---------------
library(tidyverse)
library(magrittr)
library(caret)
library(doParallel)

df_train <- read_csv("./data/pml-training.csv")
df_test  <- read_csv("./data/pml-testing.csv")

df_train$classe <- df_train$classe %>% as.factor

set.seed("1701")


## ----checking_for_NAs, echo = T, eval = T, fig.align = "center", fig.cap = "Percentage of rows per column that are NA."----
missing_values <- df_train %>% summarize_all(funs(sum(is.na(.))/dim(df_train)[1]))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>%
    ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
    geom_bar(stat="identity",fill="red")+
    coord_flip()+theme_bw()


## ----removing_NAs, echo = T, eval = T-----------------------------------------
missing_values_logic <- sapply(df_test, function(x) any(is.na(x)))
df_train_cc          <- df_train[,!missing_values_logic]
df_train_cc$classe   <- df_train$classe %>% as.factor


## ----data_split, echo = T, eval = T-------------------------------------------
trainIndex <- caret::createDataPartition(df_train_cc$classe, p = .8,
                                  list = FALSE,
                                  times = 1)
df_train_cc80 <- df_train_cc[trainIndex,]
df_train_cc20 <- df_train_cc[-trainIndex,]


## ----classification_rf, echo = T, eval = T------------------------------------
if (exists("rf_fit")) {
    rf_fit <- readRDS("fit.rds")
} else {
cl <- makePSOCKcluster(5)
    registerDoParallel(cl)

    fitControl <- caret::trainControl(method = "cv",
                                      number = 10)
    rf_fit <- caret::train(classe~.,
                           data=df_train_cc80,
                           method="rf",
                           metric="Kappa",
                           trControl=fitControl)

    stopCluster(cl)
    saveRDS(rf_fit, file = "fit.rds")
}

## ----classification_gbm, echo = T, eval = T-----------------------------------
if (!exists("gbm_fit")) {
    rf_fit <- readRDS("fit.rds")
} else {
cl <- makePSOCKcluster(5)
    registerDoParallel(cl)

fitControl <- caret::trainControl(method = "cv",
                                      number = 10)

gbm_fit <- train(classe ~ .,
                 data = df_train_cc80,
                 method = "gbm",
                 metric = "Kappa",
                 trControl = fitControl,
                 verbose = FALSE)

    stopCluster(cl)
    saveRDS(rf_fit, file = "gbm.rds")
}


## ----model_summary, echo = T, eval = T----------------------------------------
rf_fit
gbm_fit

library(lattice)
resample_result <- resamples(list(rf = rf_fit, gbm = gbm_fit))
summary(resample_result)


## ----rf_model_summary, echo = T, eval = T-------------------------------------
results <- c()
results$predicted <- predict(rf_fit,newdata = df_train_cc20)
results$classe <- df_train_cc20$classe
results <- as.data.frame(results)
print("number of incorrectly predicted rows:")
length(which(results$predicted != results$classe))

confusionMatrix(df_train_cc20$classe, predict(rf_fit, newdata = df_train_cc20))


## ----final_model, echo = T, eval = T------------------------------------------
finalResults <- predict(rf_fit, newdata = df_test)
print(finalResults)

