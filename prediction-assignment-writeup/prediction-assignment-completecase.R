## ----self_running, eval=FALSE, include = FALSE--------------------------------
## knitr::purl("./prediction-assignment-completecase.Rmd")
## source("./prediction-assignment-completecase.R")


## ----loading_data, echo = T, eval = T, warning = F, message = F---------------
pacman::p_load(tidyverse,
               magrittr,
               caret,
               doParallel)

train <- read_csv("./data/pml-training.csv")
test  <- read_csv("./data/pml-testing.csv")

train$classe <- train$classe %>% as.factor

set.seed("1701")


## ----checking_for_NAs, echo = T, eval = T, fig.align = "center", fig.cap = "Percentage of rows per column that are NA."----
missing.values <- apply(train, 2, function(x) mean(is.na(x))) > 0.95
train          <- train[, -which(missing.values, label == FALSE)]
test           <- test[, -which(missing.values, label == FALSE)]


missing.values.count <- train %>%
    dplyr::summarize_all(funs(sum(is.na(.))/dim(train)[1]))
missing.values.count <-
    gather(missing.values.count, key="feature", value="missing_pct")
missing.values.count %>%
    ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
    geom_bar(stat="identity",fill="red")+
    coord_flip()+theme_bw()


## ----handling_near_zero_var---------------------------------------------------
dim(train)
NZV   <- nearZeroVar(train)
train <- train[ ,-NZV]
test  <- test[ ,-NZV]

train <- train[ , -(1:5)]
test <- test[ , -(1:5)]


## ----removing_NAs, echo = T, eval = T-----------------------------------------
missing.values.logic <- sapply(test, function(x) any(is.na(x)))
train.cc         <- train[,!missing.values.logic]
train.cc$classe   <- train$classe %>% as.factor


## ----data_split, echo = T, eval = T-------------------------------------------
trainIndex <- caret::createDataPartition(train.cc$classe,
                                         p = .8,
                                         list = FALSE,
                                         times = 1)
train.cc80 <- train.cc[trainIndex,]
train.cc20 <- train.cc[-trainIndex,]


## ----classification_rf, echo = T, eval = T------------------------------------
#if (exists("rf_fit")) {
#    rf.fit <- readRDS("fit.rds")
#} else {
    cl <- makePSOCKcluster(5)
    registerDoParallel(cl)

    fitControl <- caret::trainControl(method = "cv",
                                      number = 10)
    rf.fit <- caret::train(classe~.,
                           data=train.cc80,
                           method="rf",
                           metric="Kappa",
                           trControl=fitControl)

    stopCluster(cl)
#    saveRDS(rf.fit, file = "fit.rds")
#}

## ----classification_gbm, echo = T, eval = T-----------------------------------
#if (!exists("gbm.fit")) {
#    gbm.fit <- readRDS("gbm.rds")
#} else {
cl <- makePSOCKcluster(5)
    registerDoParallel(cl)

fitControl <- caret::trainControl(method = "cv",
                                      number = 10)

gbm.fit <- train(classe ~ .,
                 data = train.cc80,
                 method = "gbm",
                 metric = "Kappa",
                 trControl = fitControl,
                 verbose = FALSE)

    stopCluster(cl)
#    saveRDS(rf.fit, file = "gbm.rds")
#}


## ----model_summary, echo = T, eval = T----------------------------------------
rf.fit
#gbm.fit

library(lattice)
resample.result <- caret::resamples(list(rf = rf.fit, gbm = gbm.fit))
summary(resample.result)


## ----rf_model_summary, echo = T, eval = T-------------------------------------
results <- c()
results$predicted <- predict(rf.fit,newdata = train.cc20)
results$classe <- train.cc20$classe
results <- as.data.frame(results)
print("number of incorrectly predicted rows:")
length(which(results$predicted != results$classe))

caret::confusionMatrix(train.cc20$classe, predict(rf.fit, newdata = train.cc20))


## ----final_model, echo = T, eval = T------------------------------------------
finalResults <- predict(rf.fit, newdata = test)
print(finalResults)

