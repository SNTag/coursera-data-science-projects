## ----self_running, eval=FALSE, include = FALSE--------------------------------
## knitr::purl("./prediction-assignment.Rmd")
## source("./prediction-assignment.R")


## ----loading_data, echo = F, eval = T-----------------------------------------
library(tidyverse)
library(magrittr)

df_train <- read_csv("./data/pml-training.csv")
df_test  <- read_csv("./data/pml-testing.csv")

df_train$classe <- df_train$classe %>% as.factor

set.seed("1701")


## ----checking_for_NAs, echo = F, eval = T-------------------------------------
missing_values <- df_train %>% summarize_all(funs(sum(is.na(.))/dim(df_train)[1]))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
    coord_flip()+theme_bw()


## ----numeric_sorting, echo = F, eval = T--------------------------------------
df_train_logic <- sapply(df_train, is.numeric)
df_train_num  <- df_train[ ,df_train_logic]

## ----corr_analysis------------------------------------------------------------
correlations <- cor(df_train_num, df_train$classe)
bestCorrelations <- subset(as.data.frame(as.table(correlations)), abs(Freq)>0.3)
bestCorrelations

