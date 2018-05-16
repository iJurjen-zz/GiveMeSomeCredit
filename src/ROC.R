library(data.table)
library(tidyverse)

# unbalanced dataset
val.dt <- fread("../dat/val.csv", stringsAsFactors = FALSE)

dim(val.dt[y == 0])
dim(val.dt[y == 1])

val.dt %>% 
  ggplot(aes(y_hat)) +
  geom_density() +
  facet_wrap( ~ y, ncol=2)

x <- c()
y <- c()
for (i in seq(0, 1, 0.1)){
  TN <-  dim(val.dt[y == 0 & y_hat < i])[1]
  FP <-  dim(val.dt[y == 0 & y_hat >= i])[1]
  FN <-  dim(val.dt[y == 1 & y_hat < i])[1]
  TP <-  dim(val.dt[y == 1 & y_hat >= i])[1]
  Sensitivity <-  TP / (FN + TP)
  Specificity <-  TN / (TN + FP)
  x <- c(x, 1 - Specificity)
  y <- c(y, Sensitivity)
}

plot(x, y, type="l")


# balanced dataset
val.dt <- fread("../dat/val2.csv", stringsAsFactors = FALSE)

dim(val.dt[y == 0])
dim(val.dt[y == 1])

val.dt %>% 
  ggplot(aes(y_hat)) +
  geom_density() +
  facet_wrap( ~ y, ncol=2)

x <- c()
y <- c()
for (i in seq(0, 1, 0.1)){
  TN <-  dim(val.dt[y == 0 & y_hat < i])[1]
  FP <-  dim(val.dt[y == 0 & y_hat >= i])[1]
  FN <-  dim(val.dt[y == 1 & y_hat < i])[1]
  TP <-  dim(val.dt[y == 1 & y_hat >= i])[1]
  Sensitivity <-  TP / (FN + TP)
  Specificity <-  TN / (TN + FP)
  x <- c(x, 1 - Specificity)
  y <- c(y, Sensitivity)
}

plot(x, y, type="l")
