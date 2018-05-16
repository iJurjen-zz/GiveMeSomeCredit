library(data.table)
library(caret)
library(RANN)
library(dplyr)

# Read the training data
data <- fread("../dat/cs-training.csv", stringsAsFactors = FALSE)

# Changing to shorter column names and change order
setnames(data,
         c("id", "SD", "RUUL", "Age", "no3059", "DebtRatio", "Income", "OpenCredit", "no90", "REloans", "no6089", "Dep"))
data = data[, .(SD, Age, RUUL, DebtRatio, Income, OpenCredit, REloans, Dep, no3059, no6089, no90)]
summary(data)

# Replace age==0 & income==0 with median
median_Age = as.integer(median(data$Age)) # 52
median_Income = as.integer(median(data$Income, na.rm=TRUE)) # 5400
data$Age[data$Age == 0] <- median_Age
data$Income[data$Income == 0] <- median_Income

# Replace outliers
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

RUULmax <- boxplot.stats(data$RUUL)$stats[5] # 1.35
outlierReplace(data, "RUUL", which(data$RUUL > RUULmax), RUULmax)

DebtRatio_max <- boxplot.stats(data$DebtRatio)$stats[5] # 1.9
median_DebtRatio = median(data$DebtRatio) # 0.4
outlierReplace(data, "DebtRatio", which(data$DebtRatio > DebtRatio_max), median_DebtRatio)

IncomeStats_max <- boxplot.stats(data$Income)$stats[5] # 15500
outlierReplace(data, "Income", which(data$Income > IncomeStats_max), IncomeStats_max)

outlierReplace(data, "REloans", which(data$REloans > 20), 20)
outlierReplace(data, "Dep", which(data$Dep > 10), 10)
outlierReplace(data, "no3059", which(data$no3059 > 20), 0)
outlierReplace(data, "no6089", which(data$no6089 > 20), 0)
outlierReplace(data, "no90", which(data$no90 > 20), 0)

# Impute NA's Dep with median
data$Dep[is.na(data$Dep)] <- 0

# Writing to file for doing regression on the NA values in Income
# See GiveMeSomeCredit.ipynb
fwrite(data, file = "../dat/Train1.csv")

# Reading back the imputed file
data <- fread("../dat/Train2.csv", stringsAsFactors = FALSE)

# Feature engineering
data$LateDays <- data[, no3059] * 30 + data[, no6089] * 60 + data[, no90] * 90
data$Debt <- data[, DebtRatio] * data[, Income]
data$Slack <- data[,Income] - data[,Debt]

# Looking at correlation matrix
M <- cor(data, use = "pairwise.complete.obs")
round(M,2)

# Removing correlated variables
data <- data[, -c("no3059", "no6089", "no90", "DebtRatio", "Debt")]
summary(data)

# Centralize & Scale
trans = preProcess(data[, -c("SD")], method=c("center", "scale"))
data_normalized = predict(trans, data)
data_normalized[, SD:= data[, SD]]
# Writing to file
fwrite(data_normalized, file = "../dat/Train3.csv")

# Balance training set
data_split <- split(data_normalized, as.factor(data_normalized$SD))
SD0 <- data_split$'0'
SD1 <- data_split$'1'
n <- nrow(SD1)
set.seed(42)
SD0_sample10k <- sample_n(SD0, n)
data_balanced <- rbind(SD0_sample10k, SD1)
data_balanced <- data_balanced[sample(2*n), ]
fwrite(data_balanced, file = "../dat/Train4.csv")



# Read the tests data
data <- fread("../dat/cs-test.csv", stringsAsFactors = FALSE)

# Changing to shorter column names and change order
setnames(data,
         c("id", "SD", "RUUL", "Age", "no3059", "DebtRatio", "Income", "OpenCredit", "no90", "REloans", "no6089", "Dep"))
data = data[, .(id, Age, RUUL, DebtRatio, Income, OpenCredit, REloans, Dep, no3059, no6089, no90)]
summary(data)

# Replace age==0 & income==0 with median
data$Age[data$Age == 0] <- median_Age
data$Income[data$Income == 0] <- median_Income

# Replace outliers
outlierReplace(data, "RUUL", which(data$RUUL > RUULmax), RUULmax)
outlierReplace(data, "DebtRatio", which(data$DebtRatio > DebtRatio_max), median_DebtRatio)
outlierReplace(data, "Income", which(data$Income > IncomeStats_max), IncomeStats_max)
outlierReplace(data, "REloans", which(data$REloans > 20), 20)
outlierReplace(data, "Dep", which(data$Dep > 10), 10)
outlierReplace(data, "no3059", which(data$no3059 > 20), 0)
outlierReplace(data, "no6089", which(data$no6089 > 20), 0)
outlierReplace(data, "no90", which(data$no90 > 20), 0)

# Impute NA's in Dep with median
data$Dep[is.na(data$Dep)] <- 0

# Writing to file for doing regression on the NA values in Income
fwrite(data, file = "../dat/Test1.csv")

# Reading back the imputed file
data <- fread("../dat/Test2.csv", stringsAsFactors = FALSE)

# Feature engineering
data$LateDays <- data[, no3059] * 30 + data[, no6089] * 60 + data[, no90] * 90
data$Debt <- data[, DebtRatio] * data[, Income]
data$Slack <- data[,Income] - data[,Debt]

# Removing correlated variables
data <- data[, -c("no3059", "no6089", "no90", "DebtRatio", "Debt")]
summary(data)

# Centralize & Scale
trans = preProcess(data[, -c("id")], method=c("center", "scale"))
data_normalized = predict(trans, data)
data_normalized[, id:= data[, id]]
# Writing to file
fwrite(data_normalized, file = "../dat/Test3.csv")
