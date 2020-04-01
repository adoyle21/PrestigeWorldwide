library(class)
library(tidyverse)
library(caret)
library(heuristica)


allames <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE, sep = ",")

amesfireplaces <- subset(ameslist, select = -c(Alley))
ameswithout <- subset(ameslist, select = -c(Fireplaces,Alley))

amesfireplaces[is.na(amesfireplaces)] = 0
ameswithout[is.na(ameswithout)] = 0

set.seed(9)

# With Fireplaces
num_obs_with = nrow(amesfireplaces)

with_train_index = sample(num_obs_with, size = trunc(0.50 * num_obs_with))
with_train_data = amesfireplaces[with_train_index, ]
with_test_data = amesfireplaces[-with_train_index, ]

knn(train_data, test_data, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

# Without Fireplaces
num_obs_without = nrow(ameswithout)

without_train_index = sample(num_obs_without, size = trunc(0.50 * num_obs_without))
without_train_data = amesfireplaces[without_train_index, ]
without_test_data = amesfireplaces[-without_train_index, ]

knn(train_data, test_data, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

confusionMatrix(with_train_data, with_test_data)

