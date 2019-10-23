# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("readxl")

library(rpart)
library(rpart.plot)
library(readxl)

my_data = read_excel("R Data.xlsx")

cols <- c(2:5, 7:16, 19)
my_data[,cols] <- data.frame(apply(my_data[cols], 2, as.factor))
my_data$TotalCharges[is.na(my_data$TotalCharges)] <- 0

my_data <- my_data[, c(2:ncol(my_data))]

fit1 <- rpart(Churn ~ ., data = my_data, method = "anova")
fit2 <- rpart(Churn ~ ., data = my_data, method = "class")

rpart.plot(fit1, type=3, digits=3, fallen.leaves=TRUE)
rpart.plot(fit2)