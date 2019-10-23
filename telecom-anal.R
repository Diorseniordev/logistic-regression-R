
#------- install necessary packages -------

# install.packages("ggplot2")
# install.packages("party")
# install.packages("readxl")
# install.packages("funModeling")
# install.packages("cowplot")
# install.packages("rpart")
# install.packages("rpart.plot")

#----------- import data ---------------

# load readxl package
library("readxl")

# read data
my_data <- read_excel("R Data.xlsx")

#----------------- EDA -----------------

# load libraries
library(funModeling)
library(tibble)

# show the underlying data
glimpse(my_data)

# convert SeniorCitizen into categorical variable
my_data$SeniorCitizen <- as.factor(my_data$SeniorCitizen)

# replace na values from TotalCharges into zeros
my_data[is.na(my_data)] <- 0

# shows the metrics about data types, zeros, infinite numbers, and missing values
df_status(my_data)

# analyzing categorical variables
cols <- c(2:5, 7:16, 19)
category_data <- my_data[cols]
freq(category_data)

# analyzing numerical variables
profiling_num(my_data)
plot_num(my_data)

# summary of the data variables
describe(my_data)

#----------- logistic regression ------

# first convert category variables into factors
my_data[,cols] <- data.frame(apply(my_data[cols], 2, as.factor))

# replace NA values with 0
my_data$TotalCharges[is.na(my_data$TotalCharges)] <- 0

# create a logistic regression model
logit_model <- glm(Churn ~ gender + SeniorCitizen + HasPartner + HasDependents +
                     PhoneService + MultipleLines + InternetService + OnlineSecurity +
                     OnlineBackup + DeviceProtection + TechSupport + Contract +
                     PaperlessBilling + PaymentMethod + TenureInMonths + MonthlyCharges +
                     TotalCharges, family = "binomial", data = my_data)

# output the summary of the model created
summary(logit_model)

# calculate the confidence intervals for the coefficient estimates
confint(logit_model)

# change in deviance
with(logit_model, null.deviance - deviance)

# change in degrees of freedom
with(logit_model, df.null - df.residual)

# chi square test p-value
with(logit_model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# likelihood ratio test
logLik(logit_model)

#------------ test the model created --------

# predict data
predicted.data <- data.frame(
  probability.of.Churn = logit_model$fitted.values,
  Churn = my_data$Churn)

# sort predicted data
predicted.data <- predicted.data[
  order(predicted.data$probability.of.Churn, decreasing = FALSE), ]

# add new column to the data.frame that has the rank of each sample
predicted.data$rank <- 1:nrow(predicted.data)

# load libraries
library(ggplot2)
library(cowplot)

# draw the predicted data
ggplot( data = predicted.data, aes(x = rank, y = probability.of.Churn) ) +
  geom_point( aes(color = Churn), alpha = 1, shape = 4, stroke = 2 ) +
  xlab("Index") + 
  ylab("Predicted probability of leaving service")

# output the result in pdf
ggsave("Churn probability.pdf")

#-------------- regression tree model ---------
library(rpart)
library(rpart.plot)

# remove the first column since customerID is just an index not an important variable
my_data <- my_data[, c(2:ncol(my_data))]

fit <- rpart(Churn ~ ., data = my_data, method = "class")

rpart.plot(fit, type=5, digits=3, fallen.leaves=TRUE)
