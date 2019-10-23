
#----------- the data cleaning process
# read data from csv file
training.data.raw <- read.csv('logistic-regression/train.csv',header=T,na.strings=c(""))

# check missing values and how many vaules are there in each column
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))

# shows graph of missing values vs observed
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")

# select only relevant columns
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

# taking care of missing values
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm=T)

contrasts(data$Sex)
contrasts(data$Embarked)

# cleaning data
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

#-----------------------model fitting
# split data into 2 chunks named "train" and "test"
train <- data[1:800,]
test <- data[801:889,]

# make the model
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

# summary the model
summary(model)

