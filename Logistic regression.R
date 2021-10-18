# Logistic Regression

library(ggplot2)
library(cowplot)

# Load the data

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header=FALSE)

# Manipulate the data

# Change columns names

colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
  )

# Change columns data structures

str(data)    # "Sex" is a number, but it is supposed to be a factor
             # "cp" (chest pain) is a number, but it is supposed to be a factor
             # "ca" and "thal" are correctly called factors, but one of the 
             # levels is "?" when we need it to be "NA"
             # Other columns numbers and are supposed to be factors


data[data == "?"] <- NA

data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)

data$fbs <- as.factor(data$fbs)

data$restecg <- as.factor(data$restecg)

data$exang <- as.factor(data$exang)

data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd)


# Check for NA values

nrow(data[is.na(data$ca) | is.na(data$thal),])  # 6 total Na values

data[is.na(data$ca) | is.na(data$thal),]  # Samples: 88, 167, 193, 267, 288, 303





