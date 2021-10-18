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

# If we wanted to we could impute values for the NAs using a random forest
# or some other method. However, in this example we will just remove them.

nrow(data)

data <- data[!(is.na(data$ca) | is.na(data$thal)),]

nrow(data)

# Total samples : 297

# Now we need to make sure that Y = 0 and Y = 1 are seen in every X variable
# possible value

xtabs(~ hd + sex, data=data)   # Ok

xtabs(~ hd + cp, data=data)  # ok

xtabs(~ hd + fbs, data=data)  # OK

xtabs(~ hd + restecg, data=data)  # only 4 patients in "restecg" = 1

xtabs(~ hd + exang, data=data)  # Ok

xtabs(~ hd + slope, data=data)  # ok

xtabs(~ hd + ca, data=data) # ok

xtabs(~ hd + thal, data=data)  # OK



# Simple logistic regression

logistic <- glm(hd ~ sex, data=data, family="binomial")

summary(logistic)  # Individual significance. Wald Test. p-value
                   # Estimated coefficients

# Multiple logistic regression

logistic2 <- glm(hd ~ ., data=data, family="binomial")

summary(logistic2)  # Individual significance. Wald Test. p-value
                    # Estimated coefficients


# Calculate "Pseudo R-squared and its p-value

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))



# Graph with predicted probabilities for Y = 0, Y = 1, and their actual
# values

predicted.data <- data.frame(
  probability.of.hd=logistic2$fitted.values,
  hd=data$hd)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.hd, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")


# We see that for Y = 1 most patients are predicted to have a high probability
# of being Y = 1. And the same for Y = 0.


ggsave("heart_disease_probabilities.pdf")


