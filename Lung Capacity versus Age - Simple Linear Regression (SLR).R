# Importing Dataset
dataset = read.csv("LungCapData.csv")

# Slicing Data
LungCap = dataset [,1]
Age = dataset [,2]

# Dataset for SLR
data = data.frame(LungCap, Age)

# Importing Data Splitting library
library(caTools)

# Generating Random Numbers
set.seed(42)

# Data Splitting
split = sample.split(data$Age, SplitRatio = 0.8)

# Training data
training_set = subset(data, split==TRUE)
View(training_set)

# Testing data
testing_set = subset(data, split==FALSE)
View(testing_set)

# Applying Regression
regressor = lm(formula = Age~LungCap, data = training_set)

# Prediction
y_pred = predict(regressor, newdata = testing_set)
View(y_pred)

# Importing ggplot library
library(ggplot2)

# Visualization
plot(testing_set$LungCap, testing_set$Age, type = 'p', 
     col = 'blue', xlab = 'LUNG_CAPACITY', ylab = 'AGE')

lines(testing_set$LungCap, y_pred, type = 'o', col = 'red')

# Visualization
ggplot(geom_point(aes(x = testing_set$LungCap, y = training_set$Age,
                      color = "red"))
       + geom_line(aes(x = training_set$LungCap, y = y_pred, 
                       color = "blue")) 
       + ggtitle("Lung Capacity versus Age") 
       + xlab("LungCap") + ylab("Age")
)
