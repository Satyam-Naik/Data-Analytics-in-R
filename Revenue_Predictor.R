# Import the dataset
setwd("D:/R codes/DA Project")
sales <- read.csv("revenue.csv",header = T)
head(sales) #Displays the top 6 rows of a dataset
summary(sales) #Gives certain statistical information about the data
dim(sales) # Displays the dimensions of the dataset

plot(sales) # Plot the variables to see their trends

library(corrplot) # Library to finds the correlation between the variables
num.cols<-sapply(sales, is.numeric)
num.cols
cor.data<-cor(sales[,num.cols])
cor.data
corrplot(cor.data, method='color')

# Split the data into training and testing
set.seed(2)
library(caTools) #caTools has the split function 
split <- sample.split(sales, SplitRatio = 0.7)
split
train2 <- subset(sales, split = 'TRUE') #Creating a training set 
test2 <- subset(sales, split = 'FALSE') #Creating a testing set by assigning FALSE
head(train2)
head(test2)
View(train2)
View(test2)

#training the model
Model <- lm(Profit ~., data = train2) #Creates the model.
summary(Model) 

#prediction
pred <- predict(Model, test2) #The test2 data was kept for this purpose
pred #This displays the predicted values
res<-residuals(Model) # Find the residuals
res<-as.data.frame(res) # Convert the residual into a dataframe
res # Prints the residuals

# compare the predicted vs actual values
results<-cbind(pred,test2$Profit)
results
colnames(results)<-c('predicted','real')
results<-as.data.frame(results)
head(results)

# Let’s now, compare the predicted vs actual values
plot(test2$Profit, type = 'l', lty = 1.8, col = "red")
lines(pred, type = "l", col = "blue") #plot our test revenue
plot(pred, type = "l", lty = 1.8, col = "blue") #plot the prediction fully

#Calculating the accuracy
rmse <- sqrt(mean(pred-sales$Profit)^2) # Root Mean Square Error is the standard deviation of the residuals
rmse

# Function to Predict Profit based on User Inputs
predictProfit <- function(paid, organic, social) {
  # Create a data frame with user inputs
  input_data <- data.frame(Paid = paid, Organic = organic, Social = social)
  #print(input_data)
  # Use the trained model to predict profit
  predicted_profit <- predict(Model, newdata = input_data)
  
  # Return the predicted profit
  return(predicted_profit)
}

# Example Usage:
# Let users input values for "paid," "organic," and "social"
paid_input <- 150000
organic_input <- 97000
social_input <- 471784

# Predict profit based on user inputs
predicted_profit <- predictProfit(paid_input, organic_input, social_input)

# Display the predicted profit
cat("Predicted Profit:", predicted_profit, "\n")









