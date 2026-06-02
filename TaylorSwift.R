#R does not care about whitespace
#There are two data strcutures in R: vectors, which represent a column (in other words, only one data type), and data frames, which represent tables with both rows and columns.
#Ctrl + Enter runs a line of code in RStudio, can also click the run button or highlight a portion of text and run or click ctrl+enter.
library(dplyr)
library(randomForest)

#Read in data
#No using hyphens for variable names: R thinks its a minus sign
swift_data <- read.csv(file = "data/swift-data.csv")

#show first few rows of data
head(swift_data)

#Run a linear model with all predictors
linear_model <- lm(peak_position ~ ., data = swift_data)

rf_model <- randomForest(peak_position ~ ., data = swift_data)

#predict song's peak position based on linear model
linear_prediction <- predict(linear_model, newdata = swift_data)

#predict song's peak position based on random forest model
rf_prediction <- predict(rf_model, newdata = swift_data)

#Root Mean Square Error:
#Error is the difference between the actual and predicted values
#You square to get rid of negatives and root it to remove the square

#Calculate squared errors for linear model
linear_sqerr <- (swift_data$peak_position - linear_prediction)^2

#Calculate RMSE for linear model (by taking the mean of the squares above and square rooting it to undo the squared we did)
linear_rmse <- sqrt(mean(linear_sqerr))

#Calculate square errors for random forest model
rf_sqerr <- (swift_data$peak_position - rf_prediction)^2

#Calculate RMSE for random forest model
rf_rmse <- sqrt(mean(rf_sqerr))

#Difference between linear model and random forest model
#linear model is a linear regression
#Random forest is tree based method where it determines which value has the biggest effect, then it asks which one has the next biggest effect, kind of like ranking

linear_rmse #26.48797 means on average, our model was off by about 26 spots on average

rf_rmse #random forest has a little bit of randomness, so this value's decimal places might be different each time

#The random forest seems better because it has "memorized" the data, so it may not be good for new data. It does not have generalizable knowledge. To test models, don't train it on 20% of the data and test it on that. This should be done 5 times so that every piece of data has the opportunity to be in the testing data cohort. Training data vs testing data (vocab terms).

#overwriting
#This is saying that there are 5 slots/bins because we need to do this 5 times.
linear_rmse <- numeric(5)
rf_rmse <- numeric(5)

#Create bin vector
bins <- rep(x = 1:5, length.out = nrow(swift_data)) #repeats something a certain number of times, the second one is how many times we wanted it to repeat

#Build and evaluate models 5 times
for (i in 1:5) {
  print(i)
  #Split data into training/testing
  training <- swift_data %>% filter(bins != i) # %>% is called the pipe
  #The pipe sends what is immediately to the left to what is on the right/on the next line
  testing <- swift_data %>% 
    filter(bins == i)
  #Estimate model with training data
  # the dot means "all the other variables that are in our data set
  linear_model <- lm(peak_position ~ ., data = training)
  rf_model <- randomForest(peak_position ~ ., data = training)
  #Make predictions based on test data
  linear_prediction <- predict(linear_model, newdata = testing)
  rf_prediction <- predict(rf_model, newdata = testing)
  #Calculate RMSE value
  linear_sqerr <- (testing$peak_position - linear_prediction)^2
  linear_rmse[i] <- sqrt(mean(linear_sqerr))
  rf_sqerr <- (testing$peak_position - rf_prediction)^2
  rf_rmse[i] <- sqrt(mean(rf_sqerr))
  
  # type in ?predict to find out the documentation for the function predict
  #predict behaves differently with different models, if you look at the definition, it specifies the first parameter, and then the rest of the parameters are represented by ... If you pass in an argument(s) in the order that it expects, you don't need to specify using "parameter = ". Otherwise, you do, such as when it behaves differently with different models.
  #put the cursor at the beginning of the first { or the end of the last } to run the for loop all at once, or else it will just run a single line!
}

linear_rmse #model is always worse with new data
mean(linear_rmse)

mean(rf_rmse) #shows how different it is from when we tested it on the same data set as the training dataset. This is known as overfitting.
#So our objective was to find the best model by comparing models, and we found out that RF is slightly better than linear for our needs.

#Build model with all data
full_model <- randomForest(peak_position ~ ., data = swift_data)

#https://bit.ly/swift-new
#You can only have letters, numbers, underscores, and dots for variable names, but they can't start with a number
#We tend not to use dots because people coming from other programming languages are used to it being an operator that accesses class members
new_album <- read.csv(file = "data/swift-new.csv") 

#Make peak position prediction for new album
new_predict <- predict(full_model, newdata = new_album)
songs_predict <- data.frame(track_num = 1:12, #1:12 creates a list of integers from 1 through 12. Try typing "1:12" in the console.
                            peak_position = new_predict,
                            track_name = new_album$track_names) #makes a new data object
songs_predict

songs_predict %>% arrange(peak_position)

#The tilde tells us that the right side is the independent variables and the left side is the dependent variable (what we are trying to predict). We are using the things on the right to predict the things on the left.
#We use bins because it saves us the calculations when determining what is 20% and 80% of 157. It is also for ease of communication: you say "assign to a hole (what a bin is called)" widely in machine learning.
#The red circle with a white x next to the line number represents  syntax errors
#in ML, we don't care how things work, we just need a model that makes predictions well, which is a contrast to inferential statistics.