# Installing packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(mltools)) install.packages("mltools", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(modelr)) install.packages("modelr", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(rsample)) install.packages("rsample", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(ISLR)) install.packages("ISLR", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(yardstick)) install.packages("yardstick", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos='http://cran.us.r-project.org')
if(!require(mlbench)) install.packages("mlbench", repos='http://cran.us.r-project.org')

defaultW <- getOption("warn")
options(warn = -1)


# Importing Libraries
library(tidyverse)
library(caret)
library(MASS)
library(mltools)
library(data.table)
library(modelr)
library(broom)
library(rsample)
library(magrittr)
library(ranger)
library(ISLR)
library(car)
library(pROC)
library(psych)
library(gridExtra)
library(corrplot)
library(RColorBrewer)
library(plotly)
library(yardstick)
library(GGally)
library(Metrics)
library(randomForest)
library(mlbench)
library(ggplot2)

# Loading the dataset
df_utility <- read_csv('FIT5149_Assessment1.csv')

# Displaying the first few rows to understand the dataset
head(df_utility)
tail(df_utility)

# Displaying the Structure of the dataset
str(df_utility)

# Displaying the summary of the dataset
summary(df_utility)

# Advanced Descriptive Statistics of the dataset
describe(df_utility)

# Month variable must be converted into a factor
df_utility$month <- as.factor(df_utility$month)

# Checking for missing values in each column of the dataset
cbind(lapply(lapply(df_utility, is.na), sum))

df_utility[df_utility$num_rooms<=0,]

# Imputing the num_rooms with -1 or 0 values to the median value
df_utility[df_utility$num_rooms<=0,]$num_rooms <- 2

df_utility[df_utility$num_people==0|df_utility$num_people==-1,]

# Imputing the num_people with -1 or 0 values to the median value
df_utility[df_utility$num_people<=0,]$num_people <- 5

# Checking for negative value in ave_monthly_income
df_utility[df_utility$ave_monthly_income<0,]

# Imputing the ave_monthly_income with negative values to the median value
df_utility[df_utility$ave_monthly_income<=0,]$ave_monthly_income <- 12371.285

df_utility[df_utility$num_people<df_utility$num_children,]

df_utility[df_utility$num_people<df_utility$num_children,]$num_people <- df_utility[df_utility$num_people<df_utility$num_children,]$num_people + df_utility[df_utility$num_people<df_utility$num_children,]$num_children

df_utility[df_utility$num_people<df_utility$num_children,]

df_utility[df_utility$num_people==df_utility$num_children,]

df_utility[df_utility$num_people==df_utility$num_children,]$num_people <- df_utility[df_utility$num_people==df_utility$num_children,]$num_children + 1

df_utility[df_utility$num_people==df_utility$num_children,]

# Setting seed
set.seed(1234)

# Shuffling the dataset before splitting the data into training and testing dataset
df_utility <- df_utility[sample(1:nrow(df_utility), replace = TRUE),] 

# Splitting the dataset into training and testing dataset with a ratio of 80:20
df_train <- df_utility[1:800,]
df_test <- df_utility[801:1000,]

df_train_data <- df_train[,-11]
df_train_target <- df_train[,11]

# Splitting the testing dataset into data and target variables
df_test_data <- df_test[,-11]
df_test_target <- df_test[,11]


# Advanced Descriptive Statistics of the training dataset
round(describe(df_train),3)

# Function to change the size of the plot
fig <- function(width, height){
     options(repr.plot.width = width, repr.plot.height = height)
}

# Reshaping the dataframe for plotting
m1 <- melt(as.data.frame(df_train[c(-4,-5,-6,-9)]))

# Boxplots for the continuous and discrete variables
ggplot(m1,aes(x = variable,y = value)) +
  facet_wrap(~variable, scales="free") +
  geom_boxplot() +
  scale_y_continuous(labels=function (n) {format(n, scientific=FALSE)}) +
  theme(text = element_text(size = 18))
fig(10,15)


# Using the Pair plot for all the variables
ggpairs(df_train) +
  theme(text = element_text(size = 18))
fig(30,20)

# Checking the correlation between the variables leaving out the categorical variables
round(cor(df_train[c(-4,-5,-6,-9,-10)]),3)

scatterplotMatrix(df_train[c(c(-4,-5,-6,-9,-10))],cex=0.2)
fig(20,15)

# Changing the categorical values from Double to String in the training dataset
df_train$is_ac <- ifelse(df_train$is_ac == 1, 'Yes', 'No')
df_train$is_tv <- ifelse(df_train$is_tv == 1, 'Yes', 'No')
df_train$is_flat <- ifelse(df_train$is_flat == 1, 'Yes', 'No')
df_train$is_urban <- ifelse(df_train$is_urban == 1, 'Yes', 'No')

# Changing the categorical values from Double to String in the testing dataset
df_test_data$is_ac <- ifelse(df_test_data$is_ac == 1, 'Yes', 'No')
df_test_data$is_tv <- ifelse(df_test_data$is_tv == 1, 'Yes', 'No')
df_test_data$is_flat <- ifelse(df_test_data$is_flat == 1, 'Yes', 'No')
df_test_data$is_urban <- ifelse(df_test_data$is_urban == 1, 'Yes', 'No')

a1<-ggplot(aes(x=num_children,y=housearea), data = df_train) +
    geom_density(aes(color=is_tv),stat='summary',fun.y=median)
 
a2<-ggplot(aes(x=num_children,y=amount_paid), data = df_train) +
    geom_density(aes(color=is_tv),stat='summary',fun.y=median)
  
a3<-ggplot(aes(x=housearea,y=amount_paid), data = df_train) +
    geom_jitter(aes(color=is_tv),stat='summary',fun.y=median)

a4<-ggplot(aes(x=ave_monthly_income,y=amount_paid), data = df_train) +
    geom_jitter(aes(color=is_tv),stat='summary',fun.y=median)

a5<-ggplot(aes(x=as.numeric(month),y=amount_paid), data = df_train) +
    geom_density(aes(color=is_tv),stat='summary',fun.y=median)

a6<-ggplot(aes(x=num_people,y=amount_paid), data = df_train) +
    geom_density(aes(color=is_tv),stat='summary',fun.y=median)

grid.arrange(a1,a2,a3,a4,a5,a6,ncol=3)

a1<-ggplot(aes(x=num_children,y=housearea), data = df_train) +
    geom_density(aes(color=is_ac),stat='summary',fun.y=median)
 
a2<-ggplot(aes(x=num_children,y=amount_paid), data = df_train) +
    geom_density(aes(color=is_ac),stat='summary',fun.y=median)
  
a3<-ggplot(aes(x=housearea,y=amount_paid), data = df_train) +
    geom_jitter(aes(color=is_ac),stat='summary',fun.y=median)

a4<-ggplot(aes(x=ave_monthly_income,y=amount_paid), data = df_train) +
    geom_jitter(aes(color=is_ac),stat='summary',fun.y=median)

a5<-ggplot(aes(x=as.numeric(month),y=amount_paid), data = df_train) +
    geom_density(aes(color=is_ac),stat='summary',fun.y=median)

a6<-ggplot(aes(x=num_people,y=amount_paid), data = df_train) +
    geom_density(aes(color=is_ac),stat='summary',fun.y=median)

grid.arrange(a1,a2,a3,a4,a5,a6,ncol=3)

a1<-ggplot(aes(x=num_children,y=housearea), data = df_train) +
    geom_density(aes(color=is_flat),stat='summary',fun.y=median)
 
a2<-ggplot(aes(x=num_children,y=amount_paid), data = df_train) +
    geom_density(aes(color=is_flat),stat='summary',fun.y=median)
  
a3<-ggplot(aes(x=housearea,y=amount_paid), data = df_train) +
    geom_jitter(aes(color=is_flat),stat='summary',fun.y=median)

a4<-ggplot(aes(x=ave_monthly_income,y=amount_paid), data = df_train) +
    geom_jitter(aes(color=is_flat),stat='summary',fun.y=median)

a5<-ggplot(aes(x=as.numeric(month),y=amount_paid), data = df_train) +
    geom_density(aes(color=is_flat),stat='summary',fun.y=median)

a6<-ggplot(aes(x=num_people,y=amount_paid), data = df_train) +
    geom_density(aes(color=is_flat),stat='summary',fun.y=median)

grid.arrange(a1,a2,a3,a4,a5,a6,ncol=3)

a1<-ggplot(aes(x=num_children,y=housearea), data = df_train) +
    geom_density(aes(color=is_urban),stat='summary',fun.y=median)
 
a2<-ggplot(aes(x=num_children,y=amount_paid), data = df_train) +
    geom_density(aes(color=is_urban),stat='summary',fun.y=median)
  
a3<-ggplot(aes(x=housearea,y=amount_paid), data = df_train) +
    geom_jitter(aes(color=is_urban),stat='summary',fun.y=median)

a4<-ggplot(aes(x=ave_monthly_income,y=amount_paid), data = df_train) +
    geom_jitter(aes(color=is_urban),stat='summary',fun.y=median)

a5<-ggplot(aes(x=as.numeric(month),y=amount_paid), data = df_train) +
    geom_density(aes(color=is_urban),stat='summary',fun.y=median)

a6<-ggplot(aes(x=num_people,y=amount_paid), data = df_train) +
    geom_density(aes(color=is_urban),stat='summary',fun.y=median)

grid.arrange(a1,a2,a3,a4,a5,a6,ncol=3)

# Calculating the percentages of 'is_ac' & 'is_tv' to plot
table(df_train$is_ac)/nrow(df_train)*100
a <- table(df_train[,c('is_tv','is_ac')])
apply(a, 1, function(x) x/sum(x) * 100)

# Plotting
barplot(a, main='is_tv vs. is_ac', xlab='Variable', ylab='Count', col = c('red','green'))
legend('topright', c('is_tv','is_ac'), fill = c("red","green"))

# Calculating the percentages of 'is_flat' & 'is_tv' to plot
table(df_train$is_tv)/nrow(df_train)*100
a <- table(df_train[,c('is_tv','is_flat')])
apply(a, 1, function(x) x/sum(x) * 100)

# Plotting
barplot(a, main='is_tv vs. is_flat', xlab='Variable', ylab='Count', col = c('red','green'))
legend('topright', c('is_tv','is_flat'), fill = c("red","green"))

# Calculating the percentages of 'is_urban' & 'is_tv' to plot
table(df_train$is_tv)/nrow(df_train)*100
a <- table(df_train[,c('is_tv','is_urban')])
apply(a, 1, function(x) x/sum(x) * 100)

# Plotting
barplot(a, main='is_tv vs. is_urban', xlab='Variable', ylab='Count', col = c('red','green'))
legend('topright', c('is_tv','is_urban'), fill = c("red","green"))

# Number of household with TV
cat(crayon::bold("Number of households with TV out of 800"))
nrow(df_train[df_train$is_tv=='Yes',])

# Average Utility bill for a household with TV
cat(crayon::bold("Average Utility bill for a household with TV:"))
round(mean(df_train[df_train$is_tv=='Yes',]$amount_paid),2)

# Average Utility bill for a household without TV
cat(crayon::bold("Average Utility bill for a household without TV:"))
round(mean(df_train[df_train$is_tv=='No',]$amount_paid),2)

# Calculating the percentages of 'is_ac' & 'is_flat' to plot
table(df_train$is_ac)/nrow(df_train)*100
a <- table(df_train[,c('is_flat','is_ac')])
apply(a, 1, function(x) x/sum(x) * 100)

# Plotting
barplot(a, main='is_flat vs. is_ac', xlab='Variable', ylab='Count', col = c('red','green'))
legend('topright', c('is_flat','is_ac'), fill = c("red","green"))

# Calculating the percentages of 'is_ac' & 'is_urban' to plot
table(df_train$is_ac)/nrow(df_train)*100
a <- table(df_train[,c('is_urban','is_ac')])
apply(a, 1, function(x) x/sum(x) * 100)

# Plotting
barplot(a, main='is_urban vs. is_ac', xlab='Variable', ylab='Count', col = c('red','green'))
legend('topright', c('is_urban','is_ac'), fill = c("red","green"))

# Number of household with AC
cat(crayon::bold("Number of households with AC out of 800"))
nrow(df_train[df_train$is_ac=='Yes',])

# Average Utility bill for a household with AC
cat(crayon::bold("Average Utility bill for a household with AC:"))
round(mean(df_train[df_train$is_ac=='Yes',]$amount_paid),2)

# Average Utility bill for a household without AC
cat(crayon::bold("Average Utility bill for a household without AC:"))
round(mean(df_train[df_train$is_ac=='No',]$amount_paid),2)

# Calculating the percentages of 'is_urban' & 'is_flat' to plot
table(df_train$is_flat)/nrow(df_train)*100
a <- table(df_train[,c('is_flat','is_urban')])
apply(a, 1, function(x) x/sum(x) * 100)

# Plotting
barplot(a, main='is_flat vs. is_urban', xlab='Variable', ylab='Count', col = c('red','green'))
legend('topright', c('is_flat','is_urban'), fill = c("red","green"))

# Number of household is a flat
cat(crayon::bold("Number of flats out of 800"))
nrow(df_train[df_train$is_flat=='Yes',])

# Average Utility bill for a household is a flat
cat(crayon::bold("Average Utility bill for a flat:"))
round(mean(df_train[df_train$is_flat=='Yes',]$amount_paid),2)

# Average Utility bill for a household without AC
cat(crayon::bold("Average Utility bill for a household that is not a flat:"))
round(mean(df_train[df_train$is_flat=='No',]$amount_paid),2)

# Number of household in urban areas
cat(crayon::bold("Number of households in urban areas out of 800"))
nrow(df_train[df_train$is_ac=='Yes',])

# Average Utility bill for a household in urban areas
cat(crayon::bold("Average Utility bill for a household in urban areas:"))
round(mean(df_train[df_train$is_ac=='Yes',]$amount_paid),2)

# Average Utility bill for a household in country
cat(crayon::bold("Average Utility bill for a household in country-side:"))
round(mean(df_train[df_train$is_ac=='No',]$amount_paid),2)

# Relation between 2 or more categorical variables

# Number of household in urban areas with AC
cat(crayon::bold("Number of households with AC in urban areas out of 800"))
nrow(df_train[df_train$is_ac=='Yes' & df_train$is_urban=='Yes',])

# Average Utility bill for a household with AC in urban areas
cat(crayon::bold("\nAverage Utility bill for a household with AC in urban areas:"))
round(mean(df_train[df_train$is_ac=='Yes' & df_train$is_urban=='Yes',]$amount_paid),2)

# Average Utility bill for a household without AC in urban areas
cat(crayon::bold("\nAverage Utility bill for a household without AC in urban areas:"))
round(mean(df_train[df_train$is_ac=='No' & df_train$is_urban=='Yes',]$amount_paid),2)


# Number of household in urban areas with TV
cat(crayon::bold("\nNumber of households with TV in urban areas out of 800"))
nrow(df_train[df_train$is_tv=='Yes' & df_train$is_urban=='Yes',])

# Average Utility bill for a household with TV in urban areas
cat(crayon::bold("\nAverage Utility bill for a household with TV in urban areas:"))
round(mean(df_train[df_train$is_tv=='Yes' & df_train$is_urban=='Yes',]$amount_paid),2)

# Average Utility bill for a household without TV in urban areas
cat(crayon::bold("\nAverage Utility bill for a household without TV in urban areas:"))
round(mean(df_train[df_train$is_tv=='No' & df_train$is_urban=='Yes',]$amount_paid),2)

# Converting the Categorical Variables from String to Numerical 0 and 1 for the training dataset
df_train$is_tv <- ifelse(df_train$is_tv == 'Yes',1,0)
df_train$is_ac <- ifelse(df_train$is_ac == 'Yes',1,0)
df_train$is_flat <- ifelse(df_train$is_flat == 'Yes',1,0)
df_train$is_urban <- ifelse(df_train$is_urban == 'Yes',1,0)

# Converting the Categorical Variables from String to Numerical 0 and 1 for the testing dataset
df_test_data$is_tv <- ifelse(df_test_data$is_tv == 'Yes',1,0)
df_test_data$is_ac <- ifelse(df_test_data$is_ac == 'Yes',1,0)
df_test_data$is_flat <- ifelse(df_test_data$is_flat == 'Yes',1,0)
df_test_data$is_urban <- ifelse(df_test_data$is_urban == 'Yes',1,0)

train_dummy <- dummyVars('~.', data = df_train)
train_mod <- data.frame(predict(train_dummy, newdata = df_train))
head(train_mod)

test_dummy <- dummyVars('~.', data = df_test_data)
test_mod <- data.frame(predict(test_dummy, newdata = df_test_data))
head(test_mod)

train_mod_val <- train_mod[1:600,]
val_mod <- train_mod[601:800,]

# Splitting the training data to data and target dataframes for cross-validation
train_mod_data <- train_mod[,-ncol(train_mod)]
train_mod_target <- as.data.frame(train_mod[,ncol(train_mod)])

val_mod_data <- val_mod[,-ncol(val_mod)]
val_mod_target <- as.data.frame(val_mod[,ncol(val_mod)])


lm_full_model <- lm(amount_paid ~ ., train_mod_val)
summary(lm_full_model)

par(mfcol=c(2,2)) 
plot(lm_full_model) + theme(text = element_text(size = 20))

plot(lm_full_model,which = 4)

outlierTest(lm_full_model, cutoff=0.05, digits = 1)

influencePlot(lm_full_model, scale=5, id.method="noteworthy", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Predicting the amount_paid for the validation dataset and the train dataset
val_pred <- as.data.frame(round(as.double(predict(lm_full_model,val_mod_data)),3))
val_mod_target <- round(val_mod_target,3)

train_pred <- as.data.frame(round(as.double(predict(lm_full_model,train_mod_data)),3))
train_mod_target <- round(train_mod_target,3)

# Creating a RMSE function to calculate RMSE
rmse_func <- function(data,target){
  error <- data.frame(1:nrow(data))
  for(i in 1:nrow(data)){
    error[i,] <- data[i,] - target[i,]
  }
  rmse_error <- sqrt(colMeans(error**2)/nrow(data))
  return (rmse_error)
}

# Calling the RMSE function
cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - Full Model is for validation dataset: "))
rmse_func(val_pred,val_mod_target)

cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - Full Model is for training dataset: "))
rmse_func(train_pred,train_mod_target)

# Executing the stepwise function AIC to the lm_full_model
step_full_model <- stepAIC(lm_full_model, trace = 0)
summary(step_full_model)

# Predicting the amount_paid for the test dataset
val_pred_step_full_model <- as.data.frame(round(as.double(predict(step_full_model,val_mod_data)),3))
val_mod_target <- round(val_mod_target,3)

train_pred_step_full_model <- as.data.frame(round(as.double(predict(step_full_model,train_mod_data)),3))
train_mod_target <- round(train_mod_target,3)

# Calling the RMSE function
cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - Step-wise for Full Model for the validation set is: "))
rmse_func(val_pred_step_full_model,val_mod_target)

cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - Step-wise for Full Model for the training set is: "))
rmse_func(train_pred_step_full_model,train_mod_target)

sig_param_model <- lm(amount_paid ~ num_people + is_ac + is_tv + is_flat + ave_monthly_income + num_children + is_urban, train_mod_val)
summary(sig_param_model)

# Predicting the amount_paid for the test dataset
val_pred_sig_param_model <- as.data.frame(round(as.double(predict(sig_param_model,val_mod_data)),3))
val_mod_target <- round(val_mod_target,3)

train_pred_sig_param_model <- as.data.frame(round(as.double(predict(sig_param_model,train_mod_data)),3))
train_mod_target <- round(train_mod_target,3)

# Calling the RMSE function to calculate the RMSE
cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - only significant parameters of the validation set is: "))
rmse_func(val_pred_sig_param_model,val_mod_target)

cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - only significant parameters for the training set is: "))
rmse_func(train_pred_sig_param_model,train_mod_target)

# Performing anova with Chi-squared test to compare the lm_full_model and the new_model
anova(lm_full_model,step_full_model, test='F')

lm_full_ext_model <- lm(amount_paid ~ . + .:., data = train_mod_val)
summary(lm_full_ext_model)

# Predicting the amount_paid for the test dataset
val_pred_ext_model <- as.data.frame(round(as.double(predict(lm_full_ext_model,val_mod_data)),3))
val_mod_target <- round(val_mod_target,3)

train_pred_ext_model <- as.data.frame(round(as.double(predict(lm_full_ext_model,train_mod_data)),3))
train_mod_target <- round(train_mod_target,3)

# Calling the RMSE function to calculate the RMSE
cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - extended predictors - of the validation set is: "))
rmse_func(val_pred_ext_model,val_mod_target)

cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - extended predictors - for the training set is: "))
rmse_func(train_pred_ext_model,train_mod_target)

# Executing the stepwise function AIC to the lm_full_model
new_ext_model <- stepAIC(lm_full_ext_model, trace = 0)
summary(new_ext_model)

# Predicting the amount_paid for the test dataset
val_pred_new_ext_model <- as.data.frame(round(as.double(predict(new_ext_model,val_mod_data)),3))
val_mod_target <- round(val_mod_target,3)

train_pred_new_ext_model <- as.data.frame(round(as.double(predict(new_ext_model,train_mod_data)),3))
train_mod_target <- round(train_mod_target,3)

# Calling the RMSE function to calculate the RMSE
cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - extended predictors - of the validation set is: "))
rmse_func(val_pred_new_ext_model,val_mod_target)

cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression - extended predictors - for the training set is: "))
rmse_func(train_pred_new_ext_model,train_mod_target)

# Setting seed
set.seed(1234)

# Defining Train control as Cross-validation and value of K equal to 10
train_control <- trainControl(method = "cv", number = 10)

# Training the model by assigning sales column as targetvariable and rest other column as independant variable
model <- train(amount_paid ~ ., data = train_mod, method = "lm", trControl = train_control)
summary(model)

print(model)

# Predicting the amount_paid for the test dataset
val_pred_cv_model <- as.data.frame(round(as.double(predict(model,val_mod_data)),3))
val_mod_target <- round(val_mod_target,3)

train_pred_cv_model <- as.data.frame(round(as.double(predict(model,train_mod_data)),3))
train_mod_target <- round(train_mod_target,3)

# Calling the RMSE function to calculate the RMSE
cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression of the validation set is: "))
rmse_func(val_pred_cv_model,val_mod_target)

cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression for the training set is: "))
rmse_func(train_pred_cv_model,train_mod_target)

# Predicting the amount_paid for the validation dataset and the train dataset
test_pred <- as.data.frame(round(as.double(predict(model,test_mod)),3))
test_mod_target <- round(df_test_target,3)

train_pred <- as.data.frame(round(as.double(predict(model,train_mod_data)),3))
train_mod_target <- round(train_mod_target,3)

# Calling the RMSE function to calculate the RMSE
cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression of the TESTING set is: "))
rmse_func(test_pred,test_mod_target)

cat(crayon::bold("\nThe RMSE for the Multiple Linear Regression for the TRAINING set is: "))
rmse_func(train_pred_cv_model,train_mod_target)

# Changing the categorical values from Double to String in the training dataset
df_train$is_ac <- ifelse(df_train$is_ac == 1, 'Yes', 'No')
df_train$is_tv <- ifelse(df_train$is_tv == 1, 'Yes', 'No')
df_train$is_flat <- ifelse(df_train$is_flat == 1, 'Yes', 'No')
df_train$is_urban <- ifelse(df_train$is_urban == 1, 'Yes', 'No')

# Changing the categorical values from Double to String in the testing dataset
df_test_data$is_ac <- ifelse(df_test_data$is_ac == 1, 'Yes', 'No')
df_test_data$is_tv <- ifelse(df_test_data$is_tv == 1, 'Yes', 'No')
df_test_data$is_flat <- ifelse(df_test_data$is_flat == 1, 'Yes', 'No')
df_test_data$is_urban <- ifelse(df_test_data$is_urban == 1, 'Yes', 'No')

head(df_train_data)

head(df_train)

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry = seq(2, 7, by = 1),
  ntree = c(500,1000),
  node_size  = seq(2, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# Number of Combinations
nrow(hyper_grid)

head(hyper_grid)

# Iterating thro different combinations of hyper-grid dataframe
for(i in 1:nrow(hyper_grid)) {
  
  # Use the Ranger Function to train model with different combinations
  model <- ranger(
    formula         = amount_paid ~ ., 
    data            = df_train, 
    num.trees       = hyper_grid$ntree[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # Add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

# Building the final random forest model based on the top hyper parameters from the hypergrid calculated above
fin.model <- randomForest(amount_paid ~ ., data = df_train, mtry = 6,
                          ntree = 1000, node_size = 2, sampe_size = 0.8, importance = TRUE)

# Predicting the amount_paid for the validation dataset and the train dataset
test_pred <- as.data.frame(round(as.double(predict(fin.model,df_test_data)),3))
test_target <- round(df_test_target,3)

train_pred <- as.data.frame(round(as.double(predict(fin.model,df_train[,1:10])),3))
train_target <- round(df_train_target,3)

# Calling the RMSE function to calculate the RMSE
cat(crayon::bold("\nThe RMSE for the RandomForest Regression of the TESTING set is: "))
rmse_func(test_pred,test_target)

cat(crayon::bold("\nThe RMSE for the RandomForest Regression for the TRAINING set is: "))
rmse_func(train_pred,train_target)
