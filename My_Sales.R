# The aim is to build a predictive model and find out the sales of each product at a 
# particular store
set.seed(1)

require(RCurl)
require(ggplot2)
require(dplyr)
require(Hmisc)
require(e1071)
require(nortest)
require(car)
library(woe)
require(usdm)  # For the Variance Inflation Factor
require(glmnet) # To perform Lasso
require(lattice)
require(doMC) # For parallel computing
require(mlr) # It has must of the ML tasks
require(caret)
require(e1071)

archivo <- getURL('https://datahack-prod.s3.ap-south-1.amazonaws.com/test_file/Test_u94Q5KV.csv')
test <- read.csv(textConnection(archivo), header = T)
archivo2 <- getURL('https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/Train_UWu5bXk.csv')
train <- read.csv(textConnection(archivo2), header = T)

sapply(train, class)
sapply(train, function(x){sum(is.na(x))})

# Set the empty spaces as NA's
train[train == ""] <- NA

# Watch the general structure of the dataset
str(train)

# The Item_Fat_Content has 5 different levels, should be if the item is low fat or not
train[train == "LF"] <- "Low Fat"
train$Item_Fat_Content[train$Item_Fat_Content == "reg"] <- "Regular"
train$Item_Fat_Content[train$Item_Fat_Content == "low fat"] <- "Low Fat"

# The train$Item_Fat_Content has the same levels as the beginning, change them with this:
train$Item_Fat_Content <- factor(train$Item_Fat_Content, levels = c("Low Fat", "Regular"))

# There is also an extra factor in the Outlet_Size variable, can be changed with:
train$Outlet_Size <- factor(train$Outlet_Size, levels = c("High", "Medium", "Small"))


# ||| Univariate Analysis ||| 
# The distribution of the variables

# First with the Item_Visibility
ggplot(train, aes(Item_Visibility)) + geom_histogram(bins = 100)
ggplot(train, aes(x = Item_Outlet_Sales, y = Outlet_Location_Type)) + geom_boxplot()
ggplot(train, aes(Item_Outlet_Sales, color = Outlet_Location_Type)) + geom_density(alpha = 0.4)
ggplot(train, aes(Item_Outlet_Sales)) + geom_density()

# See the distribution by the particular store
ggplot(train, aes(Item_Outlet_Sales, fill = factor(Outlet_Type))) +  geom_density(alpha = 0.4)
    # We can see that the sales at the Grocery Store are far better than the rest in
    # in terms of more quantity but lower prices
ggplot(train, aes(Item_Outlet_Sales, fill = factor(Outlet_Type))) +  geom_histogram(bins = 100)
    # We can see that the sales at the Supermarket Type1 are far better than the rest 
    # it has a bigger distributiion

# Graph the distribution by Outlet_Type
grocery <- train %>% 
  filter(Outlet_Type == "Grocery Store")
ggplot(grocery, aes(Item_Outlet_Sales, fill = factor(Item_Type))) + geom_density()

smarket1 <- train %>%
  filter(Outlet_Type == 'Supermarket Type1')

smarket2 <- train %>%
  filter(Outlet_Type == 'Supermarket Type2')

smarket3 <- train %>%
  filter(Outlet_Type == 'Supermarket Type3')

    # The dimensions of the datasets are not equivalent meaning that the sales
    # can be biased, i.e. it can not be concluded that the Supermarket Type 1 is 
    # better in selling items 

sapply(grocery, function(x){sum(is.na(x))}) # Missing data in Item_Weight and in Outlet_Size
sapply(smarket1, function(x){sum(is.na(x))}) # Missing data in Outlet_Size
sapply(smarket2, function(x){sum(is.na(x))}) # Perfect, without any missing value
sapply(smarket3, function(x){sum(is.na(x))}) # Missing data in Item_Weight

ggplot(train, aes(Item_Visibility)) + geom_histogram(bins = 100)
ggplot(grocery, aes(Item_Visibility)) + geom_histogram(bins = 100)
ggplot(smarket1, aes(Item_Visibility)) + geom_histogram(bins = 100)
ggplot(smarket2, aes(Item_Visibility)) + geom_histogram(bins = 100)
ggplot(smarket3, aes(Item_Visibility)) + geom_histogram(bins = 100)

# Explore the Item_Weight
# Let's make a scatter plot
ggplot(trian, aes(x = Item_Weight, y = Item_Outlet_Sales)) + geom_point(shape=1) + geom_smooth(method = lm, color='red', se=FALSE)
  # It shows that there is no direct correlation 
rcorr(train$Item_Weight, train$Item_Outlet_Sales)
  # p-value of 0.2354


# ||| Bivariate Analysis |||
# Looking at the significance level with the t-test
# With the factor or categorical data
ggplot(train, aes(x = Item_Fat_Content, y = Item_Outlet_Sales)) + geom_boxplot()
  # A lot of outliers and there is no big difference between them 

# Make some tests for the response variable
ad.test(train$Item_Outlet_Sales)
ggplot(train, aes(Item_Outlet_Sales)) + geom_density()
qqnorm(train$Item_Outlet_Sales)
qqline(train$Item_Outlet_Sales)

          # Remember that the sample size is big enough for not to make 
          # normality tests

# Going back for the categorical variable Item_Fat_Content
t.test(Item_Outlet_Sales ~ Item_Fat_Content, data = train) # 0.085
wilcox.test(Item_Outlet_Sales ~ Item_Fat_Content, data = train) # 0.083

# ||| 1.- Item_Type
ggplot(train, aes(x = Item_Type, y = Item_Outlet_Sales)) + geom_boxplot()
      # There is no possibility to make a t.test
aov_Item_Type <- aov(Item_Outlet_Sales ~ Item_Type, data = train)
summary.aov(aov_Item_Type) # p-value 0.000388
TukeyHSD(aov_Item_Type)
aov_Item_Type2 <- oneway.test(Item_Outlet_Sales ~ Item_Type, data = train) # p-value 0.000113
# tests the null hypothesis of equal group variances
bartlett.test(Item_Outlet_Sales ~ Item_Type, data = train) 
    # p-value: 2.775e-07 
fligner.test(x = train$Item_Outlet_Sales, g = train$Item_Type)
    # p-value: 0.0004245
leveneTest(y = train$Item_Outlet_Sales, group = train$Item_Type)
    # p-value: 0.005113
leveneTest(y = train$Item_Outlet_Sales, group = train$Item_Type, center = mean)
    # p-value: 2.276e-05
    
    # The null hypothesis for all these tests is that the variances are the same 
    # (homogeneous). All the tests are rejecting that hypothesis.

# Analysing the aov() output
par(mfrow = c(1,2))
plot(aov_Item_Type, 1)
plot(aov_Item_Type, 2)

# The graph on the left shows the residuals (score - group mean) they are getting 
# larger as the group means get larger, a bad pattern but not unusual when the DV
# results from counting something
# The graph on the right shows that the residuals are not normally distributed
# so the normality assumption is also violated

# With the tukey's p-values the assumptions of the ANOVA test are not well satisfied
# so the results are not robust

# The non - parametric test: Kruskal-Wallis oneway ANOVA
kruskal.test(Item_Outlet_Sales ~ Item_Type, data = train) # p-value 0.000298



# ||| 2.- Outlet_Identifier
ggplot(train, aes(x = Outlet_Identifier, y = Item_Outlet_Sales)) + geom_boxplot()
    # It shows that OUT010 and OUT019 have the lowest sales
# Just see what tipe of outlet
var <- train %>%
  filter(Outlet_Identifier == "OUT049") 
unique(var$Outlet_Type)
        # If the Outlet_Identifier is changed we can see that the grocery
        # has the lowest sales and the supermarket type 3 has the highest
# Lets see if this is statistical significant
aov_Outlet_Identifier <- aov(Item_Outlet_Sales ~ Outlet_Identifier, data = train)
summary.aov(aov_Outlet_Identifier) # p-value <2e-16
TukeyHSD(aov_Outlet_Identifier)
aov_Outlet_Identifier2 <- oneway.test(Item_Outlet_Sales ~ Outlet_Identifier, data = train) # p-value <2.2e-16
# Analysis of the variance
par(mfrow = c(1,2))
plot(aov_Outlet_Identifier, 1)
plot(aov_Outlet_Identifier, 2)
# # tests the null hypothesis of equal group variances
bartlett.test(Item_Outlet_Sales ~ Outlet_Identifier, data = train) 
    # p-value: <2.2e-16 
fligner.test(x = train$Item_Outlet_Sales, g = train$Outlet_Identifier)
    # p-value: <2.2e-16
leveneTest(y = train$Item_Outlet_Sales, group = train$Outlet_Identifier)
    # p-value: <2.2e-16
leveneTest(y = train$Item_Outlet_Sales, group = train$Outlet_Identifier, center = mean)
    # p-value: <2.2e-16

        # We can reject the null hpothesis i.e the variances are not the same

# The non - parametric test: Kruskal-Wallis oneway ANOVA
kruskal.test(Item_Outlet_Sales ~ Outlet_Identifier, data = train) # p-value <2.2e-16


# ||| 3.- Outlet_Size
ggplot(train, aes(x = Outlet_Size, y = Item_Outlet_Sales)) + geom_boxplot()
aov_Outlet_Size <- aov(Item_Outlet_Sales ~ Outlet_Size, data = train) # p-value <2e-16
summary.aov(aov_Outlet_Size)
aov_Outlet_Size2 <- oneway.test(Item_Outlet_Sales ~ Outlet_Size, data = train)
kruskal.test(Item_Outlet_Sales ~ Outlet_Size, data = train) # p-value <2.2e-16
# Analysis of variance
par(mfrow = c(1,2))
plot(aov_Outlet_Size, 1)
plot(aov_Outlet_Size, 2)


# ||| 4.- Outlet_Location_Type
ggplot(train, aes(x = Outlet_Location_Type, y = Item_Outlet_Sales)) + geom_boxplot()
ggplot(train, aes(Item_Outlet_Sales, color = Outlet_Location_Type)) + geom_density()
aov_Outlet_Location_Type <- aov(Item_Outlet_Sales ~ Outlet_Location_Type, data = train) 
summary.aov(aov_Outlet_Location_Type) # p-value <2e-16
aov_Outlet_Location_Type2 <- oneway.test(Item_Outlet_Sales ~ Outlet_Location_Type, data = train)
aov_Outlet_Location_Type2 # p-value <2.2e-16
kruskal.test(Item_Outlet_Sales ~ Outlet_Location_Type, data = train) # <2.2e-16
# Analysis of variance
par(mfrow = c(1,2))
plot(aov_Outlet_Location_Type, 1)
plot(aov_Outlet_Location_Type, 2)


# ||| 5.- Outlet_Type
ggplot(train, aes(x = Outlet_Type, y = Item_Outlet_Sales)) + geom_boxplot()
ggplot(train, aes(Item_Outlet_Sales, color = Outlet_Type)) + geom_density()
    # Here you can see that the one who has the best sales is the supermarket type 3
aov_Outlet_Type <- aov(Item_Outlet_Sales ~ Outlet_Type, data = train)
summary.aov(aov_Outlet_Type) # p-value <2e-16
oneway.test(Item_Outlet_Sales ~ Outlet_Type, data = train) # p-value <2.2e-16
kruskal.test(Item_Outlet_Sales ~ Outlet_Type, data = train) # p-value <2.2e-16
# Analysis of variance
par(mfrow = c(1,2))
plot(aov_Outlet_Type, 1)
plot(aov_Outlet_Type, 2)


# ||| Now the analysis of the continuous variables |||
ggplot(train, aes(x = Item_Visibility, y = Item_Outlet_Sales)) + geom_point(shape=1) + geom_smooth(method = lm, color='red', se=FALSE)
rcorr(train$Item_Visibility, train$Item_Outlet_Sales)
    # p-value 0

boxplot(train$Item_Weight) # no outliers 
boxplot(train$Item_Visibility)
boxplot(train$Item_MRP) # no outliers
boxplot(train$Outlet_Establishment_Year) # no outliers


# Until now there is only one variable with outliers (Item_Visibility) and 
# looking at the scatter plot it can not be seen if there is a direct corelation
# with the item sales


# Now we get the importance by Information Value
# But first lets do a backup
train_new <- train

# Eliminate de Item_Identifier
train_new$Item_Identifier <- NULL

# Calculation of information value
IV <- iv.mult(train_new, y = "Item_Outlet_Sales", TRUE)


# To see the variance inflation factor
vif(train_new)


# Perform Lasso
x <- as.matrix(train_new[, c(2, 4, 5, 6, 7, 9, 10)])
y <- as.double(as.matrix(train_new[, 11]))
  #Fitting the mode Lasso (alpha = 1)
cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

clases <- sapply(train_new, class)

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
## For univariate analysis
    # Remember that histograms and boxpots are univariate
summary(train_new$Item_Weight)
sd(train_new$Item_Weight, na.rm = T)
fivenum(train_new$Item_Weight)
quantile(train$Item_Weight, na.rm=T)
skewness(train_new$Item_Weight, na.rm = T)
IQR(train_new$Item_Weight, na.rm = T)
mad(train_new$Item_Weight, na.rm = T) # Median Average Deviation, find the median then find all the differences from the median
boxplot(train_new$Item_Weight)
ggplot(train_new, aes(Item_Weight)) + geom_density()
## For univariate analysis
boxplot(scale(train_new$Item_Weight), scale(train_new$Item_Outlet_Sales))
boxplot(train_new$Item_Weight, train_new$Item_Outlet_Sales)
ggplot(train_new, aes(x = Item_Weight, y = Item_Outlet_Sales)) + geom_dotplot(binwidth = 0.1, alpha = 0.5)
rcorr(train_new$Item_Weight, train_new$Item_Outlet_Sales)
cor(train_new$Item_Weight, train_new$Item_Outlet_Sales, use = "complete.obs")
summary(lm(train_new$Item_Outlet_Sales ~ train_new$Item_Weight))
# Something with lattice
xyplot(train_new$Item_Outlet_Sales ~ train_new$Item_Visibility | train_new$Outlet_Type)
plot.regression <- function(x,y){
  panel.xyplot(x, y)
  panel.abline(lm(y~x))
}
xyplot(train_new$Item_Outlet_Sales ~ train_new$Item_Visibility | train_new$Outlet_Type,  panel = plot.regression)

# Exploratory Data Analysis
# For univariate data we can ask: if its normal, long or short tail, skewed
# The main tool is to use proper graphics
    # Barplots: for categorical data
    # Histogram, dot plots, steam and leaf plots: for numerical data
    # Boxplots: to see summaries of a numerical distribution, useful in comparing distributions and identifying long and short-tailed distributions
    # Normal probability plots: to see if data is approximately normal

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 


# CONTINUE WITH THE LASSO ALGORITHM
    # Remember that ridge regression generalises better because 

# Before that, remove the outliers
boxplot(train_new$Item_Visibility)
sum(train_new$Item_Visibility > 0.20)
nrow(train_new)
ggplot(train_new, aes(Item_Visibility)) + geom_density()

train_new$Item_Visibility[train_new$Item_Visibility > 0.2] <- 0.2

# Perform Lasso
# x <- as.matrix(train_new[, -11])
x <- as.matrix(train_new[, c(2, 4, 5, 6, 7, 9, 10)]) # This data hos no missing values
x2 <- model.matrix(~ Item_Fat_Content + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Location_Type + Outlet_Type, train_new) 
x3 <- model.matrix(~ Item_Fat_Content + Item_Type + Item_MRP + Outlet_Identifier + factor(Outlet_Establishment_Year) + Outlet_Location_Type + Outlet_Type, train_new) 
y <- as.double(as.matrix(train_new[, 11]))
#Fitting the mode Lasso (alpha = 1)
cv.lasso <- cv.glmnet(x, y, family='mgaussian', type.multinomial="grouped", alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')
cv.lasso <- cv.glmnet(x, y=y, family='gaussian', alpha = 1, type.measure='mse')
lasso <-  cv.glmnet(x2, y = y, type.measure = 'mse', family = 'gaussian', alpha = 1)
lasso2 <- cv.glmnet(x, y = y, type.measure = 'mse', family = 'gaussian', alpha = 1)
plot(cv.lasso)
coef(cv.lasso, s = "lambda.min")
plot(cv.lasso, xvar = "lambda", label = TRUE)

plot(lasso)  
plot(lasso2)

  # For glmnet quick desciption: http://www.cs.tut.fi/kurssit/SGN-2556/glmnet_matlab/glmnet.m

# Segunda prueba
otra.x <-  as.matrix(train_new[, c(2,4,5,6,7,9,10)])
otro.lasso <- cv.glmnet(otra.x, y=y, family='multinomial', alpha = 1, type.measure='mse')
otro.lasso2 <- cv.glmnet(x, y=y, family='gaussian', alpha = 1, type.measure='mse')

# There is no other way, we must keep the cv.lasso model 
cv.lasso  # The last model, thank God we did it
plot(cv.lasso) # Two lambdas are selected (2 dotted lines)
opt_lambda <- cv.lasso$lambda.min # The lowest point in the curve, the optimal lambda that best minimised the error in cross validation
lse_lambda <- cv.lasso$lambda.1se # Gives the most regularized model such that error is within one standard error from the minimum
fit <- cv.lasso$glmnet.fit # Extract all of the fitted models
summary(fit)
    # These are the two things we need to predict new data
    # But before we need to convert one of the factors of the test set
    test[test == "LF"] <- "Low Fat"
    test$Item_Fat_Content[test$Item_Fat_Content == "reg"] <- "Regular"
    test$Item_Fat_Content[test$Item_Fat_Content == "low fat"] <- "Low Fat"
    test$Item_Fat_Content <- factor(test$Item_Fat_Content, levels = c("Low Fat", "Regular"))  

newx <- data.matrix(test[, c(3,5,6,7,8,10,11)])
y_predicted <- predict(cv.lasso, s = 'lambda.min', newx = newx, type = 'response')

as.matrix(coef(cv.lasso, s = 'lambda.min'))
coef(cv.lasso, s='lambda.min')

    # For parallel computing example
    registerDoMC(cores = 2)
    system.time(cv.glmnet(x, y, parallel = TRUE))
    ##############################################
    
testData <- test[, c(3,5,6,7,8,10,11)]
newx2 <- model.matrix(~., testData)
predict(cv.lasso, newx = newx, s = 'lambda.min', type = 'response')

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

# Lets make everything again
x_final <- data.matrix(train_new[, c(2, 4, 5, 6, 7, 9, 10)])
lassoCV <- cv.glmnet(x = x_final, y = train_new[, 11], family = 'gaussian', alpha = 1, type.measure = 'mse')
predict_final <- predict(lassoCV, type = 'response', newx = data.matrix(testData), s = 'lambda.min')
plot(lassoCV)
lassoCV$lambda.min
# Ok... now it worked

# Identify the best variables
co <- coef(lassoCV, s = 'lambda.1se')
inds <- which(co != 0)
variables <- row.names(co)[inds]
variables <- variables[!(variables %in% '(Intercept)')]

          # Just to check for variable selection with VIF: https://stats.stackexchange.com/questions/176352/variable-selection-with-glmnet-caret-package

# RMSE for lasso regression
train_new$Item_Outlet_Sales_Prediction <- predict(lassoCV, type = 'response', newx = x_final, s = 'lambda.min')
RMSE_lasso <- sqrt(mean((train_new$Item_Outlet_Sales - train_new$Item_Outlet_Sales_Prediction)^2))

# Load the Submission format
submit <- read.csv('SampleSubmission_TmnO39y.csv', header = TRUE)
testDataMatrix <- data.matrix(testData)
submit_predict_lasso <- predict(lassoCV, type = 'response', newx = testDataMatrix, s = 'lambda.min')

submit$Item_Outlet_Sales <- submit_predict_lasso

write.csv(submit, "Predictions.csv")



        # Ok at the platform the algorithm scored 1273.06 with the 577 place




# ||| Ridge Algorithm |||
# Now make the ridge algorithm and make another prediction
ridgeCV <- cv.glmnet(x = x_final, y = train_new[, 11], family = 'gaussian', alpha = 0, type.measure = 'mse')
plot(ridgeCV)

# RMSE for lasso regression
train_new$Item_Outlet_Sales_Prediction <- predict(ridgeCV, type = 'response', newx = x_final, s = 'lambda.min')
RMSE_ridge <- sqrt(mean((train_new$Item_Outlet_Sales - train_new$Item_Outlet_Sales_Prediction)^2))

# Load the Submission format
submit <- read.csv('SampleSubmission_TmnO39y.csv', header = TRUE)
testDataMatrix <- data.matrix(testData)
submit_predict_ridge <- predict(ridgeCV, type = 'response', newx = testDataMatrix, s = 'lambda.min')
submit$Item_Outlet_Sales <- submit_predict_lasso
write.csv(submit, "Predictions.csv")


        # Well with the ridge algorithm the RMSE got better but in the leaderboard
        # did not move, the score was 1273.069171 and the position was 578 out of 731


# ||| Support Vector Machine |||
SVM_model <- svm(Item_Outlet_Sales ~ Item_Fat_Content + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Location_Type + Outlet_Type, data = train_new[, c(2, 4, 5, 6, 7, 9, 10, 11)], cross = 10)
  # Getting the sqrt and then the mean of the 10 fold cv (SVM_model$MSE) the RMSE is
  # 1086.592
print(SVM_model)
# Load the submissions format
submit <- read.csv('SampleSubmission_TmnO39y.csv', header = TRUE)
submit_predict_SVM <- predict(SVM_model, testData)
submit$Item_Outlet_Sales <- submit_predict_SVM
write.csv(submit, "Predictions.csv")

        # With this algorithm the RMSE got even better with 1161.001197
        # and the position was 276 out of 734




