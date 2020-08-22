# Set Work directory
setwd('/Users/santos@us.ibm.com/Documents/FCD/Busines Analytics')

# Load packages
#----------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(randomForest)
library(xgboost)
library(corrplot)
library(pROC)
library(DMwR)
library(caret)




###########################
# Load and View the Dataset 
#----------------------------------------------------------------------

df <- read.csv('dados_func.csv')
View(df)




###########################
# Data Exploration and Stats Visualization
#----------------------------------------------------------------------

# Changing column names to english
colnames(df) <- c('satisfaction_level','last_evaluation', 'number_projects','avg_hours_month',
                  'employed_years','work_accident', 'left_company', 'last_promotion_5years',
                  'area','salary')

# Descriptive statistics
str(df)
'* 14999 rows and 10 columns
* 5 quantitative variables (satisfaction_level, last_evaluation, number_projects, avg_hours_month,
                  employed_years)
* 5 categorical variables (work_accident, left_company, last_promotion_5years, area, salary)'

summary(df)
'* satisfaction level is above 6 out of 10, however the mean dropped since the last evaulation.
* People work with an average of 4 projects at a time.
* The average of hours worked per month is approx. 200, what gives us around 50 per week, which is high.
* A person works in that company for an average of 3.5 to 4 years.
* 24% of the people leave the company.'

# Creating the Histograms to learn more about the data distribution

# Setup plotting area 2 rows of 4
par(mfrow = c(2,3))

# Function to create the 5 histograms for quantitative variables
for (col in c(1,2,3,4,5)) {
  hist(df[,col], main = paste('Histogram of',colnames(df[col])),
       xlab = colnames(df[col]),
       col = 'skyblue2', prob=T)
  lines(density(df[,col]), col = 'blue', lwd = 2)
  }

'The histograms show us that the satisfaction within the company is a little bit more to the good
than to the bad evaluation. The projects per person are indeed concentrated between 3 and 4.
Despite the fact that the majority of the employees work the regular hours every month, there is a
high number on 200 to 250 hours, which is high overtime.
Furthermore, there is a low number of employees that stay longer than 5 years in the company.'

# Setup plotting area 2 rows of 4
par(mfrow = c(1,2))

# Barplots
barplot(table(df$work_accident), main = 'Barplot of Work Accident',
        xlab= 'work_accident', ylab = 'Count', col = c('blue3','tomato'))
barplot(table(df$left_company), main = 'Barplot of Left Company',
        xlab= 'Left_Company', ylab = 'Count', col = c('blue3','tomato'))

par(mfrow = c(1,1))

barplot(table(df$last_promotion_5years), main = 'Barplot of Last Promotion in 5 Years',
        xlab= 'last_promotion_5years', ylab = 'Count', col = c('blue3','tomato'))

par(mfrow = c(1,2))
barplot(table(df$salary), main = 'Barplot of Salary',
        xlab= 'Salary', ylab = 'Count', col = c('blue3','tomato'))
barplot(table(df$area), horiz = T, main = 'Barplot of Area',col = c('blue3'), las=2,cex.lab=0.5)


# Turnover calculation
prop.table(table(df$left_company))
'The company has low rates of promotions in the last 5 years. Turnover is 24%.
Most people allocated in the Sales department, followed by Technical and Support teams.
The company pays low salaries, on average.'



# reset plotting area
par(mfrow = c(1,1))


###########################
# Data Visualization
#----------------------------------------------------------------------
# Plotting two variables

# Satisfaction level by Left Company
ggplot(data=df, aes(x=satisfaction_level,y=left_company)) +
  geom_point() + geom_smooth(method='lm') +
  labs(title = 'Satisfaction Level by Left Company')

# Correlation between the variables
cor(df$satisfaction_level,df$left_company)
'There is a negative correlation of (-0.39) - even though is is not strong - between the 
satisfaction level and leaving the company.'

#---

# Satisfaction level by Number of Projects
ggplot(data=df, aes(x=as.factor(number_projects),y=satisfaction_level)) +
  geom_boxplot(aes(group=number_projects), fill='coral2') +
  labs(title = 'Satisfaction Level by Number of Projects')

# Left_Company by Number of Projects
ggplot(data=df, aes(x=as.factor(number_projects),y=left_company)) +
  geom_col(fill='coral2') +
  labs(title = 'Letf the Company by Number of Projects')

# See the numbers Left Company by Number of Projects
df %>% group_by(number_projects) %>% count(left_company)
'We can see that the people with 3, 4 or 5 projects are more motivated. People with 2 projects are 
less motivated and are those who more leave the company.'

#---

# Satisfaction level by avg hours worked
ggplot(data=df, aes(x=avg_hours_month,y=satisfaction_level)) +
  geom_point(fill='coral2') + geom_smooth() +
  labs(title = 'Satisfaction Level by Hours Worked in a month')

# Left_Company by avg_hours
ggplot(data=df, aes(x=avg_hours_month, y=left_company)) +
  geom_point(fill='coral2') + geom_smooth()
  labs(title = 'Letf the Company by Avg Hours Worked')
'We can not see a clear relationship between hours worked and satisfaction level, however the
trend line shows that for high numbers like 250+ hours the satisfaction level drops quickly
as well as the chance of that person to leave the company.'

#---
  
# Satisfaction level by area
ggplot(data=df, aes(x=area,y=satisfaction_level)) +
  geom_boxplot(aes(group=area), fill='coral2') +
  labs(title = 'Satisfaction Level by Area')
  
# Left_Company by area
ggplot(data=df, aes(x=area,y=left_company)) +
  geom_col(fill='coral2') +
  labs(title = 'Letf the Company by Area')
'All of the areas are apparently similar in terms of satisfaction with the company.
The fact that we see more Sales, Support and Technical people leaving is just a matter of size of the team, 
knowing those are the largest teams in the company.'

#---

# Satisfaction level by employed years
ggplot(data=df, aes(x=as.factor(employed_years),y=satisfaction_level)) +
  geom_boxplot(aes(group=employed_years), fill='coral2') +
  labs(title = 'Satisfaction Level by Years in the Company')

# Left_Company by employed years
ggplot(data=df, aes(x=as.factor(employed_years),y=left_company)) +
  geom_col(fill='coral2') +
  labs(title = 'Letf the Company by Years Employed')

# See the numbers Left Company by Years employed
df %>% group_by(employed_years) %>% count(left_company)

'The satisfaction level drops from the first to the 4th year in the company.
In the 3rd and 4th years is when most of the employes decide to leave.'

#---

# Left_Company by last promotion 5 years
ggplot(data=df, aes(x=as.factor(last_promotion_5years),y=left_company)) +
  geom_col(fill='coral2') +
  labs(title = 'Letf the Company by Promotion in the last 5 years ')
# See the numbers Left Company by Last Promotion
df %>% group_by(last_promotion_5years) %>% count(left_company)

'It becomes visible that there is an impact from the promotions in the fact people leave the company.
From those who were promored within the last 5 years, only 5% left. When the group not promoted is analyzed, 
that number increases to 24%.'

#---

# Satisfaction level by Salary
ggplot(data=df, aes(x=as.factor(salary),y=satisfaction_level)) +
  geom_boxplot(aes(group=salary), fill='coral2') +
  labs(title = 'Satisfaction Level by Salary')


# Left_Company by Salary
ggplot(data=df, aes(x=as.factor(salary),y=left_company)) +
  geom_col(fill='coral2') +
  labs(title = 'Letf the Company by Salary')

# See the numbers Left Company by Salary
df %>% group_by(salary) %>% count(left_company)
'As expected, people with lower salary are those less motivated and those who more leave the company (30%).'

#---
#Correlation Matrix
cor <- cor(dplyr::select_if(df, is.numeric))
corrplot(cor, type='lower',method = 'number')
'Our model will not be linear (Logistic Regression, for example), but I wanted to plot the correlations
to check if we had any strong relationships'


###########################
# Data Cleaning and Transformation
#----------------------------------------------------------------------

# Checking for missing data
sum(is.na(df))
'No missing data'

# Changing columns to factor
df$work_accident <- as.factor(df$work_accident)
df$last_promotion_5years <- as.factor(df$last_promotion_5years)
df$area <- as.factor(df$area)
df$salary <- as.factor(df$salary)
df$left_company <- as.factor(df$left_company)
str(df)



###########################
# Split Dataset and Feature Selection
#----------------------------------------------------------------------

# Split train and test
sample <- sample.int(n=nrow(df), size= floor(0.8*nrow(df)), replace=F)
df_train <- as.data.frame(df[sample,])
df_test <- as.data.frame(df[-sample,])

library(randomForest)
rf_model <- randomForest(left_company~., data=df_train, ntree=200, importance=T)
varImpPlot(rf_model)

'After plotting the importance of the variables for the model, I will choose the following:
satisfaction_level, number_projects, last_evaluation, avg_hours_month, employed_years'

# New dataframe with only the selected columns after feature engineering
df2 <- df %>% select(7, 1:5)
df2$left_company <- as.factor(df2$left_company)



###########################
# Training Models and Testing
#----------------------------------------------------------------------

# New Train and Test Split with spit function from Tidymodels
sample <- initial_split(data = df2,strata = left_company)
'<Analysis/Assess/Total>
<11250/3749/14999>'

# Saving in two dataframes
df_train <- training(sample)
df_test <- testing(sample)


#### Random Forest

rfModel <- randomForest(left_company ~ ., ntree = 500,
                        data = df_train)
rfModel

# Predictions
preds_rf <- predict(rfModel, df_test[,-1])

# Area Under the Curve
par(pty='s')
auc(df_test$left_company, factor(preds_rf, ordered = T),
    plot=T, main='Area Under the Curve - Random Forest')
# AUC = 98.6%

# Confusion Matrix
confusionMatrix(df_test$left_company, preds_rf)
'Accuracy = 99%'

#---
# Version 2 Random Forest with Balanced Dataset

# Checking the balance of the train dataset
prop.table(table(df_train$left_company))
'76% of 0 (not left the company) against 24% left'

# Balance the Dataset
df_train_bal <- SMOTE(left_company~., data= df_train, perc.over = 190, k = 5)
prop.table(table(df_train_bal$left_company))
'Now balanced at 50/50'

rfModel2 <- randomForest(left_company ~ ., ntree = 500,
                        data = df_train_bal)
rfModel2

# Predictions
preds_rf2 <- predict(rfModel2, df_test[,-1])

# Area Under the Curve
par(pty='s')
auc(df_test$left_company, factor(preds_rf2, ordered = T),
    plot=T, main='Area Under the Curve - Random Forest v2')
# AUC = 97.6%

# Confusion Matrix
confusionMatrix(df_test$left_company, preds_rf2)
'Almost the same result: Accuracy of 98.6%
But this model is better because it has been trained over a balanced dataset, 
being able to learn better about both groups.'


#---
#### XGBoost

# Training the model
xgb <- train(left_company ~ . , data=df_train_bal, method = 'xgbTree')
xgb

# Prediction and Test
pred_xgb <- predict(xgb, df_test[,-1])

# Confusion Matrix and AUC curve
confusionMatrix(df_test$left_company, pred_xgb)
par(pty='s')
auc(df_test$left_company, factor(pred_xgb, ordered = T),
    plot=T, main='Area Under the Curve - XGB')
# Accuracy = 97%
# ROC = 96.3%

'The Random Forest model had a slightly superior performance, thus we are using it as our final model.'


###########################
# Single prediction and saving the model
#----------------------------------------------------------------------
# Testing predictions
predict(rfModel2, data.frame(satisfaction_level=0.10, last_evaluation=0.57,
                            number_projects=2,avg_hours_month=230,
                            employed_years= 4))

# Saving the model to be used on the Shiny App
saveRDS(rfModel2, "model_turnover.rds")

     