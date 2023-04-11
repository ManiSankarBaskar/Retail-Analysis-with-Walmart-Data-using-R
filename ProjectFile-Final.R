# Dataset to be handled - 'Walmart_Store_sales.csv
# Business requirement - Build  prediction models to forecast demand for Store 1.

# Reading the Dataset file :
# Lets read the file and assign the Dataset name as "WMData" :
WmData<-read.csv("D:/Walmart_Store_sales.csv")
# Alternatively, we can also Import the Dataset on the Environment Window on the right hand side.

# Exploring the WmData :
View(WmData)
summary(WmData)

# Few Inferences from the data :
# 1.) We have 6435 observations with 8 variables in total.
# 2.) Date field is of character datatype and other fields are of either integer or numeric datatypes.
# 3.) Highest Weekly Sales ever recorded in the Dataset is 3818686.
# 4.) Lowest Weekly Sales in the entire Dataset is 209986.
# 5.) Fuel_Price surged to a maximum price of 4.468 which may or may not have an impact on Sales Data.


### Question - 1: Which store has maximum sales?
### Ans :
# Step 1 : Lets first aggregate the Weekly sales by store-wise and we can find out the store that contributes to maximum sales.
Maxsales_store<-aggregate(Weekly_Sales~Store,data=WmData,sum)
View(Maxsales_store)

# Step 2 : Sorting the Dataframe in Descending order to view the Store with Maximum sales as top results.
library(dplyr)
Maxsales_store=arrange(Maxsales_store,-Weekly_Sales)
View(Maxsales_store)

# Inference :
# Store 20 has the maximum overall Weekly sales - 301397792



### Question - 2 : Which store has maximum standard deviation i.e., the sales vary a lot. Also, find out the coefficient of mean to standard deviation.
### Ans :
# Important Note : Second part of the question - i.e "Also, find out the coefficient of mean to standard deviation" is incorrect (as course faculty confirmed it) and hence I am ignoring it.
# Hence, I am going to focus on finding which store has the maximum standard deviation.

# Step 1 : Lets first aggregate the Weekly sales by store-wise and we can find out the Standard Deviation for all the entries.
MaxSD_store<-aggregate(Weekly_Sales~Store,data=WmData,sd)
View(MaxSD_store)

# Step 2 : Sorting the Dataframe in Descending order to view the Store with Maximum Standard Deviation as top results.
MaxSD_store=arrange(MaxSD_store,-Weekly_Sales)
View(MaxSD_store)

# Inference :
# Store 14 has the maximum Standard deviation - 317569.95



### Question - 3: Which store/s has good quarterly growth rate in Q3'2012?
### Ans :
# Step 1 : Since date field is a character data type, I need to convert it to Date format to leverage it completely.
WmData$Date=as.Date(WmData$Date,format="%d-%m-%Y")
class(WmData$Date)

# Step 2 : Now, Using date field in the dataset, we need to get year,quarter,semester,month fields seperately in 4 columns.
# I am going to use lubridate package to work around the date field.
library(lubridate)
WmData$Sem<-semester(WmData$Date)
WmData$Month=month(WmData$Date)
WmData$Qtr=quarter(WmData$Date)
WmData$Year<-format(WmData$Date,format="%Y")
View(WmData)

# Step 3 : Since I need the store with good quarterly growth rate in Q3’2012,I will create subsets of Data from Dataframe "WmData" for Q2'2012 and Q3'2012 and compare it.
# Creating subset of WmData for Q2'2012 and name the Dataframe as D1.
D1<-subset(WmData,Qtr==2 & Year==2012)
View(D1)
# Aggregating the Weekly sales by Store for D1
D1<-aggregate(Weekly_Sales~Store,data=D1,sum)
colnames(D1)[2]<-"Q2Sales_2012"
View(D1)
# Create another subset of WmData for Q3'2012 and name the Dataframe as D2.
D2<-subset(WmData,Qtr==3 & Year==2012)
View(D2)
# Aggregating the Weekly sales by Store for D2
D2<-aggregate(Weekly_Sales~Store,data=D2,sum)
colnames(D2)[2]<-"Q3Sales_2012"
View(D2)

# Step 4 : Create another dataframe named 'D3' and Use cbind to Combine D1 and D2 data at column level.
D3<-cbind(D1,D2)
View(D3)
# Removing one occurence of Column 'Store' since it is repeating
D3<-D3[,-3]
View(D3)

# Step 5 : Add one more column to D3 dataframe - i.e 'Growth rate'. 
# To arrive at values for each entries, use this calculation - (Q3-Q2)/Q2
D3$Growth_Rate<-(D3$Q3Sales_2012 - D3$Q2Sales_2012)/D3$Q2Sales_2012
View(D3)

# Step 6 : Sort the data by Growth rate column in Descending order to know the store with highest growth rate for Q3'2012.
D3=arrange(D3,-Growth_Rate)
View(D3)

# Inference :
# By comparing the results, Store 7 had the highest quarterly growth rate for Q3 2012 - 13.33%  



### Question - 4: Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together?
### Ans :
# Step 1 : Lets create subsets of the data for holiday and non-holiday season from the dataset "WmData".
# To create the subsets of data, I am going to utilize the 'Holiday_Flag' column from the dataframe.
# As per the given information, Holiday seasons are labelled as '1' whereas Non-holiday seasons as '0'. Using this, I am subsetting the data below
Holiday_season<-subset(WmData,Holiday_Flag==1)
View(Holiday_season)
Non_Holiday_season<-subset(WmData,Holiday_Flag==0)
View(Non_Holiday_season)

# Step 2 : Find the mean sales of Non-Holiday season across all the stores
mean(Non_Holiday_season$Weekly_Sales)
# Mean sales of Non-Holiday season across all the stores is 1041256

# Step 3 : Use aggregate function to find out the mean sales by each Holiday Events (i.e Date).
Meansales_Holiday<-aggregate(Weekly_Sales~Date,data=Holiday_season,mean)
View(Meansales_Holiday)

# Step 4 : Compare the overall mean sales of Non-Holiday season (across the stores) with the mean sales of Holiday season for each date.
# For this, I am using ifelse condition.
Meansales_Holiday$Highersales<-ifelse(Meansales_Holiday$Weekly_Sales>mean(Non_Holiday_season$Weekly_Sales),"Yes","No")
View(Meansales_Holiday)

# Inference :
# Following are the holiday events with higher sales than mean sales in non Holiday season for all stores together.
# 2010-02-12, 2011-02-11, 2012-02-10  - Super Bowl
# 2010-11-26, 2011-11-25              - Thanksgiving        
# 2012-09-07                          - Labour Day



### Question - 5 : Provide a monthly and semester view of sales in units and give insights
### Ans :
# Total Weekly sales by Month and Semester : 
Salesview1<-aggregate(Weekly_Sales~Month+Sem,data=WmData,sum)
colnames(Salesview1)[3]<-"Salesby_Month_and_Sem"
View(Salesview1)

#Inference :
# Considering all the years in the dataset:
# There was a steady increase in Sales month by month. However, Month 5 - i.e May month had a sudden decline and then the Sales accelarated in June and July.
# After July, there is a decline in overall Sales till the end of the year (though it is not a significant decline)

# Highest Weekly sales by Month and Semester :
Salesview2=arrange(Salesview1,-Salesby_Month_and_Sem)
View(Salesview2)

# Inference :
# For all the years in the Dataset "WmData"  - July Month, which belongs to Semester 2, recorded the highest overall sales - 650000977.
# For all the years in the Dataset "WmData"  - Jan Month, which belongs to Semester 1, has the lowest overall sales - 332598438
# Walmart has to increase the inventory for July month as the demand is higher.

# Highest mean sales by Semester :
Salesview3<-aggregate(Weekly_Sales~Sem,data=WmData,mean)
colnames(Salesview3)[2]<-"Meansales_Sem"
View(Salesview3)

# Inference :
# Semester 2 has the highest average sales throughout the years and stands at 1069324.
# Walmart has to increase the inventory for Semester 2 to meet the consumer demand.

# Highest mean sales by month :
Salesview4<-aggregate(Weekly_Sales~Month,data=WmData,mean)
colnames(Salesview4)[2]<-"Monthly_meansales"
Salesview4=arrange(Salesview4,-Monthly_meansales)
View(Salesview4)

# Inference :
# Month 12 has the highest average sales.
# Month 1 has the lowest average sales.
# For Month 1 (i.e Jan) and other months with lower average sales(on business perspective), Walmart can : 
# Stock up new products that are currently in trend (i.e gaining popularity)
# Do promotional events and offer competitive discounts on those months to attract demand.



### Statistical model:
### Question - 1 : For Store 1 – Build  prediction models to forecast demand
#        Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
#        Change dates into days by creating new variable.
### Ans  : I am going to ignore the following task from the above question as confirmed by course faculty in the class - "Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order)"
# I am going to focus on rest of the tasks and also build a linear model.

# Build a Linear Regression model for Store 1:
# First of all, lets create a subset of our original dataset 'WmData' to arrive at Store 1 data.
Store_1_Data<-subset(WmData,Store==1)
View(Store_1_Data)

# To build a Linear Regression model, I am going to use dependent and independent variables that are continuous numeric and ignoring categorical variables.
# Variables I am going to consider :
# Dependent Variable - Weekly_Sales
# Independent Variables - Temperature,Fuel_Price,CPI,Unemployment

# Note : For model building, I need to take into consideration, the Data related assumptions and Error related assumptions.

# Data assumption 1: Checking for Linear relationship between dependent and independent variables in the data:
# To check the correlation, I am Excluding the categorical variables and considering only the above mentioned variables.
names(Store_1_Data)
cor(Store_1_Data[,c(-1,-2,-4,-9,-10,-11,-12)])
plot(Store_1_Data[,c(-1,-2,-4,-9,-10,-11,-12)])

# Inference : 
# As a rule of thumb, for the data to establish a linear relationship between variables, I should atleast have correlation above 0.6.
# From the correlation matrix, I infer the following:
# There is negative correlation between Weekly_Sales and Temperature.
# There is negative correlation between Weekly_Sales and Unemployment.
# There is a positive correlation between Weekly_Sales and Fuel_Price.This, however is doesnt return the expected value.
# There is a positive correlation between Weekly_Sales and CPI.The correlation value however, is less than the correlation value of 0.6. Correlation value is not significant here.
# With respect to Linear relationship between Dependent and Independent variables :
# I conclude that there is no linear relationship between Dependent variable "Weekly_Sales" and other Independent variables


# Data assumption 2 : Checking for Multicollinearity on independent variables using VIF scores:
library(sp)
library(raster)
library(usdm)
# Lets now exclude dependent variable 'Weekly_Sales' and few other categorical variable fields from the WmData to proceed with the VIF calculation for Independent variables.
names(Store_1_Data)
vif(Store_1_Data[,c(-1,-2,-3,-4,-9,-10,-11,-12)])
# Normally, businesses will have a vif threshold of 10. Sometimes, the threshold could also be 5 ( which is 80 percent R2).
# Lets set a VIF threshold of 5 for this model.
# The VIF score from above calculation is high (i.e exceeds the threshold 5) for the Variable 'CPI' and stands at 5.71.
# What this means is there is mulicollinearity between the independent variable 'CPI' with rest of the independent variables.
# To fix this,I am dropping CPI and performing the VIF calculation again
vif(Store_1_Data[,c(-1,-2,-3,-4,-7,-9,-10,-11,-12)])
# Now, that we have fixed the multicollinearity between the variables and now that the VIF scores are low for rest of the variables, lets proceed with model building with the following
# Dependent variable - 'Weekly_Sales'
# Independent variables - Temperature+Fuel_Price+Unemployment

# Model building Approach 1 : VIF based model building
names(Store_1_Data)
VIF_Linearmodel=lm(Weekly_Sales~Temperature+Fuel_Price+Unemployment,data=Store_1_Data)
summary(VIF_Linearmodel)

# Inference :
# As you can see, we have an R2 value of 0.0856 which translates to 8.56 percent. R2 is very low than expected.
# Variable 'Temperature' has a p value less than 0.05 and falls within the range 0 to 0.001.
# Actual p value for variable 'Temperature' is 0.00156 and is statistically significant.
# However, p value for variables 'Fuel_Price' and 'Unemployment' are greater than 0.05 threshold.
# It means, that the variables 'Fuel_Price' and 'Unemployment' is not explaining much of dependent variable 'Weekly_Sales'.



# Model building Approach 2 : Significance based model building : 
# I am using all the dependent and independent variables which are continuous numeric in this Dataset.
names(Store_1_Data)
Significance_Linearmodel=lm(Weekly_Sales~Temperature+Fuel_Price+CPI+Unemployment,data=Store_1_Data)
summary(Significance_Linearmodel)

# Inference :
# Variable 'Fuel_Price' has the highest p value of 0.50696 which is very far from 0.05 threshold.
# Hence I am dropping the variable 'Fuel_Price' as it is not explaining much of my dependent variable 'Weekly_Sales' in this significance based model.
# Lets build another model by excluding the variable 'Fuel_Price'.

Second_Significance_Linearmodel=lm(Weekly_Sales~Temperature+CPI+Unemployment,data=Store_1_Data)
summary(Second_Significance_Linearmodel)

# Inference :
# Variable 'Unemployment' has a highest p value of 0.16231 which exceeds 0.05 threshold and are statistically insignificant.
# Lets drop this variable too and build another model.

Third_Significance_Linearmodel=lm(Weekly_Sales~Temperature+CPI,data=Store_1_Data)
summary(Third_Significance_Linearmodel)

# Inference :
# Now, we only have two variables left in the Third_Significance_Linearmodel.
# In case of Regression :
# Null Hypothesis(H0) - Independent variable is not influencing dependent variable.(i.e p value>0.05)
# Alternate Hypothesis(H1) - Independent variable is influencing dependent variable.(i.e p value<0.05)
# As per this model summary, p value for both the Independent variables 'Temperature' and 'CPI' are well within the p value threshold of 0.05 and are statistically significant.
# In other words, Independent variables - 'Temperature' and 'CPI' are actually influencing the dependent variable 'Weekly_Sales'.
# Since p value is less than 0.05, we can reject the Null Hypothesis and hence Alternate Hypothesis(H1) holds true.



# Model building Approach 3 : Step model building :
names(Store_1_Data)
Step_Linearmodel=lm(Weekly_Sales~Temperature+Fuel_Price+CPI+Unemployment,data=Store_1_Data)
Step_final_Linearmodel=step(Step_Linearmodel)
summary(Step_final_Linearmodel)

# Inference :
# The model started iterating and gave me an optimum model (i.e model with a least possible AIC score=3407.53) with variables - Temperature,CPI and Unemployment.
# Only the following independent variables - Temperature,CPI,Unemployment is contributing to the dependent variable Weekly_Sales.



### Concluding on the best model :
# Lets now compare the linear models built using VIF approach,Significance approach and Step Approach.
# As per the documented results that I infer from the respective model summary, 
# "Third_Significance_Linearmodel" is the best model I can suggest to the business.
# Reason : Difference between R2 and adjusted R2 is less compared to other Linear models built.
# What this tells me is that this model has very little insignificance between R2 and adjusted R2 compared to other models.
# Though this model has a very low R2 of 11.39, based on the available data,this is the best model among the other models that we built.


# Error related Assumptions :
# Lets perform Error related assumptions on the best available model "Third_Significance_Linearmodel".

# Assumption 1 : Homoscedasticity and Normal Distribution of Errors :
plot(Third_Significance_Linearmodel)

# Inference :
# In the Residuals vs Fitter Values graph,Data is being distributed across the graph and we see a red line in the middle with no trend ( the line is flat). 
# All the Error terms are randomly distributed over the places.What this tells us is that we have homoscedasticity of Error terms.
# In the QQ plot, I could see the normally distributed data with few data points away from the line, which tells us there are outliers in the data.
# I am not checking the other 2 plots as we have already confirmed normal distribution and homoscedasticity of error terms based on Residuals vs Fitted graph and QQplot.

# Assumption 2 : Autocorrelation of Error Terms :
library(zoo)
library(lmtest)
# Performing a Durbin-Watson test for Autocorrelation check :
dwtest(Third_Significance_Linearmodel)

# Inference :
# Null Hypothesis (H0) is that autocorrelation does not exist.(if p value is >0.05 - H0 is true)
# Alternate Hypothesis (H1) is that autocorrelation does exist.(if p value is <0.05 then H1 is true)
# Based on Durbin-Watson test results, we got a p value which is less than 0.05.
# Hence, Null Hypothesis(H0) gets rejected and alternate Hypothesis(H1) holds true. i.e there is Autocorrelation amongst the Error terms.

# MAPE (Mean Absolute Percentage Error) :
mean(abs(Third_Significance_Linearmodel$residuals/Store_1_Data$Weekly_Sales))
# We have MAPE value of almost 6.16%
# Recommended percentage of MAPE is <=5%. In this case, MAPE value is a little higher than 5%.
# This MAPE is an accuracy measure that tells me the relative percentage of errors in my Model.
# Though this is not a very good model, I conclude that this is somewhat a decent model with better MAPE for the available dataset.


### Weekly_Sales Prediction using Linear model - "Third_Significance_Linearmodel" : 
Predicted_sales<-as.data.frame(Third_Significance_Linearmodel$fitted.values)
colnames(Predicted_sales)<-"Predicted_Weekly_Sales"
View(Predicted_sales)
# Now, lets see how the Actual Weekly_Sales looks like
Actual_sales<-as.data.frame(Store_1_Data[,3])
class(Actual_sales)
colnames(Actual_sales)<-"Actual_Weekly_Sales"
View(Actual_sales)
# Now, lets find the residual - i.e difference of Sales between Actual and Predicted Weekly_Sales.
Residuals_sales<-as.data.frame(Third_Significance_Linearmodel$residuals)
colnames(Residuals_sales)<-"SalesDiff_ActualvsPredicted"
View(Residuals_sales)
# Lets, now consolidate the Prediction results in a logical way
Sales_Stats<-cbind(Store_1_Data[,c(1,2,9,10,11,12)],Actual_sales,Predicted_sales,Residuals_sales)
View(Sales_Stats)



# Question - 2 : Change dates into days by creating new variable.
# Lets create a dataframe "WD" and extract the Weekday information from Date column in "Sales_Stats" Dataframe
WD<-as.data.frame(c(weekdays(Sales_Stats$Date)))
colnames(WD)[1]<-"Weekday"
View(WD)
# Adding Weekday column from "WD" dataframe to the "Sales_Stats" Dataframe.
# Since I have Date,Sem,Month,Qtr columns in a sequential order in "Sales_Stats", let me add Weekday column next to it.
# To make use of add_column functionality, I am importing a library "tibble".
library("tibble")
Sales_Stats=add_column(Sales_Stats,WD,.after=6)
View(Sales_Stats)


# Exporting the Sales_Stats dataset :
write.csv(Sales_Stats,file="D:/Sales_Stats.csv",row.names = FALSE)
