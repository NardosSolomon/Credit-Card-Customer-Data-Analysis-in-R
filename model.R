library(readxl)
library(tidyverse)
library(ggcorrplot)
library(stargazer)

#load data
CustomerData <- read_excel("CustomerData_M(1).xlsx", sheet = "Sheet1")

df<- CustomerData%>%
  select(CustomerID,Gender,Age, EducationYears, UnionMember, EmploymentLength,
         HHIncome,DebtToIncomeRatio,OtherDebt, CreditDebt)

#Data structure
str(df)
#data has 5000 observations and 10 variables 

#remove duplicate and NAs
df<- df%>%
  distinct()%>%
  drop_na()

str(df)
#After removing duplicate and missing values data has 4967 observations
#and 10 variables.

#Remove variable column ID 
df$CustomerID<- NULL

#summary of the data 
summary(df)

#Data distribution
#Age
hist(df$Age, main = "Age Distribution", xlab = "Age", breaks = 30)
#age is slightly skewed to the right.

hist(df$EducationYears, main = "EducationYears Distribution", xlab = "Years",
     breaks = 30)
#The data follows a normal distribution

hist(df$HHIncome, main = "HHIncome Distribution", 
     xlab = "HHIncome",breaks = 30)
#The data is strongly skewed to the right 

hist(df$CreditDebt, main = "CreditDebt Distribution", 
     xlab = "CreditDebt",breaks = 30)
#The data is also strongly skewed to the right and does not 
#follow a normal distribution. 

#categorical variables 
ggplot(data = df, aes(x=Gender, fill=Gender))+
  geom_bar()+
  labs(title = "Gender")+
  theme_bw()
#males and female are equally distributed in the data 

ggplot(data = df, aes(x=UnionMember, fill=UnionMember))+
  geom_bar()+
  labs(title = "UnionMember")+
  theme_bw()
# Most of the customers are not in any trade union. 

#outliers 
boxplot(df$CreditDebt)

#the data contains outliers We will use the out come variables 
# to remove the outliers. 

out= boxplot(df$CreditDebt, plot = F)$out
df<-df[-which(df$CreditDebt %in% out),]

#correlation
cor_df<-df%>%
  select_if(is.numeric)%>%
  cor()

ggcorrplot(cor_df,hc.order = TRUE,
           type = "lower", lab = TRUE, insig = "blank")

#The correlation between creditdebt and otherdebt is a positive moderate correlation.
#Likewise the correlation between debttoincome ration and creditdebt is positive modarate correlation

#Relationship using scatterplot

ggplot(data = df, aes(x= DebtToIncomeRatio, y= CreditDebt))+
  geom_point()+
  geom_smooth(se=F, method = 'lm')+
  labs(title = "DebtToIncomeRatio vs CreditDebt")+
  theme_bw()

#as DebtToIncomeRatio increase creditcard debt also increases

ggplot(data = df, aes(x= HHIncome, y= CreditDebt))+
  geom_point()+
  geom_smooth(se=F, method = 'lm')+
  labs(title = "HHIncome vs CreditDebt")+
  theme_bw()

#as HHIncome increases credictcard debt also increase

#building the model. 
#Is the a relationship between credit card debt and HHIncome
#H0; There is no relationship
#H1 There is a relationship

m1<- lm(CreditDebt~HHIncome, data = df)
summary(m1)
stargazer(m1, type = "text")
# the pvalue is less than 0.5, we reject the null hypothesis and conclude
# that there is a relationship between HHINCOME and credit card debt.
#at 0.05 significance level. the rsquared is 0.188. whic means that 
#that 18.8% of the variability observed in the target variable
#is explained by the regression model.

# as one unit increase in HHINCome will increase credit debt by 1.147e-05 units.



#And few variables to the model

m2<- lm(CreditDebt~HHIncome+OtherDebt+DebtToIncomeRatio, 
        data = df)
summary(m2)
stargazer(m2, type = "text")
#The main IV is still significant. We add two more IVs i.e OtherDebt+DebtToIncomeRatio,
#which are also significant at 0.05. The r-squared has improved to 0.4604
#which means that 
#that 46.04%% of the variability observed in the target variable
#is explained by the regression model.

#use all the variables 
m3<- lm(CreditDebt~., data = df)
summary(m3)
stargazer(m3, type = "text")

#only our three IVs from model 2 are statistically significant at 0.05.
#However, we see a slight improvement in the r-squared to 0.4607.
#This means that add the other variables helps improve our model. 
# now 46.07%% of the variability observed in the target variable
#is explained by the regression model

#using log y
df2<- df
df2$CreditDebt<- log(df2$CreditDebt)
df2$CreditDebt<-ifelse(is.infinite(df2$CreditDebt),NA, df$CreditDebt)
#df<- na.omit(df)
m4 <- lm(CreditDebt~., data = df2)
summary(m4)

#In this case we have converted our independent variable to log. 
#note that inf values from the log were converted to NAs. 
#We run the model using all the Ivs in the data. 
#The r-squared again improved by a slight margin to 0.4615. 
#The number of significant variables remains 3 as in our previous model.


#model assumptions
par(mfrow=c(2,2))
plot(m4)

# Residuals vs Fitted. 
# Used to check the linear relationship assumptions. 
# A horizontal line, without distinct patterns is an 
# indication for a linear relationship, what is good.
#There is a distintic pattern in our plot so the assumption was not met. 

# 
# Normal Q-Q. 
# Used to examine whether the residuals are normally distributed. 
# It’s good if residuals points follow the straight dashed line.
#The residuals does not follows the dash lines there for the normally distributed assumption was not met
# 
# Scale-Location (or Spread-Location). 
# Used to check the homogeneity of variance of the residuals
# (homoscedasticity). Horizontal line with equally spread points 
# is a good indication of homoscedasticity. 
#The assumption was not met. 
# 
# Residuals vs Leverage.
# Used to identify influential cases, that is extreme values 
# that might influence the regression results when included or
# excluded from the analysis.
#There are a few influential values as shown in the plot(4483,686,1874).The 
#do seem to be problematic we can assume that the assumption was not met. 
