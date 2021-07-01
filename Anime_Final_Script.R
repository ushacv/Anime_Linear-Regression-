######################## Task 1 deliverables  #############################

1.
#Problem Statement: Objective is to predict "Rating " given by the viewers based on 
#                   parameters of ANIME releases.

2.
# Loading(Importing) the raw Data
Anime_Final=read.csv('C:/Users/harsha/Desktop/Stats+R/CSV Files/Anime_Final.csv',
                     na.strings = c(""," ","NA","NULL","[]"), stringsAsFactors = T)
Anime_Final

View(Anime_Final)

### Target variable - "Rating" is in continuous in nature - hence, Linear Regression

# number of observations and variable
dim(Anime_Final)
summary(Anime_Final)
 # 7029 observation and 16 variables

3.
#Identifying the type of variables:
# Column Nature -

# title, description,tags,studios---- qualitative
# mediaType ,Ongoing, SznofRelease,contentWarn ---- categorical 
# eps, duration, watched, watching, 
# wantwatch,dropped, rating, votes ---- continuous columns

  
#Removing Useless column in the data , and explore the rest
Uselesscolumns = c('title','sznOfRelease','studios','description','contentWarn','tags')
Anime_Final[ ,Uselesscolumns] = NULL

# Title       = 7029 unique category ,which is difficult to club together and create inflation
#               in data hence removed
#SznofRelease = more than 30% missing value, which require huge manipulation to fill up
#               missing positions lead to loss of original nature of column 
#Studios      = it consists of 532 unique value,qualitative in nature and more than 30% missing value hence dropped
#description  = its contains 3923 unique description and qualitative in nature ,which
#               can create huge inflation in data hence eliminated
#Contentwarn  = Unique value is 131 and missing value is >30% hence dropped
#tags         = its in qualitative in nature with unique value 4869 and  missing value is 229.It also does not
#               serve importance to predict rating in our analysis hence removed


# Exploring the data set
str(Anime_Final)
head(Anime_Final)

# All columns are in correct Data Type so no need to change

############################################################################
4.
# Data pre-processing:

# checking and treating Missing Values
colSums(is.na(Anime_Final))

# watched - 87 missing value
# As watched is continuous in nature - we process with "Median" 

Anime_Final$watched[is.na(Anime_Final$watched)]=round(median(Anime_Final$watched,
                                                       na.rm = TRUE),digits = 0)

# checking the missing value after imputation in Watched column
colSums(is.na(Anime_Final))

# Duration -1696 Missing Value (24%)
# As Duration is continuous in nature - we process with "Median"

Anime_Final$duration[is.na(Anime_Final$duration)]=round(median(Anime_Final$duration,
                                                         na.rm = TRUE),digits = 0)

# checking the missing value after imputation 
colSums(is.na(Anime_Final))

# MediaType - 30 missing value
# As Mediatype is categorical in nature - we process with Mode Function

FunctionMode = function(inpData){
  Modevalue = names(table(inpData)[table(inpData)==max(table(inpData))])
  return(Modevalue)
}

FunctionMode(Anime_Final$mediaType)

#Imputing missing values for categorical columns

Anime_Final$mediaType[is.na(Anime_Final$mediaType)] = "TV"

#checking the missing value after imputation in mediaType column

colSums(is.na(Anime_Final))

########### detection of outliers and treatment of extreme outliers ##################
# eps

  boxplot(Anime_Final$eps, horizontal = T)
  max(Anime_Final$eps)
 
  quantiles=quantile(Anime_Final$eps, c(0.997, 0.9974, 0.9975))
  quantiles
 
  quantiles_final=round(quantile(Anime_Final$eps, 0.9975), digits = 0)
  quantiles_final
 
  Anime_Final$eps=ifelse(Anime_Final$eps>quantiles_final,quantiles_final, Anime_Final$eps)
 
  boxplot(Anime_Final$eps, horizontal = T)
  max(Anime_Final$eps)

# watched
  boxplot(Anime_Final$watched, horizontal = T)
  max(Anime_Final$watched)
 
  quantiles=quantile(Anime_Final$watched, c(0.99872, 0.99875, 0.99877))
  quantiles
 
  quantiles_final=round(quantile(Anime_Final$watched, 0.99877), digits = 0)
  quantiles_final
 
  Anime_Final$watched=ifelse(Anime_Final$watched>quantiles_final,quantiles_final,Anime_Final$watched)
 
  boxplot(Anime_Final$watched, horizontal = T) 
  max(Anime_Final$watched) 
 
#watching
  boxplot(Anime_Final$watching, horizontal = T)
  max(Anime_Final$watching)                                

  quantiles=quantile(Anime_Final$watching, c(0.9978, 0.9979,0.998))
  quantiles
 
  quantiles_final=round(quantile(Anime_Final$watching, 0.998), digits = 0)
  quantiles_final
 
  Anime_Final$watching=ifelse(Anime_Final$watching>quantiles_final, quantiles_final, Anime_Final$watching)
  boxplot(Anime_Final$watching,horizontal = T)
  max(Anime_Final$watching)
 
# wantWatch
  
  boxplot(Anime_Final$wantWatch, horizontal = T) 
  max(Anime_Final$wantWatch)
  
  quantiles=quantile(Anime_Final$wantWatch, c(0.9993, 0.9994, 0.99942))
  quantiles                   
 
  quantiles_final=round(quantile(Anime_Final$wantWatch, 0.99942), digits = 0)
  quantiles_final
  
  Anime_Final$wantWatch=ifelse(Anime_Final$wantWatch>quantiles_final, quantiles_final, Anime_Final$wantWatch)
  boxplot(Anime_Final$wantWatch, horizontal = T)
  max(Anime_Final$wantWatch)
  
#dropped
  boxplot(Anime_Final$dropped, horizontal = T)
  max(Anime_Final$dropped)
  
  quantiles=quantile(Anime_Final$dropped, c(0.9992,0.9993,0.99931))
  quantiles
  
  quantiles_final=round(quantile(Anime_Final$dropped, 0.99931), digits = 0)
  quantiles_final
  
  Anime_Final$dropped=ifelse(Anime_Final$dropped>quantiles_final, quantiles_final, Anime_Final$dropped)
  boxplot(Anime_Final$dropped, horizontal = T)

#votes
  boxplot(Anime_Final$votes, horizontal = T)
  max(Anime_Final$votes)  

  quantiles=quantile(Anime_Final$votes, c(0.998,0.9985,0.9987)) 
  quantiles
  
  quantiles_final=round(quantile(Anime_Final$votes, 0.9987), digits = 0)
  quantiles_final
  
  Anime_Final$votes=ifelse(Anime_Final$votes>quantiles_final, quantiles_final, Anime_Final$votes)
  boxplot(Anime_Final$votes, horizontal = T)

#duration 
  boxplot(Anime_Final$duration, horizontal = T)  
  max(Anime_Final$duration)
  
  quantiles=quantile(Anime_Final$duration, c(0.999, 0.9991, 0.9992))
  quantiles
  
  quantiles_final=round(quantile(Anime_Final$duration, 0.9992), digits = 0)
  quantiles_final
  
  Anime_Final$duration=ifelse(Anime_Final$duration>quantiles_final, quantiles_final, Anime_Final$duration)
  boxplot(Anime_Final$duration, horizontal = T)
#############################################################################    
5. 
#Univariate Analysis:

#exploring each "Potential"predictor for Distribution and Quality

############## Exploring Single Continuous feature#######################

hist(Anime_Final$rating)
summary(Anime_Final$rating)

############## Exploring Multiple Continuous features ################

ColsForHist=c("rating","eps","duration","watched","watching","wantWatch",
              "dropped","votes")

ColsForHist
par(mfrow=c(2,4))

for (hist_cols in ColsForHist) {
  hist(Anime_Final[ ,c(hist_cols)], main = paste('Histogram of:', hist_cols),
       col = brewer.pal(8,"Paired"))
}

library(RColorBrewer)

############## Exploring Multiple categorical feature ###############

ColsForBar=c("mediaType","ongoing")
ColsForBar
par(mfrow=c(1,2))

for (bar_cols in ColsForBar) {
  barplot(table(Anime_Final[ ,c(bar_cols)]), main = paste('Histogram of:', bar_cols),
          col = brewer.pal(8,"Paired"))
}
# in ongoing column "NO" category is almost throughout in plot so will decide whether to keep this 
# column or not based on ANOVA test

##########################################################################
# Bivariate Analysis  

#Visual Relationship between Predictors and Targetvariable 
# Continuous Vs Continuous ---- Scatter Plot
# Continuous Vs Categorical --- Box Plot

######### Continuous Vs Continuous - Scatter Plot #########

#Single continuous Variable

par(mfrow=c(1,1))

plot(x=Anime_Final$votes, y=Anime_Final$rating, col ="Blue")

#Multiple Continuous Variable

continuousCols=c("rating","eps","duration","watched","watching","wantWatch",
                 "dropped","votes")

continuousCols

plot(Anime_Final[ ,continuousCols], col = "Blue")

######### Continuous Vs Categorical Visual Analysis : BOXPLOT #########

categorical_cols=c("mediaType","ongoing")
categorical_cols

par(mfrow=c(1,2))

for (box_cols in categorical_cols) {
  boxplot(rating~(Anime_Final[ , c(box_cols)]), data = Anime_Final,
          main = paste('Box Plot of:', box_cols), col = brewer.pal(8,"Paired"))
}

##############################################################################
# strength of relationship between predictors and target variables.
# Continuous Vs Continuous ---- Correlation test
# Continuous Vs Categorical---- ANOVA test

 ######### Continuous Vs Continuous --Correlation test #########

# Single continuous variable

cor(Anime_Final[ ,c("rating","wantWatch")], use = "complete.obs")

# positive relation between "rating" and "wantWatch" as r is positive but close to 0.5

# Multiple Continuous Variable

continuousCols=c("rating","eps","duration","watched","watching","wantWatch",
                 "dropped","votes")
continuousCols

corrData=cor(Anime_Final[ ,continuousCols], use = "complete.obs")
corrData

#Final Continuous columns to be selected for modeling 
#The correlation is very low for almost all the columns, hence decreasing the threshold

names(corrData['rating',][abs(corrData['rating',])>0.4])

#"watched","wantWatch","votes" - are good variable 

######### Continuous Vs Categorical Correlation strength - ANOVA#########

#H0 := variables are not Correlated

colsforAnova=c("mediaType","ongoing")
colsforAnova

for (Aovcols in colsforAnova) {
  Anovaresult=summary(aov(Anime_Final$rating~Anime_Final[ ,c(Aovcols)]))
  print(Aovcols)
  print(Anovaresult)
}

# P is very much <0.05, hence we reject the null hypothesis
# conclusion : Very high correlation between Target Variable(rating) and 
# predictor variables(mediaType,ongoing)


######################## Task 2 deliverables  ###############################
# Log Transformation :

# First,we will check skewness of "votes" and wantwatch" column as these variable's  
# absolute value of correlation with the target variable is >0.4.


# we will check this by logarithmic transformation and see improvement on histogram and 
# correlation test as shown below:

# votes:

hist(Anime_Final$votes)
cor(x=Anime_Final$votes, y=Anime_Final$rating)

# As We can see from graph, it's highly skewed in nature and correlation between this
# two variable is 0.415

hist(log(Anime_Final$votes))
cor(x=log(Anime_Final$votes), y=Anime_Final$rating)

# After Logarithmic transformation ,we can see that skewness of graph has reduced and correlation value is also 
# improve to 0.724.

# Create new Log column of "Votes" in Anime_Final Dataset:

Anime_Final$votes_new=log(Anime_Final$votes)

# wantWatch:

hist(Anime_Final$wantWatch)
cor(x=Anime_Final$wantWatch, y=Anime_Final$rating)

#As We can see from graph, it's highly skewed in nature and correlation between this
# two variable is 0.5484

# Treating the Zeros in the column: 

Anime_Final$wantWatch[Anime_Final$wantWatch==0]=1

# Apply Log :

hist(log(Anime_Final$wantWatch))
cor(x=log(Anime_Final$wantWatch), y=Anime_Final$rating)

# After Logarithmic,we can see that skewness of graph has reduced and correlation value is also 
# improve to 0.7643.

# Create new Log column of "wantWatch" in Anime_Final Dataset:

Anime_Final$wantWatch_new=log(Anime_Final$wantWatch)

#####################################################################################
6.
# Feature selection
# Generating the Data frame for machine learning

InputData=Anime_Final
TargetVariableName='rating'
TargetVariableName

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis

BestPredictorName= c("watched","wantWatch_new","mediaType","ongoing","votes_new")
BestPredictorName

## Extracting Target and predictor variables from data to create a generic dataset

TargetVariable=InputData[ ,c(TargetVariableName)]
TargetVariable
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable

PredictorVariable=InputData[ ,BestPredictorName]
PredictorVariable
str(PredictorVariable)

##creating the final data to be used for ML

DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

#############################################################################
7.
# Splitting the data into train & test:

set.seed(123)

TrainingSampleIndex=sample(1:nrow(DataForML), size = 0.7*nrow(DataForML))
length(TrainingSampleIndex)

#Use those row indexes which are selected in TrainingSampleIndex for train set
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTrain

#Use those row indexes which are NOT selected in TrainingSampleIndex for test set
DataForMLTest=DataForML[ -TrainingSampleIndex, ]
DataForMLTest

#Rows selected for training will not be selected for testing
dim(DataForMLTrain)
dim(DataForMLTest)

#############################################################################
8.
# Model Building:

# Linear Regression 

startTime=Sys.time()

Model_Reg_1=lm(TargetVariable~. ,data = DataForMLTrain)
summary(Model_Reg_1)

endTime=Sys.time()
endTime-startTime

# removed -mediaType Other (0.496)
Model_Reg_2=lm(TargetVariable~ ongoing+watched+
                 wantWatch_new+votes_new+I(mediaType=="Movie")+I(mediaType=="TV")
               +I(mediaType=="Music Video")+I(mediaType=="OVA")+I(mediaType=="TV Special")
               +I(mediaType=="Web"), data = DataForMLTrain)
summary(Model_Reg_2)

# removed - watched (0.5242)
Model_Reg_3=lm(TargetVariable~ ongoing+
                 wantWatch_new+votes_new+I(mediaType=="Movie")+I(mediaType=="TV")
               +I(mediaType=="Music Video")+I(mediaType=="OVA")+I(mediaType=="TV Special")
               +I(mediaType=="Web")    , data = DataForMLTrain)
summary(Model_Reg_3)

# removed- mediaType web (0.236)
Model_Reg_4=lm(TargetVariable~ ongoing+
                 wantWatch_new+votes_new+I(mediaType=="Movie")+I(mediaType=="TV")
               +I(mediaType=="Music Video")+I(mediaType=="OVA")+I(mediaType=="TV Special")
               , data = DataForMLTrain)
summary(Model_Reg_4)

# removed- votes_new(0.135)
Model_Reg_5=lm(TargetVariable~ ongoing+
                 wantWatch_new+I(mediaType=="Movie")+I(mediaType=="TV")
               +I(mediaType=="Music Video")+I(mediaType=="OVA")+I(mediaType=="TV Special")
               , data = DataForMLTrain)
summary(Model_Reg_5)

# removed- mediaType Tv(0.119)
Model_Reg_6=lm(TargetVariable~ ongoing+
                 wantWatch_new+I(mediaType=="Movie")
               +I(mediaType=="Music Video")+I(mediaType=="OVA")+I(mediaType=="TV Special")
               , data = DataForMLTrain)
summary(Model_Reg_6)


#Multiple R-squared :0.6168, Adjusted R-Squared : 0.6163

##########################################################################
9.
# Multicollinearity check: 
library(car)

VIF=vif(Model_Reg_6)
data.frame(VIF)

#VIF
#ongoing                       1.006493
#wantWatch_new                 1.366283
#I(mediaType == "Movie")       1.068373
#I(mediaType == "Music Video") 1.388480
#I(mediaType == "OVA")         1.067163
#I(mediaType == "TV Special")  1.022819



#####################################################################################
10. 
# Model accuracy:
# checking accuracy on Testing Data   

head(DataForMLTest)
DataForMLTest$Pred_LM=predict(Model_Reg_6, DataForMLTest)
head(DataForMLTest)

# Calculating  the absolute Percentage Error for each prediction

DataForMLTest$LM_APE=100*(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/
                            DataForMLTest$TargetVariable)
head(DataForMLTest)

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)
print(paste('## Mean Accurcy of Linear Regression Model is:', 100 - MeanAPE))
print(paste(' ## Median Accuracy of Linear Regression Model is:', 100 - MedianAPE))


# Accuracy :
# 1. MeanAPE         = 18.106
# 2. MedianAPE       = 11.460
# 3. Mean Accuracy   = 81.893%
# 4. Median Accuracy = 88.539%

####################################################################################
##########################  Decision Tree  #########################################
1. 
# generating Decision Tree 

library(party)

Model_CTREE = ctree(TargetVariable~. , data = DataForMLTrain)
Model_CTREE

plot(Model_CTREE)

2.
#Checking Accuracy of model on  Testing Data

DataForMLTest$Pred_CTREE=as.numeric(predict(Model_CTREE,DataForMLTest))
head(DataForMLTest)

DataForMLTest$CTREE_APE=100*(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_CTREE)/
                                   DataForMLTest$TargetVariable)
head(DataForMLTest)

print(paste('## Mean Accuracy of Decision tree Model is:', 100 -mean(DataForMLTest$CTREE_APE)))
print(paste('## Median Accuracy of Decision Tree Model is:', 100 - median(DataForMLTest$CTREE_APE)))

# Accuracy :
# Mean Accuracy   = 81.774
# Median Accuracy = 88.158

#Conculsion :  Linear Regression shows accuracy slightly higher than Decision Tree.


######################################################################################
# Business Recommendation :

1.

#we can see from VIF value that movie, Music video, Ova(original video animation) , 
#Tv, TV Special -these are the Media more popular for viewers to rate for Anime.
#Additionally, Anova test also shows high correlation with Mediatype. 
#so, its recommended that these are the platform should use more to increase 
#the number of viewer to rate and help to increase revenue ultimately.

2.

#Ongoing : as this column consist of most of  "No" category and very few"yes" category so,
#it's difficult to judge the impact on this column on rating hence more data is required to get 
#insight of exact importance of this column towards rating although ANOVA test show good correlation with rating.

3.

#if the year of release Anime and year/month of ended data will be added then 
#it will help to analyze in which year, what kind of shows(title) get popularity 
#so, we can produce that kind of shows more to get more acceptance.
 
4.

#Anime with the most episode is released on Spring season, which has contributed majorly
#towards rating column but as SznofRelease column contain more than 30% missing value, which
#can lead misleading information to get result hence I have dropped this column for analysis,but 
#to get clear insight of this data more data of this column required so that we can confirm 
#the best season to gain more popularity among audience.This column also have influence on 
#mediaType, as we can see from VIF value of TV Special, movie, Music Video and Ova has high 
#positive relationship with rating but season for this predictor in sznforRelease column is missing
#hence more data of this column helps us to predict more favorable season for viewers.

5.

#columns such as : eps, duration, watching, dropped --> these columns do not show 
#very positive correlation with rating as the correlation value is 0.13 - 0.32 whereas,
#watched, wantwatch and votes are slightly more positive correlated means the factors are linear with rating column.
#Also, we can see most of mediaType for all these columns contribute towards "high Rating" in data is "TV".
#means, these columns have influence on mediatype.
#Through analysis , we also come to know that "wantwatch" column has greater impact on improving model performance 
#but do get benefit of this column, we required more data of wantwatch to obtained high positive linearity with rating column.
#This can be done by recommending next shows  as "advertisement" that viewers might be interested to see.

