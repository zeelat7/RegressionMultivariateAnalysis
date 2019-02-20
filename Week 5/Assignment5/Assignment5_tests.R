# Zeeshan Latifi
# 10.21.2017
# ames_waterfall.R

# Read in csv file for Ames housing data;

# Note that back slash is an escape character in R so we use \\ when we want \;
path.name <- '/Users/Zeeshan/Desktop/PREDICT 410/Week 1/';
file.name <- paste(path.name,'ames_housing_data.csv',sep='');

# Read in the csv file into an R data frame;
amesiowa.df <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);

# Single ifelse() statement
# ifelse(condition, value if condition is TRUE, value if the condition is FALSE)

# Nested ifelse() statement
# ifelse(condition1, value if condition1 is TRUE,
#	ifelse(condition2, value if condition2 is TRUE,
#	value if neither condition1 nor condition2 is TRUE
#	)
# )


# Create a waterfall of drop conditions;
# Work the data frame as a 'table' like you would in SAS or SQL;
amesiowa.df$dropConditions <- ifelse(amesiowa.df$SubClass!= 020 & amesiowa.df$SubClass != 060 & amesiowa.df$SubClass != 080,'01: Not SFR',
  ifelse(amesiowa.df$Zoning!='RH' & amesiowa.df$Zoning!='RL' & amesiowa.df$Zoning!='RM','02: Non-Residential Zoning',
  ifelse(amesiowa.df$Street!='Pave','03: Street Not Paved',
  ifelse(amesiowa.df$Utilities!='AllPub', '04: Not All Utilities Included',
  ifelse(amesiowa.df$OverallQual<5, '05: Overall Quality Under 5',
  ifelse(amesiowa.df$OverallCond<5, '06: Overall Condition Under 5',
  ifelse(amesiowa.df$YearBuilt<1950, '07: Homes Built Pre-1950',
  ifelse(amesiowa.df$ExterQual!='TA' & amesiowa.df$ExterQual!='Gd'& amesiowa.df$ExterQual!='Ex', '08: Below Good Exterior Quality',
  ifelse(amesiowa.df$ExterCond!='TA' & amesiowa.df$ExterCond!='Gd'& amesiowa.df$ExterCond!='Ex', '09: Below Good Exterior Condition',
  ifelse(amesiowa.df$FirstFlrSF<800, '10: First Floor Under 800 SqFt',
  ifelse(amesiowa.df$CentralAir!='Y', '11: No Central Air',
  ifelse(amesiowa.df$PavedDrive!='Y', '12: No Paved Driveway',
  ifelse(amesiowa.df$BldgType!='1Fam', '13: Not a Single Family Home',
  ifelse(amesiowa.df$LotArea<5000 | amesiowa.df$LotArea>20000, '14: Not a Normal Lot Area',
  ifelse(amesiowa.df$GrLivArea>2000, '15: Abnormal Ground Living Area',
  ifelse(amesiowa.df$GarageFinish=='NA', '16: No Garage',       
  '99: Eligible Sample')
  )))))))))))))));


table(amesiowa.df$dropConditions)

# Save the table
waterfalls <- table(amesiowa.df$dropConditions);

# Format the table as a column matrix for presentation;
as.matrix(waterfalls,15,1)


# Eliminate all observations that are not part of the eligible sample population;
myeligible.population <- subset(amesiowa.df,dropConditions=='99: Eligible Sample');

# Check that all remaining observations are eligible;
table(myeligible.population$dropConditions);

head(myeligible.population)

##########################################################################################
#Assignment 5 
#Part 2 predictive modeling framework

set.seed(123)
myeligible.population$u <- runif(n=dim(myeligible.population)[1],min=0,max=1);

myeligible.population$QualityIndex <- myeligible.population$OverallQual*myeligible.population$OverallCond; 
myeligible.population$TotalSqftCalc <- myeligible.population$BsmtFinSF1 + myeligible.population$BsmtFinSF2 + myeligible.population$GrLivArea;
myeligible.population$TotalBaths <- myeligible.population$BsmtFullBath + myeligible.population$BsmtHalfBath*0.5 +
  myeligible.population$FullBath + myeligible.population$HalfBath*0.5


# Create train/test split;
train.df <- subset(myeligible.population, u<0.70); 
test.df <- subset(myeligible.population, u>=0.70);
# Check your data split. The sum of the parts should equal the whole. # Do your totals add up?
dim(myeligible.population)[1]
dim(train.df)[1]
dim(test.df)[1] 
dim(train.df)[1]+dim(test.df)[1]

framework.table <- matrix(c(dim(train.df)[1],dim(test.df)[1], dim(train.df)[1]+dim(test.df)[1]),ncol=3,byrow=TRUE)
colnames(framework.table) <- c("Training Set","Test Set","Total Set")
rownames(framework.table)<-c("Count")
fm.table <- as.table(framework.table)
fm.table
##########################################################################################
#Assignment 5 
#Part 3 Model Identification by Automated Variable Selection

drop.list <- c('SID','PID','LotConfig','dropConditions','Utilities','Zoning','LotFrontage','Street', 'Fence',
               'Exterior1','Exterior2','BsmtFinSF1','BsmtFinSF2','CentralAir','YrSold','MoSold','SaleCondition',
               'u','train','I2010','BsmtFullBath','BsmtHalfBath','FullBath','HalfBath', 'FireplaceInd1',
               'FireplaceInd2','RoofStyle','RoofFlat','PoolArea','LandContour','LandSlope','HeatingQC', 'PoolQC',
               'Alley','FireplaceQu','MiscFeature','KitchenAbvGr','LowQualFinSF','Functional','EnclosedPorch', 
               'ThreeSsnPorch','PavedDrive','BldgType','RoofMat','Condition2','BsmtCond', 'Electrical','GarageQual',
               'GarageCond','ScreenPorch','MasVnrArea','BsmtQual','BsmtExposure','BsmtFinType1','BsmtFinType2','Heating',
               'GarageType','Neighborhood','OverallQual','OverallCond','QualityIndex');

train.clean <-train.df[,!(names(myeligible.population) %in% drop.list)];
head(train.clean)

colnames(train.clean)


#Model Identification
library(MASS)

# Define the upper model as the FULL model 
upper.lm <- lm(SalePrice ~ .,data=train.clean); 
summary(upper.lm)

# Define the lower model as the Intercept model 
lower.lm <- lm(SalePrice ~ 1,data=train.clean);
summary(lower.lm)

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean); 
summary(sqft.lm)

#unlist(lapply(train.clean, function(x) any(is.na(x))))

# Call stepAIC() for variable selection
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1),direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward')); 
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1), direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=train.df)
summary(junk.lm)

# Compute the VIF values
library(car) 
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE) 
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)


vif(forward.lm)
vif(backward.lm)
vif(stepwise.lm)
vif(junk.lm)

forward.info <- c(AIC(forward.lm),BIC(forward.lm), mean(abs(forward.lm$residuals)), mean(forward.lm$residuals^2), 
                  summary(forward.lm)$adj.r.squared, 1)

backward.info <- c(AIC(backward.lm),BIC(backward.lm), mean(abs(backward.lm$residuals)), 
                            mean(backward.lm$residuals^2), summary(backward.lm)$adj.r.squared, 2)

stepwise.info <- c(AIC(stepwise.lm),BIC(stepwise.lm), mean(abs(stepwise.lm$residuals)), 
                            mean(stepwise.lm$residuals^2), summary(stepwise.lm)$adj.r.squared, 3)

junk.info <- c(AIC(junk.lm),BIC(junk.lm), mean(abs(junk.lm$residuals)), mean(junk.lm$residuals^2), 
                        summary(junk.lm)$adj.r.squared, 4)

options(scipen = 9999)
models.info <- matrix(c(AIC(forward.lm),BIC(forward.lm), mean(abs(forward.lm$residuals)), mean(forward.lm$residuals^2), 
                 summary(forward.lm)$adj.r.squared, 1, AIC(backward.lm),BIC(backward.lm), mean(abs(backward.lm$residuals)), 
                 mean(backward.lm$residuals^2), summary(backward.lm)$adj.r.squared, 2, AIC(stepwise.lm),BIC(stepwise.lm), mean(abs(stepwise.lm$residuals)), 
                 mean(stepwise.lm$residuals^2), summary(stepwise.lm)$adj.r.squared, 3, AIC(junk.lm),BIC(junk.lm), mean(abs(junk.lm$residuals)), mean(junk.lm$residuals^2), 
                 summary(junk.lm)$adj.r.squared, 4),ncol=6,byrow=TRUE)

#models.table <- matrix(c(forward.info,backward.info,stepwise.info,junk.info, nrow=6,byrow=TRUE))
colnames(models.info) <- c('AIC','BIC','MAE','MSE','Adj R2','Rank')
rownames(models.info)<-c('Forward','Backward','Stepwise','Junk')

model.tbl <- as.table(models.info)
model.tbl

##########################################################################################
#Assignment 5 
#Part 4 Predictive Accuracy
forward.test <- predict(forward.lm,newdata=test.df);
backward.test <- predict(backward.lm,newdata=test.df);
stepwise.test <- predict(stepwise.lm,newdata=test.df)
junk.test <- predict(junk.lm,newdata=test.df)

forward.pred.mae <- mean(abs(forward.test-test.df$SalePrice))
forward.pred.mse <- mean((forward.test-test.df$SalePrice)^2)

backward.pred.mae <- mean(abs(backward.test-test.df$SalePrice))
backward.pred.mse <- mean((backward.test-test.df$SalePrice)^2)

stepwise.pred.mae <- mean(abs(stepwise.test-test.df$SalePrice))
stepwise.pred.mse <- mean((stepwise.test-test.df$SalePrice)^2)

junk.pred.mae <- mean(abs(junk.test-test.df$SalePrice))
junk.pred.mse <- mean((junk.test-test.df$SalePrice)^2)



##########################################################################################
#Assignment 5 
#Part 5 Operational Validation

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                  ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                  ))
forward.trainTable <- table(forward.PredictionGrade) 
forward.trainTable/sum(forward.trainTable)

#--------------------------------------------------------------------------------
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                  ))
backward.trainTable <- table(backward.PredictionGrade) 
backward.trainTable/sum(backward.trainTable)

#--------------------------------------------------------------------------------
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                  ))
stepwise.trainTable <- table(stepwise.PredictionGrade) 
stepwise.trainTable/sum(stepwise.trainTable)

#--------------------------------------------------------------------------------
junk.pct <- abs(junk.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                  ))
junk.trainTable <- table(junk.PredictionGrade) 
junk.trainTable/sum(junk.trainTable)

#--------------------------------------------------------------------------------
# Test Data

# Abs Pct Error
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice; 
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice; 
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice; 
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice;

# Assign Prediction Grades;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                      ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                      ))
forward.testTable <-table(forward.testPredictionGrade) 
forward.testTable/sum(forward.testTable)

#--------------------------------------------------------------------------------
backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                              ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                       ))
backward.testTable <-table(backward.testPredictionGrade) 
backward.testTable/sum(backward.testTable)


#--------------------------------------------------------------------------------
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                             ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                      ))
stepwise.testTable <-table(stepwise.testPredictionGrade) 
stepwise.testTable/sum(stepwise.testTable)

#--------------------------------------------------------------------------------
junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                              ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                       ))
junk.testTable <-table(junk.testPredictionGrade) 
junk.testTable/sum(junk.testTable)







