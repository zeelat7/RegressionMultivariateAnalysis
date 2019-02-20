# Zeeshan Latifi
# 9.30.2017
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
     '99: Eligible Sample')
     ))))))))))))));


table(amesiowa.df$dropConditions)

# Save the table
waterfalls <- table(amesiowa.df$dropConditions);

# Format the table as a column matrix for presentation;
as.matrix(waterfalls,14,1)


# Eliminate all observations that are not part of the eligible sample population;
myeligible.population <- subset(amesiowa.df,dropConditions=='99: Eligible Sample');

# Check that all remaining observations are eligible;
table(myeligible.population$dropConditions);

head(myeligible.population)

#totalSQFT <- myeligible.population$FirstFlrSF + myeligible.population$TotalBsmtSF + myeligible.population$SecondFlrSF +


##########################################################################################
#Final Table

final.pop <- data.frame(myeligible.population$SubClass, myeligible.population$Zoning, myeligible.population$LotArea, 
                        myeligible.population$Street, myeligible.population$Utilities, myeligible.population$BldgType, 
                        myeligible.population$HouseStyle, myeligible.population$OverallQual, myeligible.population$OverallCond, 
                        myeligible.population$YearBuilt, myeligible.population$YearRemodel,myeligible.population$ExterQual, 
                        myeligible.population$ExterCond, myeligible.population$BsmtFinType1, myeligible.population$FirstFlrSF, 
                        myeligible.population$GarageCars, myeligible.population$PavedDrive, myeligible.population$PoolArea, 
                        myeligible.population$CentralAir, myeligible.population$SalePrice)

head(final.pop)

##########################################################################################
#Data Quality Check
as.data.frame(table(myeligible.population$SubClass))

as.data.frame(table(myeligible.population$Zoning))

summary(myeligible.population$LotArea)
myeligible.population[is.element(myeligible.population$LotArea, max(myeligible.population$LotArea)),]

as.data.frame(table(myeligible.population$Street))

as.data.frame(table(myeligible.population$Utilities))

as.data.frame(table(myeligible.population$BldgType))

as.data.frame(table(myeligible.population$HouseStyle))

as.data.frame(table(myeligible.population$OverallQual))

as.data.frame(table(myeligible.population$OverallCond))

as.data.frame(table(myeligible.population$YearBuilt))
summary(myeligible.population$YearBuilt)

as.data.frame(table(myeligible.population$YearRemodel))
summary(myeligible.population$YearRemodel)

as.data.frame(table(myeligible.population$ExterQual))

as.data.frame(table(myeligible.population$ExterCond))

as.data.frame(table(myeligible.population$BsmtFinType1))

as.data.frame(table(myeligible.population$OverallQual))

summary(myeligible.population$FirstFlrSF)
sd(myeligible.population$FirstFlrSF)

as.data.frame(table(myeligible.population$GarageCars))

as.data.frame(table(myeligible.population$PavedDrive))

summary(myeligible.population$PoolArea)
as.data.frame(table(myeligible.population$PoolArea))

as.data.frame(table(myeligible.population$CentralAir))

summary(final.pop$myeligible.population.SalePrice)
sd(final.pop$myeligible.population.SalePrice)


##########################################################################################
#Exploratory Data Analysis
par(mfrow = c(1,1))

boxplot(myeligible.population$SalePrice)

qqplot(myeligible.population$YearBuilt, myeligible.population$SalePrice)
plot(myeligible.population$YearBuilt, myeligible.population$SalePrice)

######****************************************************************
par(mfrow = c(1,2))
boxplot(myeligible.population$YearBuilt, main = 'Year Built Distribution', col = 'deepskyblue', ylab = 'Year Built')
scatter.smooth(myeligible.population$YearBuilt, myeligible.population$SalePrice/1000, main = 'Year Built vs. Sales Price', 
               col = 'deepskyblue', ylab = 'Sales Price (000)', xlab = 'Year Built')

par(mfrow = c(1,1))
boxplot(myeligible.population$FirstFlrSF)
qqplot(myeligible.population$FirstFlrSF, myeligible.population$SalePrice)
plot(myeligible.population$FirstFlrSF, myeligible.population$SalePrice)
summary(myeligible.population$FirstFlrSF)

######****************************************************************
par(mfrow = c(1,2))
boxplot(myeligible.population$FirstFlrSF, main = 'First Floor Distribution', col = 'deepskyblue', ylab = 'First Floor SQFT')
scatter.smooth(myeligible.population$FirstFlrSF, myeligible.population$SalePrice/1000, main = 'First Floor SQFT vs. Sales Price', 
               col = 'deepskyblue', ylab = 'Sales Price (000)', xlab = 'First Floor SQFT')

par(mfrow = c(1,1))

plot(myeligible.population$OverallQual, myeligible.population$SalePrice)

plot(myeligible.population$OverallCond, myeligible.population$SalePrice)

plot(myeligible.population$LotArea, myeligible.population$SalePrice)
boxplot(myeligible.population$LotArea)


style_table <- table(myeligible.population$HouseStyle)
barplot(style_table)

bldg_table <- table(myeligible.population$BldgType)
barplot(bldg_table)

par(mfrow = c(1,2))
extqual_table <- table(myeligible.population$ExterQual)
barplot(extqual_table, col = 'deepskyblue', main = 'Exterior Quality')

extcond_table <- table(myeligible.population$ExterCond)
barplot(extcond_table,col = 'salmon', main = 'Exterior Condition')

par(mfrow = c(1,1))
plot(myeligible.population$YearRemodel, myeligible.population$SalePrice)
boxplot(myeligible.population$YearRemodel)


##########################################################################################
#Regression Analysis on 3 variables
par(mfrow = c(1,2))
boxplot(myeligible.population$SalePrice, ylim = c(60000,760000), col = 'deepskyblue', main = 'Sales Price Box Plot', ylab = 'Sales Price')
boxplot(log10(myeligible.population$SalePrice), col = 'deepskyblue', main = 'Log10 Sales Price Box Plot', ylab = 'Sales Price')




qqplot(myeligible.population$YearBuilt, myeligible.population$SalePrice, ylim = c(60000,760000), col = 'deepskyblue', 
       main = 'Year Built vs. Sales Price - QQ', ylab = 'Sales Price', xlab = 'Year Built')
plot(myeligible.population$YearBuilt, myeligible.population$SalePrice, ylim = c(60000,760000), 
     col = 'deepskyblue', main = 'Year Built vs. Sales Price', ylab = 'Sales Price', xlab = 'Year Built')
cor(myeligible.population$YearBuilt, myeligible.population$SalePrice)




qqplot(myeligible.population$FirstFlrSF, myeligible.population$SalePrice, ylim = c(60000,760000), col = 'deepskyblue', 
       main = 'First Floor SQFT vs. Sales Price - QQ', ylab = 'Sales Price', xlab = 'First Floor SQFT')
plot(myeligible.population$FirstFlrSF, myeligible.population$SalePrice, ylim = c(60000,760000), col = 'deepskyblue', 
     main = 'First Floor SQFT vs. Sales Price', ylab = 'Sales Price', xlab = 'First Floor SQFT')
cor(myeligible.population$FirstFlrSF, myeligible.population$SalePrice)




qqplot(myeligible.population$LotArea, myeligible.population$SalePrice, ylim = c(60000,760000), col = 'deepskyblue', 
       main = 'Lot Area vs. Sales Price - QQ', ylab = 'Sales Price', xlab = 'Lot Area')
plot(myeligible.population$LotArea, myeligible.population$SalePrice, ylim = c(60000,760000), col = 'deepskyblue', 
     main = 'Lot Area vs. Sales Price', ylab = 'Sales Price', xlab = 'Lot Area')
cor(myeligible.population$LotArea, myeligible.population$SalePrice)

par(mfrow = c(1,1))
boxplot(myeligible.population$YearBuilt, col = 'deepskyblue', ylab = 'Year Built')












##########################################################################################################################
#Assignment 2 
# First floor predictor variable



##########################################################################################
# Year Built predictor variable

model.2 <- lm(SalePrice ~ YearBuilt, data=myeligible.population)

# Display model summary
summary(model.2)

# List out components of lm object
names(model.2)

model.2$coefficients

par(mfrow = c(1,2))
qqnorm(model.2$residuals, main = 'QQ Plot Year Built Residuals', col = 'salmon')
qqline(model.2$residuals)

# Make a scatterplot
plot(myeligible.population$YearBuilt,model.2$residuals/1000, main = 'Year Built SQFT vs. Sales Price Residuals', 
     col = 'deepskyblue', xlab = 'Year Built', ylab = 'Year Built - Sales Price Residuals (000)')

##########################################################################################
# Multiple regression plot with both variables

model.3 <- lm(SalePrice ~ YearBuilt + FirstFlrSF, data=myeligible.population)

# Display model summary
summary(model.3)

# List out components of lm object
names(model.3)

model.3$coefficients

par(mfrow = c(1,3))
qqnorm(model.3$residuals, main = 'QQ Plot Multiple Residusal - Year Built & First Floor Residuals', col = 'salmon')
qqline(model.3$residuals)

# Make a scatterplot
#par(mfrow = c(1,2))
plot(myeligible.population$YearBuilt,model.3$residuals/1000, main = 'Year Built vs. Multiple Residuals', 
     col = 'deepskyblue', xlab = 'Year Built', ylab = 'Residuals (000)')
plot(myeligible.population$FirstFlrSF,model.3$residuals/1000, main = 'First Floor SQFT vs. Multiple Residuals', 
     col = 'deepskyblue', xlab = 'First Floor SQFT', ylab = 'Residuals (000)')





##########################################################################################
#reproduce models 1,2,3 with log(salesprice)

model.4 <- lm(log(SalePrice) ~ FirstFlrSF, data=myeligible.population)

# Display model summary
summary(model.4)
model.4$coefficients

par(mfrow = c(1,2))
qqnorm(model.4$residuals, main = 'QQ Plot First Floor Log Sales Price Residuals', col = 'salmon')
qqline(model.4$residuals)

# Make a scatterplot
plot(myeligible.population$FirstFlrSF,model.4$residuals/1000, main = 'First Floor SQFT vs. Log 10 Sales Price Residuals', 
     col = 'deepskyblue', xlab = 'First Floor SQFT', ylab = 'First Floor SQFT - Log 10 Sales Price Residuals (000)')

######################################### 
#Model 5
model.5 <- lm(log(SalePrice) ~ YearBuilt, data=myeligible.population)

# Display model summary
summary(model.5)
model.5$coefficients

par(mfrow = c(1,2))
qqnorm(model.5$residuals, main = 'QQ Plot Year Built Log Sales Price Residuals', col = 'salmon')
qqline(model.5$residuals)

# Make a scatterplot
plot(myeligible.population$YearBuilt,model.5$residuals/1000, main = 'Year Built SQFT vs. Log 10 Sales Price Residuals', 
     col = 'deepskyblue', xlab = 'Year Built', ylab = 'Year Built - Log 10 Sales Price Residuals (000)')

######################################### 
#Model 6

model.6 <- lm(log(SalePrice) ~ YearBuilt + FirstFlrSF, data=myeligible.population)

# Display model summary
summary(model.6)
model.6$coefficients

par(mfrow = c(1,3))
qqnorm(model.6$residuals, main = 'QQ Plot Multiple Variable & Log Sales Price Residuals', col = 'salmon')
qqline(model.6$residuals)

# Make a scatterplot
plot(myeligible.population$YearBuilt,model.6$residuals/1000, main = 'Year Built vs. Log 10 Sales Multiple Residuals', 
     col = 'deepskyblue', xlab = 'Year Built', ylab = 'Residuals (000)')
plot(myeligible.population$FirstFlrSF,model.6$residuals/1000, main = 'First Floor SQFT vs. Log 10 Sales Multiple Residuals', 
     col = 'deepskyblue', xlab = 'First Floor SQFT', ylab = 'Residuals (000)')








#Tests
par(mfrow = c(1,1))

model.test <- lm(SalePrice ~ LotArea, data=myeligible.population)

# Display model summary
summary(model.test)

# List out components of lm object
names(model.test)

model.test$coefficients

qqnorm(model.test$residuals, main = 'QQ Plot First Floor Residuals', col = 'salmon')
qqline(model.test$residuals)

# Make a scatterplot
plot(myeligible.population$LotArea,model.test$residuals/1000, main = 'Ground Living vs. Sales Price Residuals', 
     col = 'deepskyblue', xlab = 'Ground Living', ylab = 'Ground Living - Sales Price Residuals (000)')


boxplot(myeligible.population$LotArea)

plot(myeligible.population$LotArea/1000, myeligible.population$SalePrice/1000)
boxplot(myeligible.population$LotArea)





























##########################################################################################
#Assignment 3 
#part 2 initial EDA

par(mfrow = c(1,1))
boxplot(myeligible.population$SalePrice/1000, main = 'Sales Price Distribution', col = 'deepskyblue', ylab = 'Sales Price')

par(mfrow = c(1,2))
scatter.smooth(myeligible.population$YearBuilt, myeligible.population$SalePrice/1000, col = 'deepskyblue', 
               main = 'Sales Price vs. Year Built', xlab = 'Year Built', ylab = 'Sales Price (000)')

qqplot(myeligible.population$YearBuilt, myeligible.population$SalePrice/1000, col = 'deepskyblue', 
       main = 'Sales Price vs. Year Built', xlab = 'Year Built', ylab = 'Sales Price (000)')

######****************************************************************
par(mfrow = c(1,1))
boxplot(myeligible.population$FirstFlrSF, main = 'First Floor SQFT Distribution', col = 'deepskyblue', 
        ylab = 'First Floor SQFT')

par(mfrow = c(1,2))
scatter.smooth(myeligible.population$FirstFlrSF, myeligible.population$SalePrice/1000, main = 'Sales Price vs. First Floor SQFT', 
               col = 'deepskyblue', ylab = 'Sales Price (000)', xlab = 'First Floor SQFT')

qqplot(myeligible.population$FirstFlrSF, myeligible.population$SalePrice/1000, main = 'Sales Price vs. First Floor SQFT', 
       col = 'deepskyblue', ylab = 'Sales Price (000)', xlab = 'First Floor SQFT')

summary(myeligible.population$FirstFlrSF)


model.first <- lm(SalePrice ~ FirstFlrSF, data=myeligible.population)

# Display model summary
summary(model.first)

# List out components of lm object
names(model.first)

model.first$coefficients

par(mfrow = c(1,2))
qqnorm(model.first$residuals, main = 'QQ Plot First Floor Residuals', col = 'salmon')
qqline(model.first$residuals)

# Make a scatterplot
plot(myeligible.population$FirstFlrSF,model.first$residuals/1000, main = 'Sales Price Residuals vs. First Floor', 
     col = 'deepskyblue', xlab = 'First Floor SQFT', ylab = 'First Floor - Sales Price Residuals (000)')

######****************************************************************
par(mfrow = c(1,1))
boxplot(myeligible.population$LotArea, main = 'Lot Area Distribution', col = 'deepskyblue', 
        ylab = 'Lot Area SQFT')

par(mfrow = c(1,2))
scatter.smooth(myeligible.population$LotArea, myeligible.population$SalePrice/1000, main = 'Sales Price vs. Lot Area', 
               col = 'deepskyblue', ylab = 'Sales Price (000)', xlab = 'Lot Area SQFT')

qqplot(myeligible.population$LotArea, myeligible.population$SalePrice/1000, main = 'Sales Price vs. Lot Area', 
       col = 'deepskyblue', ylab = 'Sales Price (000)', xlab = 'Lot Area SQFT')

summary(myeligible.population$LotArea)

model.lot <- lm(SalePrice ~ LotArea, data=myeligible.population)

# Display model summary
summary(model.lot)

# List out components of lm object
names(model.lot)

model.lot$coefficients

par(mfrow = c(1,2))
qqnorm(model.lot$residuals, main = 'QQ Plot Lot Area Residuals', col = 'salmon')
qqline(model.lot$residuals)

# Make a scatterplot
plot(myeligible.population$LotArea,model.lot$residuals/1000, main = 'Sales Price Residuals vs. Lot Area', 
     col = 'deepskyblue', xlab = 'Lot Area SQFT', ylab = 'Lot Area - Sales Price Residuals (000)')






















##########################################################################################
#Assignment 3 
#part 3 MLR
model.comb <- lm(SalePrice ~ FirstFlrSF + LotArea, data=myeligible.population)

# Display model summary
summary(model.comb)

# List out components of lm object
names(model.comb)

model.comb$coefficients

par(mfrow = c(1,3))
qqnorm(model.comb$residuals, main = 'QQ Plot First Floor & Lot Area Residuals', col = 'salmon')
qqline(model.comb$residuals)

# Make a scatterplot
plot(myeligible.population$FirstFlrSF,model.comb$residuals/1000, main = 'Sales Price Residuals vs. First Floor', 
     col = 'deepskyblue', xlab = 'First Floor SQFT', ylab = 'First Floor - Sales Price Residuals (000)')

plot(myeligible.population$LotArea,model.comb$residuals/1000, main = 'Sales Price Residuals vs. Lot Area', 
     col = 'deepskyblue', xlab = 'Lot Area SQFT', ylab = 'Lot Area - Sales Price Residuals (000)')







##########################################################################################
#Assignment 3 
#part 4 Neighborhood Accuracy
boxplot(model.comb$residuals~myeligible.population$Neighborhood, main = 'Residuals Distribution by Neighborhood',
        col = 'deepskyblue', las = 2)


# change the column names
avgSalePrice <- aggregate(myeligible.population$SalePrice, by=list(Neighborhood=myeligible.population$Neighborhood), FUN=mean)
colnames(avgSalePrice) <- c('Neighborhood','AvgSalePrice')
avgSalePrice

avgPricePerSQFT <- aggregate(myeligible.population$SalePrice/myeligible.population$GrLivArea, by=list(Neighborhood=myeligible.population$Neighborhood), FUN=mean)
colnames(avgPricePerSQFT) <- c('Neighborhood','avgPricePerSQFT')
avgPricePerSQFT

avgPricePerSQFTCalc <- myeligible.population$SalePrice/myeligible.population$GrLivArea

#plot(myeligible.population$Neighborhood, myeligible.population$SalePrice/myeligible.population$GrLivArea)
model.hood <- lm(avgPricePerSQFT$avgPricePerSQFT ~ avgPricePerSQFT$Neighborhood)

summary(model.hood)
mae.1 <- mean(abs(model.hood$residuals))
mae.1

#plot(avgPricePerSQFT$avgPricePerSQFT)








