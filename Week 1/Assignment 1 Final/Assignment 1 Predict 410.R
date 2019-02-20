# Zeeshan Latifi
# 9.23.2017
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
  ifelse(amesiowa.df$FirstFlrSF<600, '10: First Floor Under 600 SqFt',
  ifelse(amesiowa.df$CentralAir!='Y', '11: No Central Air',
  ifelse(amesiowa.df$PavedDrive!='Y', '12: No Paved Driveway',
  '99: Eligible Sample')
  )))))))))));


table(amesiowa.df$dropConditions)

# Save the table
waterfalls <- table(amesiowa.df$dropConditions);

# Format the table as a column matrix for presentation;
as.matrix(waterfalls,11,1)


# Eliminate all observations that are not part of the eligible sample population;
myeligible.population <- subset(amesiowa.df,dropConditions=='99: Eligible Sample');

# Check that all remaining observations are eligible;
table(myeligible.population$dropConditions);

head(myeligible.population)

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

boxplot(myeligible.population$FirstFlrSF)
qqplot(myeligible.population$FirstFlrSF, myeligible.population$SalePrice)
plot(myeligible.population$FirstFlrSF, myeligible.population$SalePrice)
summary(myeligible.population$FirstFlrSF)

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

boxplot(myeligible.population$YearBuilt, col = 'deepskyblue', ylab = 'Year Built')
