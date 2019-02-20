# Chad R Bhatti
# 12.25.2016
# ames_waterfall.R

# Read in csv file for Ames housing data;

# Note that back slash is an escape character in R so we use \\ when we want \;
path.name <- '/Users/Zeeshan/Desktop/PREDICT 410/Week 1/';
file.name <- paste(path.name,'ames_housing_data.csv',sep='');

# Read in the csv file into an R data frame;
ames.df <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);

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
ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
	ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
	ifelse(ames.df$Street!='Pave','03: Street Not Paved',
	ifelse(ames.df$YearBuilt <1950,'04: Built Pre-1950',
	ifelse(ames.df$TotalBsmtSF <1,'05: No Basement',
	ifelse(ames.df$GrLivArea <800,'06: LT 800 SqFt',
	'99: Eligible Sample')
	)))));


table(ames.df$dropCondition)

# Save the table
waterfall <- table(ames.df$dropCondition);

# Format the table as a column matrix for presentation;
as.matrix(waterfall,7,1)


# Eliminate all observations that are not part of the eligible sample population;
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample');

# Check that all remaining observations are eligible;
table(eligible.population$dropCondition);


##########################################################################################
# Notes:
##########################################################################################

(1) We have not defined our actual data sample yet.  We have defined the set of eligible
observations. Due to the manner in which R handles missing values we will need to take
one more step to define the sample population.  THAT STEP - decide which predictor
variables that we want to use, par the data frame down to those variables, and then use 
na.omit() to remove the observations with missing values.

(2) We still need to define our predictor variables.  Some predictor variables are defined
on the data frame.  Some predictor variables will be defined using fields on the data
frame.















