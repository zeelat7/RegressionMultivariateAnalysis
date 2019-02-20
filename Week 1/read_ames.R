# Chad R Bhatti
# 12.25.2016
# read_ames.R

# Read in csv file for Ames housing data;

# Note that back slash is an escape character in R so we use \\ when we want \;
path.name <- '/Users/Zeeshan/Desktop/PREDICT 410/Week 1/';
file.name <- paste(path.name,'ames_housing_data.csv',sep='');

# Read in the csv file into an R data frame;
ames.df <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);

# Show the header of the data frame;
head(ames.df)

# List out the contents of the data frame;
# Use the structure function str();
str(ames.df)

# Find help for str
help(str)

# Show the distribution (and levels) of a discrete/factor type variable;
table(ames.df$LotShape)

# Note that table() will suppress the count of missing values;
# Here is how we force table() to show the missing values as a level;
table(ames.df$Fence,useNA=c('always'))



##########################################################################################
# Notes:
##########################################################################################

#(1) We have a data dictionary.  I would suggest that you consult the data dictionary to 
	better understand our data set.

#(2) We have missing values.  We will need to account for these missing values at some
	point in time.  After we have found a good subset of predictor variables, then we
	can eliminate observations with missing values.  Note the order of thought here.
	Do not reverse this order of thought.  Predictor variables first, not missing values
	first.

#(3) Now that we have our data read into R, we need to define our sample population.
	What observations should define our sample population?  Let's think about our problem,
	and let's consult our data dictionary to find fields that would identify observations
	that do not belong in our sample population.



