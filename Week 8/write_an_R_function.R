# Chad R Bhatti
# 07.27.2017
# write_an_R_function.R

# Here is an example of how we can write our own R function;

# Load the MASS library
library(MASS)

# The MASS library contains a famous data set called the Boston Housing data set;
# We do not need to load or require this data set explicitly;
# Once we load the MASS library then 'Boston' is an object in the active workspace;
help(Boston)


# R has a built in summary() function
summary(Boston$indus)


# Let's write our own summary function;
# We will name it my.summary() so we do not alias the R function summary();
# We will compute the min, max, deciles, mean, and standard deviation and
# we will output these summary statistics as a data frame;



#############################################################################
# my.summary(x)
#############################################################################
# x: input vector to summarize
#############################################################################

my.summary <- function(x){
	value <- c(min(x),quantile(x,probs=seq(0,1,0.1)),max(x),mean(x),sd(x))
	names(value)[1] <- 'min';
	names(value)[13] <- 'max';
	names(value)[14] <- 'mean';
	names(value)[15] <- 'sd';
	out.df <- as.data.frame(value);
	return(out.df)
}

#############################################################################


# Call the function
my.summary(Boston$indus)


# Did we need that min and max in there?


#############################################################################
# Progamming Exercise:
#############################################################################
(1) Take the min and max out of my.summary().
(2) Add mad() and IQR() after the standard deviation. 
	What do mad() and IQR() compute?









