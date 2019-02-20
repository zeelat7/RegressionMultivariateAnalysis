# Chad R Bhatti
# 07.28.2017
# apply_function.R

# Example of how to use the apply() function;

# Load the MASS library
library(MASS)

# The MASS library contains a famous data set called the Boston Housing data set;
# We do not need to load or require this data set explicitly;
# Once we load the MASS library then 'Boston' is an object in the active workspace;
help(Boston)

# Let's skinny down the data frame to some variables that are continuous;
boston.skinny <- Boston[,c('crim','indus','nox','dis','lstat')];
head(boston.skinny)


# How do we use the apply() function?
help(apply)


# Compute the column means;
apply(X=boston.skinny,MARGIN=2,FUN=mean)
mean(Boston$crim)


# Compute the column medians;
apply(X=boston.skinny,MARGIN=2,FUN=median)
median(Boston$crim)


# Compute the column deciles;
apply(X=boston.skinny,MARGIN=2,FUN=quantile,probs=seq(0,1,0.1))
quantile(Boston$crim,probs=seq(0,1,0.1))

# Note that we pass the additional arguments for FUN after we specify the FUN;


###################################################################################
# Practice Exercise:
###################################################################################
# Try using some other functions like min, max, mad, and IQR;
# Also try the trimmed mean and trim 10% of the data, that would be 5% from each
# tail of the data.  Do we know how to compute a trimmed mean?
# See the help page for mean;

help(mean)













