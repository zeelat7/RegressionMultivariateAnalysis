# Chad R Bhatti
# 07.27.2017
# missing_values_in_R.R

# Basic example related to missing values in R;
# First, remember that R is designed to be a statistical programming languge,
# not a statistical software for a novice user.
# This means that R is designed for you to know what you are doing.


# Take a look at the function mean();
# How does mean() handle missing observations?
help(mean)


# Let's see with a simple example;
x <- c(1,2,3,4,5,NA,6,7,NA,8);

# What is the type of x?
mode(x)


# Why can I put NA in the same vector as the numbers 1-8?
# Can I just put letters and numbers together?
# Is NA special?

y <- c(1,2,3,4,5,N,6,7,N,8)


# What about this?
y <- c(1,2,3,4,5,'N',6,7,'N',8)

# What is the type of y?
mode(y)


# Compute the mean;
mean(x)
mean(y)


# Did any of that work the way that we thought that it would work?
# Depends on your experience with R and programming in general.

# Specify the na.rm option in mean();
mean(x, na.rm=TRUE)


# Check value against a clean sequence;
mean(seq(1,8,1))


# na.rm is an option in nearly all R summary statistics functions;
# WARNING: Some R functions use narm instead of na.rm.  If na.rm does not
# work, then double check the help page for the correct 'na remove' parameter.


#############################################################################
# Try these functions for practice;
#############################################################################

x <- rnorm(n=1000,mean=0,sd=1);
x[seq(1,1000,23)] <- NA;

mean(x)
mean(x, na.rm=TRUE)


# Now do these functions for practice;
# min, max, sd, mad, IQR, quantile











































