# Chad R Bhatti
# 07.28.2017
# vectorized_functions.R

# R has several built-in functions called vectorized functions;
# Vectorized functions are designed to avoid for loops since for loops are slow in R;
# We will look at a couple of examples;


# Load the MASS library
library(MASS)

# The MASS library contains a famous data set called the Boston Housing data set;
# We do not need to load or require this data set explicitly;
# Once we load the MASS library then 'Boston' is an object in the active workspace;
help(Boston)


###################################################################################
# First - How do we measure execution time in R;
###################################################################################
help(proc.time)


start <- proc.time();

# Wait 5 seconds;
stop <- proc.time()
elapsed.time <- stop - start;
elapsed.time




###################################################################################
# The ifelse() function; 
###################################################################################

head(Boston)
summary(Boston$medv)


# Here is how we would define an indicator variable in a traditional programming language;

# Initialize output/storage vector;
above.avg <- rep(NA,length(Boston$medv));

# Loop through observations and evaluate condition;
start <- proc.time();
for (j in 1:length(Boston$medv)){
	if (Boston$medv[j] > 22.53){
		above.avg[j] <- 1;
	}else{
		above.avg[j] <- 0;
	}#end else
}#end j for loop
stop <- proc.time()


# How much time did this standard programming approach take?
stop-start

# View results
table(above.avg)



# Now let's use the built-in R function ifelse();
start <- proc.time();
above.avg <- ifelse(Boston$medv>22.53,1,0);
stop <- proc.time()

# How much time did we use?
stop-start

# View results
table(above.avg)


# Note that this is a small data set so our computation time savings are minimal;
# However, our programming time savings are easy to see;
# Much less code to use ifelse();


#####################################################################################
# The primary general vectorizing function in R is the apply() function and its
# variants lapply() and sapply().  Eventually we all need to be comfortable using
# these functions if we want to be serious R users.
























