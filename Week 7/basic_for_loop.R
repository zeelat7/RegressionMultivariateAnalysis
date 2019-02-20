# Chad R Bhatti
# 07.27.2017
# basic_for_loop.R


################################################################
# Basic for loop syntax;
################################################################

for (counter in values){
	inside loop here
}


################################################################
# Example 1
################################################################

for (j in 1:5){
	print(paste('Loop Count =',j));
}


# Note that R recognized 1:5 as a sequence, and that the for loop can use
# any sequence in its iteration


for (j in 5:1){
	print(paste('Loop Count =',j));
}


################################################################
# Example 2
################################################################

j.sequence <- seq(1,100,10);

for (j in j.sequence){
	print(paste('Loop Count =',j));
}



################################################################
# Example 3
################################################################

alpha.sequence <- c('a','b','c','d','e');

for (j in alpha.sequence){
	print(paste('Loop Count =',j));
}


# This is not the typical for loop, but knowing that R can take a sequence
# of characters to loop over can be useful in some data processing;

# In particular if you want to check for particular values in a nested
# for loop.



################################################################
# Example 4 - Nested for loop
################################################################
x <- c('a','d','c','b','d','d','b');
a <- c('a','b','c');

for (j in 1:length(x)){
	for (k in 1:length(a)){
		if (x[j]==a[k]){
			print(paste('x[',j,']',' is a match to ',a[k],sep=''));
		}else{
			print(paste('x[',j,']',' is not a match to ',a[k],sep=''));
		}
	}
}


# This is a purely mechanical example so that we can see how to code a 
# nested for loop in R and an if-else statement in R.













