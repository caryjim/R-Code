#Conducting a parallel analysis with a correlation matrix in R

#From the TABLE4_1 data file, create a full correlation matrix in Excel
#with the correlations above and below the main diagonal

#Read in the new .txt file
TABLE4_1 <- read.delim("~/Dropbox/NASCAR Drive/Courses/EPSY 6220/TABLE4_1.txt", header=FALSE)

#Then run the parallel analysis using the Psych package, indicating that the number of observations with
#this correlation is 300
fa.parallel(TABLE4_1, n.obs = 300)
#This will produce Scree Plots and the following output:
#Parallel analysis suggests that the number of factors =  2  and the number of components =  2

#To view the random eigenvalues, create an object out of the parallel analysis like this:
fa = fa.parallel(TABLE4_1, n.obs = 300)
#Then, to see the random eignvalues we can call:
describe(fa$values)

