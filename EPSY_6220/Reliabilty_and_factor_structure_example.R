#Script from class exercise
test4 <- read.csv("~/Documents/Dropbox/NASCAR Drive/Courses/EPSY 6220/test4.csv")
View(test4)

library(psych)

#now that we have the psych package available, we can ask it to compute the reliability
#of the data in test4.csv
alpha(test4)

#The next thing we would like to do is examine the factor structure of the data.
#We can do this first with a principal components analysis in psych
pc<-principal(test4, rotate="varimax")
#then call the result
pc
#We would like to examine the possibilty of more than 1 component or factor.
#To do this, we would run a principal axis factor analysis, like this...
pa<-fa(test4,fm="pa",rotate="varimax")
pa
#the result is pretty much what we just got with the PCA.
#To run a 2 factor solution, we need to specify it:
pa2<-fa(test4,nfactors=2,fm="pa",rotate="varimax")
pa2

