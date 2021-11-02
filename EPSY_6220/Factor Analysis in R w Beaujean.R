###Syntax to accompany Beaujean (2013) Practical Assessment, Research & Evaluation
#
#make an empty matrix with 8 rows and columns
WASIWRIT.cor<-matrix(NA, 8,8)
#input ones on the diagonal elements of the matrix
diag(WASIWRIT.cor)<-1
#input the lower triangle of the correlation matrix
WASIWRIT.cor [lower.tri(WASIWRIT.cor)]<-
c(.57, .79, .62, .69, .83, .56, .51, .57, .65, .51, .54, .59, .66, .60, .70, .74, .58, .55, .53, .57, .71, .62, .71, .65, .51, .58, .53, .62)
#input the upper triangle of the correlation matrix
WASIWRIT.cor[upper.tri(WASIWRIT.cor)]<-t(WASIWRIT.cor)[upper.tri(WASIWRIT.cor)]
#Name the rows and columns of the correlation matrix
dimnames(WASIWRIT.cor)<-list(c(paste("WASI.", c("Voc", "BD", "Sim", "MR"), sep=""), paste ("WRIT.", c("VerbAn", "Voc", "Mat", "Dia"), sep="")), c(paste("WASI.", c("Voc", "BD", "Sim", "MR"), sep=""), paste("WRIT.", c("VerbAn", "Voc", "Mat", "Dia"), sep="")))
#Create a vector of means
WASIWRIT.mean<-c(97.75, 97.87, 103.81,99.81, 101.51, 100.63, 101.45, 100.64)
#Name the columns of the mean vector
names(WASIWRIT.mean)<-c(paste("WASI.", c("Voc", "BD", "Sim", "MR"), sep=""), paste("WRIT. ", c("VerbAn", "Voc", "Mat", "Dia"), sep=""))
#Create a vector of standard deviations
WASIWRIT.sd<-c(17.37, 14.49, 17.26, 16.61, 14.77, 16.42, 16.17, 13.92)
#Name the columns of the standard deviaitons vector
names(WASIWRIT.sd)<-c(paste("WASI.", c("Voc", "BD", "Sim", "MR"), sep=""), paste("WRIT." , c("VerbAn", "Voc", "Mat", "Dia"), sep=""))
#########
#install the Psych package
install.packages("psych")
#load the Psych package
library(psych)
###Exploratory Factor Analysis###
fa(WASIWRIT.cor, nfactors=1, n.obs=152, fm="pa")
#Rotate the extracted factors
fa(WASIWRIT.cor, nfactors=2, n.obs=152, fm="pa", rotate="varimax")
#produce Structure coefficients
fa(WASIWRIT.cor, nfactors=2, n.obs=152, fm="pa", rotate="promax")$Structure
promax.2fac<-fa(WASIWRIT.cor, nfactors=2, n.obs=152, fm="pa", rotate="promax")
promax.2fac$Structure
#conduct quartimin rotation
quartimin<-factanal(covmat=WASIWRIT.cor, factors=2, rotation="cfQ", control=list(rotate=list(kappa=0)))
#Determine the number of factors using Parallel Analysis
fa.parallel(WASIWRIT.cor, n.obs=152, fm="pa")
#Determine the number of factors using Velicer's minimum average partial (MAP) procedure
VSS(WASIWRIT.cor, n.obs=152)
#Conduct Confirmatory Factor Analysis using Lavaan
library(lavaan)
#First, convert the correlation matrix to a covariance matrix
WASIWRIT.cov<-cor2cov(WASIWRIT.cor,WASIWRIT.sd)
#Try a single factor model
singleFactor.model<-'2 + g=∼WASI.Voc + WASI.Sim + WRIT.VerbAn + WRIT.Voc + WASI.BD + WASI.MR + WRIT.Mat + WRIT.Dia 3+ ’
