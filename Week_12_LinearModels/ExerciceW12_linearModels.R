##A Follow up from the regression of gene expression on dn/ds ratio in mammals  

# NB the data is from 
# Nguyen L-P, Galtier N, Nabholz B. 2015 Gene expression, chromosome heterogeneity and the fast-X effect
# in mammals. Biol. Lett.11 : 20150010.
# Data source:  
# http://dx.doi.org/10.1098/rsbl.2015.0010
# The data underlying this study are available on Dryad:
# doi:10.5061/dryad.qr20n

getwd()
setwd("~/Dropbox/2017.FALL.Datascience.in.Bioinformatics/Dropbox.Data.Science.in.Bioinformatics/Week_11.correlation_regression/")
mammals = read.table("rsbl20150010supp1.csv", header = T, sep=",") #adjust path here!

### Question 2 : reproducing and examining the linear model used by the authors of dn/ds X vs A study####
# Q2.1 :Is chr is a factor or covariate in the regression model reported in Table 1? Check that your variable in R is matching that 
# Hint: is.factor() is useful to do so and as.factor() can be used to indicate to R a variable shoudl be treated as factor
is.factor(mammals$chr)

tab1Reg=lm(log(dNdS)~Species+ chrMark + log(RPKM)+ Tau+ GC3 ,data=mammals) 

summary(tab1Reg)
plot(tab1Reg)
# Important hint: You can use the function anova to test the significance of the different terms of our regression model 
anova(tab1Reg)
# Q2.2: Compare the output of the summary() & anova() with the results reported by authors in table 1

#IMPORTANT but subtle point: there is more than 1 way to test effects of terms in a linear model. Authors chose to use so called type II sum of square. 
# Here we use the Anova() form the car package to replicate the analysis of the authors
# There is a HUGE litterature dating back 60 years on what type of sum of squares can be used to "properly" test effect of factors in linear models
# Here is a quick read on that and the implementation in R as the car package
# http://www.uni-kiel.de/psychologie/rexrepos/posts/anovaSStypes.html

library(car) ##Install car if you dont have that package yet
Anova(tab1Reg, test.statistic = "F", type ="II") ## Compare the results with the authors Table 1 ---> 99.9% MATCH!!

#Q2.3: are regression variables bringing relatively independent information or are they hoplessly confounded ?
## Hint the function vif() can be used to investigate that point

#Q2.4: If you had naively compared differences in dn/ds of X versus Autosome genes, would you have concluded to a faster X effect ? 
tabNaiveReg=lm(log(dNdS)~Species+ chrMark  ,data=mammals) 
summary(tabNaiveReg)
Anova(tabNaiveReg, test.statistic = "F", type ="II") ## Compare the results with the authors Table 1 ---> 99.9% MATCH!!

#Why do we then choose to do the full model ? 