#### Week 03 ####

#### R for data science exercises: Sampling from data & Sampling theoretical distributions  ####

# In week 1 and 2, you have spent some time learning how to manipulate data in the R tidyverse
# This week there is no online tutorials but a step by step tutorial and set of small exercises 
# We advice you to make a new file (R script) and use that for doing the tutorial.
# When done - save the R script with an appropriate filename and in an appropriate location


#### How to  import in R datasets used in the textbook ####
install.packages("abd") #First install the abd library that is a companion to the abd textbook.
library(abd)
library(tidyverse) # you can also load directly tidyverse
library(ggplot2)
install.packages('compare')
install.packages('data.table')
library(data.table)
library(compare)

data("HumanGeneLengths") # loading in R the human gene length dataset


#### Sampling from ideal and empirical idealized distributions ####

# Ideal, probability distribution
myRandomNumbers = rnorm(n = 10^4, mean=0, sd=1) # In one go you generate 10^4 draws from a standard normal distribution 
myRandomNumbers
# NB R is very good at generating draws from a while suite of probabilty distributions (more on that in week 4)

# Q1: Using ggplot, make a histogram of these 10^4 draws, check different binning options
dd <- data.frame(x=myRandomNumbers)

ggplot(dd, aes(x=x)) +
  geom_histogram() +
  stat_bin(aes(y=..count.., label=..count..), geom = "text")

hist(myRandomNumbers)

# An empirical distribution used in the book
summary(HumanGeneLengths)

# Q: What is the unit of gene length?
# integers?

# Q2: Using ggplot, make a histogram that mimicks the figure of the book for human gene length
ggplot(HumanGeneLengths, aes(x=gene.length)) +
  geom_histogram(colour='black', fill='red', bins='20') +
  xlim(c(0,15000))

# Q3: Add some meaningful labels on the X and Y axis. X should be "Human Gene length (pb), Y should be "observed counts"
# TO do so use the scale_x_continuous and scale_y_continuous option in ggplot
?scale_x_continuous

ggplot(HumanGeneLengths, aes(x=gene.length)) +
  geom_histogram(colour='black', fill='red', bins='30') +
  scale_y_continuous(name='Observed counts') +
  scale_x_continuous(name='Human Gene length (pb)', limits=c(0,15000))

#### Sampling from an empirical (finite) distribution ####
MySmallSample= sample(x = HumanGeneLengths$gene.length, size = 30, replace = T) ## 30 genes sampled WITH replacement

#Q What is the diffference between sampling with and without replacement?
#  When we sample with replacement, the two sample values are independent.
#Practically, this means that what we get on the first one doesn't affect what w
# get on the second. Mathematically, this means that the covariance between the two is zero.
#In sampling without replacement, the two sample values aren't independent. Practically, this
# means that what we got on the for the first one affects what we can get for the second one.
# Mathematically, this means that the covariance between the two isn't zero. 


#Q What is the line of code below doing ?
MyBigSample= sample(x = HumanGeneLengths$gene.length, size = length(HumanGeneLengths$gene.length), replace = F)
summary(MyBigSample)
# It sums up HumanGeneLengths dependently

#### Drawing a series of samples to build the sampling distribution of a statistic ####

# My first loop in R
my100Means=rep(-9,100)
for (i in 1:100){
  myNewSample=sample(HumanGeneLengths$gene.length, size = 30, replace = T)
  myNewMean=mean(myNewSample)
  my100Means[i]=myNewMean
}

#Q: my100Means is a small empirical distribution. What does it represent ? (hint examine figure 4.1-3 in the ABD book)
# independently

#Q Compare the mean of my100Means and the mean of HumanGeneLengths. 
m100 <- summary(my100Means)
hgl <- summary(sample(HumanGeneLengths$gene.length))
m100 
hgl
# HumanGeneLength mean 2622, my100MEans mean 2621, close to each other

#Q Now modify the small snippet of R code above to draw 5000 samples and compare again
my5000Means=rep(-9,5000)
for (i in 1:5000){
  myNewSample=sample(HumanGeneLengths$gene.length, size = 30, replace = T)
  myNewMean=mean(myNewSample)
  my100Means[i]=myNewMean
}

m5000 <- summary(my5000Means)
hgl2 <- summary(sample(HumanGeneLengths$gene.length))
m5000
hgl2

#mean is now -9

#Q Compare the Y axis of figure 4.1-2 and 4.1-3: what is the difference. Search how to get ggplot to draw histograms of both types.
#Hint try the option  geom_histogram(aes(y = ..density..))


#### Comparing sampling distributions of mean and median gene length ####

#Q: Adapt the code given above to re create the Figure 4.1-4 of the ABD book .

#Q: Adapt the code to now investigate the sampling distribution of the median gene expression



#### What are confidence intervals in statistics ? ####
# Q: Draw 100 samples of size 30 genes
# and for each sample 
# calculate a rough confidence interval for the mean by using the empirical + - 2 SEs 
# record whether or not the confidence interval calculated contains the true mean of the distribution

#### Distribution of levels of gene expression in Orangutans (upcoming ...) ####

# The overall goal for this exercise is: 
# 1. To "reproduce" the vizualization of the data presented in the scientific paper:
# Nguyen L-P, Galtier N, Nabholz B. 2015 Gene expression, chromosome heterogeneity and the fast-X effect
# in mammals. Biol. Lett.11 : 20150010.
# Data source:  
# http://dx.doi.org/10.1098/rsbl.2015.0010
# The data underlying this study are available on Dryad:
# doi:10.5061/dryad.qr20n
# 2. get familiarized with data summaries for "noisy" variables



##Preamble for loading the data
setwd(dir = "/Users/tbata/Dropbox/2017.FALL.Datascience.in.Bioinformatics/Dropbox.Data.Science.in.Bioinformatics/Week_03_SamplingDistributions/") #ADapt to your path here
mammals=read.csv(file = "rsbl20150010supp1.csv", header=T)
head(mammals) # a glimpse of the data

#### Q0: Understand the data ####
#identify how each column of the data corresponds to the variables described in the methods section of the paper 


#Question 1: extract a subdataset containing the chimpanzee data & vizualize the distribution of gene expression
#Question 2: do a scatter plot gene expression against dNdS: are X linked genes  "atypical"?
#Question 3: Suggest ways to better vizualize this relationship
# Question 4: Reproduce Figure 1 ####
# There are many ways of doing this ... I dont want an exact match of the look .. but I want to see a graph depicting the same info

