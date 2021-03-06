###################################################################
# Likelihood from binomially & Poisson distributed data
###################################################################


##############################################
#### ExampleA: example 20.3 from AGD Book ####
##############################################

################# Walk through example #################################################
nT=32
Yobs=23
# The likelihood function is based on a specific choice: 
# Yobs is assumed to be binomial with unknown probabilty p
# and represents the number of wasps who choose mated females.
# nT is the number of trials. observations
# Here p is the parameter we want to estimate from the data. 

likelihoodWasp=function(p){
  return(dbinom(x = Yobs,size = nT,prob = p,log = F))
}

# When making likelihood calculation we often work with the so called LogLikelihood (on a)
# this is merely the natural log tof the the likelihood function)

LoglikelihoodWasp=function(p){
  return(dbinom(x = Yobs,size = nT,prob = p,log = T))
}

# The fuction defined above is only a function of one parameter 
# because the number of trials (nT) and the observed number of "success" Yobs are the data and hence fixed.

# Getting the likelihood of a specific parameter value
LoglikelihoodWasp(0.7)
LoglikelihoodWasp(0) ## what happened here ?
# A: if the parameter 0 equaled the observed value (no obs. value equals 0)

# Representing the likelihood function (using base function you can also use ggplot)
xs=seq(from = 0.01,to = 0.999, by=0.01) # the points at which we want to evaluate the likelihood
ys=LoglikelihoodWasp(xs) # here we give a vector to the function
plot(xs,ys, xlab="Parameter", ylab="LogL(Data)", type="l", lwd=2)

##Zooming around the region of the maximum
xs=seq(from = 0.6,to = 0.8, by=0.005) # the points at which we want to evaluate the likelihood
ys=LoglikelihoodWasp(xs) # here we give a vector to the function
plot(xs,ys, xlab="Parameter", ylab="LogL(Data)", type="l", lwd=2)

###Finding the MLE over a grid of possible parameters:
#- finding the mle directly:
maxL=max(ys)
#-finding the value of xs maximizing ys.
which.max(ys) ##finding the index position of the max in the ys vector, max=25
xs[which.max(ys)] ##you get the value at the index position of the max in the ys vector. 0.72


### NB another alternative is to find numerically the maximum of the function. 
# Not needed here but often used in practice when more than 2 parameters are involved in the likelihood function

#### Note that in that very simple case the MLE is known analytically: 
### MLE is just p=yobs/nT for the binomial case... Does it make sense ?


##Likelihood based confidence interval for the population parameter(reproducing fig 20-3-2):

xs=seq(from = 0.4,to = 0.9, by=0.005) # the points at which we want to evaluate the likelihood
ys=LoglikelihoodWasp(xs) # here we give a vector to the function
plot(xs,ys, xlab="Parameter", ylab="LogL(Data)", type="l")
abline(h = maxL-1.92)
## About the confidence interval:
# the confidence interval for the parameter p-real is the one where values pobs are not
# significantly different from p-real. This can be tested with a chi-2 test on the likelihood ratio,
# which reject H0 with a risk of 0.05 if the ratio is above 3.84. 
# The likelihood ratio test is 2[loglikelihood(p-real)-loglikelihood(pobs)]. 
# so we reject H0 if likelihood(pobs)<=likelihood(p-real)-1.92 which is the horizontal line we just drew.

# The loglikelihood ratio test statistic and its associated p-value:

G=2*(LoglikelihoodWasp(0.72)- LoglikelihoodWasp(0.5))
# Under the hypothesis Ho: p=0.5, we expect G to be Chi^2 with 1 df. 
# So any G value above 3.84 (the 0.95 cutoff of a Chi^2 with 1.d.f) is rejecting Ho at the 0.05 level
qchisq(0.95,df=1) #(qchisq gives you the minimum chi square value where 95 of the values falls.)
# 3.84

##########################################################################################################

############# Supplementary Questions based on the same example ##########################################

# Question 1: Find the boundaries of the confidence interval 
#Hint: find the subset of xs that give a LogL within 1.92 of the maximum value

# take subset of a vector and then take the range of it (min, max)

# Question 2: First using the R function giving the CDF of chi^2 distibution, find the p value associated with G.
#! Reminder: cumulative function= P(X<=x).

# 1 - pchisq(G,1)

# Question 3. Check the accuracy of the assumption of the Likelihood ratio test regarding the Chi^2 distribution of the G statistic. 
# To do so Generate 10,000 datasets under the null hypothesis: ie in that case nT=32, and p=1/2 
# and for each simulated dataset, compute G. 
# Compare visually the simulated distribution for G under Ho and the Chi^2 distribution with 1 d.f. 
# that is classically used to approximate that distribution.

# Question 4: Imagine the same experiment was repeated with a larger sample size 
# but the same observed proportion (in practice it is not likely but lets assume that ;-)). 
# Graph and compare the likelihood profiles for the three datasets on the same graph
# Dataset 1: nT = 32, Yobs =23 
# Dataset 2: nT = 64, Yobs= 46
# Dataset 3 nT=160, Yobs = 115
# Recalculate the likelihood ratio test and its associated p-value for each dataset

########################################################################################################
#### Example B: Whole Genome resequencing of genomes stemming from an experimental evolution design ####
########################################################################################################

###################################################### Background
# A number of lines of bacteria have been transferred regularly in the lab (so called experimental evolution). 
# Lines were founded from the same ancectral genotype but with some specific genes "knocked out"
# 
# Data consists in observed counts of SNPs detected relative to the Nod+ ancestor at the end of the experiment
# We have as data:
# in the hrpg(STOP) background 6 experimental lines (Tgens of evolution in each line) 
#   yielding in total  39+47+84+48+46+86 =350  SNPs mutations
# in the hrcv(STOP) background 3 exp lines (Tgens in each line) yielding in total 33+38+25 = 96 SNPs mutations


# The number of point mutations (SNPs) detected in the experimental lines suggests that 
# lines evolving in the hrpg(STOP) background undergo a higher mutation rate relative to 
# the lines evolving from a hrcv(STOP) ancestor.
# The purpose of the questions below is to explore whether  
# there is statistical evidence for that assertion using a likelihood framework. 
# 
# 
# Model Assumptions for calculating the likelihood of the data:
# The mutation process  is assumed to be a Poisson (stochastic) process with rate lambda= Tgens*mu, 
# where Tgens is the actual number of generation separating the ancestral strains from the resequenced strains 
# at the end of the experiment and mu the mutation rate (define per genome and per generation)
#
# Here, we assume that Tgens is 600 but note that although it will affect 
# the actual per genome per generation mutation rate estimates (it will scale it accordingly) 
# it does not affect the finding i.e. that the genome have a different mutation rate or not if 
# Tgens in unknown but identical for all lines.
# 
# mu is defined as a mutation rate per genome and per generation for point mutations (SNPs). 
# In all rigor this is a mutation rate per "effectively covered genome" 
# (defined over the fraction of the genome that is covered by reads that can be mapped without ambiguity). 
#
# So in practice, an important point to check is that the fraction of the genome covered by reads 
# in each experimental line is not widely different.. 
# and if so to calculate a relative genome coverage for each experimental line...
# but here we assume that the number of nucleotide over which SNP mutations could be "called" is identical. 

########################################################## End of background


### Question 1: Write down the likelihood function as a function of 
# a single parameter we will call U which is defined as the per genome per generation mutation rate 
# and as a function of the observed data which is the number of SNPs.
#
# Hint: use the density of a poisson distribution dpois()



### Question 2: For each genomic background find the MLE of U and an approx 95% confidence interval for U.

### Question3: Use a likelihood ratio test to compare a null model where both genomic background 
# have identical mutation rate U versus an alternative model 
# where U1 and U2 are background specific mutation rates for respectively 
# the hrpg(STOP) and hrcv(STOP) background.
#
# Hint: here we compare a null model with 1 fitted parameter (U) 
# to an alternative model with two fitted parameters (U1 and U2) so in principle 
# the G statistic of the Likelihood ratio test should be 
# Chi^2 with 1 d.f if under the null hypothesis that both genomic background have identical U.


### Question 4: Imagine that the experiment could be running for potentially 2, 5 or 10 times as long, 
# i.e. Tgens= 1200, 3000 or 6000 generations. 
# Assume 2 lines are re-sequenced in each background. 
# For each value of Tgens, what would be the probability of such an experiment to detect a 20% change 
# in mutation rate if we assume that one background has mutation rate U1=0.05 per genome per gen 
# and the other has U2 = 0.06 per genome per gen?
#
# We further assume that we reject the null if the likelihood ratio test for comparing a model 
# with two backgrounds genome mutation rates versus a single one has a p-value smaller than alpha=0.05.

