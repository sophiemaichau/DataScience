library(tidyverse)
d <-read_delim(file="dgrp2.tgeno",delim= " ", n_max=10000000)

## Q0. Import the data (warning the file is big, consider using skip and nmax when reading in data)
##Describe briefly the structure of the data and what the different variables mean
##- how many different chromosomes do you have?



## Q1
##- Locate the genomic position of the first and last SNP
##- filter out all SNPs for which less than 75%, 80%, 90%, 95%  of the individuals could be genotyped, 
##report how many SNPs are left based on the filter you apply

d<-subset(d, chr=="3L")
d<-d %>% filter(chr== "3L") %>% slice(1:100000)
head(d$pos)
tail(d$pos)
d<-d %>% filter(ref%in% c("A", "T", "G", "C"))
d<-d %>% filter(alt%in% c("A", "T", "G", "C"))
length(grep(x = colnames(d), pattern = "line"))
d<-d %>% filter((refc+altc)/205 >=0.75)
d<-d %>% filter((refc+altc)/205 >=0.80)
d<-d %>% filter((refc+altc)/205 >=0.90)
d<-d %>% filter((refc+altc)/205 >=0.95)
count(d)

## Q2. Graph the distribution of SNP allele frequencies, graph the distribution of coverage of SNPs ("cov") 
##and the distribution of number of lines genotyped for each snp
ggplot(data = d) + 
  geom_bar(mapping = aes(x = altc))

ggplot(data = d) + 
  geom_bar(mapping = aes(x = cov))

ggplot(data = d) + 
  geom_bar(mapping = aes(x =(refc+altc)/205 ))

## Q3. If coverage was homogenous throughout the genome (by that we mean that on average the coverage is the same for 
##any given posiiton),
##- what probability distribution is expected to capture well the coverage ? 

##Poisson distribution


## Q4. Make goodness of fit test of the coverage data: is the theoretical distribution proposed in Q3 a good fit for 
##the data?







