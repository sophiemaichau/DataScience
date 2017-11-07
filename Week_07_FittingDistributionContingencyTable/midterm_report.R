# In this assignment we will look at a data set about Drosophila melanogaster Genetic Reference Panel, a community resource for analysis of population genomics and quantitative traits.
library(tidyverse)

install.packages()
#### Q0 ####
### Structure of the data ###

# For looking at the datasets structure, I used read.table with skip and read only 10 rows out of 4.438.427 rows (SNP individuals), because it's faster and gives an overview
#dg <- read.table(file="dgrp2.tgeno", skip=0, nrow=10, sep=" ", header=TRUE)

# For importing the whole dataset (took a bit of time), I used read_delim on the space delimited dataset. Here I only choose the 'relevant' columns for analyzing the data set (the first couple of questions)
d = read_delim(file = "dgrp2.tgeno", delim = c(" "),
               col_type = cols_only(chr='c', pos='i', id='c', ref='c', alt='c', refc='i', altc='i', cov='i')
)

# Names shows the columns we are working with now:
names(d)

# , where each row represent a SNP and each column are properties of that SNP:
# chr is where the SNP is located (chromosomal region). In this data set we only have a subset of the chromosomes (6).
# pos is the genomic position
# id unique. It contains the chromosome, position and type of change/SNP.
# ref is the DNA base (ACG or T) of the reference allele
# alt is the DNA base (ACG or T) of the alternative allele
# refc is the reference allele frequency in this SNP
# altc is the alternative allele frequency in this SNP
# cov describes coverage/depth. It is the number of reads that include a given SNP in the reconstructed sequence.

# In the data set, there are 6 different chromosomes: 2L, 2R, 3L, 3R, 4, X
unique(select(d, chr), incomparables= FALSE)




#### Q1 ####
### The first 100.000 SNPs located on chromosome 3L ###

# Exclude all polymorphisms that are not stricly speaking SNPs located on chromosome 3L (the last 3 letters in id ends with SNP)
# and excluding all refc and altc that is zero (making sure there are difference between them)
snp_100 = d %>%
  filter(chr=="3L", nchar(ref)==1, nchar(alt)==1, refc > 0, altc > 0)

# Slice up the data set (takes the first 100.000 SNPs)
snp_100 <-slice(snp_100, 1:100000)

# The table now looks like this:
View(snp_100)

# genomic position of the first SNP:
(select(snp_100, pos))[1,]
# genomic position of the last SNP:
select(arrange(snp_100, desc(pos)), pos)[1,]

# Initialising the number of lines (line_21,line_26,...,line_913) that we excluded earlier
line_numbers <- 205

# Inserting columns: filter and lines_genotyped
# filter can be used when filtering out all SNPs less than the filter number (SNP individuals that could be genotyped)
# lines_genotyped used in next question, where we want to plot the distribution lines genotyped.
snp_100 <- snp_100 %>%
  mutate(filter=(refc+altc)/line_numbers, lines_genotyped=refc+altc)

# The table now looks like this:
View(snp_100)

# Now we filter out all SNPs for which less than 75%, 80%, 90% and 95%. The next lines output how many SNPs are left after filtering:
nrow(filter(snp_100, filter >= 0.75)) # 98.195
nrow(filter(snp_100, filter >= 0.80)) # 97.673
nrow(filter(snp_100, filter >= 0.85)) # 96.942
nrow(filter(snp_100, filter >= 0.90)) # 95.625
nrow(filter(snp_100, filter >= 0.95)) # 90.725

# Now we restrict our attention to SNPs genotyped in at least 95% of individuals:
snp_100 <- filter(snp_100, filter >= 0.95)




#### Q2 ####
### Graph distributions over SNP: allele frequencies, coverage and lines genotyped ###

# Distribution of SNP allele frequency
ggplot(snp_100) +
  geom_histogram(aes(altc, color="alternative"), bins=30) +
  geom_histogram(aes(refc, color="reference"), bins=30) +
  xlim(c(0,100)) +
  labs(title="Distribution of SNP allele frequencies") +
  xlab("Allele Frequency") +
  ylab("Number of SNPs")

# Calculating and inserting minor alle frequency(MAF) as column:
snp_100 <- snp_100 %>%
  mutate(maf=ifelse(altc < refc, altc/(altc+refc), refc/(altc+refc)))

View(snp_100)

# Distribution of coverage of SNPs, looks like a normal distribution
ggplot(snp_100, aes(x=cov)) +
  geom_histogram(binwidth = 0.5) +
  xlim(c(0,70)) +
  labs(title="Distribution of coverage of SNPs") +
  xlab("Coverage(x)") +
  ylab("Number of SNPs")

# Distribution of number of lines genotyped for each SNP. Using geom_bar because we work with discrete variables.
ggplot(snp_100, aes(x=lines_genotyped)) +
  geom_bar() +
  labs(title="Distribution of number of lines genotyped for each SNP") +
  xlab("Number of lines genotyped") +
  ylab("Number of SNPs")

### Statistical association

# Adding two columns with frequency for "true" SNPs with positions where A/T - and G/C are found
# and filtering away all SNPs where no "true" SNPs are found.
# Here we use alternative count (altc) as allele frequency.
snp_100 <- snp_100 %>%
  mutate(at_freq=ifelse(ref %in% c("A","T") & alt %in% c("A","T"),altc,0)) %>%
  mutate(gc_freq=ifelse(ref %in% c("G","C") & alt %in% c("G","C"),altc,0)) %>%
  filter((at_freq > 0) | (gc_freq > 0))

# Distribution of SNP allele frequencies with positions where A/T (weak alleles) - and G/C (strong alleles) are found
ggplot(snp_100) + 
  geom_bar(aes(x=at_freq, colour="A/T")) +
  geom_bar(aes(x=gc_freq, colour="G/C")) +
  xlim(c(0,100)) +
  ylim(c(0,3000)) +
  labs(title="Distribution of SNP allele frequencies") +
  xlab("Alternative Allele Frequency") +
  ylab("Number of SNPs")

# It seems like there is a statistical association between the two classes of SNPs (A/T and G/C)
# and allele frequency for them. Generally the A/T SNPs are greater than G/C SNPs.
summary(snp_100)
# Which can also be seen by looking at the means:
# A/T SNP frequency: 17.99 > G/C SNP frequency: 11.04

# Correct: there is no statistical association between the two classes.


#### Q3 ####
### Homogeneous coverage throughout the genome ###

# The Poisson distribution is expected to capture well the coverage, since on average the coverage is the same for any given position.
# This distribution describes when (successful) events happen independently of each other and occur with equal probability. Here the coverage.
# Goodness of fit tests indicate whether or not it is reasonable to assume that a random sample
# comes from a specific distribution. Using the Chi Square goodness-of-fit test we can test the fit of the
# Poisson distribution. The null and alternative hypotheses are as follows:
# H0: the coverage data fits the poisson distribution (the number of coverage follows a poisson distribution)
# HA: the coverage data do not fit the poisson distribution




#### Q4 ####
### Goodness of fit test of coverage date ###
snp_100 <- snp_100 %>%
  mutate(exp_pr=ppois(q=cov, lambda=mean(cov))) %>% # ppois gives the (log) distribution function
  mutate(n=n(), exp_cov=exp_pr*n) # theoretical/expected coveragex

obs <- snp_100$cov # observed coverage
exp <- snp_100$exp_cov # expected coverage
chisq.test(obs, exp)
# X-squared = 1052400, df = 2401, p-value < 2.2e-16

# The p-value is significant, so we can reject the null hypothesis.
# Therefore we can conclude that the observed coverage do not fit a Poisson distribution.





#### Q5 ####
### 100 kb region on chromosole 3L ###
f <- snp_100 %>%
  mutate(bin = (pos %/% 100) * 100, ld=0, bp=0) %>%
  select(pos,altc,refc,bin,ld, bp) %>%
  group_by(bin)

# Frequency of "0" alleles along the region
ggplot(f) +
  geom_histogram(aes(altc), bins=30) +
  labs(title="\"0\" allele frequencies on chromosome 3L") +
  xlab("\"0\" Allele Frequency") +
  ylab("Number of SNPs")

# Statistical association between allelic state of each pair of neighboring SNPs (Linkage Disequilibrium)

# refc is the frequency for 0's
# altc is the frequency for 2's

# LD = f(22)-f_1(2)-f_2(2)
#    = (n22/n)-(n22+n20/n)(n22+n02/n)

n = sum(f$altc) + sum(f$refc)

# Calculating LD, take 10 seconds
for(i in 1:nrow(f)-1){
  n22 = (f$altc[i]*f$altc[i+1])
  n20 = (f$altc[i]*f$refc[i+1])
  n02 = (f$refc[i]*f$altc[i+1])
  f22 = n22/n
  f1 = (n22+n20)/n
  f2 = (n22+n02)/n
  ld = f22-(f1*f2)
  f$ld[i] = ld
}

# Decay of LD (r^2) with physical distance on chromosome 3L
View(f)

# Calculating distance (bp) between SNPs, take 10 seconds
for(i in 2:nrow(f)){
  dist = (f$pos[i]-f$pos[i-1])
  if(dist <= 1000){
    f$bp[i] = dist
  } else {
    f$bp[i] = 0
  }
}

ggplot(f, aes(x=bp, y=ld)) +
  geom_point() +
  labs(title="Decay of linkage disequilibrium (r^2)") +
  xlab("Distance between SNPs (bp)") +
  ylab("LD (r-squared)")

ggplot(f) +
  geom_bar(aes(x=bp))

# This scatterplot has some similarities to figure 1C from the paper, but there is probably some calculation
# mistakes, since they do not look the same. LD are not decreasing the same way like in the figure.
# Maybe it is because I don't know how to plot the like as in the paper.
# The distance(bp) look more like the figure, so I think bp is calculated correct.
# One way to solve the plot could be if two bp are equal, take the minimum LD of them and delete the other.