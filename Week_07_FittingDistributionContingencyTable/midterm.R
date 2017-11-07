library(tidyverse)
# use the melt function

# Q0:Import the data (warning the file is big, consider using skip and nmax when reading in data)

# For looking at the datasets structure, I used read.table, because it's faster and gives an overview
#dgrp <- read.table(file="dgrp2.tgeno", skip=0, nrow=10, sep=" ", header=TRUE)

# For importing the whole dataset (takes a while), I used read_delim on the space delimited dataset. Here I only choose the 'relevant' columns for analysing the data set.
d = read_delim(file = "dgrp2.tgeno", delim = c(" "),
               col_type = cols_only(chr='c', pos='i', id='c', ref='c', alt='c', refc='i', altc='i', qual='i', cov='i')
               )
d
summary(d)

# Tabular formatted genotype
# This data set is space delimited. We only ned to read the first 9 columns.
names(d)
# chromosome, position, id (type of chromosome), reference allele, alternate allele, ref count, alt count, quality, coverage...
# describes SNPs location on which chromosome, genomic position...
# either ref, ref or alt,alt
# 4.438.427 SNPs individuals

# 6 different chromosomes: 2L, 2R, 3L, 3R, 4, X
unique(select(d, chr), incomparables = FALSE)
# ------------------------- -------------------------  ------------------------- 

## Q1. Extract the first 100,000 SNPs located on chromosome 3L,
snp_100 = d %>%
  filter(chr=="3L", nchar(ref)==1, nchar(alt)==1) # exclude all polymorphisms that are not stricly speaking SNPs
snp_100 <-slice(snp_100, 1:100000)
snp_100
summary(snp_100)
View(snp_100)
# Chromosome 3L has position, individuals and reference, but no alt,ref,cov

# genomic position of the first SNP:
(select(snp_100, pos))[1,] #19.766
# genomic position of the last SNP:
select(arrange(snp_100, desc(pos)), pos)[1,] #2.483.531

# chip hypridization (genotyping) - sequencing is reading from start to end (every single letter)
# genotyping, looking at specific position for a letter
# sequencing gives a lot more information (but costs more)
# single based sequencing (genotyping)
# SNP typing gets cheaper and cheaper

# SNPs > 75% of the individuals could be genotyped
line_numbers <- 205

snp_100 <- snp_100 %>%
  mutate(filter=(refc+altc)/line_numbers, lines_genotyped=refc+altc)

View(snp_100)

nrow(filter(snp_100, filter >= 0.75)) #SNPs are left after 75 filter
nrow(filter(snp_100, filter >= 0.80))
nrow(filter(snp_100, filter >= 0.85))
nrow(filter(snp_100, filter >= 0.90))
nrow(filter(snp_100, filter >= 0.95))

snp_100 <- filter(snp_100, filter >= 0.95)

# ------------------------- -------------------------  ------------------------- 

## Q2. Graph the distribution of SNP allele frequencies, graph the distribution of coverage of SNPs ("cov")
# and the distribution of number of lines genotyped for each snp

# should we calculate maf??? minor altc refc
# weak/strong alleles

# "frequency" the alternative allele frequency (from week 2)
# distribution of SNP allele frequencies
ggplot(snp_100, aes(x=altc)) +
  geom_histogram(binwidth = 0.5) +
  xlim(c(0,100)) +
  labs(title="Distribution of SNP allele frequencies") +
  xlab("Frequency") +
  ylab("Number of SNPs")

# distribution of coverage of SNPs, looks like the binomial distribution
ggplot(snp_100, aes(x=cov)) +
  geom_histogram(binwidth = 0.5) +
  xlim(c(0,70)) +
  labs(title="Distribution of coverage of SNPs") +
  xlab("Coverage(x)") +
  ylab("Number of SNPs")

# distribution of number of lines genotyped for each SNP
ggplot(snp_100, aes(x=lines_genotyped)) +
  geom_histogram() +
  labs(title="Distribution of number of lines genotyped for each SNP") +
  xlab("Number of lines genotyped") +
  ylab("Number of SNPs")

# There is a relationship between different types of SNPs (genetic variation/SNV) and the allele frequency of SNPs; it is classified as a common SNP-type if
# it is a high maf SNP (overall allele frequency of SNPs with maf > 0.05) or rare/uncommon SNP-type if it is a low maf SNP (maf < 0.05)
# - which is hard to identify and validate.

# strong/weak alleles
# look at different variations, transition/transversions
# chi square test

# ------------------------- -------------------------  ------------------------- 


## Q3. If coverage was homogenous throughout the genome (by that we mean that on average the coverage is the same for any given position)

# If the coverage on average is the same for any given position, the Binomial distribution is expected to capture the coverage well, since it provides
# the probability distribution for the number of "successes" in a fixed number of independent trials, when the probability of suceess is the same in
# each trial.

# Poisson....

# ------------------------- -------------------------  ------------------------- 


## Q4. Make goodness of fit test of the coverage data: is the theoretical distribution proposed in Q3 a good fit for the data?

# Binomial goodness-of-fit test

p20 = 0.5
n = 100
n20 = 30
p = sum(dbinom(x = n20:n, size = n, prob=p20))
p
dbinom(x = n20:n, size = n, prob=p20)

# p=0.999. As the histogram shows from Q1, the binomial distribution from Q3 fits the data very well.

## OR Chi-square test????
chisq.test(snp_100$cov)
?chisq.test()
summary(snp_100)
glm(formula = snp_100$cov, family = poisson)

set.seed(612312)

n <- 1000
x <- runif(n)
mean <- exp(x)
y <- rpois(n,mean)
mod <- glm(y~x, family=poisson)
summary(mod)
pchisq(mod$deviance, df=mod$df.residual, lower.tail=FALSE)

snp_100 <- snp_100 %>%
  mutate(p=dpois(x=snp_100$cov, lambda = 1, log = FALSE))

?poisson.test

poisson.test(x=n, T=1, r=1, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)



poisson <- function(x, lambda) {
  probfn <- exp(-lambda) * (lambda ^ x) / factorial(x)
  return(probfn)
}


N <- sum(snp_100$cov)
RF <- snp_100$cov/N
DF <- data.frame(snp_100$cov, round(RF, 5))
MEAN <- sum(RF * N)
VAR <- (sum(N^2*snp_100$cov) - N*MEAN^2)/(N-1) # else use (MEAN+MEAN^2/R)
DISP <- VAR/MEAN # over dispersion
THETA <- 1/DISP
R <- MEAN*THETA/(1-THETA) # MEAN = R(1-THETA)/THETA
cbind(MEAN,VAR,DISP,THETA,R)

x=n
x
E_poi = round(N * dpois(x, lambda=MEAN),5)
E_poi
par(mfrow=c(1,1))
barplot(matrix(c(snp_100$cov,E_poi),nr=21479, byrow = TRUE), beside=T, 
        col=c("aquamarine3","coral"), 
        names.arg=x)
legend("topright", c("Observed","Expected Poisson"), pch=15, 
       col=c("aquamarine3","coral"), 
       bty="n")


#x <- sum(((snp_100$cov-snp_100$t_cov)^2)/snp_100$t_cov)




# hypothesis: homogeneus
# how many is low/high coverage, use quantile function (percent tile), ~9000 data points in each block low cov

# they are dependent of each other, can't use possion to capture the coverage 
# 10 blocks









n <- nrow(snp_100)
x <- runif(snp_100$cov)
mean <- exp(x)
y <- rpois(n,mean)
mod <- glm(y~x, family=poisson)
summary(mod)
#To calculate the p-value for the deviance goodness of fit test we simply calculate the probability
# to the right of the deviance value for the chi-squared distribution on 21476 degrees of freedom:
pchisq(mod$deviance, df=mod$df.residual, lower.tail=FALSE) # 6.117013e-55
# Our null hypothesis is that our model is correctly specified (to Poisson), and we have strong
# evidence to reject that hypothesis. So we have strong evidence that our model fits badly

# ------------------------- -------------------------  ------------------------- 

## Q5. Pick yourself a small (100 kb) region on chromosome 3L

# min pos + 100000

bin100 <- snp_100 %>%
  mutate(bin=pos %/% 100000 * 100000 + 100000) %>%
  group_by(bin) %>% 
  summarize(frequency_0=sum(altc),
            n = n()
            ) %>%
  ungroup()

View(bin100)

# Graph the frequency of "0" alleles along that region
ggplot(bin100, aes(x=frequency_0, y=n)) +
  geom_point() +
  labs(title="Distribution of \"0\" alleles along 100kb region") +
  xlab("\"0\" alleles") +
  ylab("Number of SNPs")

# Calculate the statistical association between allelic states of each pair of neighboring
# SNPS (this association is also called LD: Linkage Disequilibrium)
ld = matrix(data=x, nrow=2, ncol=2, byrow=T)

# pos and id??

# Explore visually how LD varies with the physical distance beween SNPs (measured in bp)

# Compare and discuss with the Fig 1 of the paper.









# total number of alleles for a SNP with a MAF of 0.05: #SNPs * 0.05 (shows how many times the MAF is present this region)

mafs=matrix(data=c(13,77,14,22,12,14), nrow=2, ncol=2, byrow=T)
rownames(mafs)=c("Strong", "Weak")
colnames(mafs)=c("ref", "alt")
mosaicplot(t(mafs), main="", col=c("red","yellow"), cex=2)
