library(tidyverse)
library(gdata)
library(reshape)
library(plotly)

pheno = read.csv("aggression.male.csv", header =F)
names(pheno)=c("LineId","Aggression")

hist(pheno$Aggression)
select(pheno, Agression)

ggplot(pheno, aes(x=Aggression)) +
  geom_histogram()

?read.xls
pnas = read.xls("pnas.xlsx", sheet = 1)
pnas

d = read.csv("aggression.csv", header=T, sep=" ")


### 1) The data: phenotypes (agression score) and genotypes (1 SNP)

### 2) The tests for numbers in agression scores

### A) ANOVA. Agression (response) ~ SNP (factor)
## Fobs or R^2 model


names(d)

colnames(d) = c("line_id", "snp1", "snp2", "snp3", "snp4", "aggression")

d1 = filter(d, snp1!="-")
d2 = filter(d, snp2!="-")
d3 = filter(d, snp3!="-")
d4 = filter(d, snp4!="-")

# Residuals: #observations - #groups

lm_snp1 = lm(data=d1, aggression ~ snp1)
a1 = anova(lm_snp1)
s1 = summary(lm_snp1)
s1$r.squared
a1$`Sum Sq`

lm_snp2 = lm(data=d2, aggression ~ snp2)
a2 = anova(lm_snp2)
a2

lm_snp3 = lm(data=d3, aggression ~ snp3)
a3 = anova(lm_snp3)
a3

lm_snp4 = lm(data=d4, aggression ~ snp4)
a4 = anova(lm_snp4)
a4

?sample

d1 <- d1 %>%
  mutate(perm_snp1 = sample(x=d1$snp1, size=nrow(d1), replace=FALSE, prob=NULL))

d2 <- d2 %>%
  mutate(perm_snp2 = sample(x=d2$snp2, size=nrow(d2), replace=FALSE, prob=NULL))

d3 <- d3 %>%
  mutate(perm_snp3 = sample(x=d3$snp3, size=nrow(d3), replace=FALSE, prob=NULL))

d4 <- d4 %>%
  mutate(perm_snp4 = sample(x=d4$snp4, size=nrow(d4), replace=FALSE, prob=NULL))

f1 = list()

for(i in 1:1000){
  d1 <- d1 %>%
    mutate(perm_snp1 = sample(x=d1$snp1, size=nrow(d1), replace=FALSE, prob=NULL))
  lm_snp1 = lm(data=d1, aggression ~ perm_snp1)
  a1 = anova(lm_snp1)
  f1[[i]] = a1$`F value`[1]
}

###

f2 = list()

for(i in 1:1000){
  d2 <- d2 %>%
    mutate(perm_snp2 = sample(x=d2$snp2, size=nrow(d2), replace=FALSE, prob=NULL))
  lm_snp2 = lm(data=d2, aggression ~ perm_snp2)
  a2 = anova(lm_snp2)
  f2[[i]] = a2$`F value`[1]
}

###

f3 = list()

for(i in 1:1000){
  d3 <- d3 %>%
    mutate(perm_snp3 = sample(x=d3$snp3, size=nrow(d3), replace=FALSE, prob=NULL))
  lm_snp3 = lm(data=d3, aggression ~ perm_snp3)
  a3 = anova(lm_snp3)
  f3[[i]] = a3$`F value`[1]
}

###

f4 = list()

for(i in 1:1000){
  d4 <- d4 %>%
    mutate(perm_snp4 = sample(x=d4$snp4, size=nrow(d4), replace=FALSE, prob=NULL))
  lm_snp4 = lm(data=d4, aggression ~ perm_snp4)
  a4 = anova(lm_snp4)
  f4[[i]] = a4$`F value`[1]
}

f_values_frame = data.frame(f1 = unlist(f1),
                            f2 = unlist(f2),
                            f3 = unlist(f3),
                            f4 = unlist(f4))

melted_frame <- melt(f_values_frame)

ggplotly(
ggplot(melted_frame, aes(x=value, colour=variable)) +
  geom_histogram(bins = 23) +
  facet_wrap(~variable)
)

#lm(data=agression, Aggression ~ X3L1)

# Identify the design: fixed design, we want to know something about every indidual SNP



### B) t-test. unequal variance, permutation

### D) Nonparametric version of A/B

### E) Categorize agression (low, medium, high)