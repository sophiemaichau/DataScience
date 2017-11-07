#### Week 02 ####

#### R for data science exercises: describing data ####

# The next part of the exercises will learn you to manipulate data in the R tidyverse
# Like last week you will work through online tutorials that will introduce you to data manipulation
# We advice you to make a new file (R script) and use that for doing the tutorial.
# When done - save the R script with an appropriate filename and in an appropriate location
#
#
# The tutorial is from the free book "R for data science" written by the R Overlord Hadley Wickham
# URL: http://r4ds.had.co.nz/index.html
#
#
# Read Welcome
# Read and do 4. and 5. 
# For general introduction to Rstudio: read 6.
#
# If you need help have a look at the different cheatsheets
# And if you are stuck - use the online forum!
#


#### Use your new knowledge to Work on real data ####

library(tidyverse)

# By using the knowledge from above we will now work on a dataset
# The data is the SNP allele frequencies from the 1000 genomes project
# from chromosome 6 (you can see the README file to see how I downloaded and formatted this dataset)

# We can read zipped files (saves transfer time and space on disk)
d = read_delim(file="1000g.allele.frequencies.tsv.gz",delim=c("\t"))
d
#### Counting SNPs ####

# Q: What is the distribution of the variable "frequency"?
dist <- ggplot(data = d, mapping = aes(x = frequency)) +
  geom_histogram(binwidth = 0.05)

dist

# Q: Is it different for the different populations?

dist + facet_wrap(~population)

# Notice the few SNPs with high frequency (> 0.5)
# Q: How many SNPs pr population have frequency > 0.5?
nrow(filter(d, frequency > 0.5, population == "EUR"))
nrow(filter(d, frequency > 0.5, population == "AFR"))
nrow(filter(d, frequency > 0.5, population == "EAS"))

halfFreq <- d %>%
  filter(frequency > 0.5) %>%
  group_by(population) %>%
  summarise(count = n())

View(halfFreq)

ggplot(data = filter(d, frequency > 0.5), mapping = aes(x = frequency)) +
  geom_histogram(binwidth = 0.01) +
  facet_wrap(~population)

# Q: What does this mean? (a frequency > 0.5)

# We calculate minor allele frequency
d = d %>% mutate(maf=ifelse(frequency>0.5, 1-frequency, frequency))

# Q: How many SNPs have maf > 0 (are polymorphic) for each population?
nrow(filter(d, maf > 0, population == "EUR"))
nrow(filter(d, maf > 0, population == "AFR"))
nrow(filter(d, maf > 0, population == "EAS"))

maf_0 <- d %>%
  filter(maf > 0) %>%
  group_by(population) %>%
  summarise(count = n())

View(maf_0)

# Q: How many SNPs have maf > 0.05 (common polymorphism) for each population?
nrow(filter(d, maf > 0.05, population == "EUR"))
nrow(filter(d, maf > 0.05, population == "AFR"))
nrow(filter(d, maf > 0.05, population == "EAS"))

maf_005 <- d %>%
  filter(maf > 0.05) %>%
  group_by(population) %>%
  summarise(count = n())

View(maf_005)

#### Interquartiles ####

# Q: Calculate the mean, sd and median and the 0.25, 0.5 and 0.75 quantiles of the maf for each population
# HINT: ?quantile
?quantile

# MAF (Mutation Annotation Format)

d %>% 
  filter(maf > 0) %>%
  group_by(population) %>%
  summarise(mean_maf = mean(maf),
            sd_maf=sd(maf),
            median_maf=median(maf), 
            q25 = quantile(maf, probs=0.25),
            q50 = quantile(maf, probs=0.50),
            q75 = quantile(maf, probs=0.75))


# Q: How many SNPs are fixed for the reference allele in AFR but fixed for the alternative allele in EUR and EAS respectively?
# HINT: the original frequency is of the alternative allele

# A fixed allele means all individuals have the same allele (e.g. G/G = homozygote)
# For the computer scientists: you inherit an allele from you mother and your father.
# So basically your genotype can be:
# REF/REF = homozygotic reference allele
# REF/ ALT = heterozygote
# ALT/ALT = homozygotic alternative allele
# If the frequency==0.0 it means the reference allele is fixed (and 1.0 corresponds to the alternative allele being fixed)

d %>% 
  group_by(population) %>% 
  summarize(fixed_ref = sum(frequency == 0.0),
            fixed_alt = sum(frequency == 1.0))

# AFR fixed_ref: 1.379.734, EUR fixed_alt: 4.981, EAS fixed_alt: 10.501
  
#### Cumulative  frequencies ####

# Q: Make a cumulative frequency plot of the minor allele frequency for the different populations (color)
# Zoom in to maf 0-0.05
# HINT: ?stat_ecdf()
ggplot(d, aes(x=maf, colour = population)) +
  stat_ecdf(pad=F) +
  xlim(c(0,0.05))

# Q: Explain the plot - what do you see?

# Q: Which population has the most "rare" alleles?
# East Asia


#### Looking for interesting patterns ####

# Now we will try and see if there is somewhere on the chromosome with a funny allele frequency structure.
# Basically we ask - can you find something interesting?

# Q: Try and plot a scatterplot of x=position and y=maf 
d[sample(nrow(d),1000),] %>%
  ggplot(mapping = aes(x = position, y = maf, color = population)) +
  geom_point()

plotCrash <- ggplot(sample_n(d, 100), aes(x=position, y = maf, colour = population)) + 
  geom_point()

plotCrash

# Sorry - it will likely crash your machine
# HINT: ... %>% sample_random(100) will select 100 random rows from your data

# Q: Are you able to detect a pattern or not?

# Q: What kind of pattern do you see?

# Q: Instead of only plotting some of the data we want to summarise the data.
# Add a "bin" variable that is the binned position, in bins of size 25000
# and summarise the mean and sd pr. bin (of the minor allele frequency)
# Also get the number of observations for each bin
# HINT: 1223 %/% 100 = 12,  12 * 100.00  + 50 = 1250 = all values 1200-1299 will become 1250 
# ? %/% and ?%% - I think you used them in the R for datascience exercises as well.

d <- d %>%
  mutate(bin = position %/% 25000 * 25000 + 12500)

head(d)
tail(d)
slice(d, 1:10)

bin_var <- d %>%
              group_by(bin, population) %>%
              filter(maf > 0) %>%
              summarize(mean = mean(maf),
                        sd = sd(maf),
                        count = n()
                        )

# Q: Plot the number of SNPs pr. bin - a subplot for each population - do you see something?
ggplot(data = bin_var, mapping = aes(x = bin, y = count, color = population)) +
  geom_point() +
  facet_wrap(~population)
  
# Q: What do you see - is there a pattern in snp density and/or allele frequency along the chromosome?
# They looks like the same.

# Q: Plot the mean MAF pr. bin - a subplot for each population (facet) and also illustrate first sd then number of SNPs in bin (color)
ggplot(data = bin_var, mapping = aes(x = bin, y = mean, color = count)) +
  geom_point() +
  facet_wrap(~population, ncol = 1)

# Q: Do you see a peak in maf somewhere?

# Q: Use the ggplot function coordinate_cartesian(xlim=c(,)) to zoom in on this region
ggplot(data = bin_var, mapping = aes(x = bin, y = mean, color = population)) +
  geom_point() +
  facet_wrap(~population) +
  xlim(c(25.0e+06,35.0e+06))

# Q: What kind of natural selection do you think is present here? (purifying or balancing selection?)
# Balancing because we have a lot of variations

# Q: What kind of genes do you think are present here?
# Google or use a genome browser to help you




#### Transitions and transvertions ####

# A-G  and C-T are transitions (purine - purine and pyrimidine - pyrimidine)
# A-T and A-C are transversions, i.e. when REF allele is purine and ALT allele is a pyrimidine or the other way around

purines     = c("A", "G")
pyrimidines = c("C", "T")
transitions = c(purines, pyrimidines)
transversions = c(c("A", "T"), c("A", "C"))

# Q: If all mutations were random: what is the expected transition:transversion ratio?
# 1/3 transition, 2/3 transversion

# Q: What is the observed transition:transversion ratio for each population?
d %>%
  filter(maf > 0) %>%
  group_by(population) %>%
  select(population, ref = reference_allele, alt = alternative_allele) %>%
  summarise(transitions = sum(ifelse((ref %in% c("G","A") & alt %in% c("G","A")) |
                                       (ref %in% c("T","C")  & alt %in% c("C","T")),
                                     1,
                                     0)),
            count = n(),
            ratio = transitions / count
            )
# End of exercise
