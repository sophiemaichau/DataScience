##### Loading libraries, data and cleaning up #####

library(tidyverse)
library(ggrepel)
library(plotly)
library(ggrepel)

# Loading data sets from 2011-2017
d11 = read.csv("2011sosr.csv", header = F)
d12 = read.csv("2012sosr.csv", header = F)
d13 = read.csv("2013sosr.csv", header = F)
d14 = read.csv("2014sosr.csv", header = F)
d15 = read.csv("2015sosr.csv", header = F)
d16 = read.csv("2016sosr.csv", header = F)
d17 = read.csv("2017sosr.csv", header = F)

# Cleaning up in the data and take out what I think are interesting
# Country, age, occupation, programming languages and OS.
d11 <- d11[-c(1,2),c(1,3,7,31:41,44)]
colnames(d11) = c("country","age","occupation","java","javascript","css","php","python","ruby","sql","c#","c++","c","perl","os")



##### Countries and number of users #####

# 19 Stack Overflow countries/areas (19)
countries <- data.frame(unique(select(d11, country), incomparables=FALSE)$country)
colnames(countries)[which(names(countries) == "unique.select.d11..country...incomparables...FALSE..country")] <- "country"

# Number of users assigned for each country
countries <- countries %>%
  mutate(people = 0)

for(i in 1:length(countries$country)){
  p <- nrow(filter(d11, country==(countries$country)[i]))
  countries$people[i] <- p
  print(countries$people[i])
}

# colorful and good overview of stackoverflow users
ggplot(countries, aes(x=country, y=people, label=country, color=country)) +
  geom_point() +
  geom_text_repel() +
  theme(axis.text.x=element_blank())

# interactive plot, show the same as above
ggplotly(
  ggplot(countries, aes(x=country, y=people)) +
    geom_point() +
    theme(axis.text.x=element_blank())
)



##### Country and primary occupation #####

# 13 different occupation
occ <- data.frame(unique(select(d11, occupation), incomparables= FALSE))
occ <- filter(occ, occupation != "")
countries$country

# For every country we assign each occupation with number of users with that occupation

# Adding every country as column in dataframe occ
for(c in countries$country){
  occ[c] <- NA
}

# A list of occupations
oc_list = c()
for(c in occ$occupation){
  oc_list = c(oc_list, c)
}

# fill out all number of occupations for each country
for(k in 1:length(oc_list)){
  for(i in 2:length(occ[1,])){
    occ[k,i] <- nrow(filter(filter(d11, country==countries$country[i-1]), occupation==oc_list[k]))
  }
}

# The most popular occupation among Stack overflow users
occ["SumCountries"] <- NA
for(i in 2:20){
  for(k in 1:length(oc_list)){
    occ$SumCountries[k] <- rowSums(filter(occ, occupation==oc_list[k])[2:20])
  }
}
maxval <- max(occ$SumCountries)
filter(occ, SumCountries==maxval)$occupation # Web Application Developer
maxval/sum(occ$SumCountries) # 40,28% of Stackoverflow users are webapp devops

