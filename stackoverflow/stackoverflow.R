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


#### 2011 ####

# Cleaning up in the data and take out what I think are interesting
# Country, age, occupation, programming languages and OS.
d11 <- d11[-c(1,2),c(1,3,7,31:41,44)]
colnames(d11) = c("country","age","occupation","java","javascript","css","php","python","ruby","sql","c#","c++","c","perl","os")



#### Countries and number of users

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



#### Country and primary occupation

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

# distribution of occupations
pie(occ$SumCountries, labels = oc_list, main="Pie Chart of occupations (2011)")



#### Distribution of occupations from 2012 ####
d12 <- d12[-c(1,2),c(1,3,7)]
colnames(d12) = c("country","age","occupation")
countries12 <- data.frame(unique(select(d12, country), incomparables=FALSE)$country)
colnames(countries12)[which(names(countries12) == "unique.select.d12..country...incomparables...FALSE..country")] <- "country"

occ12 <- data.frame(unique(select(d12, occupation), incomparables= FALSE))
occ12 <- filter(occ12, occupation != "")

for(c in countries12$country){
  occ12[c] <- NA
}

# A list of occupations
oc_list12 = c()
for(c in occ12$occupation){
  oc_list12 = c(oc_list12, c)
}

# fill out all number of occupations for each country
for(k in 1:length(oc_list12)){
  for(i in 2:length(occ12[1,])){
    occ12[k,i] <- nrow(filter(filter(d12, country==countries12$country[i-1]), occupation==oc_list12[k]))
  }
}

occ12["SumCountries"] <- NA
for(i in 2:20){
  for(k in 1:length(oc_list12)){
    occ12$SumCountries[k] <- rowSums(filter(occ12, occupation==oc_list12[k])[2:20])
  }
}

pie(occ12$SumCountries, labels = oc_list12, main="Pie Chart of occupations (2012)")

#### Distribution of occupations from 2013 ####
d13 <- d13[-c(1,2),c(1,3,7)]
colnames(d13) = c("country","age","occupation")
countries13 <- data.frame(unique(select(d13, country), incomparables=FALSE)$country)
colnames(countries13)[which(names(countries13) == "unique.select.d13..country...incomparables...FALSE..country")] <- "country"

occ13 <- data.frame(unique(select(d13, occupation), incomparables= FALSE))
occ13 <- filter(occ13, occupation != "")

for(c in countries12$country){
  occ13[c] <- NA
}

# A list of occupations
oc_list13 = c()
for(c in occ13$occupation){
  oc_list13 = c(oc_list13, c)
}

# fill out all number of occupations for each country
for(k in 1:length(oc_list13)){
  for(i in 2:length(occ13[1,])){
    occ13[k,i] <- nrow(filter(filter(d13, country==countries13$country[i-1]), occupation==oc_list13[k]))
  }
}

occ13["SumCountries"] <- NA
for(i in 2:20){
  for(k in 1:length(oc_list13)){
    occ13$SumCountries[k] <- rowSums(filter(occ13, occupation==oc_list13[k])[2:20])
  }
}

pie(occ13$SumCountries, labels = oc_list13, main="Pie Chart of occupations (2013)")


#### Distribution of occupations from 2014 ####
d14 <- d14[-c(1,2),c(1,4,7)]
colnames(d14) = c("country","age","occupation")
countries14 <- data.frame(unique(select(d14, country), incomparables=FALSE)$country)
colnames(countries14)[which(names(countries14) == "unique.select.d14..country...incomparables...FALSE..country")] <- "country"

occ14 <- data.frame(unique(select(d14, occupation), incomparables= FALSE))
occ14 <- filter(occ14, occupation != "")

for(c in countries14$country){
  occ14[c] <- NA
}

# A list of occupations
oc_list14 = c()
for(c in occ14$occupation){
  oc_list14 = c(oc_list14, c)
}

# fill out all number of occupations for each country
for(k in 1:length(oc_list14)){
  for(i in 2:length(occ14[1,])){
    occ14[k,i] <- nrow(filter(filter(d14, country==countries14$country[i-1]), occupation==oc_list14[k]))
  }
}

occ14["SumCountries"] <- NA
for(i in 2:20){
  for(k in 1:length(oc_list14)){
    occ14$SumCountries[k] <- rowSums(filter(occ14, occupation==oc_list14[k])[2:20])
  }
}

pie(occ14$SumCountries, labels = oc_list14, main="Pie Chart of occupations (2014)")


#### Distribution of occupations from 2015 ####
d15 <- d15[-c(1,2),c(1,2,6)]
colnames(d15) = c("country","age","occupation")
countries15 <- data.frame(unique(select(d15, country), incomparables=FALSE)$country)
colnames(countries15)[which(names(countries15) == "unique.select.d15..country...incomparables...FALSE..country")] <- "country"

occ15 <- data.frame(unique(select(d15, occupation), incomparables= FALSE))
occ15 <- filter(occ15, occupation != "")

for(c in countries15$country){
  occ15[c] <- NA
}

# A list of occupations
oc_list15 = c()
for(c in occ15$occupation){
  oc_list15 = c(oc_list15, c)
}

# fill out all number of occupations for each country
for(k in 1:length(oc_list15)){
  for(i in 2:length(occ15[1,])){
    occ15[k,i] <- nrow(filter(filter(d15, country==countries15$country[i-1]), occupation==oc_list15[k]))
  }
}

occ15["SumCountries"] <- NA
for(i in 2:20){
  for(k in 1:length(oc_list15)){
    occ15$SumCountries[k] <- rowSums(filter(occ15, occupation==oc_list15[k])[2:20])
  }
}

pie(occ15$SumCountries, labels = oc_list15, main="Pie Chart of occupations (2015)")


#### Distribution of occupations from 2016 ####
d16 <- d16[-c(1,2),c(3,6,10)]
colnames(d16) = c("country","age","occupation")

countries16 <- data.frame(unique(select(d16, country), incomparables=FALSE)$country)
colnames(countries16)[which(names(countries16) == "unique.select.d16..country...incomparables...FALSE..country")] <- "country"

occ16 <- data.frame(unique(select(d16, occupation), incomparables= FALSE))
occ16 <- filter(occ16, occupation != "")

for(c in countries16$country){
  occ16[c] <- NA
}

# A list of occupations
oc_list16 = c()
for(c in occ16$occupation){
  oc_list16 = c(oc_list16, c)
}

# fill out all number of occupations for each country
for(k in 1:length(oc_list16)){
  for(i in 2:length(occ16[1,])){
    occ16[k,i] <- nrow(filter(filter(d16, country==countries16$country[i-1]), occupation==oc_list16[k]))
  }
}

occ16["SumCountries"] <- NA
for(i in 2:20){
  for(k in 1:length(oc_list16)){
    occ16$SumCountries[k] <- rowSums(filter(occ16, occupation==oc_list16[k])[2:20])
  }
}

pie(occ16$SumCountries, labels = oc_list16, main="Pie Chart of occupations (2016)")

#### Distribution of occupations from 2017 ####
d17 <- d17[-c(1,2),c(4,16)]
colnames(d17) = c("country","occupation")

countries17 <- data.frame(unique(select(d17, country), incomparables=FALSE)$country)
colnames(countries17)[which(names(countries17) == "unique.select.d17..country...incomparables...FALSE..country")] <- "country"

occ17 <- data.frame(unique(select(d17, occupation), incomparables= FALSE))
occ17 <- filter(occ17, occupation != "")

for(c in countries17$country){
  occ17[c] <- NA
}

# A list of occupations
oc_list17 = c()
for(c in occ17$occupation){
  oc_list17 = c(oc_list17, c)
}

# fill out all number of occupations for each country
for(k in 1:length(oc_list17)){
  for(i in 2:length(occ17[1,])){
    occ17[k,i] <- nrow(filter(filter(d17, country==countries17$country[i-1]), occupation==oc_list17[k]))
  }
}

occ17["SumCountries"] <- NA
for(i in 2:20){
  for(k in 1:length(oc_list17)){
    occ17$SumCountries[k] <- rowSums(filter(occ17, occupation==oc_list17[k])[2:20])
  }
}

pie(occ17$SumCountries, labels = oc_list17, main="Pie Chart of occupations (2017)")