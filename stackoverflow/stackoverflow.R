##### Loading libraries, data and cleaning up #####

library(tidyverse)
library(ggrepel)
library(plotly)

# Loading data sets from 2011-2017
d11 = read.csv("2011sosr.csv", header = F)
d12 = read.csv("2012sosr.csv", header = F)
d13 = read.csv("2013sosr.csv", header = F)
d14 = read.csv("2014sosr.csv", header = F)
d15 = read.csv("2015sosr.csv", header = F)
d16 = read.csv("2016sosr.csv", header = F)
d17 = read.csv("2017sosr.csv", header = F)

#### Functions ####
distOcc = function(d, year){
  countries <- data.frame(unique(select(d, country), incomparables=FALSE)$country)
  colnames(countries) <- "country"
  countries <- filter(countries, country != "")
  occ <- data.frame(unique(select(d, occupation), incomparables= FALSE))
  occ <- filter(occ, occupation != "")
  
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
      occ[k,i] <- nrow(filter(filter(d, country==countries$country[i-1]), occupation==oc_list[k]))
    }
  }
  
  occ["SumCountries"] <- NA
  for(i in 2:20){
    for(k in 1:length(oc_list)){
      occ$SumCountries[k] <- rowSums(filter(occ, occupation==oc_list[k])[2:20])
    }
  }
  
  pie(occ$SumCountries, labels = oc_list, main=paste(c("Pie Chart of occupations (", year, ")"), collapse=""))
}




distLanguage = function(d, year){
  for(i in 4:14){
    language <- as.factor(colnames(d11)[i])
    print(language)
    #length(filter(d, language != "")$language)
  }
}

#### 2011 ####

# Cleaning up in the data and take out what I think are interesting
# Country, age, occupation, programming languages and OS.
d11 <- d11[-c(1,2),c(1,3,7,31:41)]
colnames(d11) = c("country","age","occupation","java","javascript","css","php","python","ruby","sql","c#","c++","c","perl")

distLanguage(d11[,4], 2011)

languages <- data.frame(d11[1,c(4:14)])
languages[,1] <- length(filter(d11, java != "")$java)
languages[,2] <- length(filter(d11, python != "")$python)
languages[,3] <- length(filter(d11, css != "")$css)

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
d12 <- d12[-c(1,2),c(1,3,7,23:35)]
colnames(d12) = c("country","age","occupation","java","javascript","css","php","python","objective-c","ruby","sql","c#","c++","c","perl", "html5")

distOcc(d12, 2012)

#### Distribution of occupations from 2013 ####
d13 <- d13[-c(1,2),c(1,3,7,57:62,64:69)]
colnames(d13) = c("country","age","occupation","c","c++","c#","java","javascript","jquery","nodejs","objective-c","php","python","ruby","sql")

distOcc(d13, 2013)


#### Distribution of occupations from 2014 ####
d14 <- d14[-c(1,2),c(1,4,7,43:53,55:65)]
colnames(d14) = c("country","age","occupation","c","c++","c#","java","javascript","nodejs","objective-c","php","python","ruby","sql","nodejs","haskell","coffeescript","dart","c++11","redis","mongodb","f#","go","hadoop","angularjs")

distOcc(d14, 2014)

#### Distribution of occupations from 2015 ####
d15 <- d15[-c(1,2),c(1,2,6,11:15,17,23,27:29,31:33,35:39,45,47,50)]
colnames(d15) = c("country","age","occupation","anuglar","c","c++","c++11","c#","coffeescript",
                  "go","java","javascript","lamp","mongodb","nodejs","objective-c","php","python",
                  "R","redis","ruby","sql","swift","wordpress")

distOcc(d15, 2015)

#### Distribution of occupations from 2016 ####
colnames(d16) = c("country","age","occupation")

distOcc(d16, 2016)

#### Distribution of occupations from 2017 ####
d17 <- d17[-c(1,2),c(4,16)]
colnames(d17) = c("country","occupation")

distOcc(d17, 2017)


