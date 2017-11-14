library(tidyverse)
library(ggrepel)
df = data.frame(x=1:100, y = 5 + 0.05*(1:100) + rnorm(100))

# cor() is used for calculating correlations?
# cor.test() for testing
?cor
cor(x = df$x, y = df$y, method = "pearson")
?cor.test
cor.test(x=df$x, y=df$y, method="pearson")

# We use the lm function to build a linear model in R: 
?lm

fit = lm(y ~ x, data = df) # linear model
summary(fit)
names(fit)

ggplot(data = df, mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm")

# US states only
us = read_delim("us_states_guns_and_deaths.csv",delim = ";" )
summary(us)
names(us)

ggplot(data = us, mapping = aes(x = gun_ownership, y = rate_all, label=state)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm")

lm_death = lm(rate_all ~ gun_ownership, data=us)
summary(lm_death)

#testing correlation
cor.test(x=us$gun_ownership, y=us$rate_all, method="pearson")
# 0.603

# USA vs. rest of the world, guns pr 100.000 person
world = read_csv("country-gun-stats.csv")
summary(world)
names(world)

# Get industrialized countries only
world <- world %>%
  filter(GDP > 20000)

ggplot(data = world, mapping = aes(x = guns_pr_capita, y = homicides, label=Country)) +
  geom_point(aes(colour=Country)) +
  geom_text_repel() +
  geom_smooth(method = "lm", se=TRUE, lty="dashed", color="#808080")

linear_model = lm(homicides~guns_pr_capita, data=world)
summary(linear_model)
cor.test(x=world$guns_pr_capita, y=world$homicides, method="pearson")
