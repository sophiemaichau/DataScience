######## 4 Workflow: basics

# R as calculator
1/200*30
(59+73+2)/3
sin(pi/2)
x <- 3*4

float_number <- 3.6
x+float_number

list_of_number <- seq(1,10)
list_of_number

y <- seq(1,10, length.out = 5)

my_var <- 10
my_var
my_variable <- 2
my_variable

library(tidyverse)
# dota, fliter, =, diamond
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
?mpg
?filter
?diamonds

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)

#Alt + Shift + K: key shortcuts



########### 5 Data transformation

library(nycflights13)
nycflights13::flights
View(flights)
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))
# near, equals

filter(flights, arr_delay <= 120, dep_delay <= 120)


### 5.2.4 Exercises
?flights
filter(flights, arr_delay > 120)
filter(flights, dest == "HOU" | dest == "IAH")
?airlines
filter(flights, carrier %in% c("UA", "AA", "DL"))
filter(flights, month %in% c(7,8,9))
filter(flights, arr_delay > 120 & dep_delay <= 0)
filter(flights, arr_delay < 30 & dep_delay >= 60)
filter(flights, dep_time >= 00 & dep_time <= 6)
filter(flights, between(dep_time,00,6))
filter(flights, is.na(dep_time))
filter(flights, is.na(NA^0))
#NA^0==1, NA|TRUE, FALSE&NA==FALSE, NA*0==NA are not columns

arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))


#### 5.3.1 Exercises
arrange(df, desc(is.na(x)))
arrange(flights, desc(arr_delay))
arrange(flights, !desc(dep_time))
arrange(flights, !desc(air_time))
arrange(flights, desc(distance))
arrange(flights, !desc(distance))

##

select(flights, year, month, day)
?select


### 5.4.1 Exercises
filter(select(flights, dep_time), between(dep_time, 00, 06))
select(arrange(flights, desc(arr_delay)), arr_delay:flight)
select(arrange(flights, desc(dep_delay)), dep_delay, carrier)
select(arrange(flights, desc(dep_delay)), arr_time)

select(flights, arr_time, arr_time) # does nothing

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
one_of("year", vars = "year") # is years one of vars?
select(flights, contains("TIME"))

#####

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

(x <- 1:10)
lag(x)
lead(x)
cumsum(x)
cummean(x)

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)
ntile(y, 5) #a rough rank, which breaks the input vector into n buckets.

#### 5.5.2 Exercises
min_flights <- select(flights, dep_time, sched_dep_time)
arrange(min_flights, !desc(dep_time))
transmute(min_flights,
         dep_time,
         sched_dep_time,
         dep_time_MAM = (dep_time %/% 100) * 60 + (dep_time %% 100),
         sched_dep_time_MAM = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100)
)

transmute(select(flights, arr_time, dep_time, air_time),
          air_time,
          arr_dep_diff = arr_time - dep_time)
# they are not the same, no need to fix because the flight is not always in the air, also drives

select(flights, dep_time, sched_dep_time, dep_delay)

select(arrange(flights, desc(dep_delay)), dep_delay, carrier)
1:3 + 1:10 # not the same length
?Trig
cos(x)
sin(x)
tan(x)

acos(x)
asin(x)
atan(x)
atan2(y, x)

cospi(x)
sinpi(x)
tanpi(x)

####

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

#####

batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)

batters %>% 
  arrange(desc(ba))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

sd(x)
IQR(x)
mad(x)

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

first(x)
nth(x,2)
last(x)
x[length(x)]

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year  <- summarise(per_month, flights = sum(flights)))

daily %>% 
  ungroup() %>%         
  summarise(flights = n())

#### 5.6.7 Exercises

airlines

####

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

### 5.7.1 Exercises

###