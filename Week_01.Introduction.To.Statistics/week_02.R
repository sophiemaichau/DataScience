?rnorm

library(ggplot2)

numbers = rnorm(1000000, 0, 5) #rnorm(n,mean=0,sd=5)
sd(numbers) #standard deviation
qplot(numbers)

numbers2 = rnorm(10000, 300, 500)
sd(numbers2) #500,7
mean(numbers2) #304.38
qplot(numbers2)

######

x = c(70:90, 1000, 1100, 1200)
mean(x) #207.5
median(x) #81.5
sd(x) # 345.8964
quantile(x) #  0%     25%     50%     75%    100% 
            # 70.00   75.75   81.50   87.25 1200.00 
mad(x) # 8.8956

celsius = c(0, 10, 20, 30, 40)
fahren = c(32,50,68,86,104)
sd(celsius)/mean(celsius) #CV = 0.7905694
sd(fahren)/mean(fahren) #CV = 0.4185367

kelvin = celsius + 273.15
rankine = fahren + 459.67
sd(kelvin)/mean(kelvin) # CV = 0.05393617
sd(rankine)/mean(rankine) # CV = 0.05393617