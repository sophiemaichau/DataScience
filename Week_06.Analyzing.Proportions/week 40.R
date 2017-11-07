
log(0.5) < log(0.01)

dbinom(x=10, size=25, prob=0.061, log = FALSE)
dbinom(x=10:25, size=25, prob=0.061, log = FALSE)
sum(dbinom(x=0:25, size=25, prob=0.061, log = FALSE))
sum(dbinom(x=10:25, size=25, prob=0.061, log = FALSE))

# multiply by two because it's a two-tailed test
2*sum(dbinom(x=10:25, size=25, prob=0.061, log = FALSE))
#multiply n when n-tailed

# is a lot faster
pbinom(q=9, size=25, p=0.061, lower.tail = F)

binom.test(x=10, n=25, p=0.061)

#Best estimate p =
10/25

#best estimate on SE of proportion


######### ######### ######### 2 experiments dice

binom.test(x=2, n=12, p=1/6) #if you guess 5 correct, it's significant more than expected

# 3 F
# 4 F
# 5 F
# 6 T
# 3 T
# 4 F
# 6 F
# 2 T
# 1 F
# 1 F
# 4 F
# 6 F

3/12 #overlaps the CI
#My p value is 0.25 and is significant higher than expected
binom.test(x=3, n=12, p=1/6)

# H_0 p=1/6
# H_a: p < 1/4

#95-CI 0.05486064 > p > 0.57185846

#population proportion 0.25


# SE
p <- 0.25
sqrt((p*(1-p))/12)

# CI with Wald method
new <- 1.96 * sqrt((p*(1-p))/12+4)
new
p - new # -3.677649
        # < p <
p + new # 4.177649

#0/12 does not overlap the true p 1/6
# 0 < p
# SE 0

# 1 are physic (1/6*2)
2*0.05
#95% may overlap the CI
# threw dice 120 in one experiment, 5% will be significant
# n experiments: n * (0.05) chance

#adjust p-value: change alpha

# For thursday: save figure
