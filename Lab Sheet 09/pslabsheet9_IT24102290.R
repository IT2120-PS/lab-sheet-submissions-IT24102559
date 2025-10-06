setwd("C:\\Users\\sasdj\\OneDrive\\Desktop\\lab9_PS")

x<-c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2)
t.test(x, mu=3)

weight <- c(17.6, 20.6, 22.2, 15.3, 20.9, 21.0, 18.9, 18.9, 18.9, 18.2)
t.test(weight, mu=25 , alternative= "less")

res <- t.test(weight , mu=25 , alternative= "less")
res$statistic
res$p.value
res$conf.int

y <- rnorm(30, mean = 9.8, sd = 0.05)

t.test(y, mu=10 , alternative= "greater")


# Part i: Generate a random sample of size 25 for the baking time.
# Population Mean (mu) = 45, Population Standard Deviation (sd) = 2
baking_time <- rnorm(25, mean = 45, sd = 2)

# Optional: Print the generated sample
print(baking_time)

# Part ii: Test whether the average baking time is less than 46 minutes at a 5% level of significance.

# The t.test command is used for the one-sample test.
# mu = 46 (the value in the null hypothesis)
# alternative = "less" (for the left-tailed test)
test_results <- t.test(baking_time, mu = 46, alternative = "less")

# Print the full results of the hypothesis test
print(test_results)
