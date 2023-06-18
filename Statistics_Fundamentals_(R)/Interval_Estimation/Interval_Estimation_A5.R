# Assignment 5

# 1
# Develop a 95% confidence interval estimate of the population mean monthly rent.
n <- 120
x.bar <- 3486
sigma <- 650
alpha <- 1 - 0.95
z <- qnorm(1 - alpha / 2, mean = 0, sd = 1, lower.tail = TRUE)
# Assume large enough population if not specified
x.bar.se <- sigma / sqrt(n)
print(paste("Interval estimate lower bound:", x.bar - z * x.bar.se))
print(paste("Interval estimate upper bound:", x.bar + z * x.bar.se))
#2
n <- 200
p <- 46/n
z <- 1.96
lower.confidence <- p-z*sqrt((p*(1-p))/n)
print(lower.confidence)
higher.confidence <- p+z*sqrt((p*(1-p))/n)
print(higher.confidence)
#3
#1.	How large a sample should be selected to provide a 95% confidence interval with a margin of error of 10? Assume that the population standard deviation is 40.
sigma <- 40
alpha <- 1 - 0.95
E <- 10
z <- qnorm(1 - alpha / 2, mean = 0, sd = 1, lower.tail = TRUE)
print(paste("The minimum sample size should be:", (z ^ 2) * (sigma ^ 2) / (E ^ 2)))
#4
# Load into RStudio
SGC.df <- read.csv("Data-SouthGermanCredit.csv")
#4-1
n <- 40
set.seed(1100)
sample.customer <- sample(SGC.df$X, n, replace = FALSE)
print(sample.customer)
#4-2
#Compute the sample mean and sample standard deviation of variable “amount”.
SGC.df.sample <- SGC.df[match(sample.customer, SGC.df$X),]
x.bar <- mean(SGC.df.sample$amount)
print(paste("Sample mean of amount:", x.bar))
s <- sd(SGC.df.sample$amount)
print(paste("Sample standard deviation of amount:", s))
#4-3
#Compute the sample proportion of variable “telephone” that takes value “yes (under customer name)”.
p.bar <- sum(SGC.df.sample$telephone == "yes (under customer name)") / n
print(paste("Sample proportion of customers with telephone:", p.bar))
#4-4
n <- dim(SGC.df)[1]
x.bar <- mean(SGC.df$amount)
sigma <- s
alpha <- 1 - 0.95
z <- qnorm(1 - alpha / 2, mean = 0, sd = 1, lower.tail = TRUE)
x.bar.se <- sigma / sqrt(n)
print(paste("Confidence Interval lower bound:", x.bar - z * x.bar.se))
print(paste("Confidence Interval upper bound:", x.bar + z * x.bar.se))
#4-5
n <- dim(SGC.df)[1]
p <- p.bar
z <- 1.96
lower.confidence <- p-z*sqrt((p*(1-p))/n)
print(lower.confidence)
higher.confidence <- p+z*sqrt((p*(1-p))/n)
print(higher.confidence)
