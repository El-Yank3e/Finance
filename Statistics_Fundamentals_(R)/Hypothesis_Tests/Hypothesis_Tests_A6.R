# Assignment 6
# Load into RStudio
SGC.df <- read.csv("Data-SouthGermanCredit.csv")

# 1
# Sample mean, standard deviation, and sample proportion
x.bar <- mean(SGC.df$amount)
print(paste("Sample mean of amount:", x.bar))
s <- sd(SGC.df$amount)
print(paste("Sample standard deviation of amount:", s))
n <- nrow(SGC.df)
print(n)
p.bar <- sum(SGC.df$telephone == "yes (under customer name)") / n
print(paste("Sample proportion of customers with telephone:", p.bar))

# 2
mu.zero1 <- 3000
# Assume population standard deviation is known
sigma1 <- 2800
x.bar.se1 <- sigma1 / sqrt(n)
# |---p-value approach: ====
z <- (x.bar - mu.zero1) / x.bar.se1
p <- pnorm(z, mean = 0, sd = 1, lower.tail = FALSE)
alpha <- 0.05
print(paste("p =", p, "and alpha =", alpha))
print(paste("p <= alpha is ", p <= alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(p <= alpha, "", "not "), "rejected.", sep = ""))
# |---CV approach: ====
z <- (x.bar - mu.zero1) / x.bar.se1
alpha <- 0.05
z.alpha <- qnorm(alpha, mean = 0, sd = 1, lower.tail = FALSE)
print(paste("z =", z, "and z.alpha =", z.alpha))
print(paste("Rejection region: (-inf, ", z.alpha, "]", sep = ""))
print(paste("z <= z.alpha is ", z <= z.alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(z <= z.alpha, "", "not "), "rejected.", sep = ""))

# 3
mu.zero2 <- 3400
x.bar.se2 <- s / sqrt(n)
# |---p-value approach: ====
t <- (x.bar - mu.zero2) / x.bar.se2
p <- pt(t, df = n - 1, lower.tail = TRUE)
alpha <- 0.05
print(paste("p =", p, "and alpha =", alpha))
print(paste("p <= alpha is ", p <= alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(p <= alpha, "", "not "), "rejected.", sep = ""))
# |---CV approach: ====
t <- (x.bar - mu.zero2) / x.bar.se2
alpha <- 0.05
t.alpha <- qt(alpha, df = n - 1, lower.tail = TRUE)
print(paste("t =", t, "and t.alpha =", t.alpha))
print(paste("Rejection region: [", t.alpha, ", +inf)", sep = ""))
print(paste("t >= t.alpha is ", t >= t.alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(t >= t.alpha, "", "not "), "rejected.", sep = ""))

# 4
mu.zero3 <- 44
x.bar.se3 <- s / sqrt(n)
# |---p-value approach: ====
t <- (x.bar - mu.zero3) / x.bar.se3
# Note that p value includes both tails!
p <- 2 * min(pt(t, df = n - 1, lower.tail = TRUE),
             pt(t, df = n - 1, lower.tail = FALSE))
alpha <- 0.01
print(paste("p =", p, "and alpha =", alpha))
print(paste("p <= alpha is ", p <= alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(p <= alpha, "", "not "), "rejected.", sep = ""))
# |---CV approach: ====
t <- (x.bar - mu.zero3) / x.bar.se3
alpha <- 0.01
# Note that alpha needs to be "distributed" among the two tails!
t.alpha <- qt(alpha / 2, df = n - 1, lower.tail = FALSE)
print(paste("t =", t, "and t.alpha =", t.alpha))
print(paste("Rejection region: (-inf, ", -t.alpha, "] and [", t.alpha, ", +inf)", sep = ""))
print(paste("t <= -t.alpha is ", t <= -t.alpha, ", t >= t.alpha is ", t >= t.alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(t <= -t.alpha | t >= t.alpha, "", "not "), "rejected.", sep = ""))

