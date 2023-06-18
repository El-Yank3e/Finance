# 1
ambulance.df <- read.csv("Data-Ambulance.csv")
ambulance.df$County <- factor(ambulance.df$County, levels = c("Urban", "Rural"))
ambulance.df$Day.of.Week <- factor(ambulance.df$Day.of.Week,
                             levels = c("Sunday", "Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday"))
# Compute the observed frequency of emergency
# ambulance calls in different counties by the day of week
ambulance.table.f <- table(ambulance.df[c("County", "Day.of.Week")])
ambulance.table.f
# Compute the sample size
n <- margin.table(ambulance.table.f)
# Compute the proportion of emergency
# ambulance calls in different counties regardless of the day of week
p.i <- margin.table(ambulance.table.f, 1) / n
# Compute the proportion of emergency ambulance calls
# during each day of the week regardless of county
p.j <- margin.table(ambulance.table.f, 2) / n
# Compute the expected frequency of emergency
# ambulance calls in different counties by the day of week
ambulance.table.e <- p.i %o% p.j * n
ambulance.table.e
ambulance.table.test <- (ambulance.table.f - ambulance.table.e) ^ 2 / ambulance.table.e
chisq <- margin.table(ambulance.table.test)
df <- (dim(ambulance.table.f)[1] - 1) * (dim(ambulance.table.f)[2] - 1)
alpha <- 0.05
# |---p-value approach: ====
print("Null Hypothesis (H0): Ambulance calls for each county (urban and rural) are independent of the days of the week.")
print("Alternative Hypothesis (Ha): Ambulance calls for each county (urban and rural) are not independent of the days of the week.")
p.value <- pchisq(chisq, df = df, lower.tail = FALSE)
print(paste("p.value =", p.value, "and alpha =", alpha))
print(paste("p.value <= alpha is ", p.value <= alpha, ".", sep = ""))
print(paste("H0 is", ifelse(p.value <= alpha, "", " not"), " rejected.", sep = ""))
# |---Critical value approach: ====
print("Null Hypothesis (H0): Ambulance calls for each county (urban and rural) are independent of the days of the week.")
print("Alternative Hypothesis (Ha): Ambulance calls for each county (urban and rural) are not independent of the days of the week.")
chisq.alpha.rt <- qchisq(alpha, df = df, lower.tail = FALSE)
print(paste("chisq =", chisq, "and chisq.alpha.rt =", chisq.alpha.rt))
print(paste("Rejection region: [", chisq.alpha.rt, ", +inf)", sep = ""))
print(paste("chisq >= chisq.alpha.rt is ", chisq >= chisq.alpha.rt, ".", sep = ""))
print(paste("H0 is ", ifelse(chisq >= chisq.alpha.rt, "", "not "), "rejected.", sep = ""))

# 2
finalexam.df <- read.csv("Data-Grades.csv")
n <- length(finalexam.df$Grade)
n
x.bar <- mean(finalexam.df$Grade)
s <- sd(finalexam.df$Grade)
print(paste("Sample mean =", x.bar))
print(paste("Sample standard deviation =", s))
# Determine the number of intervals
k <- floor(n / 4)
# Determine the break points for bincode function
breaks <- x.bar + c(-Inf, qnorm(seq(1, k) / k)) * s
# Create observed frequency vector
# The observed frequencies is different from the ones in the book!
finalexam.f <- table(.bincode(finalexam.df$Grade, breaks, include.lowest = TRUE))
finalexam.f
# Create expected frequency vector
# Note that finalexam.f / finalexam.f is used to create a
# table with all 1s with the same column names as finalexam.f
finalexam.e <- (n / k) * finalexam.f / finalexam.f
finalexam.e
finalexam.test <- (finalexam.f - finalexam.e) ^ 2 / finalexam.e
chisq <- sum(finalexam.test)
df <- k - 2 - 1
alpha <- 0.05
# |---p-value approach: ====
print(paste("Null hypothesis (H0): The final examination grades follow a normal distribution with mean ",
            x.bar, " and standard deviation ", s, ".", sep = ""))
print(paste("Alternative hypothesis (Ha): The final examination grades do not follow a normal distribution with mean ",
            x.bar, " and standard deviation ", s, ".", sep = ""))
p.value <- pchisq(chisq, df = df, lower.tail = FALSE)
print(paste("p.value =", p.value, "and alpha =", alpha))
print(paste("p.value <= alpha is ", p.value <= alpha, ".", sep = ""))
print(paste("H0 is", ifelse(p.value <= alpha, "", " not"), " rejected.", sep = ""))
# |---Critical value approach: ====
print(paste("Null hypothesis (H0): The final examination grades follows a normal distribution with mean ",
            x.bar, " and standard deviation ", s, ".", sep = ""))
print(paste("Alternative hypothesis (Ha): The final examination gradess do not follow a normal distribution with mean ",
            x.bar, " and standard deviation ", s, ".", sep = ""))
chisq.alpha.rt <- qchisq(alpha, df = df, lower.tail = FALSE)
print(paste("chisq =", chisq, "and chisq.alpha.rt =", chisq.alpha.rt))
print(paste("Rejection region: [", chisq.alpha.rt, ", +inf)", sep = ""))
print(paste("chisq >= chisq.alpha.rt is ", chisq >= chisq.alpha.rt, ".", sep = ""))
print(paste("H0 is ", ifelse(chisq >= chisq.alpha.rt, "", "not "), "rejected.", sep = ""))

# 3.1
flights.df <- read.csv("Data-Flights.csv")
flights.df$Status <- factor(flights.df$Status,
                                  levels = c("ontime", "delayed"))
flights.df
# Compute the observed frequency of flights by carrier
# and their status
flights.table.f <- table(flights.df[c("Carrier", "Status")])
flights.table.f
# Compute the sample size
n <- margin.table(flights.table.f)
# Compute the proportion of carriers
# regardless of their status
p.i <- margin.table(flights.table.f, 1) / n
# Compute the proportion of flights and their status
# regardless of which carrier
p.j <- margin.table(flights.table.f, 2) / n
# Apply the above proportion to compute the expected
# frequency of flights by carrier and their status
# %o% stands for outer product (of two vectors):
# x1 <- c(1, 2, 3)
# x2 <- c(4, 5, 6)
# t(t(x1))
# t(x2)
# x1 %o% x2
flights.table.e <- p.i %o% p.j * n
flights.table.e
flights.table.test <- (flights.table.f - flights.table.e) ^ 2 / flights.table.e
flights.table.test
chisq <- margin.table(flights.table.test)
df <- dim(flights.table.f)[1] - 1
alpha <- 0.05
# |---p-value approach: ====
print("Null Hypothesis (H0): The proportions of flights that were on time are the same across all carriers.")
print("Alternative Hypothesis (Ha): The proportions of flights that were on time are not the same across all carriers.")
p.value <- pchisq(chisq, df = df, lower.tail = FALSE)
print(paste("p.value =", p.value, "and alpha =", alpha))
print(paste("p.value <= alpha is ", p.value <= alpha, ".", sep = ""))
print(paste("H0 is", ifelse(p.value <= alpha, "", " not"), " rejected.", sep = ""))
# |---Critical value approach: ====
print("Null Hypothesis (H0): The proportions of flights that were on time are the same across all carriers.")
print("Alternative Hypothesis (Ha): The proportions of flights that were on time are not the same across all carriers.")
chisq.alpha.rt <- qchisq(alpha, df = df, lower.tail = FALSE)
print(paste("chisq =", chisq, "and chisq.alpha.rt =", chisq.alpha.rt))
print(paste("Rejection region: [", chisq.alpha.rt, ", +inf)", sep = ""))
print(paste("chisq >= chisq.alpha.rt is ", chisq >= chisq.alpha.rt, ".", sep = ""))
print(paste("H0 is ", ifelse(chisq >= chisq.alpha.rt, "", "not "), "rejected.", sep = ""))

#3.2
# |---Marascuilo procedure: ====
alpha <- 0.05
# Loop through all possible combination
# without repetition
for(i1 in 1 : (dim(flights.table.f)[1] - 1)){
  for(i2 in (i1 + 1) : dim(flights.table.f)[1]){
    carrier.1 <- rownames(flights.table.f)[i1]
    carrier.2 <- rownames(flights.table.f)[i2]
    flights.table.f.1 <- flights.table.f[i1,]
    flights.table.f.2 <- flights.table.f[i2,]
    n.1 <- sum(flights.table.f.1)
    n.2 <- sum(flights.table.f.2)
    p.1.bar <- flights.table.f.1[c("ontime")] / n.1
    p.2.bar <- flights.table.f.2[c("ontime")] / n.2
    print(paste("Null hypothesis (H0): The proportions of flights that were on time are the same between ",
                carrier.1, " and ", carrier.2, ".", sep = ""))
    print(paste("Alternative hypothesis (Ha): The proportions of flights that were on time are not the same between ",
                carrier.1, " and ", carrier.2, ".", sep = ""))
    test.statistic <- abs(p.1.bar - p.2.bar)
    chisq.alpha.rt <- qchisq(alpha, df = df, lower.tail = FALSE)
    critical.value <- sqrt(chisq.alpha.rt) * sqrt(p.1.bar * (1 - p.1.bar) / n.1 + p.2.bar * (1 - p.2.bar) / n.2)
    print(paste("Test statistic = ", test.statistic, ", critical value = ", critical.value, ".", sep = ""))
    print(paste("Rejection region: [", critical.value, ", +inf)", sep = ""))
    print(paste("Test statistic >= critical value is ", test.statistic >= critical.value, ".", sep = ""))
    print(paste("H0 is ", ifelse(test.statistic >= critical.value, "", "not "), "rejected.", sep = ""))
    print("========================================")
  }
}

