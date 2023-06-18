# Assignment 7

# 1
n1 <- 40
n2 <- 50
x1.bar <- 56100
x2.bar <- 59400
s1 <- 6000
s2 <- 7000
x1.bar.se <- s1 / sqrt(n1)
x2.bar.se <- s2 / sqrt(n2)
df <- (x1.bar.se ^ 2 + x2.bar.se ^ 2) ^ 2 /
  ((x1.bar.se ^ 4) / (n1 - 1) + (x2.bar.se ^ 4) / (n2 - 1))
# |---p-value approach: ====
print("Null hypothesis (H0): Nurses in Tampa get paid the same or more than nurses in Dallas.")
print("Alternative hypothesis (Ha): Nurses in Tampa get paid less than nurses in Dallas.")
t <- (x1.bar - x2.bar) / sqrt(x1.bar.se ^ 2 + x2.bar.se ^ 2)
alpha <- 0.05
p <- pt(t, df = df, lower.tail = FALSE)
print(paste("p =", p, "and alpha =", alpha))
print(paste("p <= alpha is ", p <= alpha, ".", sep = ""))
print(paste("H0 is", ifelse(p <= alpha, "", " not"), " rejected.", sep = ""))
# |---Critical value approach: ====
print("Null hypothesis (H0): Nurses in Tampa get paid the same or more than nurses in Dallas.")
print("Alternative hypothesis (Ha): Nurses in Tampa get paid less than nurses in Dallas.")
t <- (x1.bar - x2.bar) / sqrt(x1.bar.se ^ 2 + x2.bar.se ^ 2)
alpha <- 0.05
t.alpha <- qt(alpha, df = df, lower.tail = FALSE)
print(paste("t =", t, "and t.alpha =", t.alpha))
print(paste("Rejection region: [", t.alpha, ", +inf)", sep = ""))
print(paste("t >= t.alpha is ", t >= t.alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(t >= t.alpha, "", "not "), "rejected.", sep = ""))

# 2
n1 <- 900
n2 <- 1000
p1.bar <- 315 / n1
p2.bar <- 410 / n2
p.bar <- (n1 * p1.bar + n2 * p2.bar) / (n1 + n2)
p.bar.se <- sqrt(p.bar * (1 - p.bar) * (1 / n1 + 1 / n2))
# |---p-value approach: ====
print("Null hypothesis (H0): The proportion of financially fit adults from 2010 and 2012 are the same.")
print("Alternative hypothesis (Ha): The proportion of financially fit adults from 2010 and 2012 are different.")
z <- (p1.bar - p2.bar) / p.bar.se
alpha <- 0.05
p <- 2 * min(pnorm(z, mean = 0, sd = 1, lower.tail = TRUE),
             pnorm(z, mean = 0, sd = 1, lower.tail = FALSE))
print(paste("p =", p, "and alpha =", alpha))
print(paste("p <= alpha is ", p <= alpha, ".", sep = ""))
print(paste("H0 is", ifelse(p <= alpha, "", " not"), " rejected.", sep = ""))
# |---Critical value approach: ====
print("Null hypothesis (H0): The proportion of financially fit adults from 2010 and 2012 are the same.")
print("Alternative hypothesis (Ha): The proportion of financially fit adults from 2010 and 2012 are different.")
z <- (p1.bar - p2.bar) / p.bar.se
alpha <- 0.05
z.alpha <- qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE)
print(paste("z =", z, "and z.alpha =", z.alpha))
print(paste("Rejection region: (-inf, ", -z.alpha, "] and [", z.alpha, ", +inf)", sep = ""))
print(paste("z <= -z.alpha is ", z <= -z.alpha, ", z >= z.alpha is ", z >= z.alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(z <= -z.alpha | z >= z.alpha, "", "not "), "rejected.", sep = ""))

