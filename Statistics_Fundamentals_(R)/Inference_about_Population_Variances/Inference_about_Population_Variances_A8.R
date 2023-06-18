# Assignment #8

#1
n <- 18
s <- sqrt(0.36)
alpha <- 1 - 0.90
df <- n - 1
chisq.lt <- qchisq(alpha / 2, df = df, lower.tail = TRUE)
chisq.rt <- qchisq(alpha / 2, df = df, lower.tail = FALSE)
lb <- (n - 1) * s ^ 2 / chisq.rt
ub <- (n - 1) * s ^ 2 / chisq.lt
print(paste("Interval estimate: [", lb, ", ", ub, "]", sep = ""))

#2
n <- 18
df <- n - 1
var.zero <- .2
s <- sqrt(.36)
alpha <- 0.05
# |---p-value approach: ====
print("Null hypothesis (H0): The difference (both positive and negative) in variance of drug weight is no more than 0.2.")
print("Alternative hypothesis (Ha): The difference (both positive and negative) in variance of drug weight is more than 0.2.")
chisq <- (n - 1) * s ^ 2 / var.zero
p <- 2 * min(pchisq(chisq, df = df, lower.tail = TRUE),
             pchisq(chisq, df = df, lower.tail = FALSE))
print(paste("p =", p, "and alpha =", alpha))
print(paste("p <= alpha is ", p <= alpha, ".", sep = ""))
print(paste("H0 is", ifelse(p <= alpha, "", " not"), " rejected.", sep = ""))
# |---Critical value approach: ====
print("Null hypothesis (H0): The difference (both positive and negative) in variance of drug weight is no more than 0.2.")
print("Alternative hypothesis (Ha): The difference (both positive and negative) in variance of drug weight is more than 0.2.")
chisq <- (n - 1) * s ^ 2 / var.zero
chisq.alpha.lt <- qchisq(alpha / 2, df = df, lower.tail = TRUE)
chisq.alpha.rt <- qchisq(alpha / 2, df = df, lower.tail = FALSE)
print(paste("chisq =", chisq, "and chisq.alpha.lt =", chisq.alpha.lt, "and chisq.alpha.rt =", chisq.alpha.rt))
print(paste("Rejection region: (0, ", chisq.alpha.lt, "] and [", chisq.alpha.rt, ", +inf)", sep = ""))
print(paste("chisq <= chisq.alpha.lt is ", chisq <= chisq.alpha.lt, ", chisq >= chisq.alpha.rt is ", chisq >= chisq.alpha.rt, sep = ""))
print(paste("H0 is ", ifelse(chisq <= chisq.alpha.lt | chisq >= chisq.alpha.rt, "", "not "), "rejected.", sep = ""))

#3
# Note that the hypotheses are already framed
# such that the test is an upper one-tailed test
n1 <- 16
n2 <- 21
df1 <- n1 - 1
df2 <- n2 - 1
s1 <- sqrt(5.8)
s2 <- sqrt(2.4)
alpha <- 0.05
# |---p-value approach: ====
print("Null hypothesis (H0): Population 1's variance is not significantly greater than population 2's.")
print("Alternative hypothesis (Ha): Population 1's variance is significantly greater than population 2's.")
f <- s1 ^ 2 / s2 ^ 2
p <- pf(f, df1 = df1, df2 = df2, lower.tail = FALSE)
print(paste("p =", p, "and alpha =", alpha))
print(paste("p <= alpha is ", p <= alpha, ".", sep = ""))
print(paste("H0 is", ifelse(p <= alpha, "", " not"), " rejected.", sep = ""))
# |---Critical value approach: ====
print("Null hypothesis (H0): Population 1's variance is not significantly greater than population 2's.")
print("Alternative hypothesis (Ha): Population 1's variance is significantly greater than population 2's.")
f <- s1 ^ 2 / s2 ^ 2
f.alpha.rt <- qf(alpha, df1 = df1, df2 = df2, lower.tail = FALSE)
print(paste("f =", f, "and f.alpha.rt =", f.alpha.rt))
print(paste("Rejection region: [", f.alpha.rt, ", +inf)", sep = ""))
print(paste("f >= f.alpha.rt is ", f >= f.alpha.rt, sep = ""))
print(paste("H0 is ", ifelse(f >= f.alpha.rt, "", "not "), "rejected.", sep = ""))

