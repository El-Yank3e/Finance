#1 Probability that the cost will be more than $450
print(paste("P(x > 450) =", pnorm(450, mean = 367, sd = 88, lower.tail = FALSE)))

#2 Probability that the cost will be less than $250
print(paste("P(x < 250) =", pnorm(250, mean = 367, sd = 88, lower.tail = TRUE)))

#3 Probability that the cost will be between $250 and $450
print(paste("P(250 < x < 450) =", pnorm(450, mean = 367, sd = 88)
            - pnorm(250, mean = 367, sd = 88)))

#4 Cost if car repair is in the lower 5% of automobile repair charges
print(paste("Given P(x <= x0) = 0.05, x0 =", qnorm(0.05, mean = 367, sd = 88, TRUE)))