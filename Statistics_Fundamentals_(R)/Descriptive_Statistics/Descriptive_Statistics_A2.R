# Assignment 2
# Load into Rstudio
Cars_df <- read.csv("Data-MTCars.csv")

# 1 #
# Frequency
cyl.freq <- data.frame(table(Cars_df$cyl))
names(cyl.freq)[1] <- "Cylinders"
# Relative Frequency
cyl.freq$Rel.Freq <- cyl.freq$Freq / sum(cyl.freq$Freq)
# Percent Frequency
cyl.freq$Pct.Freq <- 100.00 * cyl.freq$Rel.Freq
cyl.freq

# 2 #
# Bar Chart
barplot(cyl.freq$Freq, names.arg = cyl.freq$Cylinder,
        xlab = "Cylinders", ylab = "Frequency", main = "Bar Chart of Cylinders")
# Pie Chart
pie(cyl.freq$Freq, main = "Pie Chart of Cylinders",
    labels = paste(cyl.freq$Soft.Drink, " (", cyl.freq$Pct.Freq, "%)", sep = ""))

# 3 #
mpg.bin.num <- nclass.Sturges(Cars_df$mpg)
Cars_df$mpg.Bin <- cut(Cars_df$mpg, mpg.bin.num)
mpg.freq <- data.frame(table(Cars_df$mpg.Bin))
names(mpg.freq)[1] <- "mpg.Bin"
mpg.freq
# Relative and percent frequency distribution 
mpg.freq$Rel.Freq <- mpg.freq$Freq / sum(mpg.freq$Freq)
mpg.freq$Pct.Freq <- 100.00 * mpg.freq$Rel.Freq
mpg.freq
# Cumulative (relative and percent) frequency distribution 
mpg.freq$Cum.Freq <- cumsum(mpg.freq$Freq)
mpg.freq$Cum.Rel.Freq <- cumsum(mpg.freq$Rel.Freq)
mpg.freq$Cum.Pct.Freq <- cumsum(mpg.freq$Pct.Freq)
mpg.freq

# 4 #
hist(Cars_df$mpg, breaks = "sturges", xlab = "MPG",
     main = "Histogram of MPG")

# 5 #
# Crosstabulation
mpg.bin.num <- nclass.Sturges(Cars_df$mpg)
Cars_df$mpg.Bin <- cut(Cars_df$mpg, mpg.bin.num)
cars.freq <- table(Cars_df$mpg.Bin, Cars_df$hp)
cars.col.pct.freq <- apply(cars.freq, 2, function(x){100.00 * x / sum(x)})
cars.col.pct.freq
cars.row.pct.freq <- apply(cars.freq, 1, function(x){100.00 * x / sum(x)})
cars.row.pct.freq
cars.col.cum.pct.freq <- apply(cars.freq, 2, function(x){100.00 * cumsum(x) / sum(x)})
cars.col.cum.pct.freq
cars.row.cum.pct.freq <- apply(cars.freq, 1, function(x){100.00 * cumsum(x) / sum(x)})
cars.row.cum.pct.freq

# 6 #
# Scatter chart and trend line
plot(Cars_df$hp ~ Cars_df$mpg, pch = 19,
     xlab = "MPG", ylab = "HP",
     main = "HP vs. MPG")
# Use simple linear regression to generate trend line
abline(lm(Cars_df$hp ~ Cars_df$mpg))

# 7 #
# Mean, Median, Range, Variance, SD, Max, Min
print(paste("Mean (Average):", mean(Cars_df$hp)))
print(paste("Median:", median(Cars_df$hp)))
print(paste("Range:", max(Cars_df$hp) - min(Cars_df$hp)))
print(paste("Variance:", var(Cars_df$hp)))
print(paste("Standard deviation:", sd(Cars_df$hp)))
print(paste("Smallest value:", min(Cars_df$hp)))
print(paste("Largest value:", max(Cars_df$hp)))

# 8 #
# covariance and correlation coefficient
print(paste("Covariance:", cov(Cars_df$mpg, Cars_df$hp)))
print(paste("Correlation coefficient:", cor(Cars_df$mpg, Cars_df$hp)))