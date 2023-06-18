# Assignment 10

# Load into R
UsedCars.df <- read.csv("Data-UsedCars.csv")
# install.packages("car")
library(car)
# Penalize R for using scientific way to display number
options(scipen = 999)

# 1
class(UsedCars.df$Fuel_Type)
levels(UsedCars.df$Fuel_Type)
levels.order <- c("Petrol", "Diesel","CNG")
UsedCars.df$Fuel_Type <- factor(UsedCars.df$Fuel_Type, levels.order)
levels(UsedCars.df$Fuel_Type)
UsedCars.lm <- lm(Price ~ Age_08_04 + Met_Color + Weight + HP + KM + Quarterly_Tax + Fuel_Type,
                 data = UsedCars.df)
UsedCars.lm.summary <- summary(UsedCars.lm)

# 2-6
print(UsedCars.lm.summary)

# 7
vif(UsedCars.lm)

# 8
UsedCars.dv.est <- UsedCars.lm$fitted.values
UsedCars.res.std <- rstudent(UsedCars.lm)
plot(UsedCars.res.std ~ UsedCars.dv.est, pch = 19, xlab = "Fitted Value of DV",
     ylab = "Standardized Residual", main = "Residual Analysis",
     ylim = c(min(-3, min(UsedCars.res.std)), max(3, max(UsedCars.res.std))))
abline(h = -1.5, lty = 3)
abline(h = 1.5, lty = 3)
