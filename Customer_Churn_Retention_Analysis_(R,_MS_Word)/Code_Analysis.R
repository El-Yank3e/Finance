# Remove comments to install if needed. Comment once installed
#install.packages("arules")
#install.packages("caret")

# ------ TASK 1: Data Exploration ------ #
churn.df <- read.csv("Telco-Customer-Churn.csv")

View(churn.df)
nrow(churn.df)

#Task 1 visualize individual variables and all variables with variable churn
#remove incomplete cases
Churn <- churn.df[complete.cases(churn.df),]
nrow(Churn)
par(mar = c(1, 1, 1, 1))
#VGender
ggplot(data=Churn, aes(x=gender)) +geom_bar(stat="count")

#Interaction
a1 <- table(Churn$gender, Churn$Churn)
barplot(a1,legend.text = TRUE,main = "Count of Churn by Gender",xlab = "Churn",ylab = "Count")

#VSeniorCitizen
b <- ggplot(data=Churn, aes(x=SeniorCitizen)) +geom_bar(stat="count")
b
#Interaction
b1 <- table(Churn$SeniorCitizen, Churn$Churn)
barplot(b1,legend.text = TRUE,main = "Count of Churn by SeniorCitizen",xlab = "Churn",ylab = "Count")

#VPartner
c <- ggplot(data=Churn, aes(x=Partner)) +geom_bar(stat="count")
c
#Interaction
c1 <- table(Churn$Partner, Churn$Churn)
barplot(c1,legend.text = TRUE,main = "Count of Churn by Partner",xlab = "Churn",ylab = "Count")

#VDependents
d <- ggplot(data=Churn, aes(x=Dependents)) +geom_bar(stat="count")
d
#Interaction
d1 <- table(Churn$Dependents, Churn$Churn)
barplot(d1,legend.text = TRUE, main = "Count of Churn by Dependents", xlab = "Churn", ylab = "Count")

#VTenure
hist(Churn$tenure, xlab = "Tenure in Years")
#Interaction
ggplot(Churn, aes(x=Churn, y=tenure)) + geom_boxplot()

#VPhoneService
e <- ggplot(data=Churn, aes(x=PhoneService)) +geom_bar(stat="count")
e
#Interaction
e1 <- table(Churn$PhoneService, Churn$Churn)
barplot(e1,legend.text = TRUE,main = "Count of Churn by PhoneService",xlab = "Churn",ylab = "Count")

#VMultipleLines
f <- ggplot(data=Churn, aes(x=MultipleLines)) +geom_bar(stat="count")
f
#Interaction
f1 <- table(Churn$MultipleLines, Churn$Churn)
barplot(f1,legend.text = TRUE,main = "Count of Churn by MultipleLines",xlab = "Churn",ylab = "Count")

#VInternetService
g <- ggplot(data=Churn, aes(x=InternetService)) +geom_bar(stat="count")
g
#Interaction
g1 <- table(Churn$InternetService, Churn$Churn)
barplot(g1,legend.text = TRUE,main = "Count of Churn by InternetService",xlab = "Churn",ylab = "Count")

#VOnlineSecurity
h <- ggplot(data=Churn, aes(x=OnlineSecurity)) +geom_bar(stat="count")
h
#Interaction
h1 <- table(Churn$OnlineSecurity, Churn$Churn)
barplot(h1,legend.text = TRUE,main = "Count of Churn by OnlineSecurity",xlab = "Churn",ylab = "Count")

#VOnlineBackup
i <- ggplot(data=Churn, aes(x=OnlineBackup)) +geom_bar(stat="count")
i
#Interaction
i1 <- table(Churn$OnlineBackup, Churn$Churn)
barplot(i1,legend.text = TRUE,main = "Count of Churn by OnlineBackup",xlab = "Churn",ylab = "Count")

#VDeviceProtection
j <- ggplot(data=Churn, aes(x=DeviceProtection)) +geom_bar(stat="count")
j
#Interaction
j1 <- table(Churn$DeviceProtection, Churn$Churn)
barplot(j1,legend.text = TRUE,main = "Count of Churn by DeviceProtection",xlab = "Churn",ylab = "Count")

#VTechSupport
k <- ggplot(data=Churn, aes(x=TechSupport)) +geom_bar(stat="count")
k
#Interaction
k1 <- table(Churn$TechSupport, Churn$Churn)
barplot(k1,legend.text = TRUE,main = "Count of Churn by TechSupport",xlab = "Churn",ylab = "Count")

#VStreamingTV
l <- ggplot(data=Churn, aes(x=StreamingTV)) +geom_bar(stat="count")
l
#Interaction
l1 <- table(Churn$StreamingTV, Churn$Churn)
barplot(l1,legend.text = TRUE,main = "Count of Churn by StreamingTV",xlab = "Churn",ylab = "Count")

#VStreamingMovies
m <- ggplot(data=Churn, aes(x=StreamingMovies)) +geom_bar(stat="count")
m
#Interaction
m1 <- table(Churn$StreamingMovies, Churn$Churn)
barplot(m1,legend.text = TRUE,main = "Count of Churn by StreamingMovies",xlab = "Churn",ylab = "Count")

#VContract
n <- ggplot(data=Churn, aes(x=Contract)) +geom_bar(stat="count")
n
#Interaction
n1 <- table(Churn$Contract, Churn$Churn)
barplot(n1,legend.text = TRUE,main = "Count of Churn by Contract",xlab = "Churn",ylab = "Count")

#VPaperlessBilling
o <- ggplot(data=Churn, aes(x=PaperlessBilling)) +geom_bar(stat="count")
o
#Interaction
o1 <- table(Churn$PaperlessBilling, Churn$Churn)
barplot(o1,legend.text = TRUE,main = "Count of Churn by PaperlessBilling",xlab = "Churn",ylab = "Count")

#VPaymentMethod
p <- ggplot(data=Churn, aes(x=PaymentMethod)) +geom_bar(stat="count")
p
#Interaction
p1 <- table(Churn$PaymentMethod, Churn$Churn)
barplot(p1,legend.text = TRUE,main = "Count of Churn by PaymentMethod",xlab = "Churn",ylab = "Count")

#VMonthlyCharges
hist(Churn$MonthlyCharges, xlab = "MonthlyCharges")
#Interaction
ggplot(Churn, aes(x=Churn, y=MonthlyCharges)) + geom_boxplot()

#VTotalCharges
hist(Churn$TotalCharges, xlab = "TotalCharges")
#Interaction
ggplot(Churn, aes(x=Churn, y=TotalCharges)) + geom_boxplot()

#VChurn
q <- ggplot(data=Churn, aes(x=Churn)) +geom_bar(stat="count")
q

#Summary Statistics
summary(churn.df)

#Correlation of Numeric columns
library(psych)
par(mar = c(1, 1, 1, 1))
corPlot(churn.df[c('SeniorCitizen','tenure','MonthlyCharges', 'TotalCharges')], cex = 1.2)

# ------ TASK 2: Data Pre-processing: ------
# checking for nulls 
data <- na.omit(churn.df)
head(data)
library(dplyr)

#Drop customerID
data <- subset(data, select = -c(customerID))
head(data)

#Show number of unique data points for each column. (Distinct Count)
sapply(data, function(x) n_distinct(x))

class(data$TotalCharges) #data already read as numeric; no need to change from string

# Checking for Outliers
for(col in data){
  print(class(col))
} 
#identifies 2nd, 5th, 18th + 19th columns as those containing numeric values
for(col in colnames(data)){
  print(col)
} 
#identified columns: "SeniorCitizen", "tenure", "MonthlyCharges", "TotalCharges"
numeric_cols <-c("SeniorCitizen", "tenure", "MonthlyCharges", "TotalCharges")
test.df <- data[,numeric_cols]
i=1
for(col in test.df){
  upper_range <- mean(col)+ 3*sd(col)
  lower_range <- mean(col)- 3*sd(col)
  print(c(numeric_cols[i],"Lower Limit:", lower_range, "Upper Limit:", upper_range))
  if(sapply(test.df[i], min)< lower_range | sapply(test.df[i], max) > upper_range){
    print("Outlier Detected.")
  }
  else{
    print(c("No Outliers found."))
  }
  i <- i + 1
  print('\  ')
}
head(test.df$tenure)

# Computing New Variables: by discretizing existing variables 
# and encoding them into binary input for our later ML models
library(arules)

# discretization tenure to 3 section 
data['tenure']=discretize(data$tenure, method = "interval", breaks=3, labels = c("low", "medium","large"))
data$low_tenure <- ifelse(data$tenure == "low", 1,0)
data$medium_tenure <- ifelse(data$tenure == "medium", 1,0)
data$large_tenure <- ifelse(data$tenure == "large", 1,0)

# discretization MonthlyCharges to 4 section 
data['MonthlyCharges']=discretize(data$MonthlyCharges, method = "interval", breaks=4, labels = c("low", "medium","large", "very high"))
data$low_MonthlyCharges <- ifelse(data$MonthlyCharges == "low", 1,0)
data$medium_MonthlyCharges <- ifelse(data$MonthlyCharges == "medium", 1,0)
data$large_MonthlyCharges <- ifelse(data$MonthlyCharges == "large", 1,0)
data$very_high_MonthlyCharges <- ifelse(data$MonthlyCharges == "very high", 1,0)

# discretization TotalCharges to 4 section 
data['TotalCharges']=discretize(data$TotalCharges, method = "interval", breaks=4, labels = c("low", "medium","large", "very high"))
data$low_TotalCharges <- ifelse(data$TotalCharges == "low", 1,0)
data$medium_TotalCharges <- ifelse(data$TotalCharges == "medium", 1,0)
data$large_TotalCharges <- ifelse(data$TotalCharges == "large", 1,0)
data$very_high_TotalCharges <- ifelse(data$TotalCharges == "very high", 1,0)

#Creating Remaining Dummy variables for "gender", "Partner" "Dependents" "PhoneService" "MultipleLines"
data$female_gender <- ifelse(data$gender == "Female", 1,0)
data$male_gender <- ifelse(data$gender == "Male", 1,0)
data$no_partner <- ifelse(data$Partner == "No", 1,0)
data$yes_partner <- ifelse(data$Partner == "Yes", 1,0)
data$no_dependents <- ifelse(data$Dependents == "No", 1,0)
data$yes_dependents <- ifelse(data$Dependents == "Yes", 1,0)
data$no_phone <- ifelse(data$PhoneService == "No", 1,0)
data$yes_phone <- ifelse(data$PhoneService == "Yes", 1,0)
data$no_multiple_lines <- ifelse(data$MultipleLines == "No" | data$MultipleLines == "No phone service" , 1,0)
data$yes_multiple_lines <- ifelse(data$MultipleLines == "Yes", 1,0)

#Creating Remaining Dummy variables for "InternetService" "OnlineSecurity" "OnlineBackup" "DeviceProtection"
data$no_internet <- ifelse(data$InternetService == "No", 1,0)
data$fiber_internet <- ifelse(data$InternetService == "Fiber optic", 1,0)
data$dsl_internet <- ifelse(data$InternetService == "DSL", 1,0)
data$no_online_security <- ifelse(data$OnlineSecurity == "No" | data$OnlineSecurity == "No internet service" , 1,0)
data$yes_online_security <- ifelse(data$OnlineSecurity == "Yes", 1,0)
data$no_online_backup <- ifelse(data$OnlineBackup == "No" | data$OnlineBackup == "No internet service" , 1,0)
data$yes_online_backup <- ifelse(data$OnlineBackup == "Yes", 1,0)
data$no_device_protection <- ifelse(data$DeviceProtection == "No" | data$DeviceProtection == "No internet service" , 1,0)
data$yes_device_protection <- ifelse(data$DeviceProtection == "Yes", 1,0)

#Creating Remaining Dummy variables for "TechSupport" "StreamingTV" "StreamingMovies" "Contract" 
data$no_tech_support <- ifelse(data$TechSupport == "No" | data$TechSupport == "No internet service" , 1,0)
data$yes_tech_support <- ifelse(data$TechSupport == "Yes", 1,0)
data$no_tv_streaming <- ifelse(data$StreamingTV == "No" | data$StreamingTV == "No internet service" , 1,0)
data$yes_tv_streaming <- ifelse(data$StreamingTV == "Yes", 1,0)
data$no_movie_streaming <- ifelse(data$StreamingMovies == "No" | data$StreamingMovies == "No internet service" , 1,0)
data$yes_movie_streaming <- ifelse(data$StreamingMovies == "Yes", 1,0)
data$month_to_month_contract <- ifelse(data$Contract == "Month-to-month", 1,0)
data$one_year_contract <- ifelse(data$Contract == "One year", 1,0)
data$two_year_contract <- ifelse(data$Contract == "Two year", 1,0)

#Creating Remaining Dummy variables for "PaperlessBilling" "PaymentMethod" "Churn"
data$no_paper_billing <- ifelse(data$PaperlessBilling == "No", 1,0)
data$yes_paper_billing <- ifelse(data$PaperlessBilling == "Yes", 1,0)
data$payment_echeck <- ifelse(data$PaymentMethod == "Electronic check", 1,0)
data$payment_check <- ifelse(data$PaymentMethod == "Mailed check", 1,0)
data$payment_bank_transfer <- ifelse(data$PaymentMethod == "Bank transfer (automatic)", 1,0)
data$payment_credit_card <- ifelse(data$PaymentMethod == "Credit card (automatic)", 1,0)

# Trim Data of remaining unused variables
dim(data)
data$y_churn <- ifelse(data$Churn == "Yes", 1,0)
data <- subset(data, select = -c(tenure, MonthlyCharges, TotalCharges, gender, Partner, Dependents, PhoneService, MultipleLines, InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod, Churn))

head(data,6)

# ------ TASK 3: Data and Dimension Reduction ------ #

# Splitting our (x) dependent & (y) independent variables
x <- subset(data, select = -c(y_churn))
y <- data$y_churn

# Prepare the data for PCA
data.std <- as.data.frame(scale(x))
apply(data.std, 2, var)
apply(data.std, 2, mean)

pca <- prcomp(data.std, scale=T)
print(pca)
print(pca$x)
summary(pca)

#Dimensionality Reduction to 99% 27/46 components
reduced_pca <- as.data.frame(pca$x)
reduced_pca <- reduced_pca[,1:27]

# Re-attach our classifier variable to the newly arranged PCA data
reduced_pca$churn <- y
names(reduced_pca)

# Scatter plot of first two components
library(ggplot2)
ggplot(reduced_pca) + geom_point(aes(x=reduced_pca[,1], y=reduced_pca[,2], color=churn)) 

# Re-naming for readability
FINAL.DATA <- as.data.frame(reduced_pca)
head(FINAL.DATA)

# Create Index Matrix
library(caret)
set.seed(1985)
index <- createDataPartition(FINAL.DATA$churn, p =.9, list = FALSE, times = 1)

#Split final data into train and test
train_df <- FINAL.DATA[index,]
test_df <- FINAL.DATA[-index,]

#Label outcome values to identify customer churn as 1 = lost and 0 = retained
train_df$churn[train_df$churn==1] <- "lost"
train_df$churn[train_df$churn==0] <- "retained"

test_df$churn[test_df$churn==1] <- "lost"
test_df$churn[test_df$churn==0] <- "retained"

# Convert to factor
train_df$churn <- as.factor(train_df$churn)
test_df$churn <- as.factor(test_df$churn)

class(train_df$churn)
class(test_df$churn)

# ------ TASK 4: Partition the Data ------ #

# K-Fold into 10 splits
ctrl <- trainControl(method = "cv", number = 10, 
                     savePredictions = "all", 
                     classProbs = TRUE)

# ------ TASK 5: Model Building ------ #

#seed to make this example reproducible
set.seed(360)

# Build / Train Logistic Regression Model
names(train_df)
model <- train(churn ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27, 
             data = train_df,
             method = "glm",
             family = "binomial",
             trControl=ctrl)

# Build / Train Gaussian Naive Bayes Model
library(e1071)
model2 <- naiveBayes(churn ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27, 
               data = train_df,
               method = "nb",
               family = "gaussian",
               trControl=ctrl)

# ------ TASK 6: Model Evaluation ------ #

# A Quick Look at the Logistic Regression Model
print(model)
summary(model)

#Significant Variables
varImp(model)

# By comparison, test_df
pred <- predict(model, newdata=test_df)
confusionMatrix(data=pred, test_df$churn)

# A Quick Look at the Gaussian Naive Bayes Model
print(model2)
summary(model2)

# By comparison, test_df
pred2 <- predict(model2, newdata=test_df)
confusionMatrix(data=pred2, test_df$churn)

# ROC Chart for LogReg / AUC curve for NB

# Lift Chart?

# ------ TASK 7: Model Deployment ------ #

names(data)
head(data)
data$y_churn <- as.factor(data$y_churn)

final_model <- train(y_churn ~ SeniorCitizen+low_tenure+medium_tenure+large_tenure+low_MonthlyCharges+
                       medium_MonthlyCharges+large_MonthlyCharges+very_high_MonthlyCharges+
                       low_TotalCharges+medium_TotalCharges+large_TotalCharges+very_high_TotalCharges+
                       female_gender+male_gender+no_partner+yes_partner+no_dependents+yes_dependents+
                       no_phone+yes_phone+no_multiple_lines+yes_multiple_lines+no_internet+fiber_internet+
                       dsl_internet+no_online_security+yes_online_security+no_online_backup+yes_online_backup+
                       no_device_protection+yes_device_protection+no_tech_support+yes_tech_support+
                       no_tv_streaming+yes_tv_streaming+no_movie_streaming+yes_movie_streaming+
                       month_to_month_contract+one_year_contract+two_year_contract+
                       no_paper_billing+yes_paper_billing+payment_echeck+payment_check+payment_bank_transfer+
                       payment_credit_card,
               data = data,
               method = "glm",
               family = "binomial",
               trControl=ctrl)

print(final_model)
summary(final_model)