######
# Individual Report 1 - Logistic Regression
# by Lucas Hagelstein
######
# Set-up
setwd("C:/Users/lucas_0ey5q9s/Dropbox/University/Erasmus University Rotterdam/Block 2/Seminar Data Science and Marketing Analytics/Individual Assignment 1 - Logistic Regression")
load("TransportMode.RData")

library(car)
library(corrplot)
library(MASS)   
library(caret)

######
# Rename Binary Variable
colnames(TransportMode)[1] <- ("by_car")
# Inspect data
summary(TransportMode)
# Check correlation of variables
corrplot(cor(TransportMode[2:8]), method="number")
#> in-vehicle time for train and in-vehicle time in car are very high correlated (r=0.86)

### Create new variables
## Income per person
TransportMode$income_pp <- TransportMode$hinc / TransportMode$psize
## Combination of highly correlated variables
TransportMode$time_diff_tc <- TransportMode$invt_t - TransportMode$invt_c

### Fit a model with the whole data 
m1 <- glm(by_car ~. , data=TransportMode[,c(-7,-8,-10)],family=binomial)

### Diagonstics
## Multicollinearity
vif(m1)
#> VIF of invt_c =8.8 and invt_t = 11.68 might be critical!

## Variable time_diff_tc might solve the problem! 
# Value is positive (negative) if the car (train) is faster!
m1a <- glm(by_car ~. , data=TransportMode[,c(-4,-6,-7,-8)],family=binomial)
summary(m1a)

vif(m1a)
#> VIF < 2 for all variables!
#> Problem solved, continue with m1a

## Autocorrelation
plot(m1a$residuals, main="Correlation of error term", ylab=("Residuals"))        # Plot residuals
abline(a=0, b=0, col="red")
## No funneling detected
#> No Autocorrelation

## Influential values
par(mfrow=c(2,2))
plot(m1a)
# Cooks distance
plot(m1a, which = 4)
#> observation 57, 73, 91 may influence the regression

## Investigate the observations and compare them to mean values without the outliers
subset <- TransportMode[c(57,73,91),c(1,2,3,5,9,10)]
subset
summary(TransportMode[c(-57,-73,-91),c(1,2,3,5,9,10)])

## After comparing mean values without outliers to the outliers we see observation 91 as irrational customer:
#> obs. 57: The car is 522 minutes slower, but very cheap compared to the train travel costs! CHOICE: CAR -> no outlier
#> obs. 73: Same pattern as in obs. 57! CHOICE: CAR -> no outlier
#> obs. 91: Waiting time of 99 minutes is very high! Also the car is cheaper than the train but 36 minutes slower.
#>          This observation is very irrational but is kept due to the very small sample! (Selection-bias)
#>          --> Also might be driven by a very "green-attitude" or no access to a car!
#> All suspected outliers are kept!

### Randomly split the data into 75/25 (train/test)
set.seed(777)
sample_size = floor(0.75*nrow(TransportMode))
train = sample(seq_len(nrow(TransportMode)),size = sample_size)
train_data = TransportMode[train,]
test_data =TransportMode[-train,]

### Fit models with the training data and evaluate on test data

## Fit Final_Model with training data
#> This model contains a combination of invt_t and invt_c (=time_diff_tc)
#> Corrects for high VIF of both variables!
m_final <- glm(by_car ~. , data=train_data[,c(-4,-6,-7,-8)],family=binomial)
summary(m_final)

#predict the test data with m2
predicted <- predict(m_final, test_data,type="response")
test_data$predicted <- predicted

#Create a confusion matrix with the caret-package
confusionMatrix(data = as.factor(as.numeric(predicted>0.5)), reference = test_data$by_car)

## show variables that are wrong predicted
test_data$predicted <- as.factor(as.numeric(predicted>0.5))
test_data[test_data$by_car != test_data$predicted,][,c(1,2,3,5,9,10)]

## Fit alternative model 
# The alternative model includes both highly correlated variables seperately
m2 <- glm(by_car ~. , data=train_data[,c(-7,-8,-10)],family=binomial)
summary(m2)

# predict the test data with the alternative model
predicted <- predict(m2, test_data,type="response")
test_data$predicted <- predicted

# Create a confusion matrix with the caret-package
confusionMatrix(data = as.factor(as.numeric(predicted>0.5)), reference = test_data$by_car)

# show variables that are wrong predicted
test_data$predicted <- as.factor(as.numeric(predicted>0.5))
test_data[test_data$by_car != test_data$predicted,][,c(1,2,3,4,5,6,9)]

### Bootstrap 1000 combinations of train-test splits to measure average accuracy
##> Try to overcome the reliance on the split by train and test each combination
##> and measuring the accuracy and beta coefficients during this process!
boot_acc1 <- matrix(,0,4)
boot_coef1 <- matrix(,0,7)
boot_acc2 <- matrix(,0,4)
boot_coef2 <- matrix(,0,6)
for (i in 1:1000){
  set.seed(i)
  sample_size = floor(0.75*nrow(TransportMode))
  # randomly split data
  train = sample(seq_len(nrow(TransportMode)),size = sample_size)
  train_sample <- TransportMode[train,]
  test_sample <- TransportMode[-train,]
  boot_model1 <- glm(by_car ~. , data=train_sample[,c(-7,-8,-10)],family=binomial)
  boot_model2 <- glm(by_car ~. , data=train_sample[,c(-4,-6,-7,-8)],family=binomial)
  # predict test_sample and create confusion matrix 
  boot_pred1 <- predict(boot_model1, test_sample,type="response")
  boot_pred2 <- predict(boot_model2, test_sample,type="response")
  conf1 <- confusionMatrix(data = as.factor(as.numeric(boot_pred1>0.5)), reference = test_sample$by_car)
  conf2 <- confusionMatrix(data = as.factor(as.numeric(boot_pred2>0.5)), reference = test_sample$by_car)
  # store accuracy measures and beta coefficients
  boot_acc1 <- rbind(boot_acc1, conf1$overall[1:4])
  boot_acc2 <- rbind(boot_acc2, conf2$overall[1:4])
  boot_coef1 <- rbind(boot_coef1, boot_model1$coefficients)
  boot_coef2 <- rbind(boot_coef2, boot_model2$coefficients)
}

## Create 95% CI to compare the model coefficients and accuracy measures
boot_acc_CI1 <- apply(boot_acc1, 2, quantile, c(0.025,0.5, 0.975))
boot_coef_CI1 <- apply(boot_coef1, 2, quantile, c(0.025,0.5, 0.975))
round(boot_acc_CI1,3)
round(boot_coef_CI1,3)
boot_acc_CI2 <- apply(boot_acc2, 2, quantile, c(0.025,0.5, 0.975))
boot_coef_CI2 <- apply(boot_coef2, 2, quantile, c(0.025,0.5, 0.975))
round(boot_acc_CI2,3)
round(boot_coef_CI2,3)

## Interpretation
#> Both models perform equal in terms of accuracy 
#> Final Model has smaller 95% CI for the beta coefficients
#> Combination of highly correlated variables might improve the model 

## Create boxplots of the bootstrapped coefficients
#> Check if the actual coefficients of the analysis are over-/ underestimated
par(mfrow=c(2,1),mar=c(2,10,0.5,0.5))
boot_coef1 <- as.data.frame(boot_coef1)
colnames(boot_coef1) <- c("Intercept", "Waiting Time (Train)", 
                          "Travel Costs (Train)", "Travel Time (Train)", 
                          "Travel Costs (Car)", "Travel Time (Car)", 
                          "Income (per capita)")
boxplot(boot_coef1[,c(4,6,3,5,2,7)], horizontal = TRUE, las=1, 
        ylim =c(-0.3,0.3), 
        main="", col = "lightblue")
points(y = 1:6, x = m1$coefficients[c(4,6,3,5,2,7)],col="red", pch=20, cex = 1.2)
rect(-1, 0, 1, 2.5, col="darkgrey", density = 5)

boot_coef2 <- as.data.frame(boot_coef2)
colnames(boot_coef2) <- c("Intercept", "Waiting Time (Train)", 
                          "Travel Costs (Train)", "Travel Costs (Car)", 
                          "Income (per capita)", "Travel Time (Train - Car)")
boxplot(boot_coef2[,c(6, 3, 4, 2, 5)], horizontal = TRUE, las=1, 
        ylim =c(-0.3,0.3), xlab="Coefficient Estimate", 
        main="", col = "lightblue")
points(y = 1:5, x = m2$coefficients[c(6, 3, 4, 2, 5)],col="red", pch=20, cex = 1.2)
rect(-1, 0, 1, 1.5, col="darkgrey", density = 5)