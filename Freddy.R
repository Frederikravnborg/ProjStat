
setwd("/Users/beheard/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/4. Semester/02445_ProjectStatisticalEvaluation/Project")

#setwd("/Users/frederikravnborg/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/DTU-Frederik’s MacBook Pro/ProjStat/Project, Group")
(d <- read.table("horse_data23.txt", header=TRUE, as.is=TRUE))

#### Initial Plots ####



#### Goal 1 ####
(H <- as.factor(d$horse))
plot(H)
hist(d$A, breaks = 40)
hist(d$S, breaks = 40)
hist(d$W, breaks = 40)

par(mfrow = c(1,3))

# If horse has a significant effect on A
fit <- lm(d$A ~ H)
anova(fit)
resA = resid(fit)
qqnorm(resA, main = "Q-Q plot of residuals of A")
qqline(resA)

#plot(fit)

# If horse has a significant effect on S
fit <- lm(d$S ~ H); anova(fit)
#plot(fit)
resS = resid(fit)
qqnorm(resS, main = "Q-Q plot of residuals of S")
qqline(resS)

# If horse has a significant effect on W
fit <- lm(d$W ~ H); anova(fit)
resW = resid(fit)
qqnorm(resW,main = "Q-Q plot of residuals of W")
qqline(resW)


hist(resA, breaks = 40)
hist(resS, breaks = 40)
hist(resW, breaks = 40)


#Wallyplot 
library("MESS")


qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}

wallyplot(resW, FUN=qqwrap, ylim=c(-3,3))


#plot(fit)

#### Goal 2 ####
d2 <- d
for (i in 1:dim(d)[1]){
  if (d2$lameLeg[i] == "none"){d2$lameLeg[i] <- (0)}
  else {d2$lameLeg[i] <- (1)}
}

plot(d2$A, d2$W, col=as.factor(d2$lameLeg))
plot(d2$pc3, d2$pc4, col=as.factor(d2$lameLeg))
d2$lameLeg

# Binary classification (A/W)
fitAW <- lm(lameLeg ~ A + W, data=d2)
summary(fitAW)
plot(fitAW)

# Binary classification (PC3/PC4)
fitPC <- lm(lameLeg ~ pc3 + pc4, data=d2)
summary(fitPC)
plot(fitPC)


plot(subset(d2, lameLeg == 1)$A, pch=19, col="red", ylab="A", main="Symmetry score A")
points(subset(d2, lameLeg == 0)$A, pch=19, col="blue")
legend("topleft",legend=c("Some lameness", "No lameness"),
       col=c("red", "blue"), pch=19:19, cex=0.8)

plot(subset(d2, lameLeg == 1)$S, pch=19, col="red", ylab="S", main="Symmetry score S")
points(subset(d2, lameLeg == 0)$S, pch=19, col="blue")
legend("topleft",legend=c("Some lameness", "No lameness"),
       col=c("red", "blue"), pch=19:19, cex=0.8)

plot(subset(d2, lameLeg == 1)$W, pch=19, col="red", ylab="W", main="Symmetry score W")
points(subset(d2, lameLeg == 0)$W, pch=19, col="blue")
legend("topleft",legend=c("Some lameness", "No lameness"),
       col=c("red", "blue"), pch=19:19, cex=0.8)


length(subset(d2, lameLeg == 0)$A)
length(subset(d2, lameLeg == 1)$A)

?plot
plot(as.factor(subset(d2, lameLeg == 1)$horse))
plot(as.factor(subset(d2, lameLeg == 0)$horse))

subset(d2, horse == "B1")$lameLeg


plotd <- d
plotd$A <- round(plotd$A,2)
plotd$S <- round(plotd$S,2)
plotd$W <- round(plotd$W,2)
plotd$pc1 <- round(plotd$pc1,2)
plotd$pc2 <- round(plotd$pc2,2)
plotd$pc3 <- round(plotd$pc3,2)
plotd$pc4 <- round(plotd$pc4,2)

(nLame <- dim( (subset(d2, lameLeg == 1)) )[1])
(nNone <- dim( (subset(d2, lameLeg == 0)) )[1])
(ratio <- nLame / (nLame+nNone))



#### KNN ####
library(e1071)
library(caTools)
library(class)
library(caret)
d$lameLeg <- as.factor(d$lameLeg)

# Splitting data into train
# and test data
AW <- d[ , c("lameLeg", "A", "W")]  
split <- sample.split(AW, SplitRatio = 0.75)
train_cl <- subset(AW, split == "TRUE")
test_cl <- subset(AW, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 2:3])
test_scale <- scale(test_cl[, 2:3])

# Fitting KNN Model 
# to training dataset
classifier_knn <- function(kval){knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$lameLeg,
                      k = kval)}
classifier_knn(3)


# Confusion Matrix
(cm <- table(test_cl$lameLeg, classifier_knn(4)))

accK <- replicate(20, NA)
for (k in 1:20){
  accK[k] <- mean(classifier_knn(k) == test_cl$lameLeg)
  }
which.max(accK) # K=5 is optimal when using 75% train



train.control <- trainControl(method  = "LOOCV")

fit <- train(lameLeg~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = train.control,
             metric     = "Accuracy",
             data       = AW)
fit
qplot(fit$results$k,fit$results$Accuracy,geom = "line",
      xlab = "k", ylab = "Accuracy")

n <- 85
i <- 1

testix <- replicate(n, FALSE)
testix[i] <- TRUE
trainix <- testix == FALSE
trainD <- AW[trainix,]
testD <- AW[testix,]

# Feature Scaling
train_scale <- scale(trainD[, 2:3])
test_scale <- scale(testD[, 2:3])
classifier <- knn(train=trainD, test=testD, cl = trainD$lameLeg, k = 6)

(classifier == testD$lameLeg)
length(classifier)




classifier_knn <- function(kval){knn(train = train_scale,
                                     test = test_scale,
                                     cl = train_cl$lameLeg,
                                     k = kval)}
classifier_knn(3)






