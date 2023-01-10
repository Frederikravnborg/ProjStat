setwd("/Users/frederikravnborg/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/DTU-Frederik’s MacBook Pro/ProjStat/Project, Group")
(d <- read.table("horse_data23.txt", header=TRUE, as.is=TRUE))

#### Initial Plots ####



#### Goal 1 ####
(H <- as.factor(d$horse))
# If horse has a significant effect on A
fit <- lm(d$A ~ H); anova(fit)

# If horse has a significant effect on S
fit <- lm(d$S ~ H); anova(fit)

# If horse has a significant effect on W
fit <- lm(d$W ~ H); anova(fit)

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

# Import packages
library(e1071)
library(caTools)
library(class)
library(caret)

# Define data
d <- read.table("horse_data23.txt", header=TRUE, as.is=TRUE)
d$lameLeg <- as.factor(d$lameLeg)
AW <- d[ , c("lameLeg", "A", "W")]
PC <- d[ , c("lameLeg", "A", "W")]

#### Using A and W ####

# Find optimal K
train.control <- trainControl(method  = "LOOCV")
(fit <- train(lameLeg~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = train.control,
             metric     = "Accuracy",
             data       = AW))
qplot(fit$results$k,fit$results$Accuracy,geom = "line",
      xlab = "k", ylab = "Accuracy")

n <- 85
acc <- replicate(n, NA)
guess = replicate(n,NA)
for (i in 1:85){
    # create indexes for train and test
    testix <- replicate(n, FALSE)
    testix[i] <- TRUE
    trainix <- testix == FALSE
    traincl <- AW[trainix,][,1] # train classes
    trainD <- AW[trainix,][, 2:3]
    testD <- AW[testix,][, 2:3]
    classifier <- knn(train=trainD, test=testD, cl = traincl, k = 5)
    guess[i] <- as.character(classifier)
    acc[i] <- sum(classifier == d$lameLeg[i])
}
acc
guess = as.factor(guess)
mean(acc)
(cm <- table(d$lameLeg, guess))

#### Using PC3 and PC4 ####

# Find optimal K
train.control <- trainControl(method  = "LOOCV")
(fit <- train(lameLeg~ .,
              method     = "knn",
              tuneGrid   = expand.grid(k = 1:20),
              trControl  = train.control,
              metric     = "Accuracy",
              data       = PC))
qplot(fit$results$k,fit$results$Accuracy,geom = "line",
      xlab = "k", ylab = "Accuracy")

n <- 85
acc <- replicate(n, NA)
guess = replicate(n,NA)
for (i in 1:85){
  # create indexes for train and test
  testix <- replicate(n, FALSE)
  testix[i] <- TRUE
  trainix <- testix == FALSE
  traincl <- PC[trainix,][,1] # train classes
  trainD <- PC[trainix,][, 2:3]
  testD <- PC[testix,][, 2:3]
  classifier <- knn(train=trainD, test=testD, cl = traincl, k = 5)
  guess[i] <- as.character(classifier)
  acc[i] <- sum(classifier == d$lameLeg[i])
}
acc
guess = as.factor(guess)
mean(acc)
(cm <- table(d$lameLeg, guess))



