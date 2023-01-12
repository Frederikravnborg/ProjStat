setwd("/Users/frederikravnborg/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/DTU-Frederik’s MacBook Pro/ProjStat/Project, Group/ProjStat")
(d <- read.table("horse_data23.txt", header=TRUE, as.is=TRUE))

#### Preparing data ####
# Import packages
library(e1071)
library(caTools)
library(class)
library(caret)

# Define data
d <- read.table("horse_data23.txt", header=TRUE, as.is=TRUE)
d$lameLeg <- as.factor(d$lameLeg)
d$horse <- as.factor(d$horse)
AW <- d[ , c("lameLeg", "A", "W")]
PC <- d[ , c("lameLeg", "pc3", "pc4")]
AWPC <- d[ , c("lameLeg", "A", "W", "pc3", "pc4")]

# Collapse diagonals in data
dDia <- d
for (i in 1:85){
  if(dDia$lameLeg[i] == "right:fore" || dDia$lameLeg[i] == "left:hind"){
    dDia$lameLeg[i] <- "rflh"}
  else if (dDia$lameLeg[i] == "left:fore" || dDia$lameLeg[i] == "right:hind"){
    dDia$lameLeg[i] <- "lfrh"}
}
dDia$lameLeg <- as.factor(dDia$lameLeg)

# Extract desired features
DAW <- dDia[ , c("lameLeg", "A", "W")]
DPC <- dDia[ , c("lameLeg", "pc3", "pc4")]
DAWPC <- dDia[ , c("lameLeg", "A", "W", "pc3", "pc4")]

horses <- list("B1","B2","B3","B4","B5","B6","B7","B9")

#### KNN AW ####
# KNN with optimal K
accAWm <- matrix(ncol=85, nrow=10)
guessm <- matrix(ncol=85, nrow=10)
for (K in 1:10){
  accAW <- c()
  guess <- c()
for (i in 1:8){
  traincl <- subset(d, d$horse != horses[i])$lameLeg # train classes
  trainD <- cbind(subset(d, d$horse != horses[i])$A, subset(d, d$horse != horses[i])$W)
  testD  <- cbind(subset(d, d$horse == horses[i])$A, subset(d, d$horse == horses[i])$W)
  classifier <- knn(train=trainD, test=testD, cl = traincl, k = K)
  guess <- append(guess, classifier)
  accAW <- c(accAW, as.integer(classifier == subset(d, d$horse == horses[i])$lameLeg))
  }
  accAWm[K,] <- accAW
}

accs <- c()
for (i in 1:10) { accs <- c(accs, mean(accAWm[i,])) }
bestK <- which.max(accs)
accAW <- accAWm[bestK,]


#### KNN PC ####
# KNN with optimal K
accPCm <- matrix(ncol=85, nrow=10)
guessm <- matrix(ncol=85, nrow=10)
for (K in 1:10){
  accPC <- c()
  guess <- c()
  for (i in 1:8){
    traincl <- subset(d, d$horse != horses[i])$lameLeg # train classes
    trainD <- cbind(subset(d, d$horse != horses[i])$pc3, subset(d, d$horse != horses[i])$pc4)
    testD  <- cbind(subset(d, d$horse == horses[i])$pc3, subset(d, d$horse == horses[i])$pc4)
    classifier <- knn(train=trainD, test=testD, cl = traincl, k = K)
    guess <- append(guess, classifier)
    accPC <- c(accPC, as.integer(classifier == subset(d, d$horse == horses[i])$lameLeg))
  }
  accPCm[K,] <- accPC
}

accs <- c()
for (i in 1:10) { accs <- c(accs, mean(accPCm[i,])) }
bestK <- which.max(accs)
accPC <- accPCm[bestK,]


#### KNN AWPC ####
# KNN with optimal K
accAWm <- matrix(ncol=85, nrow=10)
guessm <- matrix(ncol=85, nrow=10)
for (K in 1:10){
  accAW <- c()
  guess <- c()
  for (i in 1:8){
    traincl <- subset(d, d$horse != horses[i])$lameLeg # train classes
    trainD <- cbind(subset(d, d$horse != horses[i])$A, subset(d, d$horse != horses[i])$W)
    testD  <- cbind(subset(d, d$horse == horses[i])$A, subset(d, d$horse == horses[i])$W)
    classifier <- knn(train=trainD, test=testD, cl = traincl, k = K)
    guess <- append(guess, classifier)
    accAW <- c(accAW, as.integer(classifier == subset(d, d$horse == horses[i])$lameLeg))
  }
  accAWm[K,] <- accAW
}

accs <- c()
for (i in 1:10) { accs <- c(accs, mean(accAWm[i,])) }
bestK <- which.max(accs)
accAW <- accAWm[bestK,]


#### KNN DAW ####



#### KNN DPC ####



#### KNN DAWPC ####










