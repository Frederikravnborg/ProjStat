setwd("/Users/frederikravnborg/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/DTU-Frederik’s MacBook Pro/ProjStat/Project, Group/ProjStat")

#### Preparing data ####
# Import packages
library(e1071)
library(caTools)
library(class)
library(caret)

# Define data
# Collapse diagonals in data
dDia <- read.table("horse_data23.txt", header=TRUE, as.is=TRUE)
for (i in 1:85){
  if(dDia$lameLeg[i] == "right:fore" || dDia$lameLeg[i] == "left:hind"){
    dDia$lameLeg[i] <- "rflh"}
  else if (dDia$lameLeg[i] == "left:fore" || dDia$lameLeg[i] == "right:hind"){
    dDia$lameLeg[i] <- "lfrh"}
}
dDia$lameLeg <- as.factor(dDia$lameLeg)

d <- read.table("horse_data23.txt", header=TRUE, as.is=TRUE)
d$lameLeg <- as.factor(d$lameLeg)

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
(bestK <- which.max(accs))
accAW <- accAWm[bestK,]
(scoreAW <- mean(accAW))
AW_K <- bestK

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
(bestK <- which.max(accs))
accPC <- accPCm[bestK,]
(scorePC <- mean(accPC))
PC_K <- bestK

#### KNN AWPC ####
# KNN with optimal K
accAWPCm <- matrix(ncol=85, nrow=10)
guessm <- matrix(ncol=85, nrow=10)
for (K in 1:10){
  accAWPC <- c()
  guess <- c()
  for (i in 1:8){
    traincl <- subset(d, d$horse != horses[i])$lameLeg # train classes
    trainD <- cbind(subset(d, d$horse != horses[i])$A, subset(d, d$horse != horses[i])$W, subset(d, d$horse != horses[i])$pc3, subset(d, d$horse != horses[i])$pc4)
    testD  <- cbind(subset(d, d$horse == horses[i])$A, subset(d, d$horse == horses[i])$W, subset(d, d$horse == horses[i])$pc3, subset(d, d$horse == horses[i])$pc4)
    classifier <- knn(train=trainD, test=testD, cl = traincl, k = K)
    guess <- append(guess, classifier)
    accAWPC <- c(accAWPC, as.integer(classifier == subset(d, d$horse == horses[i])$lameLeg))
  }
  accAWPCm[K,] <- accAWPC
}

accs <- c()
for (i in 1:10) { accs <- c(accs, mean(accAWPCm[i,])) }
(bestK <- which.max(accs))
accAWPC <- accAWPCm[bestK,]
(scoreAWPC <- mean(accAWPC))
AWPC_K <- bestK

#### KNN DAW ####
d <- dDia
# KNN with optimal K
accDAWm <- matrix(ncol=85, nrow=10)
guessm <- matrix(ncol=85, nrow=10)
for (K in 1:10){
  accDAW <- c()
  guess <- c()
  for (i in 1:8){
    traincl <- subset(d, d$horse != horses[i])$lameLeg # train classes
    trainD <- cbind(subset(d, d$horse != horses[i])$A, subset(d, d$horse != horses[i])$W)
    testD  <- cbind(subset(d, d$horse == horses[i])$A, subset(d, d$horse == horses[i])$W)
    classifier <- knn(train=trainD, test=testD, cl = traincl, k = K)
    guess <- append(guess, classifier)
    accDAW <- c(accDAW, as.integer(classifier == subset(d, d$horse == horses[i])$lameLeg))
  }
  accDAWm[K,] <- accDAW
}

accs <- c()
for (i in 1:10) { accs <- c(accs, mean(accDAWm[i,])) }
(bestK <- which.max(accs))
accDAW <- accDAWm[bestK,]
(scoreDAW <- mean(accDAW))
DAW_K <- bestK

#### KNN DPC ####
d <- dDia
# KNN with optimal K
accDPCm <- matrix(ncol=85, nrow=10)
guessm <- matrix(ncol=85, nrow=10)
for (K in 1:10){
  accDPC <- c()
  guess <- c()
  for (i in 1:8){
    traincl <- subset(d, d$horse != horses[i])$lameLeg # train classes
    trainD <- cbind(subset(d, d$horse != horses[i])$pc3, subset(d, d$horse != horses[i])$pc4)
    testD  <- cbind(subset(d, d$horse == horses[i])$pc3, subset(d, d$horse == horses[i])$pc4)
    classifier <- knn(train=trainD, test=testD, cl = traincl, k = K)
    guess <- append(guess, classifier)
    accDPC <- c(accDPC, as.integer(classifier == subset(d, d$horse == horses[i])$lameLeg))
  }
  accDPCm[K,] <- accDPC
}

accs <- c()
for (i in 1:10) { accs <- c(accs, mean(accDPCm[i,])) }
(bestK <- which.max(accs))
accDPC <- accDPCm[bestK,]
(scoreDPC <- mean(accDPC))
DPC_K <- bestK

#### KNN DAWPC ####
d <- dDia
# KNN with optimal K
accDAWPCm <- matrix(ncol=85, nrow=10)
guessm <- matrix(ncol=85, nrow=10)
for (K in 1:10){
  accDAWPC <- c()
  guess <- c()
  for (i in 1:8){
    traincl <- subset(d, d$horse != horses[i])$lameLeg # train classes
    trainD <- cbind(subset(d, d$horse != horses[i])$A, subset(d, d$horse != horses[i])$W, subset(d, d$horse != horses[i])$pc3, subset(d, d$horse != horses[i])$pc4)
    testD  <- cbind(subset(d, d$horse == horses[i])$A, subset(d, d$horse == horses[i])$W, subset(d, d$horse == horses[i])$pc3, subset(d, d$horse == horses[i])$pc4)
    classifier <- knn(train=trainD, test=testD, cl = traincl, k = K)
    guess <- append(guess, classifier)
    accDAWPC <- c(accDAWPC, as.integer(classifier == subset(d, d$horse == horses[i])$lameLeg))
  }
  accDAWPCm[K,] <- accDAWPC
}

accs <- c()
for (i in 1:10) { accs <- c(accs, mean(accDAWPCm[i,])) }
(bestK <- which.max(accs))
accDAWPC <- accDAWPCm[bestK,]
(scoreDAWPC <- mean(accDAWPC))
DAWPC_K <- bestK



#### Summary ####
# K-values
AW_K
PC_K
AWPC_K
DAW_K
DPC_K
DAWPC_K

# Accuracies
round(scoreAW,3)
round(scorePC,3)
round(scoreAWPC,3)
round(scoreDAW,3)
round(scoreDPC,3)
round(scoreDAWPC,3)

#### McNemar ####
# function to create f11, f01, f10, f00
f <- function(x,y){
  f11 <- 0; f01 <- 0; f10 <- 0; f00 <- 0;
  for (i in 1:length(x)){
    if      (x[i] == 1 & y[i] == 1){f11 <- f11+1}
    else if (x[i] == 0 & y[i] == 1){f01 <- f01+1}
    else if (x[i] == 1 & y[i] == 0){f10 <- f10+1}
    else if (x[i] == 0 & y[i] == 0){f00 <- f00+1}}
  matrix(c(f11, f01, f10, f00),
         nrow = 2,
         dimnames = list("1st Model" = c("Correct", "Wrong"),
                         "2nd Model" = c("Correct", "Wrong")))}

#### McNemar KNN ####
A <- cbind(accAW,accPC,accAWPC,accDAW,accDPC,accDAWPC)

(mcMat <- data.frame(matrix(ncol = 6, nrow = 6)))
colnames(mcMat) <- c("AW","PC","AWPC","DAW","DPC","DAWPC")
for (i in 1:6){
  for (j in i:6){ if(i != j){
    mcMat[i,j] <- p.adjust(mcnemar.test(f(A[,i], A[,j]))[3], method="BH", n=5)
  }}
}
mcNemar_pvalues_KNN <- mcMat
signif(mcNemar_pvalues_KNN,3)














