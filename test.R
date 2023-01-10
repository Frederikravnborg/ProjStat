library("caret")
library("caTools")
library("party")
library("dplyr")
library("rpart.plot")
library("Ecdat")
library("RWeka")

#data = read.table("horse_data23.txt", header = TRUE)
data = horse_data23
data$lameLeg = as.factor(data$lameLeg)

ind = sample(2, nrow(data), replace = T, prob = c(0.25, 0.75))
train = data[ind == 2, ]
test = data[ind == 1, ]

train = data.frame(train)
test = data.frame(test)

tree = rpart(train$lameLeg ~ train$A + train$W, data = train)
rpart.plot(tree)
w = 15
pred = predict(tree, test[10], type = "class")



res = pred == train$lameLeg
sum(res)
res

#res
#confusion_matrix = table(train$lameLeg, pred)
#confusion_matrix
#i = i + 1







indexes = rep(FALSE, length(data$A))
res = rep(NA, dim(length(data)))

for (i in 1:length(data$A)){
  set.seed(100 + i)
  temp = indexes
  temp[i] = TRUE
  
  train = data[!temp,]
  test = data[temp,]
  
  train = data.frame(train)
  test = data.frame(test)
  
  tree = rpart(train$lameLeg ~ train$A + train$W, method = "class", data = train)
  pred = predict(tree, data = matrix(list(test$A[1], test$W[1]), ncol = 2), type = "class")
  
  
}

res

table_mat = table(as.numeric(as.factor(data$lameLeg)), pred)
test
