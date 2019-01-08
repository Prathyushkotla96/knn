#kNN
library(dplyr)
data(iris)
summary(iris)
#use summarise function in dplyr to investigate data
summarise(iris, correlation = cor(Petal.Width, Petal.Length))
summarise_all(iris[-5], funs(mean))
iris %>% group_by(Species) %>% summarise(mean(Petal.Length), sd(Petal.Length))
iris %>% group_by(Species) %>% summarise(n(), mean(Petal.Length), sd(Petal.Length))
summarise(group_by(iris,Species), n(), mean(Petal.Length), sd(Petal.Length))
#visualize data
library(ggplot2)
iris%>%
  ggplot(aes(x=Petal.Width, fill=Species))+
  geom_histogram()
iris%>%
  ggplot(aes(x=Petal.Length, fill=Species))+
  geom_density(alpha=0.5)
iris%>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, colour = Species))+
  geom_point(size = 3)
iris%>%
  ggplot(aes(x = Petal.Length, y = Petal.Width, colour = Species))+
  geom_point(size = 3)
# random sample a training data set
set.seed(12345)
allrows <- 1:nrow(iris)
trainrows <- sample(allrows, replace = F, size = 0.6*length(allrows))
train_iris <- iris[trainrows, 1:4]
train_label <- iris[trainrows, 5]
table(train_label)
#use the remaining observations as the test set
test_iris <- iris[-trainrows, 1:4]
test_label <- iris[-trainrows, 5]
table(test_label)
#test several k's using kNN
library(class)
error.train <- replicate(0,30)
for(k in 1:30) {
  pred_iris <- knn(train = train_iris, test = train_iris, cl = train_label, k)
  error.train[k]<-1-mean(pred_iris==train_label)
}
error.train <- unlist(error.train, use.names=FALSE)
error.test <- replicate(0,30)
for(k in 1:30) {
  pred_iris <- knn(train = train_iris, test = test_iris, cl = train_label, k)
  error.test[k]<-1-mean(pred_iris==test_label)
}
error.test <- unlist(error.test, use.names = FALSE)
#plot the training set error versus the test set error
plot(error.train, type="o", ylim=c(0,0.15), col="blue", xlab = "K values", ylab = "Misclassification errors")
lines(error.test, type = "o", col="red")
legend("topright", legend=c("Training error","Test error"), col = c("blue","red"), lty=1:1)
#plot the prediction using the best test set k
pred_iris<-knn(train = train_iris, test = test_iris, cl = train_label, 6)
result <- cbind(test_iris, pred_iris)
combinetest <- cbind(test_iris, test_label)
result%>%
  ggplot(aes(x=Petal.Width, y=Petal.Length, color=pred_iris))+
  geom_point(size=3)
#plot the actual test data and note the difference with the previous plot
combinetest%>%
  ggplot(aes(x=Petal.Width, y=Petal.Length, color=test_label))+
  geom_point(size=3)
#look at the test set confusion matrix
table(test_label, pred_iris)
