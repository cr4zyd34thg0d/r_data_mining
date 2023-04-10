library(dataset)
data(iris)

#Create a Box Plot for the Iris data
plot(petal.Length ~ Species, data=iris, main="Box Plot Iris")

#Create a Violin Plot using 2 variables
install.packages("vioplot")
library(vioplot)
violinA <- iris[iris$Species == "virginica", "Sepal.Length"]
violinB <- iris[iris$Species == "versicolor", "Sepal.Length"]
violinC <- iris[iris$Species == "setosa", "Sepal.Length"]
vioplot(violinA, violinB, violinC, names = c("Virginica", "Versicolor", "Setosa"), col = "purple"main="Violin Plot of Sepal Length")


violinA <- iris$Sepal.Length[iris$Species == "virginica"]
violinB <- iris$Sepal.Length[iris$Species == "veriscolor"]
violinC <- iris$Sepal.Length[iris$Species == "setosa"]
vioplot(violinA,violinB,violinC, names=c("Virginica","Veriscolor","Setosa"), col="purple")

#Create a scatter plot from 2 variables from the iris data set.  I chose to use the Sepal.Length and the Sepal.Width.
plot(Sepal.Length ~ Sepal.Width, data=iris, main="Scatter Plot for Iris Speal Length and Width")

iris_cor<-iris[1:4]
View(iris_cor)
iris_cor_matrix<-cor(iris_cor)
iris_cor_matrix
View(correlation_matrix)
