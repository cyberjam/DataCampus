# 예제 3 타슈 데이터 설명 ================================

getwd()
tashu16 <- read.csv("C:/R/RStudio/Projects/rawdata/data3/2016.csv")
str(tashu16)
names(tashu16) <- c()



# Week 2 시작==========================================================================
## ========================================================================
rm(list=ls())

x <- c("10","5","2","3","5","3")
str(x)
# chr [1:6] "10" "5" "2" "3" "5" "3"

x1 <- as.numeric(x)
str(x1)
# num [1:6] 10 5 2 3 5 3

x2 <- as.data.frame(x1)
str(x2)
# 'data.frame':	6 obs. of  1 variable:
#   $ x1: num  10 5 2 3 5 3

x3<- as.character(x1)
str(x3)
# chr [1:6] "10" "5" "2" "3" "5" "3"

x3
# [1] "10" "5"  "2"  "3"  "5"  "3" 
      # 1   4    2    3    4    3
# 10 2 3 5
# 1  2 3 4

x4 <- as.factor(x3)
str(x4)
# Factor w/ 4 levels "10","2","3","5": 1 4 2 3 4 3

iris_test <- iris
str(iris_test)
# 'data.frame':	150 obs. of  5 variables:
#   $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

iris_test[c(2,5,7,8,50,55,60,70),1] <- NA
iris_test[c(3,5,22,55,66,70,99),4] <- NA

head(iris_test)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3          NA  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4          NA  setosa
# 6          5.4         3.9          1.7         0.4  setosa
!complete.cases(iris_test)

complete.cases(iris_test)

iris_test[!complete.cases(iris_test),]

iris_test[!complete.cases(iris_test)-1,]
iris_test[complete.cases(iris_test),]

mapply(median, iris_test[1:4], na.rm=TRUE) # 중앙값 len일 짝수일경우 평균으로

install.packages("DMwR")
library(DMwR)
apply(iris_test[1:4],2,median,na.rm=T)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 5.80         3.00         4.35         1.30

head(centralImputation(iris_test[1:4]),30) # 중앙값 사용. 중위수 또는 최빈값 사용
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1           5.1         3.5          1.4         0.2
# 2           4.9         3.0          1.4         0.2
# 3           4.7         3.2          1.3         1.3
# 4           4.6         3.1          1.5         0.2
# 5           5.0         3.6          1.4         1.3
# 6           5.4         3.9          1.7         0.4
# 7           4.6         3.4          1.4         0.3
# 8           5.0         3.4          1.5         0.2
# 9           4.4         2.9          1.4         0.2
# 10          4.9         3.1          1.5         0.1


head(knnImputation(iris_test[1:4],k=3,meth="median"),10) # KNN 분류 알고리즘 사용

# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1           5.1         3.5          1.4         0.2
# 2           4.9         3.0          1.4         0.2
# 3           4.7         3.2          1.3         0.2
# 4           4.6         3.1          1.5         0.2
# 5           5.0         3.6          1.4         0.3
# 6           5.4         3.9          1.7         0.4
# 7           4.6         3.4          1.4         0.3
# 8           5.0         3.4          1.5         0.2
# 9           4.4         2.9          1.4         0.2
# 10          4.9         3.1          1.5         0.1


iris_test1 <- iris_test
head(iris_test1[1:4])
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1          5.1         3.5          1.4         0.2
# 2          4.9         3.0          1.4         0.2
# 3          4.7         3.2          1.3          NA
# 4          4.6         3.1          1.5         0.2
# 5          5.0         3.6          1.4          NA
# 6          5.4         3.9          1.7         0.4
a <- apply(iris_test1[1:4],2,mean, na.rm=T)
a
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 5.843333     3.057333     3.758000     1.216783 


is.na(iris_test1$Sepal.Length)

# [1] FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
# [15] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [29] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [43] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
# [57] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
# [71] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [85] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [99] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [113] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [127] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [141] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

#평균값으로 바꾸기
a[1]
# Sepal.Length 
# 5.843333

iris_test1$Sepal.Length <- ifelse(is.na(iris_test1$Sepal.Length),a[1],iris_test1$Sepal.Length)

# 만약 Sepal.Length에 na값이 있다면 a[1] 로 대체, 아니라면 Sepal.Length 로.

install.packages("Amelia") #결측치를 시각화
library(Amelia)
missmap(iris_test)

boxplot(iris_test)
test_plot<- boxplot(iris_test)
test_plot
# $stats
# idx  S.len S.width ...
# [,1] [,2] [,3] [,4] [,5]
# [1,]  4.3  2.2 1.00  0.1    1 # 최소수염
# [2,]  5.1  2.8 1.60  0.3    1 # 최하값
# [3,]  5.8  3.0 4.35  1.3    2 # 중간값
# [4,]  6.4  3.3 5.10  1.8    3 # 최대값
# [5,]  7.9  4.0 6.90  2.5    3 # 최대수염
# 
# $n
# [1] 142 150 150 143 150
# 
# $conf
# [,1]     [,2]     [,3]     [,4]     [,5]
# [1,] 5.627632 2.935497 3.898477 1.101811 1.741987
# [2,] 5.972368 3.064503 4.801523 1.498189 2.258013
# 
# $out
# [1] 4.4 4.1 4.2 2.0
# 
# $group
# [1] 2 2 2 2
# 
# $names
# [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"

boxplot(iris_test)$stats
box_oulier <- boxplot(iris_test)$stats
box_oulier # matrix
# [,1] [,2] [,3] [,4] [,5]
# [1,]  4.3  2.2 1.00  0.1    1
# [2,]  5.1  2.8 1.60  0.3    1
# [3,]  5.8  3.0 4.35  1.3    2
# [4,]  6.4  3.3 5.10  1.8    3
# [5,]  7.9  4.0 6.90  2.5    3
box_oulier[1,2]
# [1] 2.2
iris_test$Sepal.Width <- ifelse(iris_test$Sepal.Width<box_oulier[1,2]|iris_test$Sepal.Width>box_oulier[5,2],NA,iris_test$Sepal.Width)
# 최소 수염 이하나 최대수염 이상은 NA로 대체, 나머지는 원래값으로.
boxplot(iris_test)
plot(iris$Sepal.Length,iris$Sepal.Width)
