y <- rep(1,10)
z <- numeric(10)
#<<- 영속대입

v1 <- c(5,9,3)
v2 <- c(10,11,12,13,14,15)
v3 <- array(c(v1,v2),dim=c(3,3,2))
v3


column.names <- c("COL1","COL2","COL3")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1","Matrix2")

v4 <- array(c(v1,v2),dim = c(3,3,2), dimnames = list(row.names,column.names,matrix.names))
print(v4[3,,2])
print(v4[1,3,1])


# list
w<-list("temp","rain")
w
x <- list("temp","rain",1:10)
x

x9 <- list("temp",1)

x9

x0 <- list(list("temp","rain",1:10))
x0
x0[1]

x[1]

x[[1]]
x[[2]]
o<-c(1,"g")
o
x[[3]]<-c(1,"g")
x[[3]]
x[3]
x[[3]][2:5]
class(x[[1]])
class(x[1])
class(x)

a <- rep(c(3,2,1),3)

b <- matrix(1:12,ncol=4)
b
mylist <- list(dataA = a, dataB=b)
mylist


#Data Frame
v <- c(1)
v
v <- list(1,2,"d")
v[1]

v0 <- list(c(1),c(2),c("d"))
v0[1]

v1 <- 1:10
v2 <- 11:20
mylist <- list(dataA = v1, dataB = v2)
mylist

data.frame(mylist)
mydataframe <- data.frame(mylist)
rownames(mydataframe)
colnames(mydataframe)
rownames(mydataframe) <- c("one","two","three","four","five","six","seven","eight","nine","ten")
mydataframe

ID <- 1:10
ID
SEX <- c("F","F","M","M","F","F","M","F","M","F")
AGE <- c(41,35,55,45,21,2,35,45,33,28)
AREA <- c("서울","부산","부산","대전","대전","경기","인천","부산","대전","경기")
dataframe_ex <- data.frame(ID,SEX,AGE,AREA)
dataframe_ex


#### 오후
#load data
#Excel
getwd()
#C:/R/RStudio/Projects/class02
setwd("C:/R/RStudio/Projects/class02")
#install.packages("readxl")
library(readxl)
deajeon_subway <- read_excel("C:/R/RStudio/Projects/class02/대전광역시_역별_수송실적.xlsx")
View(deajeon_subway)
head(deajeon_subway)
tail(deajeon_subway)



#TEXT
deajeon_subway_txt <- read.table(file="C:/R/RStudio/Projects/class02/대전광역시_역별_수송실적.txt",header=TRUE, sep="")
View(deajeon_subway_txt)

deajeon_subway_txt <- read.table(file="C:/R/RStudio/Projects/class02/대전광역시_역별_수송실적.txt",header=TRUE, skip=2)
View(deajeon_subway_txt)

deajeon_subway_txt <- read.table(file="C:/R/RStudio/Projects/class02/대전광역시_역별_수송실적.txt",header=TRUE, nrow=7)
View(deajeon_subway_txt)


#CSV
deajeon_subway_csv <-read.csv("C:/R/RStudio/Projects/class02/대전광역시_역별_수송실적.csv",header=TRUE)
View(deajeon_subway_csv)


install.packages("writexl")
library(writexl)
remove.packages("writexl")
write_xlsx(deajeon_subway,path = "deajeon_subway_1.xlsx")

write.csv(deajeon_subway, file = "deajeon_subway_2.csv")

.libPaths()


write.table(deajeon_subway,file = "deajeon_subway_2_1.csv", sep = ",",row.names = FALSE,quote = FALSE,append = TRUE,na="NA")
write.table(deajeon_subway,file = "deajeon_subway_3.txt")

#write("TMP = 'C:/R'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))

#데이터 분석 가공

1+2
5-3
3*7
20/4
-1+3

20%%7 #나머지
20%/%7 #몫
6^2 #제곱
6**2 #제곱

5 > 3 
5 >= 6
5 < 3
5 <= 6
5 == 5
5 != 5

x <- 1:3
y <- 3:1

(x>0)&(y>1)
(x>0)|(y>1)


View(deajeon_subway)
head(deajeon_subway)
tail(deajeon_subway)
str(deajeon_subway)
dim(deajeon_subway)
ls(deajeon_subway)


install.packages("hflights")
library(hflights)
View(hflights)
dim(hflights)

install.packages("dplyr")

library(dplyr)

hflights

hflights_df <- tbl_df(hflights) # 열 10개만
hflights_df

class(hflights_df)
glimpse(hflights)

#filter 행추출
filter(hflights_df, Month == 1, DayofMonth == 1) # 1월 1일
filter(hflights_df, Month == 1, DayofMonth == 1,UniqueCarrier=="AA") # 1월 1일

glimpse(hflights) # 열이름 
glimpse(hflights_df)

#arrange
arrange(hflights_df,ArrDelay,Month,Year)
arrange(hflights_df,desc(Month))

#select 열 추출
select(hflights_df,Year,Month,DayOfWeek)
select(hflights_df,Year:DayOfWeek)
select(hflights_df,-(Year:DayOfWeek))

#mutate 새로운 열 추가 수식가능
hflights_df_mutate<- mutate(hflights_df, gain = ArrDelay - DepDelay, gain_per_hour = gain/(AirTime/60))

glimpse(hflights_df_mutate)
View(hflights_df_mutate)

summarise(hflights_df,delay = mean(DepDelay, na.rm =TRUE))#na값은 행 제외하고 평균
View(hflights_df)
