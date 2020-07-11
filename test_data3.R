library(hflights)
library(dplyr)
# hflights_df <- tbl_df(hflights)
hflights_df <- hflights
View(hflights_df)
hflights_df<-arrange(hflights_df,desc(TailNum))
# hflights_df_order <- hflights_df[order('TailNum')]
# hflights_df_order
head(hflights_df)
group_by(hflights_df,TailNum)
planes <- group_by(hflights_df,TailNum)
planes
summarise(planes, count = n(), dist = mean(Distance, na.rm = TRUE),delay = mean(ArrDelay,na.rm = TRUE))
delay  <- summarise(planes, count = n(), dist = mean(Distance, na.rm = TRUE),delay = mean(ArrDelay,na.rm = TRUE))
delay
arrange(delay,count,desc(dist))
delay <- arrange(delay,count,desc(dist))
delay
filter(delay, count >20, dist <2000)
delay <- filter(delay, count >20, dist <2000)
delay
delay

#Chain
hflights %>% 
  group_by(Year, Month, DayofMonth) %>%
  summarise(arr = mean(ArrDelay, na.rm =TRUE),dep = mean(DepDelay, na.rm = TRUE)) %>% 
  filter(arr>30 | dep >30)


#BindRows

hflights_1 <- hflights[1:100000,]
hflights_2 <- hflights[100001:200000,]
bind_rows(hflights_1,hflights_2)
system.time(rbind(hflights_1,hflights_2))
system.time(bind_rows(hflights_1,hflights_2))

hflights_3 <- hflights[1:6,1:10]
hflights_3
hflights_4 <- hflights[1:6,11:15]
hflights_4
bind_cols(hflights_3,hflights_4)


#left_join

hflights_5 <- hflights[1:10,1:5]
hflights_5
hflights_5 <- hflights_5[-4:-5,] # 열 삭제
hflights_5


hflights_6 <- hflights[1:10,1:7]
hflights_6
hflights_6 <- hflights_6[-2:-3,-1:-2] #2 3 행, 1 2열 삭제
hflights_6

hflights_6 <- hflights_6[,-2:-3]

hflights_6


hflights_5
# Year Month DayofMonth DayOfWeek DepTime
# 5424 2011     1          1         6    1400
# 5425 2011     1          2         7    1401
# 5426 2011     1          3         1    1352
# 5429 2011     1          6         4    1359
# 5430 2011     1          7         5    1359
# 5431 2011     1          8         6    1355
# 5432 2011     1          9         7    1443
# 5433 2011     1         10         1    1443
hflights_6
# DayofMonth ArrTime UniqueCarrier
# 5424          1    1500            AA
# 5427          4    1513            AA
# 5428          5    1507            AA
# 5429          6    1503            AA
# 5430          7    1509            AA
# 5431          8    1454            AA
# 5432          9    1554            AA
# 5433         10    1553            AA

left_join(hflights_5,hflights_6,by="DayofMonth") # 기준이 되는 왼쪽 df 중심으로 

# Year Month DayofMonth DayOfWeek DepTime ArrTime UniqueCarrier
# 1 2011     1          1         6    1400    1500            AA
# 2 2011     1          2         7    1401      NA          <NA>
# 3 2011     1          3         1    1352      NA          <NA>
# 4 2011     1          6         4    1359    1503            AA
# 5 2011     1          7         5    1359    1509            AA
# 6 2011     1          8         6    1355    1454            AA
# 7 2011     1          9         7    1443    1554            AA
# 8 2011     1         10         1    1443    1553            AA

inner_join(hflights_5,hflights_6,by="DayofMonth") # 두 df 공통 행
# Year Month DayofMonth DayOfWeek DepTime ArrTime UniqueCarrier
# 1 2011     1          1         6    1400    1500            AA
# 2 2011     1          6         4    1359    1503            AA
# 3 2011     1          7         5    1359    1509            AA
# 4 2011     1          8         6    1355    1454            AA
# 5 2011     1          9         7    1443    1554            AA
# 6 2011     1         10         1    1443    1553            AA
full_join(hflights_5,hflights_6,by="DayofMonth") # 그냥 모두 
# Year Month DayofMonth DayOfWeek DepTime ArrTime UniqueCarrier
# 1  2011     1          1         6    1400    1500            AA
# 2  2011     1          2         7    1401      NA          <NA>
# 3  2011     1          3         1    1352      NA          <NA>
# 4  2011     1          6         4    1359    1503            AA
# 5  2011     1          7         5    1359    1509            AA
# 6  2011     1          8         6    1355    1454            AA
# 7  2011     1          9         7    1443    1554            AA
# 8  2011     1         10         1    1443    1553            AA
# 9    NA    NA          4        NA      NA    1513            AA
# 10   NA    NA          5        NA      NA    1507            AA

test <- inner_join(hflights_5,hflights_6,by="DayofMonth")
test
apply(test,1,sum) # 1 행  2 열 
apply(test[,5:6],1,sum)
apply(test[,5:6],2,sum)
apply(test[,5:6],1,mean)
apply(test[,5:6],1,max)

test1 <- c(1:5)

test2 <- c(6:12)
test3 <- list(test1,test2)

test3

sapply(test3,sum) #리스트형을 받아 sum으로 계산 후 벡터로 반환 
# 자리가 부족하면 앞에서 부터 다시 반복
mapply(sum,test1,test2)

Split1 <- split (hflights, hflights$DayOfWeek)
View(Split1)

subset(hflights, DayOfWeek ==5)
subset(hflights, DepTime > 1200)
system.time(subset(hflights, DayOfWeek == 5 & DepTime >1200)) #내장
system.time(filter(hflights, DayOfWeek == 5 & DepTime >1200)) #dplyr
# 걸리는 시간은 같다


install.packages("reshape2")
library(reshape2)

head(airquality)
names(airquality)<- tolower(names(airquality))
head(airquality)

tail(airquality)

melt_test <- melt(airquality)
head(melt_test)
tail(melt_test)

melt_test2 <- melt(airquality,id.vars = c("month", "wind"), measure.vars = "ozone")
head(melt_test2)


# cast
melt_test3 <- melt(airquality, id = c("month","day"),na.rm = TRUE)
head(melt_test3)

dcast_test <- dcast(melt_test3, month + day ~ variable)
head(dcast_test)
