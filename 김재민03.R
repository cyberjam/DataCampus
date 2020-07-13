getwd()
# "C:/R/RStudio/Projects/HW/Rex3/hw03"
#1번 기존 작업 모두 삭제============================================
rm(list=ls())

#2번 데이터 불러오기 ========================================================
ta6 <- read.csv("C:/R/RStudio/Projects/rawdata/data3/2016.csv") 
ta7 <- read.csv("C:/R/RStudio/Projects/rawdata/data3/2017.csv")
ta8 <- read.csv("C:/R/RStudio/Projects/rawdata/data3/2018.csv")
ta9 <- read.csv("C:/R/RStudio/Projects/rawdata/data3/2019.csv")

#3번 str() 속성 확인 =====================================================
str(ta6)
# 'data.frame':	715136 obs. of  6 variables:
#   $ X46            : num  152 133 133 39 92 92 91 90 88 17 ...
# $ X20160101050015: num  2.02e+13 2.02e+13 2.02e+13 2.02e+13 2.02e+13 ...
# $ X17            : num  82 172 172 57 55 55 135 192 133 23 ...
# $ X20160101050451: num  2.02e+13 2.02e+13 2.02e+13 2.02e+13 2.02e+13 ...
# $ X380           : num  3190 2070 2080 860 1500 5460 3590 4420 1060 1900 ...
# $ X0             : num  2 2 2 0 2 2 2 2 0 2 ...



# 4번 4년 중 이용객이 가장 많은 년도는 언제인가? ===========================================
dim(ta6)[1]
# [1] 715136
max_year <- c()
for(idx in 6:9){
  max_year <- rbind(max_year,c(sprintf("ta%d",idx),dim(get(sprintf("ta%d",idx)))[1]))
  # print(dim(get(sprintf("ta%d",idx)))[1])
}
dim(ta6)
# [1] 715136      6
max_year
#       [,1]  [,2]    
# [1,] "ta6" "715136"
# [2,] "ta7" "614207"
# [3,] "ta8" "491699"
# [4,] "ta9" "365814"

subset(max_year,max_year[,2]==max(max_year[,2]))
# [,1]  [,2]    
# [1,] "ta6" "715136"
print("2016년도 이용객이 가장 많습니다.")

# 5번 2019년 가장 많이 이용한 스테이션 (대여, 반납 각각)은 어디인가요? =================================

head(ta9)
# X185 X20190101000002 X185.1 X20190101011608   X0 X2
# 1   29     2.01901e+13     29     2.01901e+13  530  2
# 2   60     2.01901e+13     66     2.01901e+13  540  2
# 3  208     2.01901e+13    244     2.01901e+13 3130  2
# 4  208     2.01901e+13    244     2.01901e+13 3140  0
# 5  121     2.01901e+13    143     2.01901e+13 1970  0
# 6  122     2.01901e+13     17     2.01901e+13 2630  0

names(ta9)<-c("rentStat","rentTime","returnStat","returnTime","dist","member")
str(ta9)

# 'data.frame':	365814 obs. of  6 variables:
#   $ rentStat  : num  29 60 208 208 121 122 237 83 135 17 ...
# $ rentTime  : num  2.02e+13 2.02e+13 2.02e+13 2.02e+13 2.02e+13 ...
# $ returnStat: num  29 66 244 244 143 17 140 92 133 93 ...
# $ returnTime: num  2.02e+13 2.02e+13 2.02e+13 2.02e+13 2.02e+13 ...
# $ dist      : num  530 540 3130 3140 1970 2630 2910 0 750 3280 ...
# $ member    : num  2 2 2 0 0 0 2 2 2 0 ...
library(reshape2)
library(dplyr)

# ta9Split<-split(ta9,ta9$rentStat)
# ta9Split

#Rental 
ta9_grp_rent <- ta9 %>% 
  group_by(rentStat) %>% 
  summarise(rentStat,count = n(),na.rm=TRUE) %>% 
  distinct(count)
ta9_grp_rent
# # A tibble: 261 x 2
# # Groups:   rentStat [261]
# rentStat count
# <dbl> <int>
# 1        1   954
# 2        2  2844
# 3        3 10739
# 4        4  3108
# 5        5  2169
# 6        6  1639
# 7        7  1907
# 8        8  4871
# 9        9  1034
# 10       10  3180
# # ... with 251 more rows

filter(ta9_grp_rent,count == max(ta9_grp_rent$count))
# rentStat count
# <dbl> <int>
#   1        3 10739

print("가장 많이 대여한 곳은 스테이션 3입니다.")

ta9_grp_return <- ta9 %>% 
  group_by(returnStat) %>% 
  summarise(member, count = n()) %>% 
  distinct(count)
ta9_grp_return
# # A tibble: 262 x 2
# # Groups:   returnStat [262]
# returnStat count
# <dbl> <int>
# 1          1  1062
# 2          2  2993
# 3          3 12074
# 4          4  3102
# 5          5  1870
# 6          6  2044
# 7          7  1855
# 8          8  4444
# 9          9  1344
# 10         10  3049
# # ... with 252 more rows


filter(ta9_grp_return,count == max(ta9_grp_return$count))
# # A tibble: 1 x 2
# # Groups:   returnStat [1]
# returnStat count
# <dbl> <int>
#   1          3 12074

print("가장 많이 반납한 곳도 스테이션 3입니다.")


# 6번 5번에서 대여가 가장 많은 스테이션의 2019년 월별 대여 현황===========
head(ta9)
# rentStat    rentTime returnStat  returnTime dist member
# 1       29 2.01901e+13         29 2.01901e+13  530      2
# 2       60 2.01901e+13         66 2.01901e+13  540      2
# 3      208 2.01901e+13        244 2.01901e+13 3130      2
# 4      208 2.01901e+13        244 2.01901e+13 3140      0
# 5      121 2.01901e+13        143 2.01901e+13 1970      0
# 6      122 2.01901e+13         17 2.01901e+13 2630      0
ta9_all <- ta9 %>% 
  mutate(rentMonth = ta9$rentTime%/%100000000) %>% 
  filter(rentStat==3) %>% 
  group_by(rentMonth) %>% 
  summarise(member,count=n()) %>% 
  distinct(count)

ta9_all
# 2019 스테이션 3
# 월별 대여현황
# # A tibble: 8 x 2
# # Groups:   rentMonth [8]
# rentMonth count
# <dbl> <int>
# 1    201901   220
# 2    201902   361
# 3    201903   968
# 4    201904  1948
# 5    201905  2858
# 6    201906  1965
# 7    201907  1264
# 8    201908  1155

#0번 2019년만 이용이 저조한 까닭은?
# 2018년과 비교를 위해 전처리

names(ta8)<-c("rentStat","rentTime","returnStat","returnTime","dist","member")

ta8_all <- ta8 %>% 
  mutate(rentMonth = ta8$rentTime%/%100000000) %>% 
  filter(rentStat==3) %>% 
  group_by(rentMonth) %>% 
  summarise(member,count=n()) %>% 
  distinct(count)

ta8_all
# # A tibble: 12 x 2
# # Groups:   rentMonth [12]
# rentMonth count
# <dbl> <int>
#   1    201801   178
# 2    201802   298
# 3    201803  1822
# 4    201804  1863
# 5    201805  2402
# 6    201806  2153
# 7    201807  1167
# 8    201808   962
# 9    201809  2791
# 10    201810  1479
# 11    201811   490
# 12    201812   203
ta9_all_station <- ta9 %>% 
  mutate(rentMonth = ta9$rentTime%/%100000000) %>% 
  group_by(rentMonth) %>% 
  summarise(member,count=n()) %>% 
  distinct(count)
ta9_all_station
sum(ta9_all_station$count)
dim(ta9)


ta8_all_station <- ta8 %>% 
  mutate(rentMonth = ta8$rentTime%/%100000000) %>% 
  group_by(rentMonth) %>% 
  summarise(member,count=n()) %>% 
  distinct(count)


ta8_all_station$rentMonth<-ta8_all_station$rentMonth%%100 #join key를 만들기 위해 rentMonth에서 년도를 삭제 

ta9_all_station$rentMonth<-ta9_all_station$rentMonth%%100 


left_join(ta8_all_station,ta9_all_station,by="rentMonth") 
# rentMonth count.x count.y
#       <dbl>   <int>   <int>
  # 1         1   11318   14988
  # 2         2   13454   15900
  # 3         3   40909   31070
  # 4         4   50073   50566
  # 5         5   59502   69968
  # 6         6   64558   64343
  # 7         7   46953   58818
  # 8         8   41973   60161
  # 9         9   64073      NA
 # 10        10   52550      NA
 # 11        11   31451      NA
 # 12        12   14885      NA


print("2019년 9월에서 12월 까지 데이터가 누락되어 이용이 저조하도록 관측하였습니다.")
