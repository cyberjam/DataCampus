library(dplyr)
# 7번 O-D 분석) 문제 설명 ====================================================================
# 2019년 타슈 이용객들이 어느 정류서 빌려서 어느 정류서로 가장 많이 반납하였나요? 
# O-D table로 count하세요. (결과값을 반드시 csv파일로 저장하세요)
# ※ 가로축: 렌트정류소 Number(Origin)
# 세로축: 반남정류소 Number(Destination)
# (1) 2019년 전체 데이터에 대해서 O-D table을 만드세요.
# (결과 matrix or data.frame을 csv 파일로 저장하세요. ex.파일명: 2019.csv)

# 7-1번 년별 O-D table=====================================================================================

# 1. Data read =============================================================
getwd()
exdata2019_raw <- read.csv("C:/R/RStudio/Projects/rawdata/data3/2019.csv")
exdata2019 <-exdata2019_raw

str(exdata2019)
# 'data.frame':	365814 obs. of  6 variables:
# $ X185           : num  29 60 208 208 121 122 237 83 135 17 ...
# $ X20190101000002: num  2.02e+13 2.02e+13 2.02e+13 2.02e+13 2.02e+13 ...
# $ X185.1         : num  29 66 244 244 143 17 140 92 133 93 ...
# $ X20190101011608: num  2.02e+13 2.02e+13 2.02e+13 2.02e+13 2.02e+13 ...
# $ X0             : num  530 540 3130 3140 1970 2630 2910 0 750 3280 ...
# $ X2             : num  2 2 2 0 0 0 2 2 2 0 ...
# 2. 변수 Naming ==============================
names(exdata2019) <- c("rent_station","rent_time","return_station","return_time","moving_distance","member")
head(exdata2019)
#   rent_station   rent_time return_station return_time moving_distance member
# 1           29 2.01901e+13            29 2.01901e+13             530      2
# 2           60 2.01901e+13            66 2.01901e+13             540      2
# 3          208 2.01901e+13           244 2.01901e+13            3130      2
# 4          208 2.01901e+13           244 2.01901e+13            3140      0
# 5          121 2.01901e+13           143 2.01901e+13            1970      0
# 6          122 2.01901e+13            17 2.01901e+13            2630      0

# 3. 결측치 제거 =================================================
sum(is.na(exdata2019$rent_station))
# [1] 0
sum(is.na(exdata2019$return_station))
# [1] 3110
exdata2019_delna <- exdata2019[complete.cases(exdata2019$return_station),]# 결측치행 제거
sum(is.na(exdata2019_delna))
# [1] 0

# 4. return_station,rent_station 기준으로 Group_by 후 summarise를 통해 이용빈도 count===================
exdata2019_grp <- exdata2019_delna %>% 
  group_by(return_station,rent_station) %>% 
  summarise(n=n())
head(exdata2019_grp)
# # A tibble: 6 x 3
# # Groups:   return_station [1]
# return_station rent_station     n
# <dbl>        <dbl> <int>
# 1              1            1   169
# 2              1            2    25
# 3              1            3    42
# 4              1            4    13
# 5              1            5    11
# 6              1            6     2

#5. rent_station 기준으로 Split을 활용해 station 별 나누기 ==========================
exdata2019_split <-split(exdata2019_grp,exdata2019_grp$rent_station)
exdata2019_split[[1]]
# # A tibble: 128 x 3}
# # Groups:   return_station [128]
# return_station rent_station     n
# <dbl>        <dbl> <int>
# 1              1            1   169
# 2              2            1    20
# 3              3            1    35
# 4              4            1     3
# 5              5            1    12
# 6              6            1     4
# 7              7            1    10
# 8              8            1    13
# 9              9            1     2
# 10             10            1    21
# # ... with 118 more rows

# 6. CASE1 : Split한 list내 tbl내 데이터를 indexing하여 For문으로 O-D생성 (효율성이 낮습니다 O(n^2))=============================
#1행 각 열 값 임의추출
exdata2019_split[[1]][[1,1]] # x return_station
# [1] 1
exdata2019_split[[1]][[1,2]] # y   rent_station
# [1] 1
exdata2019_split[[1]][[1,3]] # v(value)       n 
# [1] 169

#비어있는 프레임 생성
temp_df <- data.frame()

length(exdata2019_split)#열 길이
# [1] 261
nrow(exdata2019_split[[1]])#1 스테이션의 행길이
# [1] 128

case1<- for (list_idx in 1:length(exdata2019_split)){ #열길이만큼 반복
  for (df_row in 1:nrow(exdata2019_split[[list_idx]])){ # 각 스테이션의 행길이 만큼 반복
    x = exdata2019_split[[list_idx]][[df_row,1]] # x return_station
    y = exdata2019_split[[list_idx]][[df_row,2]] # y   rent_station
    # print(sprintf("%d %d",x,y))
    v = exdata2019_split[[list_idx]][[df_row,3]] # v(value)       n 
    temp_df[x,y]= v #비어있는 데이터프레인 위치에 맞게 대입
  }
}
system.time(case1)
# 사용자  시스템 elapsed 
# 0       0       0 
names(temp_df)<-c(1:length(names(temp_df))) # 열 이름 개수만큼 1부터 naming

# csv 저장
write.csv(temp_df,file="2019_0.csv")




# 6. CASE2 : Split한 list내 tbl내 데이터와 return_station으로 키값을 만든 비어있는 df를 left-join으로 O-D생성 (효율성이 좋습니다 O(n))=========================
exdata2019_split[[1]]
# # A tibble: 128 x 3
# # Groups:   return_station [128]
# return_station rent_station     n
# <dbl>        <dbl> <int>
# 1              1            1   169
# 2              2            1    20
# 3              3            1    35
# 4              4            1     3
# 5              5            1    12
# 6              6            1     4
# 7              7            1    10
# 8              8            1    13
# 9              9            1     2
# 10             10            1    21
exdata2019_split[[1]][c(1,3)] #join을 위해 필요없는 rent_staion 제거. rent_station은 결측치 확인 했을때
# # A tibble: 128 x 2
# # Groups:   return_station [128]
# return_station     n
# <dbl> <int>
# 1              1   169
# 2              2    20
# 3              3    35
# 4              4     3
# 5              5    12
# 6              6     4
# 7              7    10
# 8              8    13
# 9              9     2
# 10             10    21
# # ... with 118 more rows
temp_df2 <- data.frame(return_station=c(1:261)) # join을 위해 return_station을 기준으로 key를 만듭니다. 

case2<- for (list_idx in 1:length(exdata2019_split)){
  temp_df2 <-left_join(temp_df2,exdata2019_split[[list_idx]][c(1,3)],by="return_station")
}
system.time(case2)
# 사용자  시스템 elapsed 
# 0       0       0 
temp_df2 <- temp_df2[-1] # join을 위한 key return_station 삭제
names(temp_df2)<-c(1:length(names(temp_df2)))# 열 이름 개수만큼 1부터 naming

# csv 저장
write.csv(temp_df2,file="2019_2.csv")


# 7-2번 일별 O-D table====================================
str(exdata2019_delna)
# 'data.frame':	362704 obs. of  6 variables:
# $ rent_station   : num  29 60 208 208 121 122 237 83 135 17 ...
# $ rent_time      : num  2.02e+13 2.02e+13 2.02e+13 2.02e+13 2.02e+13 ...
# $ return_station : num  29 66 244 244 143 17 140 92 133 93 ...
# $ return_time    : num  2.02e+13 2.02e+13 2.02e+13 2.02e+13 2.02e+13 ...
# $ moving_distance: num  530 540 3130 3140 1970 2630 2910 0 750 3280 ...
# $ member         : num  2 2 2 0 0 0 2 2 2 0 ...

exdata2019_delna$rent_month <- exdata2019_delna$rent_time%/%1000000
exdata2019_delna$return_month <- exdata2019_delna$return_time%/%1000000
filter(exdata2019_delna,rent_month!=return_month) #대여일과 반납일이 다른경우
# [1] 12066
filter(exdata2019_delna,rent_month%/%100!=return_month%/%100)#대여월과 반납월이 다른경우
# [1] 531
# 무조건 rent_station 기준으로 

exdata2019_may<-filter(exdata2019_delna,exdata2019_delna$rent_month>=20190501&exdata2019_delna$rent_month<=20190531)
tail(exdata2019_may)
#       rent_station    rent_time return_station return_time moving_distance member rent_month return_month
# 69439          247 2.019053e+13             37 2.01906e+13            2590      2   20190531     20190601
# 69440          242 2.019053e+13            143 2.01906e+13            2190      2   20190531     20190601
# 69441           59 2.019053e+13             55 2.01906e+13             590      2   20190531     20190601
# 69442          215 2.019053e+13             69 2.01906e+13            3930      2   20190531     20190601
# 69443           59 2.019053e+13             55 2.01906e+13               0      2   20190531     20190601
# 69444          242 2.019053e+13            143 2.01906e+13            2080      2   20190531     20190601

exdata2019_may_split <-split(exdata2019_may,exdata2019_may$rent_month) #5월 데이터를 일별로 나누기 Split
head(exdata2019_may_split[[2]]) #5월 2일에 대한 data
# rent_station   rent_time return_station return_time moving_distance member rent_month return_month
# 2466          199 2.01905e+13            227 2.01905e+13            1080      2   20190502     20190502
# 2467          123 2.01905e+13            163 2.01905e+13            1970      2   20190502     20190502
# 2468          141 2.01905e+13            152 2.01905e+13             980      2   20190502     20190502
# 2469           45 2.01905e+13             19 2.01905e+13             560      0   20190502     20190502
# 2470           35 2.01905e+13             16 2.01905e+13            1730      0   20190502     20190502
# 2471          182 2.01905e+13             60 2.01905e+13            2960      2   20190502     20190502
length(exdata2019_may_split)
# [1] 31
for (idx in 1:length(exdata2019_may_split)){ #CASE2로 구현하였습니다.
  print(sprintf("%d일. 31일까지!",idx))
  # split_day <-data.frame()
  # split_day_grp <-data.frame()
  # split_day_split <-data.frame()
  # temp_df <- data.frame() # for문 지역변수이므로 초기화 필요없습니다.
  temp_df <- data.frame(return_station=c(1:261)) #temp_df 초기화
  
  split_day <-exdata2019_may_split[[idx]]

  split_day_grp <- split_day %>% 
    group_by(return_station,rent_station) %>% 
    summarise(n=n()) 
  
  split_day_split <-split(split_day_grp,split_day_grp$rent_station)
  
  
  for (list_idx in 1:length(split_day_split)){
    temp_df <-left_join(temp_df,split_day_split[[list_idx]][c(1,3)],by="return_station")
  }
  temp_df <- temp_df[-1] # join을 위한 key return_station 삭제
  names(temp_df)<-c(1:length(names(temp_df)))# 열 이름 개수만큼 1부터 naming
  write.csv(temp_df,file=paste(idx,".csv"))
}







#이하 타슈 정거장 이름을 가로 세로에 매핑하고 싶었는데 나중에 시간날때 하려고 합니다.===================================
getwd()
library(readxl)
tasu_station_raw <- read_excel("C:/R/RStudio/Projects/rawdata/tasustation.xlsx",col_names=c("station_name","station_addr"))
tasu_station <- tasu_station_raw 
str(tasu_station)
# tibble [262 x 2] (S3: tbl_df/tbl/data.frame)
# $ station_name: chr [1:262] "1.무역전시관입구(택시승강장)" "2.대전컨벤션센터" "3.한밭수목원1" "4.초원아파트(104동 버스정류장)" ...
# $ station_addr: chr [1:262] "유성구 도룡동 3-8" "유성구 도룡동 4-19" "서구 만년동 396" "서구 만년동 401" ...

head(tasu_station)
# # A tibble: 6 x 2
# station_name                   station_addr        
# <chr>                          <chr>               
# 1 1.무역전시관입구(택시승강장)   유성구 도룡동 3-8   
# 2 2.대전컨벤션센터               유성구 도룡동 4-19  
# 3 3.한밭수목원1                  서구 만년동 396     
# 4 4.초원아파트(104동 버스정류장) 서구 만년동 401     
# 5 5.둔산대공원 입구(버스정류장)  서구 둔산2동 1521-10
# 6 6.백합네거리(농협)             서구 월평2동 266  
tail(tasu_station)
# # A tibble: 6 x 2
# station_name                      station_addr       
# <chr>                             <chr>              
# 1 257.대전테크노파크                유성구 용산동 605  
# 2 258.천문대입구                    유성구 신성동 458  
# 3 259.대덕대학교                    유성구 장동 48     
# 4 260.오정농수산물 도매시장         대덕구 오정동 45-1 
# 5 261.도로교통공단(건너편 라도무스) 유성구 원신흥동 608
# 6 262.반석 더샵                     유성구 반석동 704 

nrow(tasu_station)
# substr(tasu_station$station_name,
# tasu_station$station_num <- 
test <- strsplit(tasu_station$station_name[1],"\\.")
class(test)
class(test[1])
class(test[[1]])
test[[1]]
class(test[[1]][1])
View(test)
# install.packages("stringr")

library(stringr)
# re_name <- strsplit(tasu_station$station_name,"\\.")[[1]]
# re_name
tasu_split <- str_split(tasu_station$station_name,"\\.",n=2, simplify = TRUE) #simplify = TRUE matrix로
tasu_station$return_station <- tasu_split[,1] 
tasu_station$station <- tasu_split[,2] 
head(tasu_station)
tasu_station_re<-tasu_station[-1]
head(tasu_station_re)
str(tasu_station_re)
# tibble [262 x 3] (S3: tbl_df/tbl/data.frame)
# $ station_addr: chr [1:262] "유성구 도룡동 3-8" "유성구 도룡동 4-19" "서구 만년동 396" "서구 만년동 401" ...
# $ station_num : chr [1:262] "1" "2" "3" "4" ...
# $ station     : chr [1:262] "무역전시관입구(택시승강장)" "대전컨벤션센터" "한밭수목원1" "초원아파트(104동 버스정류장)" ...
tasu_station_re$return_station <- as.numeric(tasu_station_re$return_station)
str(tasu_station_re)
# tibble [262 x 3] (S3: tbl_df/tbl/data.frame)
# $ station_addr  : chr [1:262] "유성구 도룡동 3-8" "유성구 도룡동 4-19" "서구 만년동 396" "서구 만년동 401" ...
# $ return_station: num [1:262] 1 2 3 4 5 6 7 8 9 10 ...
# $ station       : chr [1:262] "무역전시관입구(택시승강장)" "대전컨벤션센터" "한밭수목원1" "초원아파트(104동 버스정류장)" ...
temp_df2 <-left_join(tasu_station_re,temp_df2,by="return_station")
View(temp_df2)

tasu_station_re_row <-t(tasu_station_re)
View(tasu_station_re_row)
tasu_station_re_row[3,]

names(temp_df2)[-1]<-tasu_station_re_row[3,]

names(temp_df2)<-c(1:length(names(temp_df2)))# 열 이름 개수만큼 1부터 naming