#install.packages("ggplot2")

library(ggplot2)
library(dplyr)

file_1 <- file.choose()
data_1 <- read.csv(file_1)
data_1 %>% head()

# 구별 정류장 수
tb <- table(data_1$구) %>% as.data.frame()

tb   
#     Var1 Freq
# 1   동구    4
# 2   서구    7
# 3 유성구    7
# 4   중구    4

ggplot(tb) + geom_bar(aes(x=Var1, y=Freq), #aes 각각
                       stat = 'identity',
                       fill = 'skyblue')

ggplot(tb) + geom_bar(aes(x=Var1, y=Freq),
                      stat = 'identity',
                      fill = 'skyblue')+
                    coord_flip()

# 원그래프
ggplot(tb) + geom_bar(aes(x="", y=Freq,fill = Var1),
                      stat = 'identity')+
  coord_polar(theta="y")

# 히스토그램
#자료 분석_전체.csv

file_2 <- file.choose()
data_2 <- read.csv(file_2)
data_2 %>% head()

ggplot(data_2) + geom_histogram(aes(pressure),
                      bins=20, fill='orange')

# 연속형으로 -분포
ggplot(data_2) + geom_density(aes(pressure),
                                fill='orange')


# 연속형 상자

ggplot(data_2) + geom_boxplot(aes(pressure),
                              fill='orange')
ggplot(data_2) + geom_boxplot(aes(y=pressure,fill='orange')
                              )
ggplot(data_2) + geom_boxplot(aes(y=pressure),fill='orange'
)

# 연속형 바이올린
# 
# ggplot(data_2) + geom_violin(aes(x="",y=pressure),
#                               fill='orange')+
#                 geom_boxplot(width=0.1)

ggplot(data_2,aes(x="",y=pressure))+
            geom_violin(fill='orange')+
            geom_boxplot(width=0.1)

# 연속형 선그림
ggplot(data_2) + geom_line(aes(x=1:nrow(data_2),y=pressure),
                              color='orange')


# 점추가
# ggplot(data_2) + geom_line(aes(x=1:nrow(data_2),y=pressure),
#                            color='orange')+
#                   geom_point(color="green")

ggplot(data_2,aes(x=1:nrow(data_2),y=pressure))+
      geom_line(color='orange')+
        geom_point(color="green",size=0.0001)
      

## 두 변수의 탐색

file_3 <- file.choose()
data_3 <- read.csv(file_3)
data_3 %>% head()

data_3_use = data_3 %>% filter(대여스테이션<=10 & 반납스테이션<=10)
data_3_use$대여스테이션 <- data_3_use$대여스테이션 %>% factor()
data_3_use$반납스테이션 <- data_3_use$반납스테이션 %>% factor()


## 범주와 범주 - 막대 그래프 barplot

## 막대 그래프 누적
ggplot(data_3_use) +
  geom_bar(aes(x=대여스테이션, fill = 반납스테이션))

## 
ggplot(data_3_use) +
  geom_bar(aes(x=대여스테이션, fill = 반납스테이션), position = "dodge")

ggplot(data_3_use) +
  geom_bar(aes(x=대여스테이션, fill = 반납스테이션), position = "fill")


## 범주형 연속형 - 상자그림 boxplot

## 이동거리 대여스테이션

ggplot(data_3_use,aes(x=대여스테이션, y=이동거리, fill = 대여스테이션))+
    geom_boxplot()

## 바이올린


ggplot(data_3_use,aes(x=대여스테이션, y=이동거리, fill = 대여스테이션))+
  geom_violin(trim=F)


## 산점도

ggplot(data_1 , aes(x=X1월승차, y=X1월하차))+
  geom_point(size=3)

## 산점도 - 회귀선

ggplot(data_1 , aes(x=X1월승차, y=X1월하차))+
  geom_point(size=3)+
  geom_smooth(method='lm')

## 산점도 다른 데이터 태양열
## 산점도

ggplot(data_2 , aes(x=PM10, y=PM2.5))+
  geom_point(color = 'orange')


## 버블 


ggplot(data_2 , aes(x=PM10, y=PM2.5,size=value*20))+
  geom_point(color = 'orange',alpha=0.5)

## 산점도 점크기 점 색상


ggplot(data_2 , aes(x=PM10, y=PM2.5,size=value*20,color=factor(month)))+
  geom_point(alpha=0.5)


# 산점도 상관계수 행렬

library(GGally)
# install.packages('GGally')

## 연속형만 
data_2_sub <- data_2 %>%  select(PM10:SO4) %>% na.omit()

ggpairs(data_2_sub)


## 선그립 연속형 변수 추가

ggplot(data_2_sub , aes(x=1:nrow(data_2_sub)))+
  geom_line(aes(y=PM10, col = 'psavert'))+
  geom_line(aes(y=PM2.5, col = 'uempmed'))


# install.packages('ggmap')
library(ggmap)
library(dplyr)

boxLocation <- c(127.25,36.24,127.52,36.47)
krMap <- get_map(location = boxLocation, source = 'stamen', maptype='terrain')
ggmap(krMap)


data_file <- file.choose()
data_raw <- read.csv(data_file)
data_raw %>% head()


# 지도 표시

ggmap(krMap) +
  geom_point(data=data_raw, aes(경도, 위도, color=구, size=거치대))

# install.packages('plotly')
library(plotly)
library(dplyr)

file_1 = file.choose()
  
data_1 = read.csv(file_1)

data_1 %>% head()


## 범주형 막대 그래프

tb <- table(data_1$구) %>% as.data.frame()
fig <- plot_ly(tb, x=~Var1, y=~Freq, type = 'bar', color = ~Var1)
fig

# 수평

fig <- plot_ly(tb, y=~Var1, x=~Freq, type = 'bar', color = ~Var1)
fig

# 범주형 원그래프

fig <- plot_ly(tb, labels= ~Var1, values=~Freq, type = 'pie')
fig

#연속형 히스토그램

file_2 <- file.choose()
data_2 <- read.csv(file_2)
data_2 %>% head()


## 히스토그램


fig <- plot_ly(data_2, x=~pressure, type = 'histogram', nbinsx=20)
fig

#연속형 분포 density

p <- ggplot(data_2)+
  geom_density(aes(pressure), fill='skyblue')

ggplotly(p)


# 연속형 상자그림

fig <- plot_ly(data_2, y=~pressure, type = 'box')
fig

# 연속형 바이올린 그림
fig <- plot_ly(data_2, y=~pressure, type = 'violin',box=list(visible = T))
fig

# 연속형 선그림
fig <- plot_ly(data_2, y=~pressure, type = 'scatter',mode='lines')
fig

## 선 그림 점추가
fig <- plot_ly(data_2, y=~pressure, type = 'scatter',mode='lines+markers')
fig

### 두변수의 탐색

file_3 <- file.choose()
data_3 <- read.csv(file_3)
data_3 %>% head()


# 자료추출
data_3_use = data_3 %>% filter(대여스테이션<=10 & 반납스테이션<=10)
data_3_use$대여스테이션 <- data_3_use$대여스테이션 %>% factor(levels=1:10)
data_3_use$반납스테이션 <- data_3_use$반납스테이션 %>% factor(levels=1:10)

## 범주형 범주형 
# 막대그래프 누적

p <- ggplot(data_3_use)+
  geom_bar(aes(x=대여스테이션, fill = 반납스테이션))

fig <- ggplotly(p)
fig

## 막대그래프

p <- ggplot(data_3_use)+
  geom_bar(aes(x=대여스테이션, fill = 반납스테이션))

fig <- ggplotly(p)
fig


## 막대그래프

p <- ggplot(data_3_use)+
  geom_bar(aes(x=대여스테이션, fill = 반납스테이션),position='fill')

fig <- ggplotly(p)
fig

# 범주 연속 - 상자그림

fig <- plot_ly(data_3_use, x = ~대여스테이션, y=~이동거리, 
              type = 'box', split = ~대여스테이션)
fig

#범주 연속 바이올린 

fig <- plot_ly(data_3_use, x = ~대여스테이션, y=~이동거리, 
               type = 'violin', split = ~대여스테이션)
fig

# 연속형 연속형 - 산점도

fig <- plot_ly(data = data_1, x = ~X1월승차, y=~X1월하차, 
               type = 'scatter',mode='markers')
fig

# 산점도 회귀 추가

# 
# fig <- plot_ly(data = data_1, x = ~X1월승차, y=~X1월하차, 
#                type = 'scatter',mode='markers')

fit.value <- lm(X1월하차~X1월승차,data= data_1) %>% fitted.values()
fig <- fig %>% 
  add_trace( x=~X1월승차, y = fit.value, mode = 'lines') %>% 
  layout(showlegend = F)

fig


#산점도

fig <- plot_ly(data = data_2, x=~PM10, y=~PM2.5,
               type = 'scatter', mode = 'markers')
fig


## 다중 변수
# 버블 차트 : 산점도 + 점크기
