install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")


library(tidyverse)
library(dplyr)
library(ggplot2)


# 1. 경로 설정 및 데이터 불러오기기 -----

setwd("C:/R")
#경로설정 

getwd()
#경로확인

titanic<-read.csv("train.csv")
#경로 안에 있는 파일 불러오기

titanic
#타이타닉 데이터

mpg
#자동차 데이터

iris
#붓꽃데이터



# 2. 이상치 확인 -----

summary(titanic)
#데이터 요약
# Age에 177가 있음을 알 수 있다.

colSums(is.na(titanic))
# S를 대문자로 사용해야 인식함
# 칼럼별 na 값의 개수를 찾음




# 2-1 이상치 처리 -----

na.omit(titanic)
#na 값이 들어가 있는 행을 전체를 제거


titanic$Age = ifelse( !is.na(titanic$Age),
                      titanic$Age,
                      round(mean(titanic$Age, na.rm=T), 2) )

outlier$weight <- ifelse(is.na(outlier$weight),
                         mean(outlier$weight, na.rm=T), 
                         outlier$weight)

# 이상치의 값을 평균으로 대체 
# mode(최빈값)
# median (중앙값)
# mean (중앙값)

titanic %>% 
  filter(!is.na(Age)) %>% 
  select(Age) %>% 
  summary()

#결측값들을 제외하고 나머지 값들로 요약통계량을 본는 것
# 평균, 중앙값, 제외한 값들과 비교해서 볼때 유용함

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
# 원래 들어가야할 값에 이상한 값이 들어가있으면 NA값으로 대체
# 그 후 위에 이상치를 처리하는 과정을 하면 됨


boxplot(titanic$Fare)
#요금 값에 매우 큰 이상치 값들이 있음을 알 수 있다.

boxplot(titanic$Fare)$stats
# 상자그래프의 통계치를 알 수 있다.

titanic$Fare <- ifelse(titanic$Fare < 0 | titanic$Fare > 52.55, NA, titanic$Fare)
# 0보다 크고 65보다 작은 값에 NA 값을 반영

mean(titanic$Fare, na.rm=T, trim=0.2)
#평균에서 상,하위 20%를 제외한 값



# 3 원하는 행 출력 및 변환 -----

head(titanic)

titanic %>% 
  filter(titanic$Sex == "male") %>% 
  head()
# 문자일경우 따옴표, 숫자일 경우 그냥

titanic %>% 
  filter(titanic$Sex == "male") %>% 
  select(-c(Name, Ticket)) %>% 
  head()
# select 함수로 빼고싶은 행을 고를 수 있음

table(mpg$class)
#칼럼의 클래스 별로 묶어서 보여줌

titanic %>% 
  group_by(Sex) %>% 
  summarise(age_mean = mean(Age,na.rm=T), age_sum = sum(Age,na.rm=T))

# Na값이 있으면 전부 NA로 나옴 mean, sum 함수 안에 na.rm=T 넣어줘야함

titanic %>% 
  summarise(age_mean = mean(Age,na.rm=T), age_sum = sum(Age,na.rm=T))

# group_by의 차이점은 전부 평균, 합
# 성별 별로 평균, 합
# #arrange(hwy_mean) 오름차순으로 정렬해줌

titanic$Sex [titanic$Sex == "male"] <- "M"
titanic$Sex [titanic$Sex == "female"] <- "F"
# 조건에 맞는 칼럼 값 변경

head(titanic)
titanic <- titanic[c(12,11,10,9,8,7,6,5,4,3,2,1)]
head(titanic)
#칼럼의 순서를 변경


# 4 원하는 데이터 시각화 -----

head(iris)

summary(iris)
# 이상치 없음

ggplot() + 
  geom_point(mapping=aes(x=Petal.Length, 
                         y=Petal.Width, 
                        color = Species,
                         shape =Species
                         ),size = 2.5,data=iris)


# aes에 넣을 수 있는 함수
# color : 매개변수들을 볼 수 있다.
# shape : 매개변수들의 모양이 달라짐
# size : 크기가 달라짐
# alpha : 투명도

# aes에 넣느냐 밖에 넣느냐 달라짐


# ------------실        습 --------------
a<-iris %>% filter(Species == 'setosa' | Species =='virginica' )
# iris를 통해 새로운 원하는 데이터 값을 추출 한다.

head(a)
ggplot()+geom_point(mapping = aes(x = Petal.Length, 
                                  y= Petal.Width, 
                                  color = Species, 
                                  shape=Species  ), 
                    size=1.5,
                    data=a)
# 추출한 결과를 가지고 시각화 한다.


head(mpg)
summary(mpg)
table(mpg$class)

# NA 값 없음

ggplot() + geom_point(mapping = aes(x))