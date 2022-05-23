#Chapter1 전처리
#0번
setwd("C:\\Users\\User\\Desktop\\P-SAT\\클린업 1주차 패키지")

library(tidyverse)
library(magrittr)
library(data.table)
library(ggpubr)
library(corrplot)
library(caret)
library(Metrics)
library(plyr)

# 데이터 불러오기 
cus_info <- fread("cus_info.csv",
                  header=TRUE, 
                  data.table = FALSE,
                  stringsAsFactors = FALSE,
                  na.strings = c("_", "-"))

act_info <- fread("act_info.csv",
                  header=TRUE,
                  data.table = FALSE,
                  stringsAsFactors = FALSE)

#1번
str(cus_info) #6개의 변수, cus_id 빼고 모두 int, ivs_icn_cd에 NA가 많아보임, 10000행
str(act_info) #3개의 변수, id 두개다 chr, act_opn_ym은 int, 23959행

#2번
cus_info %>% is.na() %>% colSums()
cus_info <- cus_info[,which(colMeans(!is.na(cus_info)) > 0.6)]

act_info %>% is.na() %>% colSums()

#3번
distinct_cus <- cus_info %>% apply(2,n_distinct) 
distinct_cus

distinct_act <- act_info %>% apply(2,n_distinct) 
distinct_act

#4번
act_info %>% select(act_opn_ym) %>% unique() %>% head()

outlier_values <- boxplot.stats(act_info$act_opn_ym)$out %>% unique()
outlier_values

act_info <- act_info[-which(act_info$act_opn_ym %in% outlier_values),]
act_info %>% apply(2,n_distinct)

#5번
act_info <- act_info %>% separate(act_opn_ym, c("act_opn_yy", "act_opn_mm"), sep=4)
act_info$act_opn_yy <- as.numeric(act_info$act_opn_yy)
act_info$act_opn_mm <- as.numeric(act_info$act_opn_mm)

#6번
cus_info <- cus_info %>% mutate_if(is.integer, as.factor)
str(cus_info)

#7번
cus_info$cus_age <- cus_info$cus_age %>% 
  revalue(c("0"="10s",
            "20"="20s",
            "25"="20s",
            "30"="30s",
            "35"="30s",
            "40"="40s",
            "45"="40s",
            "50"="50s",
            "55"="50s",
            "60"="60s",
            "65"="60s",
            "70"="70s"))

#8번
cus_info$sex_dit_cd <- ifelse(cus_info$sex_dit_cd == "1", "M", "F")
head(cus_info$sex_dit_cd)

cus_info$zip_ctp_cd <- cus_info$zip_ctp_cd %>% 
  revalue(c("41"="경기",
            "11"="서울",
            "48"="경남",
            "26"="부산",
            "27"="대구",
            "47"="경북",
            "28"="인천",
            "44"="충남",
            "46"="전남",
            "30"="대전",
            "29"="광주",
            "43"="충북",
            "45"="전북",
            "42"="강원",
            "31"="울산",
            "50"="제주",
            "36"="세종"))
head(cus_info$zip_ctp_cd)

#9번
cus_info$sex_dit_cd <- as.factor(cus_info$sex_dit_cd)
str(cus_info)

#10번
data <- merge(cus_info,act_info)
str(data)

rm(cus_info)
rm(act_info)

#11번
cus_cnt <- data %>% 
  select(cus_age,cus_id) %>% 
  unique() %>% 
  group_by(cus_age) %>% 
  dplyr::summarise(cus_cnt=n())

act_cnt <- data %>% 
  select(c(cus_age,act_id)) %>% 
  unique() %>% 
  group_by(cus_age) %>% 
  dplyr::summarise(act_cnt=n())

account__cnt <- merge(cus_cnt,act_cnt) %>% 
  mutate(mean_act_cnt = act_cnt/cus_cnt)

account__cnt



#Chapter2 시각화
#1-1번
na <- data %>% is.na() %>% colSums()
na <- data.frame("variables" = names(na),
                 "nacount" = na,
                 row.names = NULL)

na %>% ggplot(aes(x = reorder(variables, -nacount), y = nacount, fill=variables))+
  geom_bar(stat="identity",alpha=0.7) +
  scale_fill_manual(values = c("skyblue","skyblue","skyblue","skyblue","skyblue","skyblue","blue","skyblue")) +
  geom_text(aes(label = c('0%','0%','0%','0.37%','3.44%','0%','0%','0%')), position = position_stack(0.5), size = 5) +
  labs(x = "변수", y = "결측치 개수")+
  ggtitle("변수별 결측치 개수 및 비율")+
  theme_bw()+
  theme(legend.title = element_blank(),
        plot.title = element_text(size=20, face="bold"))

#1-2번
year <- data %>% select(cus_age,act_opn_yy)
year$act_opn_yy <- ifelse(year$act_opn_yy == 2020, "1", "0")

year %>% ggplot(aes(x = cus_age)) +
  geom_bar(aes(fill = act_opn_yy), position = "fill") +
  scale_fill_brewer(palette = "RdPu",labels = c("2020년 이전 개설", "2020년 개설"))+
  labs(x = "연령대")+
  ggtitle("연령대별 2020년 신규개설 계좌 비율")+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.position = "none")

#2번
str(data)
one <- data %>% group_by(cus_age,act_opn_yy) %>% 
  dplyr::summarise(n=n()) %>%
  ggplot(aes(x=act_opn_yy,y=n,group=cus_age))+
  geom_line(aes(colour=cus_age),size=0.7)+
  geom_point(aes(colour=cus_age),size=2)+
  scale_colour_discrete(name="연령대")+
  labs(x = "연도", y = "신규계좌수",
       title = "연도별 신규계좌 수",
       subtitle = "연도별로 새롭게 개설된 신규 계좌 수를 연령대별로 표현한 그래프")+
  theme_light()+
  theme(plot.title = element_text(size=20, face="bold"),
        plot.subtitle = element_text(size=15, face="italic"),
        legend.text = element_text(face="italic"))

data$act_opn_mm <- data$act_opn_mm %>% as.character()
data$act_opn_mm <- data$act_opn_mm %>% 
  revalue(c("1"="1월",
            "2"="2월",
            "3"="3월",
            "4"="4월",
            "5"="5월",
            "6"="6월",
            "7"="7월",
            "8"="8월",
            "9"="9월",
            "10"="10월",
            "11"="11월",
            "12"="12월"))

two <- data %>% filter(act_opn_yy == 2020) %>% 
  group_by(cus_age,act_opn_mm,sex_dit_cd) %>% 
  dplyr::summarise(n=n()) %>%
  ggplot(aes(x=act_opn_mm,y=n,group=cus_age))+
  geom_line(aes(colour=cus_age),size=0.6)+
  geom_point(aes(colour=cus_age))+
  scale_colour_discrete(name="연령대")+
  scale_x_discrete(limits = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월"))+
  labs(y = "개설된 계좌 수",
       title = "2020년 월별 신규 계좌 수",
       subtitle = "고객 성별로 연령별 2020년 월별 신규개설 계좌 수 추이")+
  theme_gray()+
  theme(plot.title = element_text(size=20, face="bold"),
        plot.subtitle = element_text(size=15, face="italic"),
        axis.title.x = element_blank(),
        strip.background = element_rect(colour = "black",fill = "white"),
        legend.text = element_text(face="italic"))+
  facet_grid(~sex_dit_cd)

ggarrange(one, two, nrow=2,common.legend = TRUE,legend='right')

#3번
str(account__cnt)
account__cnt %>% 
  ggplot(aes(x=cus_age,y=cus_cnt))+
  geom_bar(aes(fill = cus_age,color=cus_age),stat="identity",alpha=0.4)+
  geom_line(aes(x=cus_age, y=mean_act_cnt*300),linetype = "dashed",stat="identity",color='black',size=0.5,group=1)+
  geom_point(aes(x=cus_age, y=mean_act_cnt*300,fill=cus_age),shape = 22, size = 3)+
  labs(x='연령대',y='고객 수',title='연령대별 고객 분포와 평균 계좌 개수',
       subtitle = '연령대별 계좌 개수를 고객 수로 나누어 1인당 평균계좌 개수를 구하여 연령대별 고객 분포와 함께 표현했습니다.')+
  geom_text(aes(x=cus_age,y=mean_act_cnt*300+200,label = c('1.25개','1.65개','1.92개','2.34개','2.72개','3.38개','4.35개')))+
  theme_bw()+
  theme(plot.title = element_text(size=20, face="bold"),
        plot.subtitle = element_text(size=15, face="italic"),
        legend.title = element_blank(),
        legend.position = "none")


#4번
rm(list=ls())
data(Salaries, package="carData")

#4-1번
str(Salaries)
Salaries %>% group_by(discipline) %>%
  ggplot(aes(x=yrs.since.phd, y= salary,color=discipline,shape=discipline))+
  geom_point()+
  theme_minimal()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_manual(values=c('#66C2A5','#FC8C61'))

#4-2번
Salaries %>% ggplot(aes(x=rank,y=salary,fill=rank))+
  geom_boxplot(aes(col=rank),alpha=0,lwd=1.2)+
  geom_jitter(alpha=0.5, aes(col=rank),width=0.2)+
  theme_minimal()+
  coord_flip()+
  theme(legend.position = "none")


#Chapter3 회귀분석
#1번
str(Salaries)
Salaries %>% select(yrs.since.phd,yrs.service,salary) %>% cor() %>%
  corrplot(method = 'color', addCoef.col = 'black', tl.pos = 'd',diag = FALSE,tl.col = 'black')

#2번
Salaries$sex <- relevel(Salaries$sex, ref = "Male")
levels(Salaries$sex)
lm1 <- lm(salary~sex, data = Salaries)
summary(lm1)
anova(lm1)

#3번
set.seed(2728)

train_index <- createDataPartition(Salaries$salary, p=0.7, list=FALSE)
train <- Salaries[train_index,]
test <- Salaries[-train_index,]

#4번
model <- lm(salary~., data = Salaries)
summary(model)

#5번
train_error <- sqrt(mean((train$salary - predict(model, train)) ^ 2))
test_error <- sqrt(mean((test$salary - predict(model, test)) ^ 2))

train_error
test_error
