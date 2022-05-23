setwd('C:\\Users\\USER\\Desktop\\주제분석\\카트')
library(data.table)
library(tidyverse)
library(ggplot2)
library(dplyr)

data = fread('유저별 선호 스피드.csv')
data$speed = data$speed %>% as.character()
data %>%
  ggplot(aes(x=speed,y=n,fill = n,color=n))+
  geom_bar(stat="identity",alpha=0.6)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title.y = element_blank())+
  scale_fill_gradient(high = "#B3A068", low = "#FCE292", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = '#B3A068', low = "#FCE292", guide = "colourbar", aesthetics = "color")+
  xlab('스피드')

data = fread('최종.csv')
data %>% group_by(ne) %>% summarise(n=n()) %>% 
  ggplot(aes(x=ne,y=n,fill = n,color=n))+
  geom_bar(stat="identity",alpha=0.6)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title.y = element_blank())+
  scale_fill_gradient(high = "#AF7CCC", low = "#ECD5E3", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = '#AF7CCC', low = "#ECD5E3", guide = "colourbar", aesthetics = "color")+
  xlab('인원수')
  

data = fread('YJSR.csv')
data %>% group_by(challenge_Ratio_score_categ) %>% summarise(n=n()) %>% 
  ggplot(aes(x=challenge_Ratio_score_categ,y=n,fill = n,color=n))+
  geom_bar(stat="identity",alpha=0.6)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title.y = element_blank())+
  scale_fill_gradient(high = "#5D6B8C", low = "#DBE6FF", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = '#5D6B8C', low = "#DBE6FF", guide = "colourbar", aesthetics = "color")+
  xlab('도전')

data %>% group_by(speed_ratio_categ) %>% summarise(n=n()) %>% 
  ggplot(aes(x=speed_ratio_categ,y=n,fill = n,color=n))+
  geom_bar(stat="identity",alpha=0.6)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title.y = element_blank())+
  scale_fill_gradient(high = "#8A8F6A", low = "#D2D9A5", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = '#8A8F6A', low = "#D2D9A5", guide = "colourbar", aesthetics = "color")+
  xlab('Speed/Item')

data %>% group_by(team_ratio_categ) %>% summarise(n=n()) %>% 
  ggplot(aes(x=team_ratio_categ,y=n,fill = n,color=n))+
  geom_bar(stat="identity",alpha=0.6)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title.y = element_blank())+
  scale_fill_gradient(high = "#8F6D6C", low = "#FFDCDB", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = '#8F6D6C', low = "#FFDCDB", guide = "colourbar", aesthetics = "color")+
  xlab('Team/Solo')

data = fread('data_please.csv')
data %>% str
data = data %>% distinct

level_data = data %>% filter(level <= 108 )
level_data$level = level_data$level %>% as.numeric()
level_data$level[level_data$level >= 0 & level_data$level <= 30] <- '장갑'
level_data$level[level_data$level > 30 & level_data$level <= 60] <- '별장갑'
level_data$level[level_data$level > 60 & level_data$level <= 90] <-  '스타'
level_data$level[level_data$level > 90 & level_data$level <= 99] <-  '메달'
level_data$level[level_data$level >= 100 & level_data$level <= 108] <-  '메달'
level_data$level %>% unique


account_level = level_data %>% group_by(accountNo,level) %>% summarize(n=n())

#무작정 더하기

al2 = account_level %>% group_by(level) %>% summarize(n2=sum(n))
al2$n = c(47129,44619,21437,51240)
al2$n3 = al2$n2/al2$n

al2 %>% ggplot(aes(x=reorder(level,-n2),y=n2,fill = n2,color=n2)) +
  geom_bar(stat="identity",alpha=0.6)

#평균
al2 %>% ggplot(aes(x=reorder(level,-n3),y=n3,fill = n3,color=n3)) +
  geom_bar(stat="identity",alpha=0.6)

#계정개수
al2 %>% ggplot(aes(x=reorder(level,-n),y=n,fill = n,color=n)) +
  geom_bar(stat="identity",alpha=0.6)


level_data$time = level_data$endTime-level_data$startTime
time_l = level_data %>% group_by(accountNo,level) %>% summarise(n=sum(time))
time_l$n = as.numeric(time_l$n)
time_l = time_l %>% group_by(level) %>% summarise(n2=sum(n))
time_l$n3 = c(47129,44619,21437,51240)
time_l$time_week = time_l$n2/time_l$n3
time_l

#일주일 1인당 평균 플레이 시간 => PlayTime X
time_l %>% ggplot(aes(x=reorder(level,-time_week),y=time_week,fill = time_week,color=time_week)) +
  geom_bar(stat="identity",alpha=0.6)

#일주일 1인당 평균 플레이 시간 => PlayTime ㅇ
time_l2 = level_data %>% group_by(level) %>% summarise(n=sum(playTime))
time_l2$n2 = c(47129,44619,21437,51240)
time_l2$n3 = time_l2$n / time_l2$n2
time_l2 %>% ggplot(aes(x=reorder(level,-n3),y=n3,fill = n3,color=n3)) +
  geom_bar(stat="identity",alpha=0.6)



#아이템전
level_data %>% group_by(level,Speed_Item) %>% summarise(n=n()) %>% 
  ggplot(aes(x=reorder(level,-n),y=n,fill = n,color=n))+
  geom_bar(stat="identity",position='fill',alpha=0.6)


#2 플레이타임으로 나누자
level_data %>% 









#1주차
data = data[order(-data$`아이템 팀전`,)]


new = data[1:50]
new2 = data[1:50]
new3 = data[1:50]
new4 = data[1:50]

new %>% ggplot(aes(x=reorder(맵이름,-`스피드 개인전`),y=`스피드 개인전`,fill = `스피드 개인전`,color=`스피드 개인전`)) +
  geom_bar(stat="identity",alpha=0.6) +
  theme_classic()+
  scale_fill_gradient(high = "#4E98B8", low = "#DAF0F0", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#4E98B8", low = "#DAF0F0", guide = "colourbar", aesthetics = "color")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label=`스피드 개인전`), vjust=-0.4)

new2 %>% ggplot(aes(x=reorder(맵이름,-`스피드 팀전`),y=`스피드 팀전`,fill = `스피드 팀전`,color=`스피드 팀전`)) +
  geom_bar(stat="identity",alpha=0.6) +
  theme_classic()+
  scale_fill_gradient(high = "#4E98B8", low = "#DAF0F0", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#4E98B8", low = "#DAF0F0", guide = "colourbar", aesthetics = "color")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label=`스피드 팀전`), vjust=-0.4)

new3 %>% ggplot(aes(x=reorder(맵이름,-`아이템 개인전`),y=`아이템 개인전`,fill = `아이템 개인전`,color=`아이템 개인전`)) +
  geom_bar(stat="identity",alpha=0.6) +
  theme_classic()+
  scale_fill_gradient(high = "#E2928D", low = "#FADEE1", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#E2928D", low = "#FADEE1", guide = "colourbar", aesthetics = "color")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label=`아이템 개인전`), vjust=-0.4)

new4 %>% ggplot(aes(x=reorder(맵이름,-`아이템 팀전`),y=`아이템 팀전`,fill = `아이템 팀전`,color=`아이템 팀전`)) +
  geom_bar(stat="identity",alpha=0.6) +
  theme_classic()+
  scale_fill_gradient(high = "#E2928D", low = "#FADEE1", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#E2928D", low = "#FADEE1", guide = "colourbar", aesthetics = "color")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label=`아이템 팀전`), vjust=-0.4)
  
#--------------------------------------------------------
data = fread('final.csv')
data = data[order(-data$`리타이어율`,)]


new = data[1:50]
new2 = data[1:50]
new3 = data[1:50]
new4 = data[1:50]
new5 = data[1:20]

new %>% filter(`스개`>=0.24) %>% ggplot(aes(x=reorder(맵이름,-`스개`),y=`스개`,fill = `스개`,color=`스개`)) +
  geom_bar(stat="identity",alpha=0.6) +
  theme_classic()+
  scale_fill_gradient(high = "#9E47F5", low = "#F2C6F5", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#9E47F5", low = "#F2C6F5", guide = "colourbar", aesthetics = "color")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label=`스개`), vjust=-0.4)

new2 %>% filter(`스팀`>=0.22) %>% ggplot(aes(x=reorder(맵이름,-`스팀`),y=`스팀`,fill = `스팀`,color=`스팀`)) +
  geom_bar(stat="identity",alpha=0.6) +
  theme_classic()+
  scale_fill_gradient(high = "#9E47F5", low = "#F2C6F5", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#9E47F5", low = "#F2C6F5", guide = "colourbar", aesthetics = "color")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label=`스팀`), vjust=-0.4)

new3 %>% filter(`아개`>=0.23) %>% ggplot(aes(x=reorder(맵이름,-`아개`),y=`아개`,fill = `아개`,color=`아개`)) +
  geom_bar(stat="identity",alpha=0.5) +
  theme_classic()+
  scale_fill_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "color")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label=`아개`), vjust=-0.4)

new4 %>% filter(`아팀`>=0.28) %>% ggplot(aes(x=reorder(맵이름,-`아팀`),y=`아팀`,fill = `아팀`,color=`아팀`)) +
  geom_bar(stat="identity",alpha=0.5) +
  theme_classic()+
  scale_fill_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "color")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label=`아개`), vjust=-0.4)


new4 %>% ggplot(aes(x=reorder(맵이름,-`리타이어율`),y=`리타이어율`,fill = `리타이어율`,color=`리타이어율`)) +
  geom_bar(stat="identity",alpha=0.5) +
  theme_classic()+
  scale_fill_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "color")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label=`아개`), vjust=-0.4)

###############################################################
map = fread('map2.csv')

a = map %>% group_by(맵이름) %>% 
  summarise(n=sum(지름길개수))
b = map %>% group_by(맵이름) %>% 
  summarise(n=sum(부스터발판개수))
c = map %>% group_by(맵이름) %>% 
  summarise(n=sum(커브개수))
d = map %>% group_by(맵이름) %>% 
  summarise(n=sum(유턴개수))
z = merge(a,b,by='맵이름')
z = merge(z,c,by='맵이름')
z = merge(z,d,by='맵이름')
z
colnames(z)[2:5] <- c('지름길개수','부스터발판개수','커브개수','유턴개수')
z
write.csv(z,file="야옹.csv", row.names=TRUE)
fill = c("#ABDEE6", "#FFCCB6", "#CBAACB",'#F3B0C3')
ratio = fread('야옹.csv')
ratio %>%
  ggplot(aes(x = 맵이름, y = 비율,group = 유형, fill = 유형)) +
  geom_bar(stat='identity',alpha = 0.8)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  scale_fill_manual(values=fill)
  
###########################################################33
data = fread('final_map.csv')
str(data)
data %>% group_by(난이도,랩) %>% summarise(n=n()) %>% 
  ggplot(aes(x=난이도,y=n,fill=랩,color=랩))+
  geom_bar(stat="identity",alpha=0.8)+
  theme_classic()+
  scale_fill_gradient(high = "#9E47F5", low = "#F2C6F5", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#9E47F5", low = "#F2C6F5", guide = "colourbar", aesthetics = "color")

s = data %>% group_by(난이도) %>% summarise(스피드=sum(스피드))
i = data %>% group_by(난이도) %>% summarise(아이템=sum(아이템))
z = merge(s,i,by='난이도')
z

si = fread('난이도 스피드 아이템.csv')
si %>% ggplot(aes(x=난이도,y=합계,fill=합계,color=합계))+
  geom_bar(stat="identity",alpha=0.5)+
  theme_minimal()+
  scale_fill_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "color")+
  facet_wrap(~종류)+ylab('합계')+
  theme(strip.background = element_rect(colour="#00DBA1", fill="#A7F5DE"))+
  geom_text(aes(label=`합계`), vjust=-0.4)
###############
s = data %>% group_by(랩) %>% summarise(스피드=sum(스피드))
i = data %>% group_by(랩) %>% summarise(아이템=sum(아이템))
z = merge(s,i,by='랩')
z


si = fread('랩 스피드 아이템.csv')
si %>% ggplot(aes(x=as.factor(랩),y=합계,fill=합계,color=합계))+
  geom_bar(stat="identity",alpha=0.5)+
  theme_minimal()+
  scale_fill_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "fill") +
  scale_color_gradient(high = "#00DBA1", low = "#A7F5DE", guide = "colourbar", aesthetics = "color")+
  facet_wrap(~종류)+ylab('합계')+
  theme(strip.background = element_rect(colour="#00DBA1", fill="#A7F5DE"))+
  geom_text(aes(label=`합계`), vjust=-0.4)+
  xlab('랩')

##
me = fread('메롱.csv')
a = me %>% group_by(맵이름) %>% summarise(평균 = mean(`스피드 개인전`,na.rm=TRUE))
b = me %>% group_by(맵이름) %>% summarise(평균 = mean(`스피드 팀전`,na.rm=TRUE))
c = me %>% group_by(맵이름) %>% summarise(평균 = mean(`아이템 개인전`,na.rm=TRUE))
d = me %>% group_by(맵이름) %>% summarise(평균 = mean(`아이템 팀전`,na.rm=TRUE))
q = merge(a,b,by='맵이름')
q = merge(q,c,by='맵이름')
q = merge(q,d,by='맵이름')
colnames(q)[2:5] <- c('스피드개인전','스피드팀전','아이템개인전','아이템팀전')
write.csv(q,file="피카.csv", row.names=FALSE)

yo = fread('피카.csv')
yo %>% ggplot(aes(x=종류,y=평균,fill=종류,color=종류))+
  geom_bar(stat="identity",alpha=0.5)+
  theme_minimal()+
  facet_wrap(~맵이름)+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4),
        strip.background = element_rect(colour="#FAF1D6", fill="#FAF1D6"))


####################################3
a = data %>% group_by(난이도) %>% summarise(트랙길이평균 = mean(`트랙 길이(km)`))
b = data %>% group_by(난이도) %>% summarise(커브개수평균 = mean(커브개수))
c = data %>% group_by(난이도) %>% summarise(유턴개수평균 = mean(유턴개수))
d = data %>% group_by(난이도) %>% summarise(지름길평균 = mean(지름길개수))
e = data %>% group_by(난이도) %>% summarise(부스터발판개수평균 = mean(부스터발판개수))

h = merge(a,b, by='난이도')
h = merge(h,c, by='난이도')
h = merge(h,d, by='난이도')
h = merge(h,e, by='난이도')
h

colnames(data)

ya = fread('난이도별 트랙 특징.csv')
ya %>% ggplot(aes(x=난이도,y=평균,fill=as.factor(난이도),color=as.factor(난이도))) +
  geom_bar(stat='identity',alpha = 0.3)+
  theme_minimal()+
  facet_wrap(~종류)+
  theme(strip.background = element_rect(colour="#FFEFFF", fill="#FFEFFF"))


####################################
fu = data %>% filter(특징 == "very hard") %>% 
  group_by(난이도) %>% summarise(n=n())
fu
