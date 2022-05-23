##Chapter 1
#문제 0
setwd("C:\\Users\\User\\Desktop\\P-SAT\\3주차 패키지")

library(tidyverse)
library(data.table)
library(magrittr)
library(caret)
library(glmnet)
library(MLmetrics)
library(Epi)

#문제 1
train <- fread('train.csv')
test <- fread('test.csv')

str(train)
train %>% is.na %>% colSums()

str(test)
test %>% is.na %>% colSums()

#문제 2
train <- train %>% mutate_if(is.character, as.factor)
test <- test %>% mutate_if(is.character, as.factor)

#문제 3
train <- train %>% transform(child_num = as.factor(child_num),
                             FLAG_MOBIL = as.factor(FLAG_MOBIL),
                             work_phone = as.factor(work_phone),
                             phone = as.factor(phone),
                             email = as.factor(email),
                             family_size = as.factor(family_size),
                             credit = as.factor(credit))

test <- test %>% transform(child_num = as.factor(child_num),
                           FLAG_MOBIL = as.factor(FLAG_MOBIL),
                           work_phone = as.factor(work_phone),
                           phone = as.factor(phone),
                           email = as.factor(email),
                           family_size = as.factor(family_size))
train %>% str
test %>% str

#문제 4
s_train <- train %>% select(is.factor) %>% apply(2,n_distinct)

s_train <- data.frame('columns' = names(s_train),
                      's_train' = s_train,
                      row.names = NULL)

s_train %>% ggplot(aes(x = reorder(columns, s_train), y = s_train, fill = s_train)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste(s_train,'개'), color = s_train), hjust=-0.3, size = 5) + 
  scale_fill_gradient(low = "#C04848", high = "#480048") + 
  scale_color_gradient(low = "#C04848", high = "#480048") + 
  labs(x = "범주형변수", y = "level 개수") +
  theme(legend.title = element_blank(),
        legend.position = "none")+
  coord_flip() +
  theme_classic()

#문제 4-1
train <- train %>% select(-c(FLAG_MOBIL))

#문제 5
train <- train %>% mutate(AGE = round(abs(DAYS_BIRTH)/365.25)) %>% select(-c(DAYS_BIRTH))

#문제 6
train <- train %>% mutate(YEARS_EMPLOYED = round(abs(DAYS_EMPLOYED)/365.25)) %>% select(-c(DAYS_EMPLOYED))

#문제 7
test <- test %>% mutate(AGE = round(abs(DAYS_BIRTH)/365.25)) %>% select(-c(DAYS_BIRTH))
test <- test %>% mutate(YEARS_EMPLOYED = round(abs(DAYS_EMPLOYED)/365.25)) %>% select(-c(DAYS_EMPLOYED))

#문제 8
set.seed(123)
train_index <- createDataPartition(train$credit, p=0.8, list=FALSE)
train <- train[train_index,]
validation <- train[-train_index,]



##Chapter 2
#AIC 값은 낮을수록 좋음 (FM 대신 RM 선택)
#문제 1-1
model <- glm(credit~., data = train, family = binomial)
summary(model)

#문제 1-2
step_result <- step(glm(credit ~ ., family = binomial, data = train),direction = 'both',trace = F)
summary(step_result)

#문제 1-3
confint(step_result)

#문제 1-4
exp(coef(step_result))

#문제 1-5
pred <- as.factor(ifelse(predict(step_result, train, type="response") > 0.5,"1","0"))
confusionMatrix(pred, as.factor(train$credit))

#문제 1-6
p <- predict(step_result,validation,type='response')
roc <- ROC(p,form=credit~., data = validation, plot="ROC")

#문제 1-7
one <- Accuracy(ifelse(p >= 0.585,1,0),validation$credit)
two <- F1_Score(ifelse(p >= 0.585,1,0),validation$credit)

#문제 1-8
step_result <- step(glm(credit ~ ., family = binomial, data = train),direction = 'both',trace = F)
pred <- as.factor(ifelse(predict(step_result, train, type="response") > 0.5,"1","0"))
confusionMatrix(pred, as.factor(train$credit))

p_t <- predict(step_result,test,type='response')

#문제 2-1
train_x <- train %>% select(c(occyp_type, house_type, income_type,
                              family_type,edu_type, work_phone, reality,
                              phone, gender, email, car, credit))

train_x = model.matrix(credit~.,data=train_x)
train_y = train %>% select(credit) %>% unlist %>% as.vector()

validation <- validation %>% select(c(occyp_type, house_type, income_type,
                                      family_type,edu_type, work_phone, reality,
                                      phone, gender, email, car, credit))
validation_x = model.matrix(credit~.,data=validation)
validation_y = train %>% select(credit) %>% unlist %>% as.vector()

#문제 2-2
set.seed(123)
cv_fit <- cv.glmnet(train_x, train_y, alpha = 1, nfolds = 5,type.measure = "class",family = "binomial")
opt_lambda <- cv_fit$lambda.min
opt_lambda
lasso_fit = glmnet(x = train_x, y = train_y, alpha = 1, lambda = opt_lambda)

#문제 2-3
lasso_fit %>% coef()

#문제 2-4
p2 <- predict(lasso_fit,newx = validation_x)
roc2 <- ROC(p2,form=credit~., data = validation, plot="ROC")

#문제 2-5
three <- Accuracy(ifelse(p >= 0.633,1,0),validation$credit)
four <- F1_Score(ifelse(p >= 0.633,1,0),validation$credit)

#문제 2-6
test <- test %>% select(c(occyp_type, house_type, income_type,
                          family_type,edu_type, work_phone, reality,
                          phone, gender, email, car))
test_ma <- model.matrix(~ ., data = test)
p2_t <- predict(lasso_fit, s = opt_lambda ,newx = test_ma)

#문제 3-1
set.seed(123)
cv_fit <- cv.glmnet(train_x, train_y, alpha = 0, nfolds = 5,type.measure = "class",family = "binomial")
opt_lambda <- cv_fit$lambda.min
opt_lambda
ridge_fit = glmnet(x = train_x, y = train_y, alpha = 0, lambda = opt_lambda)
ridge_fit %>% coef()

#문제 3-2
p3 <- predict(ridge_fit,newx = validation_x)
roc3 <- ROC(p3,form=credit~., data = validation, plot="ROC")

#문제 3-3
five <- Accuracy(ifelse(p >= 0.639,1,0),validation$credit)
six <- F1_Score(ifelse(p >= 0.639,1,0),validation$credit)

#문제 3-4
p3_t <- predict(ridge_fit, s = opt_lambda ,newx = test_ma)

#문제 3-5
method <- c('logistic','logistic','lasso','lasso','ridge','ridge')
acc <- c('accuracy','f1 score','accuracy','f1 score','accuracy','f1 score')
value <- c(one,two,three,four,five,six)
final <- data.frame(method,acc,value)
final %>% ggplot(aes(x=method,y=value,fill=method))+
  geom_bar(stat = 'identity',alpha=0.9)+
  geom_text(aes(y=value, label=round(value,2)),vjust = -0.5)+
  facet_wrap(~acc)+
  scale_fill_brewer(palette = "Pastel1")+
  theme_light()

##Chapter 3
#문제 1번
rm(list = ls())

library(caret)
library(corrplot)
library(cluster)
library(factoextra)
library(gridExtra)

data(xclara,package="cluster")
str(xclara)

#문제 2번
corr <- cor(xclara)

corrplot(corr, addCoef.col = 'black', 
         method="shade",
         #diag=FALSE,
         tl.pos="d",
         tl.col="black")
scale_xclara = xclara %>% scale()

#문제 3번
plot1 = fviz_nbclust(scale_xclara,kmeans,method='wss')
plot2 = fviz_nbclust(scale_xclara,kmeans,method='silhouette')
grid.arrange(plot1,plot2,ncol=2)

#문제 4번
set.seed(1234)
km = kmeans(scale_xclara,centers = 3, nstart = 1, iter.max =30)
fviz_cluster(km,data=scale_xclara,geom="point") + theme_bw()

#문제 5번
plot_cluster <- xclara %>% mutate(cluster = km$cluster)

plot3 = plot_cluster %>% ggplot(aes(x=factor(cluster),y = V1,color=factor(cluster),fill=factor(cluster))) + 
  geom_boxplot(outlier.shape = NA, alpha=0.3) + 
  stat_boxplot(geom='errorbar',aes(col=factor(cluster))) +
  labs(x='cluster') + theme_classic()

plot4 = plot_cluster %>% ggplot(aes(x=factor(cluster),y = V2,color=factor(cluster),fill=factor(cluster))) + 
  geom_boxplot(outlier.shape = NA, alpha=0.3) + 
  stat_boxplot(geom='errorbar',aes(col=factor(cluster))) +
  labs(x='cluster') + theme_classic()

grid.arrange(plot3,plot4,ncol=2)
