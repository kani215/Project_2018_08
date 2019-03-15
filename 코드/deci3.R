
Train<-read.csv("Train_korea.csv")
Test<-read.csv("Test_korea.csv")
Train<-Train[,c(7,8,16,17)]
Test<-Test[,c(7,8,16,17)]
str(Test)
str(Train)


library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

# 1. 모델링 
m.rpart <- rpart(ac_attend~screen+act_point+attend,data=Train)



# 2. 시각화 model design
rpart.plot(m.rpart, digits=3)

#----digits는 숫자의 자리 수 fallen.leves는 잎 노드의 정렬, type과 extra는 결정과 노드가 레이블 되는 방식에 영향을 준다

rpart.plot(m.rpart, digits=4, fallen.leaves = T, type=3, extra =101)


#3. 상관 관계 평가
p.rpart <-predict(m.rpart, Test)
summary(p.rpart)
cor(p.rpart, Test$ac_attend)



#4. 모델링과 RMSE 지수

#결측치 처리 
Train2 <- Train
Test2 <- Test

Train2$ac_attend[is.na(Train2$ac_attend)] <-0
Train2$attend[is.na(Train2$attend)] <-0
Train2$screen[is.na(Train2$screen)] <-0
Train2$act_point[is.na(Train2$act_point)] <-0
Train2$profit_line[is.na(Train2$profit_line)] <-0

Test2$ac_attend[is.na(Test2$ac_attend)] <-0
Test2$attend[is.na(Test2$attend)] <-0
Test2$screen[is.na(Test2$screen)] <-0
Test2$act_point[is.na(Test2$act_point)] <-0
Test2$profit_line[is.na(Test2$profit_line)] <-0

str(Test2)
Train2
Test2

model<-randomForest(ac_attend~.,data=Train2,importance=TRUE)
randomforest_pred<-predict(model, Test2)

RMSE<-sqrt(mean(Test2$ac_attend - randomforest_pred)^2)
RMSE


#5. 모델 평가 작업 - 정의역 변수 1개 데이터 값  대입 후 실제 누적관객수와 비교 
# ex. 실제 영화인 검은사제들의 데이터값을 대입 
Test3 <-c(5440000)
Test3 <-cbind(Test3,1109)
Test3 <-cbind(Test3,0)
Test3 <-cbind(Test3,2175495)
colnames(Test3)<-c("ac_attend","screen","act_point","attend")
Test3
Test3<-as.data.table(Test3)

randomforest_pred<-predict(model, Test3)      
randomforest_pred

RMSE<-sqrt(mean(Test3$ac_attend - randomforest_pred)^2)
RMSE































































