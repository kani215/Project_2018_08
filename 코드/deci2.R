library(caret)
library(randomForest)
library(party)
library(datasets)
library(data.table)

Train<-read.csv("Train_korea.csv")
Test<-read.csv("Test_korea.csv")
Train<-Train[,c(7,8,16,17)]
Test<-Test[,c(7,8,16,17)]
str(Test)
str(Train)

#1. 모델 결측치 처리
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
#2. formula 생성 : Y변수 연속형
# ------------------------------

party_tree <- ctree(ac_attend~screen+attend+act_point,Train2)
plot(party_tree)

#3. 모델 그래프 시각화
party_pred <- predict(party_tree,Test2,type="response")
party_pred


#4. 모델링과 RMSE 지수  

model<-randomForest(ac_attend~attend+screen,data=Train2,importance=TRUE)
randomforest_pred<-predict(model, Test3)      #x 정의역 변수를 넣으면
RMSE<-sqrt(mean(Test3$ac_attend - randomforest_pred)^2)
RMSE

#5. 모델 평가 작업 - 정의역 변수 1개 데이터 값 대입 후 실제 누적관객수와 비교 
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






