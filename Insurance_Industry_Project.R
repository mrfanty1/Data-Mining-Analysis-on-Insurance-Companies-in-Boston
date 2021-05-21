insurance <- read.csv("insurance.csv")
View(insurance)

library(dplyr)
insurance1 <- filter(insurance, smoker == "no")

library(ggplot2)
ggplot(data=insurance1, aes(x=age, y=charges)) + geom_point()

library(MASS)
cor(insurance1$age, insurance1$charges, use="complete.obs")

set.seed(90)
insurance2 <- sample_n(insurance1, 1064)
train <- slice(insurance2, 1:638)
valid <- slice(insurance2, 639:1064)

insurance3 <- lm(charges~age, data=train)
summary(insurance3)

install.packages("forecast")
library(forecast)
insurancetrain <- predict(insurance3,train)
insurancevalid <- predict(insurance3,valid)
accuracy(insurancetrain,train$charges)
accuracy(insurancevalid,valid$charges)

library(GGally)
train1 <- dplyr::select(train, age, bmi, children, charges)
ggpairs(train1)

install.packages("caret")
library(caret)

dummy1<-dummyVars(~sex,data=train,fullRank = T)
train$sex=predict(dummy1, train)
dummytrain1<- lm(charges~sex, data=train)
summary(dummytrain1)

dummy2<-dummyVars(~region,data=train,fullRank = T)
train$region=predict(dummy2, train)
dummytrain2<- lm(charges~region, data=train)
summary(dummytrain2)

dummy3<- dummyVars(~sex, data=valid, fullRank = T)
valid$sex=predict(dummy3,valid)
dummyvalid1<- lm(charges~sex, data=valid)
summary(dummyvalid1)

dummy4<- dummyVars(~region, data=valid, fullRank = T)
valid$region=predict(dummy4,valid)
dummyvalid2<- lm(charges~region, data=valid)
summary(dummyvalid2)

insurance5<- lm(charges~age+sex+bmi+children+region, data=train)
summary(insurance5)
insurance6<- step(insurance5,direction = "backward")
summary(insurance6)

insurance7<- lm(charges~age+children, data=train)
summary(insurance7)

insurance8 <- lm(charges~age+children, data=train)
insurancetrain1 <- predict(insurance8,train)
insurancevalid1 <- predict(insurance8,valid)
accuracy(insurancetrain1,train$charges)
accuracy(insurancevalid1,valid$charges)

