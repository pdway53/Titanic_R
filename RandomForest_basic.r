library(mice)
library(randomForest)
library("Hmisc")


train <- read.csv('train.csv', stringsAsFactors = F)
test <- read.csv('test.csv', stringsAsFactors = F)

full  <- rbind(train[,-2], test)


# Passengers 62 and 830 are missing Embarkment   ===================================
#Method 1 
#fare is related with Pclass and age 
#other_embark <- full(filter(PassengerID != 62 & PassengerID != 830))
other_embark <- data[-c(62, 830),]
ggplot(other_embark, aes(x = Embarked, y = Fare , fill = factor(Pclass) )) + geom_boxplot()

#Passenger 62 Pclass = 1 and Fare = 80  infer that he probably embarked from C
#passenger 830 Pclass =1 and Fare = 80  infer that he probably embark from C as well 

#Method 2 predict with random forest
emf <- randomForest(as.factor(Embarked)~Fare+Pclass,other_embark, importance = TRUE, ntree = 200 )
miss_em_predict <- predict(my_forest, full[c(62,830),])
# predict missing Embark(62,830) as (C,c) 
data$Embarked[c(62,830)] <- "C" 

#predictive missing value in age with randomforest  ====================================
pdata <- mice(full[,], method='rf')
data <- complete(pdata)
#plot age distribution
par(mfrow = c(1,2))
hist(full$Age, col = "", main = "Age of Original Data")
hist(data$Age, main = "Age of Mice predictive data")



#predict the testdata  ===================================================================
#predict with randomForest
Survived <- train$Survived
train <- cbind(data[1:891,], Survived)
test <- data[892:1309,]
write.csv(data, file="FullData.csv", row.names = FALSE)

#validation
set.seed(100)
index <- sample(nrow(data))

val <- train(751:891)
train <- train(1:750)

#random forest
forest <- randomForest( as.factor(Survived)~Pclass + Sex + Age + SibSp + Parch +Fare +Embarked,  train, importance = TRUE , ntree = 1000)
prediction <- predict(forest, newdata = test)
solution <- data.frame(PassengerID = test$PassengerId , Survived = prediction)
write.csv(solution, file = "myfirstforest.csv", row.names = FALSE)

#linear regression # cant use in this case
lm_model <- lm(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data =  train)
lm_predict <- predict(lm_model, test)
lm_solution <- data.frame(passengerID = test$PassengerId, Survived = lm_predict)
write.csv(lm_solution, file = "LinearRegression.csv", row.names = FALSE)

#logist regression
glm_model <- glm(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,family = "binomial",data =  train)
glm_predict <- predict(glm_model, test,type = "response")
glm_predict <- ifelse(glm_predict > 0.5,1,0)
glm_solution <- data.frame(passengerID = test$PassengerId, Survived = glm_predict)
write.csv(glm_solution, file = "LRegression.csv", row.names = FALSE)



