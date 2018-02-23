library(caret)
library(randomForest)

traindata <- read.csv("/Users/nuthakki/Desktop/Team2_factorvariables.csv", header = T, stringsAsFactors = F)

set.seed(123)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

rfe.train <- rfe(traindata[,2:11,13:37], traindata[,12], sizes=1:12, rfeControl=control)
rfe.train
plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:11)
predictors(rfe.train)
importance(rfe.train)
#GLM
set.seed(3456)
trainIndex <- createDataPartition(traindata$SUB1, p = .70, list = FALSE, times = 1)

Train <- traindata[ trainIndex,]
Test  <- traindata[-trainIndex,]

model <- randomForest(SUB1 ~ AGE + RACE + ROUTE1)
#+ ROUTE1 + FRSTUSE1 + SUB2 + OPSYNFLG + MTHAMFLG +  ALCDRUG

importance(model)
Test$model_prob <- predict(model, Test, type = "response")

#install.packages("magrittr")  #to insall functino %>%
#library(magrittr)

Test <- Test  %>% mutate(model_pred = 1*(model_prob > .53) + 0,
                         SUB1_binary = 1*(SUB1 == "Yes") + 0)

Test <- Test %>% mutate(accurate = 1*(model_pred == SUB1_binary))
sum(Test$accurate)/nrow(Test)

cor(traindata)