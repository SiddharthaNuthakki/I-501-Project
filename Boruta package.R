install.packages("Boruta")
library(Boruta)

setwd("/Users/nuthakki/Desktop/I501 project")
getwd()

traindata <- read.csv("/Users/nuthakki/Desktop/Team2_factorvariables.csv", header = T, stringsAsFactors = F)
attach(traindata)
str(traindata)
set.seed(123)
#With Alcohol, heroin & COCAINE/CRACK
boruta.train <- Boruta(SUB1~.-X, data = traindata, doTrace = 2)
#HERFLG-METHFLG-PCPFLG-HALLFLG-AMPHFLG-STIMFLG-TRNQFLG-BARBFLG-INHFLG-OTCFLG-ALCFLG-COKEFLG
#boruta.train <- Boruta(SUB1~ SUB2, data = traindata, doTrace = 2)
print(boruta.train)

getSelectedAttributes(boruta.train, withTentative = T)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

#Logistic regression
model <- glm(SUB1 ~. -X-HERFLG-METHFLG-PCPFLG-HALLFLG-AMPHFLG-STIMFLG-TRNQFLG-BARBFLG-INHFLG-OTCFLG-ALCFLG-COKEFLG, data=traindata)
summary(model)

