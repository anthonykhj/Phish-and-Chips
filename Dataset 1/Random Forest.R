#rmb to change dataset and omit bar, training set
setwd("C:/Users/Hillary G/Downloads/archive/")
phish <- read.csv("Phishing_Legitimate_full.csv")

# change to numeric ~
for (i in seq(1,49)) {
  phish[,i]<-as.numeric(phish[,i])
}


phish <- subset(phish,select=-HttpsInHostname)

cat <- c(16:20, 24, 26, 29:33, 35:49)
for (i in cat) {
  phish[,i]<-as.factor(phish[,i])
}
str(phish)
phish[,48] <- as.factor(phish[,48])

phish <- subset(phish,select=-id) #drop id column

library(randomForest)
set.seed(2024)

RFmodel.1 <- randomForest(CLASS_LABEL~., data=phish, importance=T)

RFmodel.1

imptvar <- importance(RFmodel.1)
varImpPlot(RFmodel.1, type=1)

plot(RFmodel.1)



