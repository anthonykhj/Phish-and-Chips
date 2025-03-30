# Load necessary libraries
library(data.table)
library(ggplot2)
library(caTools)
library(dplyr)
library(car)

# Load the data
data <- read.csv("C:/Users/Anthony/Documents/NTU/Y2S2/BC2407 Project/Phishing_Legitimate_full.csv")
summary(data)
sum(is.na(data)) # Checking for null values
table(data$CLASS_LABEL) # Checking data imbalance

# Remove the id, HttpsInHostname
data <- select(data, -id, -HttpsInHostname)
dim(data)

# Factor Y variable
data$CLASS_LABEL <- as.factor(data$CLASS_LABEL)

# Factor categorical variables
data$NoHttps <- as.factor(data$NoHttps)
data$RandomString <- as.factor(data$RandomString)
data$IpAddress <- as.factor(data$IpAddress)
data$DomainInSubdomains <- as.factor(data$DomainInSubdomains)
data$DomainInPaths <- as.factor(data$DomainInPaths)
data$DoubleSlashInPath <- as.factor(data$DoubleSlashInPath)
data$EmbeddedBrandName <- as.factor(data$EmbeddedBrandName)
data$ExtFavicon <- as.factor(data$ExtFavicon)
data$InsecureForms <- as.factor(data$InsecureForms)
data$RelativeFormAction <- as.factor(data$RelativeFormAction)
data$ExtFormAction <- as.factor(data$ExtFormAction)
data$AbnormalFormAction <- as.factor(data$AbnormalFormAction)
data$FrequentDomainNameMismatch <- as.factor(data$FrequentDomainNameMismatch)
data$FakeLinkInStatusBar <- as.factor(data$FakeLinkInStatusBar)
data$RightClickDisabled <- as.factor(data$RightClickDisabled)
data$PopUpWindow <- as.factor(data$PopUpWindow)
data$SubmitInfoToEmail <- as.factor(data$SubmitInfoToEmail)
data$IframeOrFrame <- as.factor(data$IframeOrFrame)
data$MissingTitle <- as.factor(data$MissingTitle)
data$ImagesOnlyInForm <- as.factor(data$ImagesOnlyInForm)

str(data)

# Data Exploration - Correlation between X variables and CLASS_LABEL
numerical_data <- data[sapply(data, is.numeric)]
correlations <- sapply(numerical_data, function(x) cor(x, as.numeric(data$CLASS_LABEL)))
correlations

# Assuming 'correlations' contains the correlation coefficients
correlation_data <- data.frame(variable = names(correlations), correlation = correlations)

# Plotting
ggplot(correlation_data, aes(x = variable, y = correlation)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Correlation of continuous X variables with CLASS_LABEL",
       x = "Variables",
       y = "Correlation Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Logistic Regression using Backwards Elimination
set.seed(123)
# 70% trainset
train = sample.split(Y = data$CLASS_LABEL, SplitRatio = 0.7)
trainset = subset(data, train == T)
testset = subset(data, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$CLASS_LABEL)
summary(testset$CLASS_LABEL)

# Develop model on trainset
train.m1 = glm(CLASS_LABEL ~ ., family = binomial, data = trainset)
summary(train.m1)

vif(train.m1)
# Continuous Variables with VIF > 5 and Categorical Variables with VIF > 2: UrlLength, NumQueryComponents, NumAmpersand, HostnameLength, PathLength, QueryLength, PctExtHyperlinks, PctExtResourceUrls, ExtFormAction, AbnormalFormAction, PctNullSelfRedirectHyperlinks, AbnormalExtFormActionR, PctExtNullSelfRedirectHyperlinksRT 

# Refitting logistic regression model
train.m2 = glm(CLASS_LABEL ~ . - UrlLength - NumQueryComponents - NumAmpersand - HostnameLength - PathLength - QueryLength - PctExtHyperlinks - PctExtResourceUrls - ExtFormAction - AbnormalFormAction - PctNullSelfRedirectHyperlinks - AbnormalExtFormActionR - PctExtNullSelfRedirectHyperlinksRT, family = binomial, data = trainset)

summary(train.m2)

# Backwards Elimination
train.BE = step(train.m2)
summary(train.BE)

vif(train.BE)
# VIF < 5 for continuous variable

train.m3 = glm(CLASS_LABEL ~ NumDots + SubdomainLevel + PathLevel + NumDash + NumDashInHostname + TildeSymbol +
           NumPercent + NoHttps + DomainInPaths + NumSensitiveWords + ExtFavicon + InsecureForms +
           RelativeFormAction + FrequentDomainNameMismatch + RightClickDisabled + SubmitInfoToEmail +
           IframeOrFrame + MissingTitle + UrlLengthRT + PctExtResourceUrlsRT + ExtMetaScriptLinkRT,
         family = binomial, data = trainset)

summary(train.m3)

OR = exp(coef(train.m3))
OR

OR.CI = exp(confint(train.m3))
OR.CI

# Train set confusion matrix
threshold1 = 0.5

prob.train = predict(train.m3, type = 'response')
m4.predict.train = ifelse(prob.train > threshold1, "1", "0")
table3 = table(Trainset.Actual = trainset$CLASS_LABEL, m4.predict.train, deparse.level = 2)
table3
round(prop.table(table3),3)
# Overall Accuracy
mean(m4.predict.train == trainset$CLASS_LABEL)

# Confusion Matrix on Testset
prob.test = predict(train.m3, newdata = testset, type = 'response')
m4.predict.test = ifelse(prob.test > threshold1, "1", "0")
table4 = table(Testset.Actual = testset$CLASS_LABEL, m4.predict.test, deparse.level = 2)
table4
round(prop.table(table4), 3)
# Overall Accuracy
mean(m4.predict.test == testset$CLASS_LABEL)

