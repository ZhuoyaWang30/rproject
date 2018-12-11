#Assignment 4
phone.df <- read.csv("phone_sale.csv")
View(phone.df)
str(phone.df)
#1
phone.df$Any_cc_miles_12mo <- factor(phone.df$Any_cc_miles_12mo)
levels(phone.df$Any_cc_miles_12mo) <- c("N","Y")      # Does the N and Y order master??????? Yes
phone.df$Any_cc_miles_12mo <- relevel(phone.df$Any_cc_miles_12mo, ref = "Y")    #????
phone.df$Phone_sale <- as.numeric(phone.df$Phone_sale == "Yes")

#2
selected.var <- c(5, 9, 10)
selected.df <- phone.df[, selected.var]
#3
set.seed(111)
train.index <- sample(1:nrow(phone.df), nrow(phone.df)*0.6)  
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]
logit.reg <- glm(Phone_sale ~ ., data = train.df, family = "binomial")    #glm &lm difference?
summary(logit.reg)
0.021423*50 - 0.593990*1 - 1.891653
e <- exp(1) 
e ^ (-1.414493)
0.2430488/1.2430488
#6
logit.reg.pred <- predict(logit.reg, valid.df,  type = "response")
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)
library(caret)
confusionMatrix(factor(pred), factor(valid.df$Phone_sale), positive = "1")
pred <- ifelse(logit.reg.pred > 0.2, 1, 0)
confusionMatrix(factor(pred), factor(valid.df$Phone_sale), positive = "1")
#7
c <- nrow(train.df[train.df$Phone_sale == "1",])/nrow(train.df)
c

