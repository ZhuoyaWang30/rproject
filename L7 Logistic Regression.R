delays.df <- read.csv("FlightDelays.csv")

#Let's check out what kinds of variables we have
str(delays.df)    #If the ourput gives factor which means categorical data
View(delays.df)

#transform to a factor
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK)
#check values (levels) of categorical data can take on
levels(delays.df$DAY_WEEK)
#rename the levels in order becuase 1 2 3...7 are not meaningful
levels(delays.df$DAY_WEEK) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
#by dividing 100, we want get the hour part of the departure time
#we want to bin and categorize CRS_DEP_TIME into hourly intervals between 6 AM and 10 PM. 
#Since CRS_DEP_TIME is a number with the form  1430, we take the hour part by division and rounding.
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))
delays.df$CRS_DEP_TIME

#create reference category or set up base level(dummy variable)
#optional
delays.df$ORIGIN <- relevel(delays.df$ORIGIN, ref = "IAD" )
delays.df$DEST <- relevel(delays.df$DEST, ref = "LGA")
delays.df$CARRIER <- relevel(delays.df$CARRIER, ref = "US")
delays.df$DAY_WEEK <- relevel(delays.df$DAY_WEEK, ref = "Mon")

#Re-code the target variable to indicate which target value is associated with Class 1.
delays.df$Flight.Status <- as.numeric(delays.df$Flight.Status == "delayed")
delays.df$Flight.Status
View(delays.df)   #data form after the transformation

selected.var <- c(10,1,8,4,2,9,13)
selected.df <- delays.df[ , selected.var]
set.seed(5)   #have to before the sample part
train.index <- sample(1:nrow(delays.df), nrow(delays.df)*0.6)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

#Step 4: Fit a logistic regression model
#The function glm is for generalized linear models (logistic regression is one of the generalized linear models). 
#We specify that the outcome is binary by setting family = "binomial".
#run logistic model, and show coefficients 
options("scipen"=100, "digits"=4)
logit.reg <- glm(Flight.Status ~ ., data = train.df, family = "binomial")
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

pred <- ifelse(logit.reg.pred > 0.5, 1, 0)

library(caret)
confusionMatrix(factor(pred), factor(valid.df$Flight.Status), positive = "1")


library(pROC)
r <- roc(valid.df$Flight.Status, logit.reg.pred )
plot.roc(r)

#find the best threhold
coords(r, x = "best")

coords(r, x = c(0.1, 0.2, 0.5 ))

#inclass practice
bank.df <- read.csv("UniversalBank.csv")
str(bank.df)   
View(bank.df)

#transform to a factor
bank.df$Education <- factor(bank.df$Education)
#check values (levels) of categorical data can take on
levels(bank.df$Education)
#rename the levels in order becuase 1 2 3...7 are not meaningful
levels(bank.df$Education) <- c("Undergrad","Graduate","Advanced/Professional")
bank.df$Education <- relevel(bank.df$Education, ref = "Advanced/Professional" )


#5
24/(24+60)
#6


