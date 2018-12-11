#1-5
flight.df <- read.csv("FlightDelay.csv")
set.seed(88)
View(flight.df)
library(caret)
selected.var <- c(1,2,4,5,6,7,8,9)
tar.df <- flight.df[, selected.var]
a <- nrow(tar.df)
train.index <- sample(1:a, a*0.7)
train.df <- tar.df[train.index, ]
valid.df <- tar.df[-train.index, ]

library(rpart)
library(rpart.plot)
default.ct <- rpart(Flight.Status ~ ., data = train.df, method = "class")
prp(default.ct,type=1, extra = 1)
default.ct.point.pred <- predict(default.ct, valid.df, type = "class" )

library(ggplot2)
confusionMatrix(default.ct.point.pred, factor(valid.df$Flight.Status))

tr <- rpart(Flight.Status ~ ., data = train.df, method = "class")
new.df <- data.frame(CRS_DEP_TIME = "17_18", CARRIER = "CO", DEST = 'EWR', DISTANCE = 199, ORIGIN = 'DCA',
                     Weather = 'No' , DAY_WEEK = 'Tue')
pred <- predict(tr, new.df, type = "class")
pred
#6
flight.df <- read.csv("FlightDelay.csv")
set.seed(88)
library(caret)
newselected.var <- c(1,2,4,5,6,8,9)
new.tar.df <- flight.df[, newselected.var]
b <- nrow(new.tar.df)
newtrain.index <- sample(1:b, b*0.7)
train2.df <- new.tar.df[newtrain.index, ]
valid2.df <- new.tar.df[-newtrain.index, ]
library(rpart)
library(rpart.plot)
new.ct <- rpart(Flight.Status ~ ., data = train2.df, method = "class")
prp(new.ct,type=1, extra = 1 )
default2.ct.point.pred <- predict(default2.ct, valid2.df, type = "class" )

library(ggplot2)