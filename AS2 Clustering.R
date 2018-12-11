daily.df <- read.csv("dailykos.csv")
View(daily.df)

library(ggplot2)
library(caret)

set.seed(1000)
km <- kmeans(daily.df, 7 )
#1
km$size
#2
km$centers
c1 <- km$centers[1,]
c2 <- km$centers[2,]
c3 <- km$centers[3,]
c4 <- km$centers[4,]
c5 <- km$centers[5,]
c6 <- km$centers[6,]
c7 <- km$centers[7,]
c1
s1 <- sort(c1,decreasing=T)
s1
s1[1:6]
s2 <- sort(c2,decreasing=T)
s2[1:6]
s3 <- sort(c3,decreasing=T)
s3[1:6]
s4 <- sort(c4,decreasing=T)
s4[1:6]
s5 <- sort(c5,decreasing=T)
s5[1:6]
s6 <- sort(c6,decreasing=T)
s6[1:6]
s7 <- sort(c7,decreasing=T)
s7[1:6]
