#test variables
#1
library(ggplot2)
bigmart.df <- read.csv("bigmart.csv")
ggplot(data = bigmart.df) + geom_point(aes(x = Item_MRP, y = Item_Outlet_Sales, color = Outlet_Type), size = 1.7)
ggplot(data = bigmart.df) + geom_point(aes(x = Outlet_Type, y = Item_Outlet_Sales))
ggplot(data = bigmart.df) + geom_point(aes(x = Item_Visibility, y = Item_Outlet_Sales))
ggplot(data = bigmart.df) + geom_point(aes(x = Item_Type, y = Item_Outlet_Sales, color = Outlet_Location_Type))
ggplot(data = bigmart.df) + geom_boxplot(aes(x = Item_Fat_Content, y = Item_Outlet_Sales))

ggplot(data = bigmart.df) + geom_boxplot(aes(x = Outlet_Type, y = Item_Outlet_Sales))
ggplot(data = bigmart.df) + geom_bar(aes(x = Outlet_Size, y = Item_Outlet_Sales), stat = "summary", fun.y = "mean", bin = 100)



############################################
telephone.df <- read.csv('telephone.csv')
complete.cases(telephone.df)
x <- telephone.df[complete.cases(telephone.df),]
str(x)
newtele.df <-na.omit(telephone.df)
nrow(newtele.df)
View(newtele.df)
library(caret)
library(cowplot)
library(ggplot2)

library(magrittr)
library(dplyr)
options(repr.plot.width = 6, repr.plot.height = 4)
newtele.df %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#2166AC", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.02,vjust = -0.5, size = 5)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percentage")
options(repr.plot.width = 4, repr.plot.height = 9)
ggplot(newtele.df, aes(x=PaymentMethod, fill=SeniorCitizen)) +
  geom_bar() + scale_fill_manual(values=c("#000000","#FFFFFF"))

options(repr.plot.width = 4, repr.plot.height = 9)
plot_grid(ggplot(newtele.df, aes(x=Contract,fill=Churn))+ geom_bar())

ggplot(data = newtele.df) + geom_point(aes(x = PaymentMethod, y = SeniorCitizen))

options(repr.plot.width = 4, repr.plot.height = 9)
ggplot(newtele.df, aes(x=gender, fill=SeniorCitizen)) +
  geom_bar() + scale_fill_manual(values=c("#FF7F50","#32CD32"))

ggplot(data = newtele.df) + geom_boxplot(aes(x = Contract , y = MonthlyCharges))                                                                            
ggplot(data = newtele.df) + geom_bar(aes(x = PhoneService))

options(repr.plot.width = 4, repr.plot.height = 9)
plot_grid(ggplot(newtele.df, aes(x=Partner,fill=Churn))+ geom_bar())

ggplot(data = newtele.df) + geom_bar(aes(x = PhoneService))
options(repr.plot.width = 2, repr.plot.height = 9)

plot_grid(ggplot(newtele.df, aes(x=PaymentMethod,fill=SeniorCitizen))+ geom_bar())

newtele.df$SeniorCitizen <- factor(newtele.df$SeniorCitizen)
levels(newtele.df$SeniorCitizen) <- c("No","Yes")
levels(newtele.df$SeniorCitizen)
options(repr.plot.width = 2, repr.plot.height = 9)
plot_grid(ggplot(newtele.df, aes(x=PaymentMethod,fill=SeniorCitizen))+ geom_bar())
options(repr.plot.width = 2, repr.plot.height = 9)
plot_grid(ggplot(newtele.df, aes(x=PaymentMethod,fill=Churn))+ geom_bar())
options(repr.plot.width = 2, repr.plot.height = 9)
plot_grid(ggplot(newtele.df, aes(x=Contract,fill=Churn))+ geom_bar())

options(repr.plot.width = 2, repr.plot.height = 9)
plot_grid(ggplot(newtele.df, aes(x=Contract,fill=PaperlessBilling))+ geom_bar())

options(repr.plot.width = 2, repr.plot.height = 9)
plot_grid(ggplot(newtele.df, aes(x=Contract,fill=PaperlessBilling))+ geom_bar())

options(repr.plot.width = 2, repr.plot.height = 9)
plot_grid(ggplot(newtele.df, aes(x=PaperlessBilling,fill=Churn))+ geom_bar())

options(repr.plot.width =6, repr.plot.height = 2)
ggplot(newtele.df, aes(y= tenure, x = "Contract",fill=Churn)) + 
  geom_boxplot()+ 
  theme_bw()
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(newtele.df, aes(y= tenure, x = "", fill = Contract)) + 
  geom_boxplot()

options(repr.plot.width = 3, repr.plot.height = 2)
ggplot(newtele.df, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")


options(repr.plot.width = 3, repr.plot.height = 2)
ggplot(newtele.df, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

cor(newtele.df[ ,c("tenure", "MonthlyCharges", "TotalCharges")]
cor

    