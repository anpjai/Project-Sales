install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)

stores =read.csv("store.csv",na.strings=c(""," ","NA"))
train =read.csv("train.csv",na.strings = TRUE)

#*****************Performing some Data preprocessing********************************************************************************************************
dataType= function(ds)
{
  ds=merge(ds,stores,by="Store")  
  ds$Month_Yr <- as.factor(format(as.Date(ds$Date), "%Y-%m"))
  ds$Yr <- as.factor(format(as.Date(ds$Date), "%Y"))
  ds$Month <- as.factor(format(as.Date(ds$Date), "%m"))
  #Convert datatype into factor variable
  ds$DayOfWeek=as.factor(ds$DayOfWeek)
  ds$Open=as.factor(ds$Open)
  ds$Promo=as.factor(ds$Promo)
  ds$SchoolHoliday=as.factor(ds$SchoolHoliday)
  ds$Month_Yr = as.factor(ds$Month_Yr)
  ds$Store=as.factor(ds$Store)
  return(ds)
}
#*****************Checking Missing values**********************************************************************************************************************
Missing = function(df){
  mresult=data.frame()
  for(each in colnames(df))
  {
    missingVal=sum(is.na(df[each]))
    data = data.frame(each,missingVal*100/nrow(df),nrow(unique(df[each])))
    mresult = rbind(mresult,data)
  }
  colnames(mresult)=c("Column Name","% Missing Values","# Unique Values")
  return(mresult)
}

#*****************Merge and Sample the Dataset**************************
merge = dataType(train)
str(merge)
#Get the Sampled  data
set.seed(123)
sub = sample(nrow(merge), floor(nrow(merge) * 0.7))
training = merge[sub, ]
testing = merge[-sub, ]
summary(training)

#*****************Ploting graphs**************************

y2=ggplot(training, aes(x = Sales)) +ggtitle("Distribution of Sales")+
  geom_histogram(aes(y = ..count..), binwidth = 500,fill="blue") +
  scale_x_continuous(name = "Distribution of Sales") +
  scale_y_continuous(name = "Count")
ggplotly(y2)

y1=ggplot(training, aes(x = Customers)) +ggtitle("Distribution of Customers")+
  geom_histogram(aes(y = ..count..), binwidth = 100,fill="blue") +
  scale_x_continuous(name = "Distribution of Customers") +
  scale_y_continuous(name = "Count")
ggplotly(y1)

#Yr By Count

ggplot(training, aes(x = Yr)) +ggtitle(label = "Plot by Year")+
  geom_bar(aes(y = ..count..), fill="blue") +
  scale_x_discrete(name = "Yr") +
  scale_y_continuous(name = "Count")

ggplot(training, aes(x = Month)) +ggtitle(label = "Plot by Month")+
  geom_bar(aes(y = ..count..), fill="blue") +
  scale_x_discrete(name = "Month") +
  scale_y_continuous(name = "Count")


#Yr By Sales

sales=ggplot(training,aes(x=training$Yr,y=training$Sales))+ggtitle(label = "Plot of Mean(Sales) by Year")+
  xlab("Yr") + ylab("Sales")+ stat_summary(fun.y="mean", geom="bar",fill="blue")
ggplotly(sales)

cust_yr=ggplot(training,aes(x=training$Yr,y=training$Customers))+ggtitle(label = "Plot of Mean(Customers) by Year")+
  xlab("Year") + ylab("Customers")+
  stat_summary(fun.y="mean", geom="bar",fill="blue")
ggplotly(cust_yr)


#Month By Sales

mon=ggplot(training,aes(x=training$Month,y=training$Sales))+ggtitle(label = "Plot of Mean(Sales) by Month")+
  xlab("Month") + ylab("Sales")+
  stat_summary(fun.y="mean", geom="bar",fill="blue")
ggplotly(mon)

#Sales BY Customer

cust=ggplot(training,aes(x=Customers,y=Sales))+ggtitle(label = "Plot of Sales by Customer")+
  xlab("Customer") + ylab("Sales")+
  geom_point(fill="blue")+geom_smooth(method = "lm")
ggplotly(cust)


#DayOfWeek

prop.table(table(training$DayOfWeek))*100
aggregate(training$Sales,list(training$DayOfWeek),mean)
x=ggplot(training,aes(x=training$DayOfWeek,y=training$Sales))+
  xlab("Day Of Week") + ylab("Sales")+ stat_summary(fun.y="mean", geom="bar",fill="blue")
ggplotly(x)

p1=ggplot(training, aes(x = DayOfWeek)) +ggtitle(label = "Bar Plot of Day Of Week")+
  geom_bar(aes(y = ..count..), fill="blue") +
  scale_x_discrete(name = "DayOfWeek") +
  scale_y_continuous(name = "Count")
ggplotly(p1)



#Promo

prop.table(table(training$Promo))*100

p1=ggplot(training, aes(x = Promo)) +
  geom_bar(aes(y = ..count..), fill="blue") +
  scale_x_discrete(name = "Promo") +
  scale_y_continuous(name = "Count")
ggplotly(p1)

p2=ggplot(training,aes(x=training$Promo,y=training$Sales))+
  xlab("Promo") + ylab("Sales")+ geom_boxplot(fill="blue")
ggplotly(p2)
p3=ggplot(training,aes(x=training$Promo,y=training$Sales))+
  xlab("Promo") + ylab("Sales")+ stat_summary(fun.y="mean", geom="bar",fill="blue")
ggplotly(p3)
p4=ggplot(training,aes(x=training$Promo,y=training$Customer))+
  xlab("Promo") + ylab("Customer")+ stat_summary(fun.y="mean", geom="bar",fill="blue")
ggplotly(p4)

aggregate(training$Sales,list(training$Promo),mean)

#Sales by Promo Interval 
prop.table(table(training$PromoInterval))*100

aggregate(training$Sales,list(training$PromoInterval),mean)

promoInt1=ggplot(training, aes(x = PromoInterval)) +
  geom_bar(aes(y = ..count..), fill="blue") +
  scale_x_discrete(name = "PromoInterval") +
  scale_y_continuous(name = "Count")

ggplotly(promoInt1)

promoInt2=ggplot(training,aes(x=training$PromoInterval,y=training$Sales))+
  xlab("PromoInterval") + ylab("Sales")+ geom_boxplot(fill="blue")
x1=ggplotly(promoInt2)

promoInt3=ggplot(training,aes(x=training$PromoInterval,y=training$Customer))+
  xlab("PromoInterval") + ylab("Customer")+ geom_boxplot(fill="blue")
x2=ggplotly(promoInt3)


#Customers by Promo

p1=ggplot(training,aes(x=training$PromoInterval,y=training$Sales))+ggtitle(label = "Plot of Mean(Sales) by Promo Interval")+
  xlab("Promo") + ylab("Sales")+
  stat_summary(fun.y="mean", geom="bar",fill="blue")
ggplotly(p1)

p2=ggplot(training,aes(x=training$PromoInterval,y=training$Customers))+ggtitle(label = "Plot of Mean(Customers) by Promo")+
  xlab("Promo") + ylab("Customers")+
  stat_summary(fun.y="mean", geom="bar",fill="blue")
ggplotly(p2)
