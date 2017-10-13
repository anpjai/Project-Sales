#setwd("C:\\Users\\Anubha\\Documents\\Rossman store Sales")
Train_Store <- read.csv("D:/DRIVE/NEST/WORK/UCONN/COURSES/Spring/R/Project/Saurav.csv")

library(dplyr)
# Selecting 50 stores for analysis
x<- unique(select(Train_Store,Store, CompetitionOpenSinceMonth))
x<- x[is.na(x$CompetitionOpenSinceMonth)==F,]
x<- unique(x$Store)
y<- sample(x,50,replace = F)

new_train<- Train_Store[Train_Store$Store%in% y, ]

# Data Cleaning
p<- seq(1:52)
month<-month.name
df<- data.frame(p = p)
df$q<- cut(df$p, breaks = 12, labels = c(1:12))

new_train<- merge(new_train,df, by.x = "Promo2SinceWeek", by.y = "p", all.x = T)

# now calculate lags
new_train$comp_month<- as.Date(paste0(new_train$CompetitionOpenSinceMonth, "-", "01", "-", new_train$CompetitionOpenSinceYear ), format = '%m-%d-%Y'
)
new_train$comp_lag<- new_train$Date - new_train$comp_month


new_train$promo2_month<- as.Date(paste0(new_train$q, "-", "01", "-", new_train$Promo2SinceYear ), format = '%m-%d-%Y')
new_train$promo2_lag<- new_train$Date - new_train$promo2_month

final_data<- select(new_train, Store,Date,DayOfWeek,Sales,Open,Promo,StateHoliday,SchoolHoliday,StoreType,Assortment,CompetitionDistance,Promo2,PromoInterval,comp_lag,promo2_lag)

final_data$comp_lag<- ifelse(final_data$comp_lag<0,0,final_data$comp_lag)
final_data$promo2_lag<- ifelse(final_data$promo2_lag<0,-10,final_data$promo2_lag)
final_data$promo2_lag<- ifelse(is.na(final_data$promo2_lag)==T,-10,final_data$promo2_lag)
final_data<- select(final_data, - PromoInterval)

f1<- function(x)
{
  y<- length(which(is.na(x)==T))
  return(y)
}

na_summary<- summarise_all(final_data,f1)
class_summary<- summarise_all(final_data, class)

# Changing data to factor
final_data$DayOfWeek<- as.factor(final_data$DayOfWeek)
final_data$Store<- as.factor(final_data$Store)
final_data$Open<- as.factor(final_data$Open)
final_data$Promo<- as.factor(final_data$Promo)
final_data$StateHoliday<- as.factor(final_data$StateHoliday)
final_data$SchoolHoliday<- as.factor(final_data$SchoolHoliday)
final_data$StoreType<- as.factor(final_data$StoreType)
final_data$Assortment<- as.factor(final_data$Assortment)
final_data$Promo2<- as.factor(final_data$Promo2)

# running Panel Regresion Model
library("plm", lib.loc="~/R/win-library/3.3")

# Data prep for Panel Regression
data<- plm.data(final_data, index = c("Store","Date"))

x<- sample(seq(1:32197), .7*32197, replace = F)
train_plm<- data[x,]
test_plm<- data[-x,]


# Running Pooled Model
pool_model<- plm(Sales~ DayOfWeek+Open+Promo+ Promo2+StateHoliday+SchoolHoliday
                 +StoreType+
                 Assortment+CompetitionDistance, 
                 data = train_plm, model = "pooled")
summary(pool_model)

pool_predict<- predict(pool_model, test_plm)
mean(sqrt((pool_predict- test_plm$Sales)^2))

# running FIrst Difference Model

first_differnce<- plm(Sales~ DayOfWeek+Open+Promo+SchoolHoliday+CompetitionDistance, data = train_plm, model = "fd")
summary(first_differnce)

fd_predict<- predict(first_differnce, test_plm)
mean(sqrt((fd_predict- test_plm$Sales)^2))

# Running Fixed Effect Model
Within<- plm(Sales~ DayOfWeek+Open+Promo+SchoolHoliday+CompetitionDistance, data = train_plm, model = "within")
summary(Within)

Within_predict<- predict(Within, test_plm)
mean(sqrt((Within- test_plm$Sales)^2))

#########################################################################################
# Tree based Models

# Creating Test and Train data
x<- sample(seq(1:46180), .7*46180, replace = F)
train<- final_data[x,]
summary(train)

test<- final_data[-x,]

# Try CART
# Running Basic CART without tuning
library(rpart)
library(rpart.plot)

form <- as.formula(train$Sales ~ .)
tree.1 <- rpart(form,data =train[,-c(2)],control=rpart.control(minsplit=20,cp=0))

bestcp <- tree.1$cptable[which.min(tree.1$cptable[,"xerror"]),"CP"]
x<- as.data.frame(tree.1$cptable)

x$differ<- c(NA, diff(x$xerror))
x$per_diff<- round((x$differ/x$xerror)*100, 3)


#  Prune the tree using the best cp.
tree.pruned = prune(tree.1, cp = 6.015215e-03)
prp(tree.pruned) 
mean(sqrt((test$Sales - predict(tree.pruned, test))^2))
rpart.plot(tree.pruned, cex = .5)
summary(tree.pruned)
