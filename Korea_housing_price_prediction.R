library(dplyr)
library(glmnet)
library(useful)
data <- read.csv('C:/Users/bajaj/Desktop/MS/STAT/Project/Daegu_Real_Estate_data.csv')

nrow(data)
ncol(data)

## structure of data
str(data)
head(data)


#Time to BusStop
table(data$TimeToBusStop)
data$TimeToBusStop <- ifelse(data$TimeToBusStop == "0~5min",3,data$TimeToBusStop)
data$TimeToBusStop <- ifelse(data$TimeToBusStop == "10~15min",13,data$TimeToBusStop)
data$TimeToBusStop <- ifelse(data$TimeToBusStop == "5~10min",8,data$TimeToBusStop)

#TimeToSubway
table(data$TimeToSubway)
data$TimeToSubway <- ifelse(data$TimeToSubway == "0~5min",3,data$TimeToSubway)
data$TimeToSubway <- ifelse(data$TimeToSubway == "10~15min",13,data$TimeToSubway)
data$TimeToSubway <- ifelse(data$TimeToSubway == "15~20min",18,data$TimeToSubway)
data$TimeToSubway <- ifelse(data$TimeToSubway == "5~10min",8,data$TimeToSubway)
data$TimeToSubway <- ifelse(data$TimeToSubway == "no_bus_stop_nearby",60,data$TimeToSubway)


## Handling Missing values, found no missing values in data
colnames(data)[colSums(is.na(data))>0]


#####################################
######### Lasso Regression ##########
#####################################

#build a predictor matrix
houseX <- build.x(SalePrice~.,data,contrasts = FALSE)
#Response predictor
houseY <- build.y(SalePrice~.,data)
## cross validation on glmnet
house.lasso <- cv.glmnet(x=houseX,y=houseY,nfold=5)


##min lambda
house.lasso$lambda.min

house.lasso$lambda.1se

plot(house.lasso)
## Extracting coefficients
coef(house.lasso,"lambda.min")

# plot the path
plot(house.lasso$glmnet.fit,xvar='lambda' )
abline(v=log(c(house.lasso$lambda.min, house.lasso$lambda.1se)), lty=2)




########## Linear Regression############
#######################################

data1 <- data.frame(subset(data, select = -c(HeatingType,N_Parkinglot.Basement.,N_APT,
                                             N_FacilitiesNearBy.PublicOffice.,N_SchoolNearBy.University.,
                                             N_SchoolNearBy.Total.,N_FacilitiesNearBy.Total.)))

lm.fit = lm(SalePrice~., data1)

# plots
layout(matrix(c(1,2,3,4),2,2))
plot(lm.fit)

summary(lm.fit)

# calcluating predicted values
temp <- data1
temp$predicted <- predict(lm.fit)
temp$residuals <- residuals(lm.fit)

temp %>% select(SalePrice,predicted,residuals) %>% head()

