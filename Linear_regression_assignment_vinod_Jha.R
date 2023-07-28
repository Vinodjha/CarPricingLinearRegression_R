library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(MASS)
library(stringr)
#setwd("C:/Users/user/Desktop/DA/data modling/linear regression/assignment")
#reading the carprice_assignment file
c1<-read.csv("CarPrice_Assignment.csv")
#checking column names
colnames(c1)
#observing the structure
str(c1)
#checking for any duplicate row
sum(duplicated(c1)) # no rows are duplicated
#checking for any duplicate observation
sum(duplicated(c1$car_ID)) # all the obersvation id's are unique as well
sum(is.na(c1)) # No NA values either, data seems pretty much clean

c1<-c1[,-1] #car_id is removed as it won't be part of our regression model
#converting the symboling variable into factors
c1$symboling<-as.factor(c1$symboling)
head(c1$CarName)
#the varaible "CarName has the model name as well, which is to be removed. 
#keeping the company name and removing the model name
c1<-separate(c1,CarName,into=c("bname","mname"))
c1<-c1[,-which(colnames(c1)=="mname")]
levels(as.factor(c1$bname))
c1$bname<-tolower(c1$bname)
#The company name is not unique and have some spelling issues like mazda is entered as maxda in some places.
#there are porcshce and porsche, then there are toyouta and toyota, volkswagen is written in three different ways.
#some names start with capital letters as well
c1$bname<-str_replace_all(c1$bname,"maxda","mazda") #maxda is replaced with the correct name
c1$bname<-str_replace_all(c1$bname,"porcshce","porsche")
c1$bname<-str_replace_all(c1$bname,"toyouta","toyota")
c1$bname<-str_replace_all(c1$bname,"vokswagen","vw")
c1$bname<-str_replace_all(c1$bname,"volkswagen","vw")
c1$bname<-as.factor(c1$bname)
c1_chkp1<-c1
#checking for outiliers in our independent variables
boxplot(c1$highwaympg)  # no effective outiliers, as 50-60 mpg is quite reachable mileage
boxplot(c1$citympg) # same here, no outlier
boxplot(c1$peakrpm)# ~6600 rpm is not an outlier
boxplot(c1$horsepower)
max(c1$horsepower)# 288 hp is high but they exist in some high end cars
boxplot(c1$compressionratio)
max(c1$compressionratio) # compression ratio of 23 is also reachable. mostly in diesel cars
boxplot(c1$boreratio)
max(c1$boreratio) # not an outlier, vehicles are there with this boreratio
boxplot(c1$curbweight)
max(c1$curbweight) #4066 kg, not an outlier
boxplot(c1$carheight)#no outliers
boxplot(c1$carlength)
min(c1$carlength)# 141.1  .. not an outlier. 
boxplot(c1$wheelbase)
max(c1$wheelbase)# 120.9 units. high, but not an outlier. possessed by some big SUV's

#creation of dummy variables
levels(c1$fueltype) 
levels(c1$fueltype)<-c(1,0) #diesel=1, #gas=0
c1$fueltype<- as.numeric(levels(c1$fueltype)[c1$fueltype])

levels(c1$aspiration)
levels(c1$aspiration)<-c(1,0) #std=1, turbo=0
c1$aspiration<-as.numeric(levels(c1$aspiration)[c1$aspiration])

levels(c1$doornumber)
levels(c1$doornumber)<-c(1,0)# four=1, two=0
c1$doornumber<-as.numeric(levels(c1$doornumber)[c1$doornumber])

levels(c1$enginelocation)
levels(c1$enginelocation)<-c(1,0)
c1$enginelocation<-as.numeric(levels(c1$enginelocation)[c1$enginelocation])
chkp2<-c1

levels(c1$carbody)
dummy_1 <- data.frame(model.matrix( ~carbody, data = c1)) #convertible is assumed to be base of comparision
dummy_1 <- dummy_1[,-1]
c1<-cbind(c1[,-which(colnames(c1)=="carbody")],dummy_1) 

levels(c1$drivewheel) #4wd is assumed to be base of comparision
dummy_2<- data.frame(model.matrix(~drivewheel,data=c1))
dummy_2<-dummy_2[,-1]
c1<-cbind(c1[,-which(colnames(c1)=="drivewheel")],dummy_2)

levels(c1$enginetype) #dohc is cinsidered as the base of comparision
dummy_3<-data.frame(model.matrix(~enginetype,data=c1))
dummy_3<-dummy_3[,-1]
c1<-cbind(c1[,-which(colnames(c1)=="enginetype")],dummy_3)

levels(c1$cylindernumber) #"eight" is considered as the base of comparision
dummy_4<-data.frame(model.matrix(~cylindernumber,data=c1))
dummy_4<-dummy_4[,-1]
c1<-cbind(c1[,-which(colnames(c1)=="cylindernumber")],dummy_4)

levels(c1$fuelsystem) #"1bbl" is considered as the base of comparision
dummy_5<-data.frame(model.matrix(~fuelsystem,data=c1))
dummy_5<-dummy_5[,-1]
c1<-cbind(c1[,-which(colnames(c1)=="fuelsystem")],dummy_5)

levels(c1$symboling) #"-2" is considered as the base of comparision
dummy_6<-data.frame(model.matrix(~symboling,data=c1))
dummy_6<-dummy_6[,-1]
c1<-cbind(c1[,-which(colnames(c1)=="symboling")],dummy_6)

levels(c1$bname) #"alpha" is considered as the base of comparision
dummy_7<-data.frame(model.matrix(~bname,data=c1))
dummy_7<-dummy_7[,-1]
c1<-cbind(c1[,-which(colnames(c1)=="bname")],dummy_7)
# car prices are usually effected by the brand, its cc, engine power, torque, the power at which it gives the maximum torque
#type of car(SUV, Sedan etc)which is described by the widths heights and wheelbases of the car, fueltype, safety, warranty, mileage, 
#material used(interiors),drive_wheel etc. 
# all the variables except warranty and materials used for interiors are mentioned and we cannot derive these from given stats.
# looking at the data we have, we need not derive any new data.

# separating training and testing data
set.seed(100)
trainindices= sample(1:nrow(c1), 0.7*nrow(c1))
train = c1[trainindices,]
test = c1[-trainindices,]

#REGRESSION MODELS

#building our first model
model_1 <-lm(price~.,data=train)
summary(model_1)
#there are 9 NA's . there are possibily because of collinearity, hence there coefficients are coming singluar, 
#or because the the sampled data. IN the second case, it might be possible that after sampling the data into
#testing and training, all the values in training data is coming to be equal to zero for the columns in question.
#using stepAIC to reduce the number of variables

step <- stepAIC(model_1, direction="both")
step
# we have removed almost half of the insignificant varibales here. making another model with the remaining of the variables
#also the 9 variables for which coefficients were NA's are also removed.
model_2<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
                 bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
                 bnamemazda + bnamemercury + bnamemitsubishi + bnamenissan + 
                  bnameplymouth + bnamerenault + bnamesaab + 
                 bnametoyota + bnamevw, data = train)

#checking for collinearity
vif(model_2)
summary(model_2)
#here we can observe that variables with very high vif's are very significant ,so we cannot remove them. Out of the not so significant
#variables "fuelsystem2bbl" (p_value=seems to have highest vif. So this variable will be removed from our next model

model_3<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor + cylindernumberfive + cylindernumberthree + 
              symboling.1 + symboling0 + symboling3 + 
              bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
              bnamemazda + bnamemercury + bnamemitsubishi + bnamenissan + 
              bnameplymouth + bnamerenault + bnamesaab + 
              bnametoyota + bnamevw, data = train)
summary(model_3)
vif(model_3)
#"symboling0" variable which signifies symboling=0, is irrelevant with very high p value. It is to be removed from the model next
model_4<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor + cylindernumberfive + cylindernumberthree + 
              symboling.1  + symboling3 + 
              bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
              bnamemazda + bnamemercury + bnamemitsubishi + bnamenissan + 
               bnameplymouth + bnamerenault + bnamesaab + 
              bnametoyota + bnamevw, data = train)
summary(model_4)
vif(model_4)
#"symboling.1" variable which signifies symboling=-1, is irrelevant with very high p value. It is to be removed from the model next

model_5<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor + cylindernumberfive + cylindernumberthree + 
               symboling3 + 
              bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
              bnamemazda + bnamemercury + bnamemitsubishi + bnamenissan + 
               bnameplymouth + bnamerenault + bnamesaab + 
              bnametoyota + bnamevw, data = train)
summary(model_5)
vif(model_5)
#symboling3 which signifies sympboling=3 has very high p value around 10% and high vif as well
# removing it for next model

model_6<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor + cylindernumberfive + cylindernumberthree + 
              bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
              bnamemazda + bnamemercury + bnamemitsubishi + bnamenissan + 
              bnameplymouth + bnamerenault + bnamesaab + 
              bnametoyota + bnamevw, data = train)
summary(model_6)
vif(model_6)
#"bnamemercury" variable which stands for all cars with company name Mercury is a very less significant variable, though VIF is very low.
#but it won't effect my model if removed

model_7<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor + cylindernumberfive + cylindernumberthree + 
              bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
              bnamemazda  + bnamemitsubishi + bnamenissan + 
               bnameplymouth + bnamerenault + bnamesaab + 
              bnametoyota + bnamevw, data = train)
summary(model_7)
vif(model_7)
#assumptions till now have been correct as the adjusted R-squared value has not changed significantly
#removing cylindernumberfive variable from next model, which is the least significnat variable now
model_8<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor + cylindernumberthree + 
              bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
              bnamemazda  + bnamemitsubishi + bnamenissan + 
               bnameplymouth + bnamerenault + bnamesaab + 
              bnametoyota + bnamevw, data = train)
summary(model_8)
vif(model_8)
model_9<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              enginesize + stroke + peakrpm + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor + cylindernumberthree + 
              bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
              bnamemazda  + bnamemitsubishi + bnamenissan + 
              bnameplymouth + bnamerenault + bnamesaab + 
              bnametoyota + bnamevw, data = train)
summary(model_9)
vif(model_9)

#bnamesaab( brand name SAAB) is to be removed for next model, as its p-value is aprrox. 10%
model_10<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              enginesize + stroke + peakrpm + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor + cylindernumberthree + 
              bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
              bnamemazda  + bnamemitsubishi + bnamenissan + 
              bnameplymouth + bnamerenault +  
              bnametoyota + bnamevw, data = train)
summary(model_10)
vif(model_10)
#assumption was not wrong, as adjusted R-squared value is still almost same.

#enginetypeohc is to be removed next
model_11<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel + enginetypeohcf + 
               enginetyperotor + cylindernumberthree + 
               bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
               bnamemazda  + bnamemitsubishi + bnamenissan + 
                bnameplymouth + bnamerenault +  
               bnametoyota + bnamevw, data = train)
summary(model_11)
vif(model_11)
#still no significant change in adjusted R-squared
#enginetypel is to be removed next


model_12<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv  + enginetypeohcf + 
               enginetyperotor + cylindernumberthree + 
               bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
               bnamemazda  + bnamemitsubishi + bnamenissan + 
                bnameplymouth + bnamerenault +  
               bnametoyota + bnamevw, data = train)
summary(model_12)
vif(model_12)
#for the variable "cylindernumberthree" , though the vif is low bt p-value is very high
#removing it next in our new model:
model_13<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv  + enginetypeohcf + 
               enginetyperotor +  
               bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
               bnamemazda  + bnamemitsubishi + bnamenissan + 
               bnameplymouth + bnamerenault +  
               bnametoyota + bnamevw, data = train)
summary(model_13)
vif(model_13)
# p-values shows that the remainig variables are all significant. But some variables are having
#very high value of VIF. We will create a model by removing variable with highest VIF and p-value
#removing carbodysedan variable is removed from the next model
model_14<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback +  carbodywagon + drivewheelrwd + 
               enginetypedohcv  + enginetypeohcf + 
               enginetyperotor +  
               bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
               bnamemazda  + bnamemitsubishi + bnamenissan + 
               bnameplymouth + bnamerenault +  
               bnametoyota + bnamevw, data = train)
summary(model_14)
vif(model_14)
#our assumption was not wrong as adjusted R-squared value is approx. same

#removing "carbodywagon" as it has very high p-value
model_15<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback +   drivewheelrwd + 
               enginetypedohcv  + enginetypeohcf + 
               enginetyperotor +  
               bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
               bnamemazda  + bnamemitsubishi + bnamenissan + 
               bnameplymouth + bnamerenault +  
               bnametoyota + bnamevw, data = train)
summary(model_15)
vif(model_15)
#"carbodyhardtop" will be removed for having very high p-value of 19%
model_16<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + 
               carbodyhatchback +   drivewheelrwd + 
               enginetypedohcv  + enginetypeohcf + 
               enginetyperotor +  
               bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
               bnamemazda  + bnamemitsubishi + bnamenissan + 
               bnameplymouth + bnamerenault +  
               bnametoyota + bnamevw, data = train)
summary(model_16)
vif(model_16)
#"carbodyhatchback" is to be removed for having high p-value of approx 20%

model_17<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + 
                drivewheelrwd +enginetypedohcv  + enginetypeohcf + 
               enginetyperotor +  
               bnamebmw + bnamebuick + bnamedodge + bnamehonda + bnamejaguar + 
               bnamemazda  + bnamemitsubishi + bnamenissan + 
               bnameplymouth + bnamerenault +  
               bnametoyota + bnamevw, data = train)
summary(model_17)
vif(model_17)

#all the remaning variable seems significant with very low p values and VIF
#model_17 is final regression model. Lets test this on our testind data
predicted1 <- predict(model_17,test[,-which(colnames(test)=="price")])
test$test_price <- predicted1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared #0.8238 Which is a significantly high value showing high correlation between the actual
#price and predicted price.

#FINDING ERROR IN THE PREDICTED DATA
test$error<-(test$price-test$test_price)
mean(test$error) #158.41 dollars: pretty low compared to the prices
mean(test$price) #13703.55 dollars
test$id<-c(1:nrow(test))
plot(test$id,test$error,col=1, ylab="error in estimates in dollars")
lm_price<-lm(test$price~test$test_price)
abline(coef(lm_price),lwd=2)  #the graph clearly shows the prediction does not require any more variable
#as the slope of the line is already zero and the errors are spread like white noise.

#model_17 IS THE FINAL REGRESSION MODEL

#To check the consistency of the model we can apply it on another sampled data
set.seed(50)
t1<-sample(1:nrow(c1),0.25*nrow(c1))
test1<-c1[t1,]
p1<-predict(model_17,test1[,-which(colnames(test1)=="price")])
test1$pred_price<-p1
test1$error<-test1$price-test1$pred_price
mean(test1$error)# -81.18 error is consistently very low
test1$id<-c(1:nrow(test1))
plot(test1$id,test1$error,col=1, ylab="error in estimates in dollars")
lm_price<-lm(test1$price~test1$pred_price)
abline(coef(lm_price),lwd=2)  #the graph clearly shows the prediction does not require any more variable

# enginelocation and enginesize turns out to be the most significant variables.
# is negative) is highly preferred.
#then some brandnames like bmw, buick,mitsubishi are significant variables but the company is a new to 
#american market so brand name might not be a part of its price determining equation. It might be interested in 
#factors other than brand names.
#next more significant variables are aspiration, being "turbo" increases the price by 2551.80 units
#few other significant variables are engine types like rotor,ohcf etc, stroke, peak rpm,drive wheel, car width etc.
#suprisingly safety does not feature in pricing calculations as it turns out to be an insignificant variable