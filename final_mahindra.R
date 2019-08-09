#------------------------------------------------------------------------------
#Club Mahindra Data Olympics
#------------------------------------------------------------------------------

# load required libraries

load.libraries <- c('data.table', 'dplyr', 'MASS', 'dummies','car','caTools',
                    'pls','lubridate', 'stringr', 'tidyr', 'ggplot2',
                    'ridge','glmnet','h2o')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)

# Read dataset - train and test
train <- fread('train.csv',na.strings = c('',NA))
test <- fread('test.csv', na.strings = c('',NA))

# Preliminary analysis
head(train)
str(train)
summary(train)
dim(train) # 341424 * 24

head(test)
str(test)
summary(test)
dim(test) # 146765 * 23

# Let's merge train and test to begin with data prep.
# Add a new column is_train to differentiate between train and test records
train$is_train <- TRUE
test$is_train <- FALSE
test$amount_spent_per_room_night_scaled <- 0

dataset <- rbind(train,test)

#Missing values - state_code_residence - 1.43 % , seasoned holiday code = 0.03 

sapply(dataset, function(x) sum(is.na(x))/nrow(dataset)*100)

# duplicate rows - none
uniqueN(dataset)

# ------------------------ univariate analysis -------------------------

# reservation_id - unique_ID (seems like encrypted)
# Will drop this column before model building
uniqueN(dataset$reservation_id)
head(dataset$reservation_id)

dataset$reservation_id <- NULL

# booking_date - 
#convert to yyyy-mm-dd format
dataset$booking_date_conv <- dmy(dataset$booking_date)

summary(dataset[which(is_train == T)]$booking_date_conv)
summary(dataset[which(is_train == F)]$booking_date_conv)
# Max 2019-03-01 and min 2014-09-03

#check_in_date
dataset$checkin_date_conv <- dmy(dataset$checkin_date)

summary(dataset[which(is_train == T)]$checkin_date_conv)
summary(dataset[which(is_train == F)]$checkin_date_conv)
#Max 2019-03-01 Min 2012 -03-08

# seems like there are error records where checkin date < booking_date
# 14 rows 
dataset[which(dataset$checkin_date_conv<dataset$booking_date_conv),c('booking_date_conv','checkin_date_conv','checkout_date')]

dataset$is_weekend <- ifelse((weekdays(dataset$checkin_date_conv) %in% c('Friday','Saturday','Sunday')),1,0)
dataset$is_weekend <- as.factor(dataset$is_weekend)
#dataset$diff_book_checkin <-  as.integer(dataset$checkin_date_conv - dataset$booking_date_conv)
# let's set negative diff as 0
#dataset$diff_book_checkin <- ifelse(dataset$diff_book_checkin < 0, 0, dataset$diff_book_checkin)

# Let's remove booking_date_conv and booking_date and checkin_date for model building
dataset$booking_date <- NULL
dataset$checkin_date <- NULL
dataset$booking_date_conv <- NULL

#checkout date
dataset$checkout_date_conv <- dmy(dataset$checkout_date)

summary(dataset$checkout_date_conv)
# Min 2012-03-11 and max 2019-03-03

# Derived variable - no_of_days_stay
dataset$no_of_days_booked <- as.integer(dataset$checkout_date_conv - dataset$checkin_date_conv)
summary(dataset$no_of_days_booked)
summary(factor(dataset$no_of_days_booked))

# Let's remove checkin checkout date too
dataset$checkout_date <- NULL

# derived variable season - based on checkin date
#dataset[which(lubridate::month(dataset$checkin_date_conv) %in% c(4,5,6)),'season'] <- 'summer'
#dataset[which(lubridate::month(dataset$checkin_date_conv) %in% c(7,8,9)),'season'] <- 'monsoon'
#dataset[which(lubridate::month(dataset$checkin_date_conv) %in% c(10,11,12,1)),'season'] <- 'winter'
#dataset[which(lubridate::month(dataset$checkin_date_conv) %in% c(2,3)),'season'] <- 'spring'

dataset$season <- as.factor(dataset$season)

# winter - max booking ,spring - least booking
ggplot(data = dataset,aes(x = season)) + geom_bar(fill= 'blue',color = 'black')

dataset$checkin_date_conv <- NULL
dataset$checkout_date_conv <- NULL

# channel_code - 1,2,3
summary(factor(dataset$channel_code))
dataset$channel_code <- as.factor(dataset$channel_code)

# channel -1 - most of the bookings
ggplot(data = dataset,aes(x = factor(dataset$channel_code))) + geom_bar(fill= 'blue',color = 'black')

# main_product_code - categorical values as 1,2,3,4,7
summary(factor(dataset$main_product_code))
dataset$main_product_code <- as.factor(dataset$main_product_code)

ggplot(data = dataset,aes(x = factor(dataset$main_product_code))) + geom_bar(fill= 'blue',color = 'black')

# persontravellingid - categorical 45,46,47,4752,4753,4995
summary(factor(dataset$persontravellingid))
dataset$persontravellingid <- as.factor(dataset$persontravellingid)

ggplot(data = dataset,aes(x = persontravellingid)) + geom_bar(fill= 'blue',color = 'black')

# resort_region_code - categorical 1,2,3
summary(factor(dataset$resort_region_code))
dataset$resort_region_code <- as.factor(dataset$resort_region_code)

ggplot(data = dataset,aes(x = factor(dataset$resort_region_code))) + geom_bar(fill= 'blue',color = 'black')

# resort_type_code - categorical 0,1,2,3,4,5,7
summary(factor(dataset$resort_type_code))
dataset$resort_type_code <- as.factor(dataset$resort_type_code)

ggplot(data = dataset,aes(x = factor(dataset$resort_type_code))) + geom_bar(fill= 'blue',color = 'black')

# room_type_booked_code - categorical 1,2,3,4,5,6
summary(factor(dataset$room_type_booked_code))
dataset$room_type_booked_code <- as.factor(dataset$room_type_booked_code)

ggplot(data = dataset,aes(x = factor(dataset$room_type_booked_code))) + geom_bar(fill= 'blue',color = 'black')

#room nights - error record found -45
summary(factor(dataset$roomnights))
dataset[which(dataset$roomnights < 0),]
# huge mismatch in roomnights and difference between check in and check out dates
nrow(dataset[which(dataset$roomnights != dataset$no_of_days_stay),])/nrow(dataset)

#cor(dataset$roomnights,dataset$no_of_days_stay)

# derived variable - diff_roomnights_noofdaysstay
#dataset$extra_stay <- dataset$roomnights - dataset$no_of_days_booked 

#let's drop roomnights as we have no_of_days_booked and extra stay
dataset$roomnights <- NULL

#seasoned holiday code - categorical 1,2,3,4, NA
# replace NA with a new code 0
summary(factor(dataset$season_holidayed_code))

dataset[which(is.na(dataset$season_holidayed_code)),'season_holidayed_code'] <- 0
dataset$season_holidayed_code <- as.factor(dataset$season_holidayed_code)

ggplot(data = dataset,aes(x = factor(dataset$season_holidayed_code))) + geom_bar(fill= 'blue',color = 'black')

# state_code_residence
summary(factor(dataset$state_code_residence))

ggplot(data = dataset,aes(x = factor(dataset$state_code_residence))) + geom_bar(fill= 'blue',color = 'black')

sum(is.na(dataset$state_code_residence)) # 7024
uniqueN(dataset$state_code_residence) # 38
median(dataset$state_code_residence,na.rm= T)

# let's impute NA with with median
dataset[which(is.na(dataset$state_code_residence)),'state_code_residence'] <- 8

dataset$state_code_residence <- as.factor(dataset$state_code_residence)

# state_code_resort - categorical 1,2,3,4,5,6,7,8,9,10,11,13
summary(factor(dataset$state_code_resort))
dataset$state_code_resort <- as.factor(dataset$state_code_resort)

ggplot(data = dataset,aes(x = factor(dataset$state_code_resort))) + geom_bar(fill= 'blue',color = 'black')

# number of adults
summary(factor(dataset$numberofadults))

#number of children
summary(factor(dataset$numberofchildren))
#total_pax
summary(factor(dataset$total_pax))

# total_pax_booked
#dataset$total_pax_booked <- dataset$numberofadults+dataset$numberofchildren

##dataset$extra_pax <- (dataset$numberofadults+dataset$numberofchildren) - dataset$total_pax

summary(dataset$extra_pax)
dataset$extra_pax <- ifelse(dataset$extra_pax < 0,0,dataset$extra_pax)

# let's drop total_pax
dataset$total_pax <- NULL
dataset$extra_pax <- NULL
#dataset$numberofadults <- NULL
#dataset$numberofchildren <- NULL

# member_age_buckets - Ato J
summary(factor(dataset$member_age_buckets))
dataset$member_age_buckets <- as.factor(dataset$member_age_buckets)

ggplot(data = dataset,aes(x = member_age_buckets)) + geom_bar(fill= 'blue',color = 'black')

#booking_type_code -  categorical 1,2
summary(factor(dataset$booking_type_code))
dataset$booking_type_code <- as.factor(dataset$booking_type_code)

ggplot(data = dataset,aes(x = factor(dataset$booking_type_code))) + geom_bar(fill= 'blue',color = 'black')


# member_id
summary(factor(dataset$memberid))
uniqueN(dataset$memberid)

# its encrypted , let's decrypt it for our convience.
dataset$member_id_conv <- as.integer(factor(dataset$memberid, ordered = TRUE))

# lets' remove member id
dataset$memberid <- NULL

# cluster_code - categorical A to F
summary(factor(dataset$cluster_code))
dataset$cluster_code <- as.factor(dataset$cluster_code)

ggplot(data = dataset,aes(x = cluster_code)) + geom_bar(fill= 'blue',color = 'black')

# reservationstatuscode_id - categorical - Ato D
summary(factor(dataset$reservationstatusid_code))
dataset$reservationstatusid_code <- as.factor(dataset$reservationstatusid_code)

ggplot(data = dataset,aes(x = reservationstatusid_code)) + geom_bar(fill= 'blue',color = 'black')

# resort id
summary(factor(dataset$resort_id))
# its encrypted , let's decrypt it for our convience.
dataset$resort_id_conv <- as.integer(factor(dataset$resort_id, ordered = TRUE))
# 32 unique values
uniqueN(dataset$resort_id_conv)

#dataset$resort_id_conv <- as.factor(dataset$resort_id_conv)

# lets' remove resort id
dataset$resort_id <- NULL
##dataset$resort_id_conv <- NULL
#dataset$member_id_conv <- NULL

summary(train$amount_spent_per_room_night_scaled)
str(dataset)

# dummy variables to convert categorical columns(5,12,14,15,19) to one hot encoding
dummies <-
  data.frame(sapply(dataset[,c(5,12,14,15,19)], function(x)
    data.frame(
      model.matrix( ~ x - 1, data = dataset[,c(5,12,14,15,19)])
    )[, -1]))

# combine dummies , numeric and output column together
dataset_linear <- cbind(dummies,scale(dataset[,-c(5,12,14,15,19,16)]),dataset$is_train)

str(dataset_linear)

# correlation plot
corrplot::corrplot(corr = cor(dataset[,-c(5,12,14,15,19,16)]))

# split train and test from merge dataset

train_data <- dataset[which(dataset$is_train == 1),]
train_data$is_train <- NULL

#train_data <- cbind(train_data,train$amount_spent_per_room_night_scaled)

setnames(train_data,'train$amount_spent_per_room_night_scaled',
         'amount_spent_per_room_night_scaled')

test_data <- dataset_linear[which(dataset_linear$`dataset$is_train` != TRUE),]
test_data$is_train <- NULL

#------------------Data Split into train and Val set -----------------
set.seed(10)

# backup copy before split
#train_data_indices <- train_data
?sample.split
#split_indices <- sample.split(train_data, SplitRatio = 0.7)

#train_data <- train_data_indices[rep(split_indices),]

#val_data <- train_data_indices[!split_indices,]

#------------------- -Linear regression ------------------------------
model_1 <- lm(data = train_data,formula = amount_spent_per_room_night_scaled~.)
summary(model_1)

model_2 <- stepAIC(model_1, direction = "both")

summary(model_2)
vif(model_2)

# remove member_age_buckets.xH
model_3 <- lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                persontravellingid.x4995 + member_age_buckets.xC + member_age_buckets.xD + 
                member_age_buckets.xE + member_age_buckets.xF + 
                member_age_buckets.xI + member_age_buckets.xJ + cluster_code.xB + 
                cluster_code.xC + cluster_code.xD + cluster_code.xE + cluster_code.xF + 
                reservationstatusid_code.xC + season.xspring + season.xsummer + 
                season.xwinter + channel_code + main_product_code + numberofadults + 
                numberofchildren + resort_region_code + resort_type_code + 
                room_type_booked_code + season_holidayed_code + state_code_residence + 
                state_code_resort + booking_type_code + diff_book_checkin + 
                no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
              data = train_data)

summary(model_3)
vif(model_3)

# remove clustercode xe
model_4 <-  lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                 persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                 persontravellingid.x4995 + member_age_buckets.xC + member_age_buckets.xD + 
                 member_age_buckets.xE + member_age_buckets.xF + 
                 member_age_buckets.xI + member_age_buckets.xJ + cluster_code.xB + 
                 cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                 reservationstatusid_code.xC + season.xspring + season.xsummer + 
                 season.xwinter + channel_code + main_product_code + numberofadults + 
                 numberofchildren + resort_region_code + resort_type_code + 
                 room_type_booked_code + season_holidayed_code + state_code_residence + 
                 state_code_resort + booking_type_code + diff_book_checkin + 
                 no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
               data = train_data)
summary(model_4)
vif(model_4)

# remove reservationstatusid_code.xC
model_5 <- lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                persontravellingid.x4995 + member_age_buckets.xC + member_age_buckets.xD + 
                member_age_buckets.xE + member_age_buckets.xF + 
                member_age_buckets.xI + member_age_buckets.xJ + cluster_code.xB + 
                cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                 season.xspring + season.xsummer + 
                season.xwinter + channel_code + main_product_code + numberofadults + 
                numberofchildren + resort_region_code + resort_type_code + 
                room_type_booked_code + season_holidayed_code + state_code_residence + 
                state_code_resort + booking_type_code + diff_book_checkin + 
                no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
              data = train_data)
summary(model_5)

#remove seasonxwinter

model_6 <- lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                persontravellingid.x4995 + member_age_buckets.xC + member_age_buckets.xD + 
                member_age_buckets.xE + member_age_buckets.xF + 
                member_age_buckets.xI + member_age_buckets.xJ + cluster_code.xB + 
                cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                season.xspring + season.xsummer + 
                 channel_code + main_product_code + numberofadults + 
                numberofchildren + resort_region_code + resort_type_code + 
                room_type_booked_code + season_holidayed_code + state_code_residence + 
                state_code_resort + booking_type_code + diff_book_checkin + 
                no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
              data = train_data)
summary(model_6)

#member_age_buckets.x
model_7 <- lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                persontravellingid.x4995 + member_age_buckets.xC + member_age_buckets.xD + 
                member_age_buckets.xE + 
                member_age_buckets.xI + member_age_buckets.xJ + cluster_code.xB + 
                cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                season.xspring + season.xsummer + 
                channel_code + main_product_code + numberofadults + 
                numberofchildren + resort_region_code + resort_type_code + 
                room_type_booked_code + season_holidayed_code + state_code_residence + 
                state_code_resort + booking_type_code + diff_book_checkin + 
                no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
              data = train_data)
summary(model_7)

# member_age_buckets.xJ
model_8 <- lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                persontravellingid.x4995 + member_age_buckets.xC + member_age_buckets.xD + 
                member_age_buckets.xE + 
                member_age_buckets.xI + cluster_code.xB + 
                cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                season.xspring + season.xsummer + 
                channel_code + main_product_code + numberofadults + 
                numberofchildren + resort_region_code + resort_type_code + 
                room_type_booked_code + season_holidayed_code + state_code_residence + 
                state_code_resort + booking_type_code + diff_book_checkin + 
                no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
              data = train_data)
summary(model_8)

#persontravellingid.x4995
model_9 <- lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                member_age_buckets.xC + member_age_buckets.xD + 
                member_age_buckets.xE + 
                member_age_buckets.xI + cluster_code.xB + 
                cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                season.xspring + season.xsummer + 
                channel_code + main_product_code + numberofadults + 
                numberofchildren + resort_region_code + resort_type_code + 
                room_type_booked_code + season_holidayed_code + state_code_residence + 
                state_code_resort + booking_type_code + diff_book_checkin + 
                no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
              data = train_data)
summary(model_9)

#bookingtypecode
model_10 <- lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                member_age_buckets.xC + member_age_buckets.xD + 
                member_age_buckets.xE + 
                member_age_buckets.xI + cluster_code.xB + 
                cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                season.xspring + season.xsummer + 
                channel_code + main_product_code + numberofadults + 
                numberofchildren + resort_region_code + resort_type_code + 
                room_type_booked_code + season_holidayed_code + state_code_residence + 
                state_code_resort + diff_book_checkin + 
                no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
              data = train_data)
summary(model_10)
vif(model_10)

# remove state_code_resort
model_11<- lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                 persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                 member_age_buckets.xC + member_age_buckets.xD + 
                 member_age_buckets.xE + 
                 member_age_buckets.xI + cluster_code.xB + 
                 cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                 season.xspring + season.xsummer + 
                 channel_code + main_product_code + numberofadults + 
                 numberofchildren + resort_region_code + resort_type_code + 
                 room_type_booked_code + season_holidayed_code + state_code_residence + 
                 diff_book_checkin + 
                 no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
               data = train_data)
summary(model_11)
vif(model_11)

#extra_pax
model_12<- lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                member_age_buckets.xC + member_age_buckets.xD + 
                member_age_buckets.xE + 
                member_age_buckets.xI + cluster_code.xB + 
                cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                season.xspring + season.xsummer + 
                channel_code + main_product_code + numberofadults + 
                numberofchildren + resort_region_code + resort_type_code + 
                room_type_booked_code + season_holidayed_code + state_code_residence + 
                diff_book_checkin + 
                no_of_days_booked + extra_stay + resort_id_conv, 
              data = train_data)
summary(model_12)
vif(model_12)

# Model 10 - final model - as R2 decreased for model onwards 10.
# train it on originla train data
final_regression_model <-  lm(formula = amount_spent_per_room_night_scaled ~ persontravellingid.x46 + 
                   persontravellingid.x47 + persontravellingid.x4752 + persontravellingid.x4753 + 
                   member_age_buckets.xC + member_age_buckets.xD + 
                   member_age_buckets.xE + 
                   member_age_buckets.xI + cluster_code.xB + 
                   cluster_code.xC + cluster_code.xD + cluster_code.xF + 
                   season.xspring + season.xsummer + 
                   channel_code + main_product_code + numberofadults + 
                   numberofchildren + resort_region_code + resort_type_code + 
                   room_type_booked_code + season_holidayed_code + state_code_residence + 
                   state_code_resort + diff_book_checkin + 
                   no_of_days_booked + extra_stay + extra_pax + resort_id_conv, 
                 data = train_data_indices)

summary(final_regression_model)

prediction_l_final <- predict(final_regression_model,test_data)

#write.csv(cbind(test$reservation_id,prediction_l_final), 'sub_linear.csv',row.names = F)

# 99.922755342454 RMSE on public leaderboard

#---------------------------PCR regression---------------------------------

model_p_1 <- pcr(data = train_data,formula = amount_spent_per_room_night_scaled~ . ,
                 validation = "CV")

summary(model_p_1)

prediction_p_1 <- predict(model_p_1,val_data,ncomp=40)

mse_p <- mean((val_data$amount_spent_per_room_night_scaled - prediction_p_1)^2)
print(sqrt(mse_p)*100)

# use complete train dataset to train
model_p_final <- pcr(data = train_data_indices,formula = amount_spent_per_room_night_scaled~ . ,
                     validation = "CV")
summary(model_p_final)

predict_pcr_final <- predict(model_p_final,test_data,ncomp=40)

#write.csv(cbind(test$reservation_id,predict_pcr_final), 'sub_pcr.csv',row.names = F)
# RMSE - 99.9203059391169 on public leaderboard

#-----------------Ridge regression--------------------------------------
model_r_1 <- linearRidge(amount_spent_per_room_night_scaled~.,data = train_data_indices)

#summary(model_r_1)
predict_ridge_final <- predict(model_r_1,test_data)

#write.csv(cbind(test$reservation_id,predict_ridge_final), 'sub_ridge.csv',row.names = F)
# RMSE - 99.9203689052875 on public leaderboard

#-------------------------------------Lasso regression-------------------------------

lambda <- 10^seq(10, -2, length = 100)

lasso.mod <- glmnet(as.matrix(train_data[,c(1:42)]), 
                    train_data$amount_spent_per_room_night_scaled,
                    family = "gaussian",
                    alpha = 1, 
                    lambda = lambda)

print(lasso.mod)

plot(lasso.mod,xvar="lambda",label=T)

lasso.pred <- predict(lasso.mod, s = 0.0001, newx = as.matrix(val_data[,c(1:42)]))
mean((lasso.pred-val_data$amount_spent_per_room_night_scaled)^2)
# RMSE - 103.4797 on val set

#-------------------------H20 Automl --------------------------------
# afrter trying convebtional approach of Linear regression combined with PCR, lasso and ridge.
#Let's try Auto.ml 

# initialise h2o instance
h2o.init()
train_data$member_id_conv <- NULL
train_data$resort_id_conv <- NULL
train_data$state_code_residence <- NULL

indices <- caTools::sample.split(train_data, 
                                 SplitRatio = 2/3)

rf_h2o <- h2o.gbm(y = 'amount_spent_per_room_night_scaled',
                 training_frame = as.h2o(train_data[rep(indices,341424/18),]),
                 nfolds = 5,
                 ntrees = 60,
                 max_depth = 8
                )

summary(rf_h2o)

#pred_1 <- h2o.predict(rf_h2o, as.h2o(train_data[rep(!indices,341424/18),]))

val_data  <- as.h2o(train_data[rep(!indices,341424/18),])
h2o.performance(rf_h2o,val_data) 

?h2o.performance

aml_final <- h2o.automl(y = 'amount_spent_per_room_night_scaled',
                  training_frame = as.h2o(train_data),
                  max_models = 5,
                  sort_metric = "RMSE",
                  seed = 1)


# View the AutoML Leaderboard
lb <- aml_final@leaderboard
lb

pred_auto_ml_final <- h2o.predict(aml_final, as.h2o(val_data))

h2o.performance (pred_auto_ml_final, as.h2o(val_data)) 

#write.csv(cbind(test$reservation_id, as.data.frame(pred_auto_ml_final)),'sub_final.csv',row.names = F)


# 97.2235878563442 RMSE on leaderboard

#-----------------------------------Neural Net using H2O---------------------------------------------
# initialise h2o instance
#h2o.init()

model_h2o = h2o.deeplearning(y = 'amount_spent_per_room_night_scaled', 
                             training_frame = as.h2o(train_data),
                             seed = 1,
                             sparse = T,
                             variable_importances = T,
                             standardize = T,
                             activation = 'Rectifier',
                             nfolds = 5,
                             epochs = 100)

summary(model_h2o)

test_prediction_h2o <- h2o.predict(model_h2o, newdata = as.h2o(val_data[c(1:42)]))

#write.csv(cbind(test$reservation_id,as.data.frame(test_prediction_h2o)), 'sub_3_h2o.csv',row.names = F)

h2o.performance(model_h2o, newdata = as.h2o(val_data))

# Parameter tuning for deep learning

# #set parameter space
activation_opt <- c("Rectifier","RectifierWithDropout", "Maxout","MaxoutWithDropout")
hidden_opt <- list(c(10,10),c(20,15),c(50,50,50),c(100,100))
l1_opt <- c(0,1e-3,1e-5)
l2_opt <- c(0,1e-3,1e-5)

hyper_params <- list( activation=activation_opt,
                  hidden=hidden_opt,
                      l1=l1_opt,
                 l2=l2_opt )

#set search criteria
search_criteria <- list(strategy = "RandomDiscrete", max_models=10)

#train model

h2o_train_grid <- as.h2o(train_data_indices)
dl_grid <- h2o.grid("deeplearning"
                    ,grid_id = "deep_learn_new"
                    ,hyper_params = hyper_params
                    ,search_criteria = search_criteria
                    ,training_frame = h2o_train_grid
                    ,y = 'amount_spent_per_room_night_scaled', 
                    nfolds = 5
                    ,epochs = 20)

#get best model

d_grid <- h2o.getGrid("deep_learn_new",sort_by = "RMSE")

best_dl_model <- h2o.getModel(d_grid@model_ids[[1]])

#test_prediction_h2o_grid <- h2o.predict(best_dl_model, newdata = as.h2o(val_data[c(1:42)]))
#h2o.performance (best_dl_model, as.h2o(val_data)) 

test_prediction_h2o_grid <- h2o.predict(best_dl_model, newdata = as.h2o(test_data))

write.csv(cbind(test$reservation_id,as.data.frame(test_prediction_h2o_grid)), 'sub_2_grid_h2o.csv',row.names = F)

# leaderboard rmse - 98.2911017636142
# h2o shutdown
h2o.shutdown()

Y
#---------------------------------------------------------------
# Final model selected is Stacked Ensemble

