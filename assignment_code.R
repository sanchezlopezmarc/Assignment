load("~/Desktop/yelp_review_small.Rda")
load("~/Desktop/yelp_user_small.Rda")
library(jsonlite)
business<- stream_in(file("~/Desktop/yelp_academic_dataset_business.json"))

dataset_all <- merge(review_data_small, user_data_small, by="user_id", all.x=TRUE)
dataset_all <- merge(dataset_all, business, by="business_id")
library(tidyr)
library(dplyr)
dataset<-data.frame(dataset_all)
dataset <- unnest(dataset, attributes)
dataset <- unnest(dataset, hours)
dataset = subset(dataset, select = -c(1:3, 8:10, 12, 16, 17, 28, 31:37, 40, 41, 43, 45, 48:54, 56:87))
dataset <- drop_na(dataset, average_stars)
dataset <- drop_na(dataset, BusinessAcceptsCreditCards)
dataset <- drop_na(dataset, RestaurantsPriceRange2)
dataset <- drop_na(dataset, RestaurantsTakeOut)
dataset <- drop_na(dataset, RestaurantsDelivery)
dataset <- drop_na(dataset, RestaurantsReservations)

dataset$BusinessAcceptsCreditCards<-ifelse(dataset$BusinessAcceptsCreditCards, 1, 0)
dataset$RestaurantsTakeOut<-ifelse(dataset$RestaurantsTakeOut, 1, 0)
dataset$RestaurantsDelivery<-ifelse(dataset$RestaurantsDelivery, 1, 0)
dataset$RestaurantsReservations<-ifelse(dataset$RestaurantsReservations, 1, 0)

dataset$RestaurantsPriceRange2<-as.factor(dataset$RestaurantsPriceRange2)
dataset$stars.x<-as.factor(dataset$stars.x)
dataset$stars.y<-as.factor(dataset$stars.y)

dataset<-drop_na(dataset, RestaurantsDelivery)
dataset<-drop_na(dataset, RestaurantsReservations)
dataset<-drop_na(dataset, RestaurantsTakeOut)
dataset<-drop_na(dataset, BusinessAcceptsCreditCards)
dataset<-dataset %>%
  filter(dataset$RestaurantsPriceRange2 != "None")
dataset <- dataset %>%
  mutate(category_1 = ifelse(RestaurantsPriceRange2 == 1, 1, 0),
         category_2 = ifelse(RestaurantsPriceRange2 == 2, 1, 0),
         category_3 = ifelse(RestaurantsPriceRange2 == 3, 1, 0),
         category_4 = ifelse(RestaurantsPriceRange2 == 4, 1, 0))
dataset=subset(dataset, select = -c(24))

dataset<-data.frame(dataset)

library(caret)
set.seed(1)
parts = createDataPartition(dataset$stars.x, p = 0.9, list = F)
train = dataset[parts, ]
test = dataset[-parts, ]
library(adabag)
model_adaboost1<-boosting(stars.x~., data=train, boos=TRUE, mfinal=10)
predictions1<-predict(model_adaboost1, test)
print(predictions1$error)
print(model_adaboost1$importance)
print(predictions1$confusion)

library(ggplot2)
ggplot(dataset, aes(x = average_stars, y = stars.x)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Relationship between Review Stars and Average Stars Given",
       x = "Avg. Stars Given by User",
       y = "Stars of the Review")

ggplot(dataset, aes(x = stars.y, y = stars.x)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Relationship between Review Stars and Stars of the Business",
       x = "Business Stars",
       y = "Stars of the Review")

ggplot(dataset, aes(x = BusinessAcceptsCreditCards, y = stars.x)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Relationship between Review Stars and if Bus. Accepts Cards",
       x = "Business Accept Cards",
       y = "Stars of the Review")

ggplot(dataset, aes(x = RestaurantsDelivery, y = stars.x)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Relationship between Review Stars and if Bus. has Delivery",
       x = "Business has Delivery",
       y = "Stars of the Review")

dataset_model2<-data.frame(dataset_all)
dataset_model2 <- unnest(dataset_model2, attributes)
dataset_model2 <- unnest(dataset_model2, hours)
dataset_model2 = subset(dataset_model2, select = -c(1:3,5:18,20:37, 39:87))
dataset_model2 <- drop_na(dataset_model2, average_stars)
dataset_model2$stars.x<- as.factor(dataset_model2$stars.x)
dataset_model2$stars.y<-as.factor(dataset_model2$stars.y)
dataset_model2<-data.frame(dataset_model2)

set.seed(1)
parts2 = createDataPartition(dataset_model2$stars.x, p = 0.9, list = F)
train2 = dataset_model2[parts2, ]
test2 = dataset_model2[-parts2, ]
model_adaboost2<-boosting(stars.x~., data=train2, boos=TRUE, mfinal=10)
predictions2<-predict(model_adaboost2, test2)
print(predictions2$error)
print(model_adaboost2$importance)
print(predictions2$confusion)
