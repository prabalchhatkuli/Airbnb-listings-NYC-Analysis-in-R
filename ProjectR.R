setwd("C:/Users/praba/Desktop/applied statistics/project")

data <- read.csv("AB_NYC_2019.csv")

head(data)


data_r <- data[,c( "neighbourhood_group","room_type", "latitude", "longitude"
                   , "price", "minimum_nights", "reviews_per_month", "calculated_host_listings_count", "availability_365" )]

#Calculating null values in the data

colSums(is.na(data_r))

data_r[which(is.na(data_r$reviews_per_month)), ]$reviews_per_month <-0

isManhattan = as.factor(ifelse(data_r$neighbourhood_group == "Manhattan","Yes","No"))
isHome = as.factor(ifelse(data_r$room_type == "Entire home/apt", "Yes", "No"))

data_r = data.frame(data_r ,isManhattan)
data_r = data.frame(data_r ,isHome)

#Missing data for reviews_per_month

summary(data)

#Outliers in price
data_r[which(data_r$price > 5000), ]

#Only considering prices below $500 per night
data_r = data_r[-(which(data_r$price > 500)), ]

data_r= data_r[(which(data_r$price> 0)),]

data_r$price = log(data_r$price)

library(leaflet)


#Histogram of price
hist(data_r$price, xlab="log(price)", col ="gold", main="logarithm Histogram of Price")

hist(log_price, xlab="price", col ="gold", main="Histogram of Price")
#We have 3 categorical and 5 continous variables.
str(data)


#Histogram of neighbourhood
barplot(((table(data$neighbourhood_group))), col=rgb(0.8,0.5,0.6,0.6), xlab = "Neighbourhood")

barplot(((table(data$room_type))), col="orange", xlab = "Room type")

#Average price by neighbourhood
library(dplyr)
library(ggplot2)
to_plot <- data %>% group_by(neighbourhood_group) %>% summarise(Mean_Price = mean(price))

# Plotting the graph using ggplot()

ggplot(to_plot, aes(x = reorder(neighbourhood_group, -Mean_Price), y = Mean_Price)) + 
  geom_bar(stat="identity", fill = "blue") + 
  labs(title="Average Price in each Neighbourhood Group") + xlab("Neighbourhood")+ ylab("Mean Price")


#Average price by room type

to_plot <- data %>% group_by(room_type) %>% summarise(Mean_Price = mean(price))


ggplot(to_plot, aes(x = reorder(room_type, -Mean_Price), y = Mean_Price)) + 
  geom_bar(stat="identity", fill = "gold") + 
  labs(title="Average Price for each room type") + xlab("Room type")+ ylab("Mean Price")


##leaflet plot
library(leaflet)
pal <- colorFactor(palette = c("red", "green", "blue", "purple", "yellow"), domain = data_r$neighbourhood_group)

leaflet(data = data_r) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  addCircleMarkers(~longitude, ~latitude, color = ~pal(neighbourhood_group), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,
                                                                                                        label = paste("Name:", data_r$name)) %>% 
  addLegend("bottomright", pal = pal, values = ~neighbourhood_group,
            title = "Neighbourhood groups",
            opacity = 1
  )


#---------------------------------------  end of EDA  ------------------------------------------


#partitioning the data
set.seed(1)
# Sample Indexes
Index = sample(1:nrow(data_r), size = 0.7*nrow(data_r))
# Splitting Data
TrainData = data_r[Index,]
dim(TrainData)
TestData = data_r[-Index,]
dim(TestData)

######################## Data preparation##################################
x <-  model.matrix(price~., data_r)[, 10:15]   # trim off unrequired column

# leaving only the predictors
y <-  data_r$price

x_train = model.matrix(price~., TrainData)[,10:15]
x_test = model.matrix(price~., TestData)[,10:15]

y_train = TrainData$price
y_test = TestData$price

##################Ridge Regression###################################

library(glmnet)

grid = 10^seq(10, -2, length = 100)

#alpha 0 for Ridge Regression
ridge_mod = glmnet(x_train, y_train, alpha=0, lambda = grid, thresh = 1e-12)

summary(ridge_mod)

cv.out = cv.glmnet(x_train, y_train, alpha = 0) # Fit ridge regression model on training data
bestlamRidge = cv.out$lambda.min  # Select lambda that minimizes training MSE
bestlamRidge

#0.04080627

ridge_pred = predict(ridge_mod, s = bestlamRidge, newx = x_test) # Use best lambda to predict test data
summary(ridge_pred)
mean((ridge_pred - y_test)^2) # Calculate test MSE
#MSE is 0.1968215

#fitting model on full data
out = glmnet(x_train, y_train, alpha = 0) # Fit ridge regression model on full dataset
predict(out, type = "coefficients", s = bestlamRidge)

########################lasso###################################################
#alpha=1 for lasso
lasso_mod = glmnet(x_train, 
                   y_train, 
                   alpha = 1,
                   lambda = grid) # Fit lasso model on training data

plot(lasso_mod)    # Draw plot of coefficients

summary(lasso_mod)

set.seed(123)
cv.out = cv.glmnet(x_train, y_train, alpha = 1) # Fit lasso model on training data

plot(cv.out) # Draw plot of training MSE as a function of lambda

Sbestlam = cv.out$lambda.min # Select lambda that minimizes training MSE
bestlam
#0.001162177
lasso_pred = predict(lasso_mod, s = bestlam, newx = x_test) # Use best lambda to predict test data

mean((lasso_pred - y_test)^2) # Calculate test MSE
#MSE is 0.1966722

out = glmnet(x_train, y_train, alpha = 1) # Fit lasso model on full dataset
lasso_coef = predict(out, type = "coefficients", s = bestlam) # Display coefficients using lambda chosen by CV
lasso_coef

#############################PCR##################################################
library(pls)
set.seed(123)
data_r = na.omit(data_r)

pcr_fit = pcr(price~.-neighbourhood_group-latitude-longitude-room_type, data =data_r,scale = TRUE, validation = "CV")

summary(pcr_fit)

validationplot(pcr_fit, val.type = "MSEP")

pcr_fit2 = pcr(price~.-neighbourhood_group-latitude-longitude-room_type, data =TrainData, scale = TRUE, validation = "CV")
summary(pcr_fit2)
validationplot(pcr_fit2, val.type = "MSEP")

#We compute the test MSE as follows:
#lowest cv is when M=4

pcr_pred = predict(pcr_fit2, x_test, ncomp=4)
mean((pcr_pred-y_test)^2)
#MSE is 0.1966722

#model fit on train data
pcr_fit3 = pcr(y_train~x_train, scale = TRUE, ncomp = 4)
summary(pcr_fit3)

#################################PLS############################################
set.seed(123)
pls_fit = plsr(price~.-neighbourhood_group-latitude-longitude-room_type, data =TrainData, scale = TRUE, validation = "CV")

summary(pls_fit)

validationplot(pls_fit, val.type = "MSEP")

S#LOWEST CV is when M=5
pls_pred = predict(pls_fit, x_test, ncomp = 4)
mean((pls_pred - y_test)^2)
#MSE is 0.2103112

#model fit on train data
pls_fit2 = plsr(price~.-neighbourhood_group-latitude-longitude-room_type, data =TrainData, scale = TRUE, ncomp = 2)
summary(pls_fit2)

################################### linear model #############################################

fit_final_lm <- lm(price~.-neighbourhood_group-latitude-longitude-room_type, data =data_r)
lm_pred <- predict(fit.final, newdata = data_r, se.fit = TRUE,
                   interval = "prediction", level = 0.95)

summary(fit_final_lm)

sm<-summary(fit_final_lm)

mse <- function(sm) 
  mean(sm$residuals^2)

mse(sm)
#MSE is 0.1972572

#######################################################################################
set.seed(1)
# Sample Indexes
Index = sample(1:nrow(data_r), size = 0.7*nrow(data_r))
# Splitting Data
#D0
TrainData_super = data_r[Index,][5:11]
#D0
TestData_super = data_r[-Index,][5:11]

################################# Boosting ################################################

library(gbm)
set.seed(1)

boost.fit <- gbm(price ~ . , data = TrainData_super, distribution = "gaussian", n.trees = 5000)

summary(boost.fit)
boost.probs <- predict(boost.fit, newdata = TestData_super, n.trees = 5000)
summary(boost.probs)

mean((boost.probs - TestData_super$price)^2)
#MSE is 0.1857905

############################### Random Forest ###############################################
library(randomForest)

set.seed(1)

rf_spam = randomForest(price~., 
                       data = TrainData_super,  
                       importance = TRUE)


random_forest_estimate = predict(rf_spam, newdata = TestData_super)


mean((random_forest_estimate - TestData_super$price)^2)

#MSE is 0.1766407

importance(rf_spam)

varImpPlot(rf_spam)

###############################  ##############################
