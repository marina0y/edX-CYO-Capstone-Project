# DIY edX Capstone Project
# Submission by Marina Yamasaki

# Submission Requirements
# Files (3): rmd file, PDF knit from rmd file, R script
# Sections (4)
##### 1. Intro/Overview/Exec Summary: describes dataset, summarizes goals of project and key steps performed
##### 2. Methods/Analysis: Explains process/techniques used (data cleaning, data exploration, visualization, insights gained, model approach) 
#####    -- At least 2 different models or algorithms must be used (one being more advanced than linear or logistic regression)
##### 3. Results: presents modeling results and discusses model performance
##### 4. Conclusion: brief summary of report, limitations, future work
#####    -- Use relative file paths, automatically install missing packages
#####    -- Dataset should be automatically downloaded by code or provided in GitHub repo along with rest of files. If dataset is provided as zip file in GitHub, code should automatically unzip and load it.
################################

# Install packages and set-up libraries
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages('lubridate', repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages('recosystem', repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages('rmarkdown', repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages('rpart', repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages('randomForest', repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(Ckmeans.1d.dp)) install.packages("xCkmeans.1d.dp", repos = "http://cran.us.r-project.org")
if(!require(googledrive)) install.packages("googledrive", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
library('dplyr')
library('tidyverse')
library('caret')
library('data.table')
library('stringr')
library('lubridate')
library('rmarkdown')
library('recosystem')
library('rmarkdown')
library('rpart')
library('randomForest')
library('xgboost')
library('Ckmeans.1d.dp')
library('googledrive')
library('e1071')
 
# Instacart Market Basket Data Set
##### 1. Intro/Overview/Exec Summary
# Data Set: Kaggle Instacart Online Grocery Shopping Dataset 2017

# Unzip
# 6 files provided via kaggle
# aisles, departments, and products are reference datasets to provide additional detail to main data
# orders attributes each order to a shopper, details when the order was placed, how long it has been since a prior order
# orders_prior/train identify the products purchased in each order and whether and item is a reorder
# Link to dataset: https://drive.google.com/drive/folders/1p-A5scQDoUHygHhpPBJvyZg03WnF1_6d?usp=sharing
# Use the above link to download the orders and order_products__prior data set, downloader approval required due to file size

dep_id <- "1cutfNKqPkJ0bxRuCaIVZJ9G5Bi7h95l0"
ais_id <- "1sW6yTGC_y71mrFmZqkLFO8LGC9pupHuw"
prod_id <- "1nPpK8ICihE--rjQ0Yvr-_C2QVeaEeowT"
opt_id <- "124_XGBr2qZ67Ef24DR7I_yFnohmR4emt"

departments <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", dep_id))
aisles <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", ais_id))
products <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", prod_id))
order_products_train <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", opt_id))

getwd()
# setwd("./Documents/R Coding/edX Capstone Project/Instacart Market Basket Data Set")
orders <- read.csv(unzip("./orders.zip","orders.csv"))
order_products_prior <- read.csv(unzip("./order_products__prior.zip","order_products__prior.csv"))

# aisles <- read.csv("./aisles.csv")
# departments <- read.csv("./departments.csv")
# products <- read.csv("./products.csv")
# orders <- read.csv("./orders.csv")
# order_products_prior <- read.csv("./order_products__prior.csv")
# order_products_train <- read.csv("./order_products__train.csv")

aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))
products <- products %>% mutate(product_name = as.factor(product_name))
orders <- orders %>% mutate(eval_set = as.factor(eval_set))


# 3,421,084 orders
# 206,209 instacart users
# 7 variables (order_id, user_id, eval_set, order_number, order_dow, order_hour_of_day, days_since_prior)
glimpse(orders)
length(unique(orders$user_id))
# 134 aisles
# table consists of aisle_id and aisle (name)
glimpse(aisles)
# 21 departments 
# table consists of department_id and department (name)
glimpse(departments)
# 49688 products
# table consists of product_id, product (name), aisle_id, department_id
glimpse(products)


# Merge product, department, aisle tables together for easier joins
products <- products %>% left_join(aisles) %>% left_join(departments) 
rm(aisles,departments)
# Build Training/Test Data Sets
# Check order data
orders %>% filter(user_id == 1)
orders %>% filter(user_id == 100)
length(orders$eval_set[orders$eval_set == 'train'])
length(orders$eval_set[orders$eval_set == 'test'])
# Orders in train/test are from users who have previous purchase history
orders %>% filter(eval_set=="train",order_number ==1)

# Join user_id to order_products_train data
# Prepare data for model testing
# 1,384,617 orders in the train dataset, 131,209 users
glimpse(order_products_train)
glimpse(order_products_prior)
# Add user_id to order_producst_train table 
order_products_train$user_id <- orders$user_id[match(order_products_train$order_id,orders$order_id)]
length(unique(order_products_train$user_id))

# Expand prior orders table in order to develop model variables
prior_set_orders_full <- orders %>% inner_join(order_products_prior,by=c("order_id"))
prior_set_orders_full <- prior_set_orders_full %>% left_join(select(products,product_id,product_name,aisle,department))
head(prior_set_orders_full)
summary(prior_set_orders_full)

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
subset <- createDataPartition(y = prior_set_orders_full$reordered, times = 1, p = 0.05, list = FALSE)
prior_set_orders <- prior_set_orders_full[subset,]
glimpse(prior_set_orders)

########## EDA ##########
# Shoppers 
# Min of 4 orders per user, max of 100 orders per user
orders %>% group_by(user_id) %>% summarize(total_orders = max(order_number)) %>% 
  group_by(total_orders) %>% summarize(user_count = n()) %>% 
  ggplot(aes(x=reorder(total_orders,-user_count),y=user_count)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_discrete(name="Total Orders",breaks=seq(0,100,5)) + ylab("User Frequency")
orders %>% group_by(user_id) %>% summarize(total_orders = max(order_number)) %>% 
  group_by(total_orders) %>% summarize(user_count = n()) %>% arrange(desc(user_count))

prior_set_orders %>% group_by(user_id) %>% summarize(total_orders = max(order_number)) %>% 
  group_by(total_orders) %>% summarize(user_count = n()) %>% 
  ggplot(aes(x=reorder(total_orders,-user_count),y=user_count)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Total Orders") + ylab("User Frequency")

# Most Purchased Products
prior_set_orders %>% group_by(product_name) %>%
  summarize(count=n()) %>% top_n(20,count) %>% arrange(desc(count)) %>% 
  ggplot(aes(x=reorder(product_name,-count),y=count)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) + xlab("Product Name") + ylab("Product Order Count")

# Lease Purchased Products
prior_set_orders %>% group_by(product_name) %>%
  summarize(count=n()) %>% arrange(count) %>% filter(count==1) 

# Most Purchased Departments                
prior_set_orders %>% group_by(department) %>% summarize(count=n()) %>% 
  top_n(20,count) %>% arrange(desc(count)) %>% ggplot(aes(x=reorder(department,-count),y=count)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Department") + ylab("Department Order Count")
# Number of products within each department
products %>% group_by(department) %>% summarize(count=n()) %>% 
  top_n(20,count) %>% arrange(desc(count)) %>% ggplot(aes(x=reorder(department,-count),y=count)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Department") + ylab("Number of Products")
length(products$department[products$department=="snacks"])
length(products$department)

# Most Purchased Aisles
prior_set_orders %>% group_by(aisle) %>% summarize(count=n()) %>% 
  top_n(20,count) %>% arrange(desc(count)) %>% ggplot(aes(x=reorder(aisle,-count),y=count)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Aisle") + ylab("Aisle Order Count")
# Products per aisle
products %>% group_by(aisle) %>% summarize(count=n()) %>% 
  top_n(20,count) %>% arrange(desc(count)) %>% ggplot(aes(x=reorder(aisle,-count),y=count)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Aisle") + ylab("Number of Products")
length(products$aisle[products$aisle=="snacks"])
length(products$aisle)

# Items per order
temp <- prior_set_orders %>% group_by(order_id) %>% summarize(product_num=max(add_to_cart_order)) %>% select(order_id,product_num)
temp %>% group_by(product_num) %>% summarize(count=n()) %>% filter(product_num <= 50) %>% ggplot(aes(x=product_num,y=count)) + geom_bar(stat="identity") +
  scale_x_continuous(name="Products Per Order", breaks=seq(0,50,2))
min(temp$product_num)
max(temp$product_num)
summary(temp)

# Number of Orders Purchased 
temp <- prior_set_orders %>% group_by(user_id) %>% summarize(order_ct = max(order_number)) %>% select(user_id, order_ct)
temp %>% group_by(order_ct) %>% summarize(count=n()) %>% ggplot(aes(x=order_ct,y=count)) + geom_bar(stat="identity") +
  xlab("Number of Orders") + ylab("Frequency")
summary(temp)

# Order Frequency
prior_set_orders %>% ggplot(aes(x=days_since_prior_order)) + geom_histogram(binwidth=1) +
  xlab("# Days between Orders") + ylab("Frequency")
summary(prior_set_orders$days_since_prior_order)

# Reorder Frequency
temp <- prior_set_orders %>% group_by(user_id) %>% summarize(reorders = sum(reordered)) %>% select(user_id, reorders) 
temp %>% group_by(reorders) %>% summarize(count=n()) %>% arrange(desc(count))

df <- data.frame(order_num = numeric(),
                 reorder_perc = numeric())
# In future -- evaluation of reorder rate by order number
      # orders <- seq(1,50,1)
      # for(order in orders){
      #   df[order,1] <- order
      #   df[order,2] <- rt
      #   order = order + 1
      #   rt = sum(prior_set_orders$reordered[prior_set_orders$order_number == order])/
      #     length(prior_set_orders$reordered[prior_set_orders$order_number == order])
      # }
      # sum(prior_set_orders$reordered[prior_set_orders$order_number == 2])/length(prior_set_orders$reordered[prior_set_orders$order_number == 2])
      # sum(prior_set_orders$reordered[prior_set_orders$order_number == 3])/length(prior_set_orders$reordered[prior_set_orders$order_number == 3])
      # sum(prior_set_orders$reordered[prior_set_orders$order_number == 4])/length(prior_set_orders$reordered[prior_set_orders$order_number == 4])
      # sum(prior_set_orders$reordered[prior_set_orders$order_number == 5])/length(prior_set_orders$reordered[prior_set_orders$order_number == 5])

glimpse(prior_set_orders)
# Product Predictors -- popularity of products
# Introducing variables specific to each product
# How often is a product purchased, reordered, ordered for the first time, reorder ratio
prod <- prior_set_orders %>% group_by(product_id) %>% summarize(prod_totalorders = n(),
                                                        prod_totalreorders = sum(reordered),
                                                        prod_totalfirstorders = sum(is.na(days_since_prior_order))) %>%
  arrange(desc(prod_totalorders))
prod <- prod %>% mutate(prod_reorderratio = prod_totalreorders/prod_totalorders)
glimpse(prod) 

# User Predictors -- user order behavior
user <- prior_set_orders %>% group_by(user_id) %>% summarize(user_ordertotal = max(order_number),
                                                     user_productordertotal = n(),
                                                     user_avgprodsperorder = n()/max(order_number),
                                                     user_distinctprodsordered = n_distinct(product_id),
                                                     user_reordertotal = sum(reordered),
                                                     user_reorderprob = sum(reordered==1)/sum(order_number>1),
                                                     user_avgdaysbtworder = sum(days_since_prior_order,na.rm=T)/(max(order_number)-1))
glimpse(user)
# User x Product Predictors -- user behavior for specific products
userxprod <- prior_set_orders %>% group_by(user_id,product_id) %>% summarize(userxprod_ordertotal = n(),
                                                                             userxprod_reordertotal = sum(reordered),
                                                                             userxprod_avg_addtocartorder = mean(add_to_cart_order))
glimpse(userxprod)
vars <- userxprod %>% inner_join(prod) %>% inner_join(user)
order_pred_set <- vars %>% left_join(select(order_products_train,user_id,
                                             product_id,reordered))
# orderset$reordered <- as.factor(orderset$reordered,levels=c(0,1))
#Build train/test set
order_pred_set$reordered[is.na(order_pred_set$reordered)] <- 0
glimpse(order_pred)

set.seed(1, sample.kind="Rounding") 
order_pred <- order_pred_set[1:200000,]
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y=order_pred$reordered,times=2,p=0.2,list = FALSE)
train <- order_pred[-test_index[,1],]
test <- order_pred[test_index[,1],]
validate <- order_pred[test_index[,2],]

# 10% of orders are reorders -- a mix of different order numbers between 4 and 100, median of  10
summary(orders$order_number[orders$eval_set == "train"])
glimpse(train)
length(train$reordered[train$reordered == 0])
length(train$reordered[train$reordered == 1])/length(train$reordered)
length(test$reordered[test$reordered == 0])
length(test$reordered[test$reordered == 1])/length(test$reordered)


##### 2. Methods/Analysis
# Determine important variables for model development
params <- list("objective"= "reg:logistic","eval_metric"="logloss")
X <- xgb.DMatrix(as.matrix(train %>% select(-reordered)), label = train$reordered)
set.seed(1, sample.kind="Rounding")
logmodel_vartest <- xgboost(data = X, params = params, nrounds = 80)
# We estimate the importance of the predictors
var_importance <- xgb.importance(colnames(X), model = logmodel_vartest)
var_importance
# We plot the importance of the predictors
xgb.ggplot.importance(var_importance)

#parameter looping on lambda parameter
#lambda is a penalty variable, to only pull 

##### Model 1: Logistic regression 
log_fit <- lm(reordered ~ userxprod_reordertotal + 
                prod_reorderratio + user_ordertotal +
                user_id + user_avgdaysbtworder + user_reorderprob + 
                user_avgprodsperorder, data=train)
log_fit
# p_hat probability of reorder
p_hat <- predict(log_fit,test)
summary(p_hat)
y_hat <- factor(ifelse(p_hat > 0.5,1,0))
length(y_hat[y_hat==1])/length(y_hat)
# 0.94
F_meas(y_hat,factor(test$reordered))
table(predicted=y_hat,actual=test$reordered)
confusionMatrix(y_hat,factor(test$reordered))

#train model
# Decision Tree
# fit_rpart <- rpart(reordered ~ userxprod_reordertotal + 
#                      prod_reorderratio + user_ordertotal +
#                      user_id + user_avgdaysbtworder + user_reorderprob + 
#                      user_avgprodsperorder, data =train,
#                    method='class',control=rpart.control(cp=0.001,minsplit=1,minbucket=1,maxdepth = 30))
# p_hat_rpart <- predict(fit_rpart,test)
# y_hat_rpart <- factor(ifelse(p_hat_rpart[,0] > 0.5,1,0))
# F_meas(y_hat_rpart,factor(test$reordered))
# 
# 
# plot(fit_rpart)



# Random Forest 
train$reordered <- as.factor(train$reordered)
# trees <- seq(0,20,2)
# f1_list <- list()
# for(tree in trees){
#   rf <- randomForest(reordered ~ userxprod_reordertotal +
#                        prod_reorderratio + user_ordertotal +
#                        user_id + user_avgdaysbtworder + user_reorderprob +
#                        user_avgprodsperorder, data = train, ntree = tree, na.action = na.exclude)
#   y_hat_rf <- predict(rf,test)
#   F1 <- F_meas(y_hat_rf,factor(test$reordered))
#   print(F1)
# }
train$reordered <- as.factor(train$reordered)
rf <- randomForest(reordered ~ userxprod_reordertotal + 
                     prod_reorderratio + user_ordertotal +
                     user_id + user_avgdaysbtworder + user_reorderprob + 
                     user_avgprodsperorder, method='rpart',
                   data = train, ntree = 5, na.action = na.exclude)
y_hat_rf <- predict(rf,test)
F_meas(y_hat_rf,factor(test$reordered))
confusionMatrix(y_hat_rf,factor(test$reordered))

train$reordered <- as.factor(train$reordered)
rf <- randomForest(reordered ~ userxprod_reordertotal + 
                     prod_reorderratio + user_ordertotal +
                     user_id + user_avgdaysbtworder + user_reorderprob + 
                     user_avgprodsperorder, method='rpart',
                   data = train, ntree = 10, na.action = na.exclude)
y_hat_rf <- predict(rf,test)
F_meas(y_hat_rf,factor(test$reordered))
confusionMatrix(y_hat_rf,factor(test$reordered))

train$reordered <- as.factor(train$reordered)
rf <- randomForest(reordered ~ userxprod_reordertotal + 
                     prod_reorderratio + user_ordertotal +
                     user_id + user_avgdaysbtworder + user_reorderprob + 
                     user_avgprodsperorder, method='rpart',
                   data = train, ntree = 50, na.action = na.exclude)
y_hat_rf <- predict(rf,test)
F_meas(y_hat_rf,factor(test$reordered))
confusionMatrix(y_hat_rf,factor(test$reordered))


# compute F1

train_rpart <- train(reordered ~ userxprod_reordertotal + 
                       prod_reorderratio + user_ordertotal +
                       user_id + user_avgdaysbtworder + user_reorderprob + 
                       user_avgprodsperorder,
                     data = train, method = "rpart",
                     tuneLength = 10,
                      na.action = na.exclude)
plot(train_rpart)
