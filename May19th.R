#---- Installations ---- 
# You need to run this code only once.
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("stringr")
install.packages("knitr")
install.packages("DT")

#---- Imports ---- 
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)

#---- Set working directory ----
setwd("Documents/AIML/InstacartMarketBasketAnalysis/InstacartMarketBasket/")

#---- Read Data ---- 
orders <- read.csv('../data/orders.csv')
products <- read.csv('../data/products.csv')
order_products <- read.csv('../data/order_products__train.csv')
order_products_prior <- read.csv('../data/order_products__prior.csv')
aisles <- read.csv('../data/aisles.csv')
departments <- read.csv('../data/departments.csv')

#---- Explore ---- 
kable(head(orders,12)) #pretty format
head(orders,12)

str(products)
products$product_id <- as.factor(products$product_id)
products$aisle_id <- as.factor(products$aisle_id)
products$department_id <- as.factor(products$department_id)
summary(products)

str(orders)
orders$order_id <- as.factor(orders$order_id)
orders$user_id <- as.factor(orders$user_id)
orders$order_number <- as.factor(orders$order_number)
orders$order_dow <- as.factor(orders$order_dow)
summary(orders)

hist(as.numeric(orders$order_dow))
hist(orders$order_hour_of_day)

str(order_products)
order_products$order_id <- as.factor(order_products$order_id)
order_products$product_id <- as.factor(order_products$product_id)
summary(order_products)

str(order_products_prior)
order_products_prior$order_id <- as.factor(order_products_prior$order_id)
order_products_prior$product_id <- as.factor(order_products_prior$product_id)
summary(order_products_prior)

str(aisles)
aisles$aisle_id <- as.factor(aisles$aisle_id)
summary(aisles)

str(departments)
departments$department_id <- as.factor(departments$department_id)
summary(departments)

#---- Simple Data Visualizations ----

ggplot(data = orders, aes(x= orders$order_hour_of_day))
+geom_histogram()

orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram()

# How are aisles organized within depratments?

install.packages("treemap")
library(treemap)

tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(departments,by="department_id")
aisle_dept_breakup <- tmp %>% left_join(aisles,by="aisle_id")

treemap(aisle_dept_breakup,index=c("department","aisle"),vSize="n",title="",palette="Set3",border.col="#FFFFFF")



