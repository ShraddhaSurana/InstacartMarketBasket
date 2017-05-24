#---- Installations ---- 
# You need to run this code only once.
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("stringr")
install.packages("knitr")

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
aisles <- read.csv(',,/data/aisles.csv')
departments <- read.csv('../data/departments.csv')

#---- Explore ---- 
kable(head(orders,12)) #pretty format
head(orders,12)

str(orders)
str(products)
str(order_products)
str(order_products_prior)
str(aisles)
str(departments)

#---- Simple Data Visualizations ----
products$product_id <- as.factor(products$product_id)
products$aisle_id <- as.factor(products$aisle_id)
products$department_id <- as.factor(products$department_id)

orders$order_dow <- as.factor(orders$order_dow)
hist(as.numeric(orders$order_dow))
hist(orders$order_hour_of_day)
