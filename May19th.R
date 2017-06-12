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
setwd("~/Documents/AIML/InstacartMarketBasketAnalysis/InstacartMarketBasket/")

#---- Read Data ---- 
orders <- read.csv('../data/orders.csv')
products <- read.csv('../data/products.csv')
order_products <- read.csv('../data/order_products__train.csv')
order_products_prior <- read.csv('../data/order_products__prior.csv')
aisles <- read.csv('../data/aisles.csv')
departments <- read.csv('../data/departments.csv')

#---- cleaning data set ---- 
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

load('sanitisedData.RData')

#---- Simple Data Visualizations ----

#---- Load sanitised Data (data saved after all of the above steps are done) ----
load("~/Documents/AIML/InstacartMarketBasketAnalysis/InstacartMarketBasket/sanitisedData.RData")

#---- Random Visualizations ----
library(ggplot2)
orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram()

# How are aisles organized within depratments?

#install.packages("treemap")
library(treemap)

tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(departments,by="department_id")
aisle_dept_breakup <- tmp %>% left_join(aisles,by="aisle_id")

treemap(aisle_dept_breakup,index=c("department","aisle"),vSize="n",title="",palette="Set3",border.col="#FFFFFF")

#----

tmp <- order_products %>% 
group_by(product_id, add_to_cart_order) %>% 
  summarize(count = n()) %>% mutate(pct=count/sum(count)) %>% 
  filter(add_to_cart_order == 1, count>10) %>% 
  arrange(desc(pct)) %>% 
  left_join(products,by="product_id") %>% 
  select(product_name, pct, count) %>% 
  ungroup() %>% 
  top_n(10, wt=pct)

kable(tmp)

# what products are ordered most of the time?

frequently_bought_products <- order_products %>% 
  group_by(product_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  left_join(products, by = "product_id") %>%
  select(product_name, count)

kable(frequently_bought_products)
# Further questions to ask:
# Are they present in the customers previous orders?
# If they are th elikelihood of they being present again
# 
# what products are ordered most of the time in each dept?

#----- Association rule mining ----

ordr_pr <- order_products_prior
prods <- products

# get the shopping baskets
tmp1 <- ordr_pr %>% 
  inner_join(prods, by="product_id")

tmp2 <- tmp1 %>% 
  group_by(order_id) 
# On runnign the following code, the R session gets aborted :(. don't run.
final <- tmp2 %>%
  summarise(basket = as.vector(list(product_name)))

# compute transactions
transactions <- as(order_baskets$basket, "transactions")

#encode NAs in days_since_prior_order to zero.

orders$days_since_prior_order[is.na(orders$days_since_prior_order)] <- 0
summary(orders)

install.packages('arules')
library(arules)

# get the shopping baskets
order_baskets <- ordr_pr %>% 
  inner_join(prods, by="product_id") %>% 
  group_by(order_id)

# compute transactions
transactions <- as(order_baskets$basket, "transactions")


rules <- apriori(orders)
inspect(rules)