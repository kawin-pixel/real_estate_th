library(dbplyr)
library(ggplot2)
library(tidyverse)

data <- read.csv("ddproperty_2022-04-19.csv")

data <- data %>% mutate(price_sqm_condo = 
                          ifelse(data$property_type == "Condo", data$price / data$living_space,NA))

## chart code------

##condo price / sqm by district in BKK
ggplot(subset(data, property_type == "Condo" & state == "Bangkok" ), mapping = aes(x = price_sqm_condo, y = city)) +
  geom_boxplot( fill = "purple", outlier.size = 0.2) +
  scale_x_continuous(limits = c(0,290400), ##price quantile 0.95 = 290,400
                     labels = scales::comma) +
  theme_minimal() +
  labs(title = "Condo price/sqm in BKK", x = "selling_price/sqm(THB)", y ="district_name")

quantile(data$price_sqm_condo, 0.95, na.rm = T)


##condo price / sqm in BKK
ggplot(subset(data, property_type == "Condo" & state == "Bangkok" ), mapping = aes(x = price_sqm_condo, y = living_space,
                                                                                   fill = city)) +
  geom_point(shape = 21) +
  scale_x_continuous(limits = c(0,290400), ##price quantile 0.95 = 290,400
                     labels = scales::comma) +
  scale_y_continuous(limits = c(0,500), ##living space quantile 0.95 = 475
                     labels = scales::comma) +
  theme_minimal() +
  labs(title = "Condo price/sqm in BKK", x = "selling_price/sqm(THB)", y ="Living_space")

quantile(data$price_sqm_condo, 0.95, na.rm = T)
quantile(data$living_space, 0.95, na.rm = T)

##price by property type

ggplot(data, mapping = aes(y = price)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,70000000), ##price quantile 0.95 = 67m THB
                     labels = scales::comma) +
  facet_grid(~ property_type)


#note --------------------

print(data$property_type)

boxplot(data$price ~ data$property_type)

print(quantile(data$price, 0.95))

