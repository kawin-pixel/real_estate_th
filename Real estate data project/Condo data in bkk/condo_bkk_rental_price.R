library(dbplyr)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(stats)
library(readxl)

##source: https://www.kaggle.com/datasets/krityodp/condominium-data-in-bangkok
## as of Jun 2023, data source: scrapped from propertyhub.com

data <- read_excel("202306.xlsx")

## [1] change existing floor column into number
data <- data %>% 
  mutate(new_floor = gsub("[^0-9]", "", floor)) 
data$new_floor <- as.numeric(data$new_floor)

## [2] change existing sQM into column into number
data <- data %>% 
  mutate(new_room_size = gsub("sq.m.", "", room_size))## use gsub to extract number but keep decimal
data$new_room_size <- as.numeric(data$new_room_size)

## [3] calculate rental per sqm
data <- data %>%
  mutate(rental_to_sqm = rental/new_room_size)

## Chart codes --------------

#rental vs room size by project
ggplot(data, mapping = aes(x = new_room_size, y = rental, fill = unit_type)) +
  geom_point(shape = 21) +
  scale_x_continuous(limits = c(0,500)) +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = 10000, color = "red") +
  facet_wrap(~ project_name)

#rental studio and 1br only
ggplot(subset(data, unit_type == c("1 Bedroom", "Studio")), mapping = aes(y = rental, fill = unit_type)) +
  geom_boxplot(shape = 21) +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = 15000, color = "red") +
  facet_wrap(~ unit_type)

#rental vs room size studio and 1br only
ggplot(subset(data, unit_type == c("1 Bedroom", "Studio") ), mapping = aes(x = new_room_size, y = rental, fill = unit_type)) +
  geom_point(shape = 21) +
  scale_x_continuous(limits = c(0,150)) +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = 10000, color = "red") +
  facet_wrap(~ unit_type)

#rental sqm vs floor
ggplot(data, mapping = aes(x = rental_sqm, y = new_floor, fill = unit_type)) +
  geom_point(shape = 21) +
  scale_x_continuous() + #0.97 quantile
  scale_y_continuous(limits = c(0,40), labels = scales::comma)

















