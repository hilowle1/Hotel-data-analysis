install.packages("tidyverse")
library(tidyverse)

hotels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")


hotel_stays <- hotels %>%
  filter(is_canceled == 0) %>%
  mutate(
    children = case_when(
      children + babies > 0 ~ "children",
      TRUE ~ "none"
    ),
    required_car_parking_spaces = case_when(
      required_car_parking_spaces > 0 ~ "parking",
      TRUE ~ "none"
    )
  ) %>%
   
hotel_stays %>%
  mutate(arrival_date_month = factor(arrival_date_month,
                                     levels = month.name
  )) %>%
  count(hotel, arrival_date_month, children) %>%
  group_by(hotel, children) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(arrival_date_month, proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2) +
  labs(
    x = NULL,
    y = "Proportion of hotel stays",
    fill = NULL
  )

hotel_stays %>%
  count(hotel, arrival_date_year, children) %>%
  group_by(hotel, children) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(arrival_date_year, proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2) +
  labs(
    x = NULL,
    y = "Proportion of hotel stays",
    fill = NULL
  )
library(GGally)

hotel_stays %>%
  select(
    children, arrival_date_week_number,
    arrival_date_year
  ) %>%
  ggpairs(mapping = aes(color = children))

summary(data$arrival_date_year)

install.packages("ggplot2")
library(ggplot2)
gg<- ggplot(data, aes(x = arrival_date_year, y = arrival_date_week_number)) +
  geom_point(aes(col= deposit_type))
plot(gg)

install.packages("psych")
library(psych)
pairs.panels(data[c("arrival_date_year","arrival_date_week_number","arrival_date_day_of_month")])

library(ggplot2)
ggplot(data, aes(x = stays_in_week_nights, y = stays_in_weekend_nights)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")

ggplot(data, aes(arrival_date_year)) +
  geom_bar(width=0.25, fill = "blue")

ggplot(data, aes(deposit_type)) +
  geom_bar(width=0.25, fill = "blue")
ggplot(data, aes(arrival_date_week_number)) +
  geom_bar(width=0.25, fill = "blue")

ggplot(data, aes(arrival_date_week_number)) +
  geom_bar(width=0.25, fill = "blue")

ggplot(data, aes(stays_in_week_nights)) +
  geom_bar(width=0.25, fill = "blue")

ggplot(data, aes(meal)) +
  geom_bar(width=0.25, fill = "blue")

ggplot(data, aes(country)) +
  geom_bar(width=0.25, fill = "blue")
ggplot(data, aes(arrival_date_month)) +
  geom_bar(width=0.25, fill = "blue")

ggplot(data, aes(stays_in_weekend_nights)) +
  geom_bar(width=0.25, fill = "blue")

hist(arrival_date_week_number,main= 
       "Arrival Date Week Number",col=rainbow(6))

hist(arrival_date_year,main= 
       "Histogram Arrival Data year",col= rainbow(6))
pie(table(arrival_date_year))

hist(arrival_date_week_number,main= 
       "Arrival Date Week Number",col=rainbow(6))

hist(arrival_date_week_number,main= 
       "Arrival Date Week Number",col=rainbow(6))

hist(stays_in_week_nights,main= 
       "stays in week nights",col=rainbow(6))

hist(adults,main= 
       "Adults",col="blue")
hist(children,main= 
       "Children",col="blue")
hist(babies,main= 
       "babies",col=rainbow(6))
 
pie(table(arrival_date_year))

pie(table(arrival_date_month))

pie(table(adults))

plot(adults)

plot(children)

plot(babies)

plot(`days_in_staying hotel`)



attach(data)
table(adults)

table(country)

table(arrival_date_year)

table(arrival_date_month)

table(arrival_date_day_of_month)

table(arrival_date_year)

barplot(arrival_date_year)

barplot(arrival_date_day_of_month)

barplot(`days_in_staying hotel`)        
   
install.packages("ggplot2") 
 
library(skimr)

skim(arrival_date_year)
 
library(skimr)

skim(arrival_date_year)

attach(data)
summary(data$stays_in_weekend_nights)

library(ggplot2)
 ggplt <- ggplot(data,aes(x= stays_in_week_nights ,y= stays_in_weekend_nights))+
  geom_point()+
  theme_classic()
ggplt

data <- data[1:303, c("arrival_date_year", "arrival_date_month")]

print(data) #prints the column of arrival_date_year and arrival_date_month

plot(x=data$arrival_date_year, data$arrival_date_month, xlab = " arrival_date_year ", ylab = "arrival_date_month", main =
       "arrival_date_year vs arrival_date_month", col = "maroon")

 
 







  

