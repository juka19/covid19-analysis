# Animated graph of confirmed covid19 cases in 4 European countries

library(covid19.analytics)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gganimate)

# dowloading updated covid19 data

tsc <- covid19.data(case = "ts-confirmed")
View(tsc)

# data wrangling
tsc <- tsc %>% 
  filter(Country.Region %in% c("Germany", "France", 
                               "Italy", "United Kingdom")) %>% 
  filter(Province.State == "") %>% 
  pivot_longer(cols = starts_with("2020"), names_to = "Date",
               values_to = "Confirmed")

tsc$Date <- as.Date(tsc$Date)
names(tsc)[2] <- "Country"

# filter for months after may and create the plot
tsc %>% 
  filter(Date >= "2020-05-01") %>%
  ggplot(aes(Date, Confirmed, group = Country,
             color = Country)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  theme_bw() +
  ggtitle("Confirmed Cases of Covid19 since May") +
  scale_x_date(date_breaks = "months", 
               date_labels = "%B") +
  labs(x = "Date", y = "Confirmed Cases") +
  transition_reveal(Date) 
