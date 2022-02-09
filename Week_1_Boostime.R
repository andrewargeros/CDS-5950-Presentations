library(tidyverse)
library(lubridate)
library(modeltime)
library(boostime)

df = read_csv('C:/PersonalScripts/CDS_presentation1.csv')%>% 
  group_by(week = floor_date(cal_date, 'week')) %>% 
  summarise(fills = log(n_distinct(pid))) %>% 
  filter(week < floor_date(Sys.Date(), 'week'))

df %>% 
  ggplot() +
  aes(x = week, y = fills) +
  geom_line(size = 1.1, color = "#156182") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = 'Dispenses by Week of Drug X',
       x = "Date",
       y = "Number of Fills")

train = df %>% filter(year(week) < 2021)
test = df %>% filter(year(week) >= 2021)

boost_formula = 'fills ~ week +  factor(month(week, label = TRUE)) + year(week)' %>% 
  as.formula()

prophboost_model = prophet_boost(learn_rate = 0.37) %>% 
  set_engine('prophet_xgboost') %>% 
  fit(boost_formula, train)

table = prophboost_model %>% 
  modeltime_table() %>% 
  modeltime_calibrate(test)

table %>% modeltime_accuracy()

prophboost_model %>% 
  predict(test) %>% 
  tibble() %>% 
  bind_cols(test) %>% 
  mutate(ape = abs(fills - .pred)/fills) %>% 
  summarise(mean = mean(ape))

table %>% 
  modeltime_forecast(newdata = test, actual = df) %>% 
  ggplot() + 
  aes(x = .index, y = .value, group = .model_desc, color = .model_desc) +
  geom_line(size = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.25)) +
  labs(title = 'Dispenses by Week of Drug X',
       x = "Date",
       y = "Number of Fills",
       color = element_blank())
