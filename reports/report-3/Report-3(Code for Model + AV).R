library(ggplot2)
library(dplyr)
library(GGally)
library(car)

data = read.csv("../../data/day.csv")

bike_adj = data %>%
  mutate(
    season = as.factor(season),
    yr = as.factor(yr),
    mnth = as.factor(mnth),
    holiday = as.factor(holiday),
    weekday = as.factor(weekday),
    workingday = as.factor(workingday),
    weathersit = as.factor(weathersit))

bike_weekday = subset(bike_adj, weekday %in% 1:5)
bike_weekend = subset(bike_adj, weekday %in% c(0, 6))


ggpairs(bike_adj %>% select(cnt,atemp,hum,windspeed,temp),
        upper = list(continuous = wrap('cor', size = 4)),
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.1)))

bike_day = bike_adj %>%
  mutate(day_type = ifelse(weekday %in% 1:5, 1, 0),
         day_type = factor(day_type,
                           levels = c(1, 0),
                           labels = c("Weekday", "Weekend")))

model_weekday = lm(cnt ~ season + yr + mnth + weekday + holiday + weathersit + atemp + hum + windspeed, data = bike_weekday)

model_weekend = lm(cnt ~ season + yr + mnth + weekday + weathersit + atemp + hum + windspeed, data = bike_weekend)

model_day_type = lm(cnt ~ atemp + hum + windspeed + season + yr + mnth + day_type + holiday + weathersit, data = bike_day)

model = lm(cnt ~ atemp + hum + windspeed + season + yr + mnth + weekday + holiday + weathersit, data = bike_adj)

summary(model_weekend)
summary(model_weekday)
summary(model_day_type)
summary(model)

plot(model)


