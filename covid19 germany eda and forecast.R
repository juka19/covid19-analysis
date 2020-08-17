library(covid19.analytics)

# downloading the data

ag <- covid19.data(case = "aggregated")
tsc <- covid19.data(case = "ts-confirmed")
tsa <- covid19.data(case = "ts-ALL")
View(ag)

# some exploratory data analysis

report.summary(Nentries = 5,
               graphical.output = T)
tots.per.location(tsc, geo.loc = "Germany")
growth.rate(tsc, geo.loc = "Germany")

totals.plt(tsa)
totals.plt(tsa, c("Germany"))

live.map(tsc)

generate.SIR.model(data = tsc, "Germany", tot.population = 82000000)


### Forecasting Covid19 Cases in Germany


library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)

# filtering, data wrangling

tsc_ger <- tsc %>% 
  filter(Country.Region == "Germany")
tsc_ger <- data.frame(t(tsc_ger))
tsc_ger <- cbind(rownames(tsc_ger), data.frame(tsc_ger, row.names = NULL))
colnames(tsc_ger) <- c("Date", "Confirmed")
tsc_ger <- tsc_ger[-c(1:4), ]

str(tsc_ger)
tsc_ger$Date <- ymd(tsc_ger$Date)
tsc_ger$Confirmed <- as.numeric(tsc_ger$Confirmed)
str(tsc_ger)

ggplot(tsc_ger, aes(Date, Confirmed)) + geom_line()


# create dataframe that can be passed to prophet
ds <- tsc_ger$Date
y <- tsc_ger$Confirmed
df <- data.frame(ds, y)

# create model, forecast and predictions
m <- prophet(df)
future <- make_future_dataframe(m, periods = 30)
forecast <- predict(m, future)

# Plot forecast & forecast components

dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)


# model performance

pred <- forecast$yhat[1:208]
actual <- m$history$y

df_2 <- data.frame(pred, actual)

# plot actual vs. predicted values

ggplot(df_2, aes(actual, pred)) + geom_point() +
  geom_smooth(col = "red")

# examine model fit

summary(lm(pred~actual))
