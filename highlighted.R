library(dplyr)
library(readr)
library(highcharter)
library(htmlwidgets)

options(browser = "/usr/bin/firefox")

data <- read_csv("../african_latest_data.csv")
data$dateRep <- as.Date(data$dateRep, format="%d/%m/%Y")
data$popData2019 <- data$popData2019 / 1000000
data$cases_per_million <- data$cumulative_cases / data$popData2019

data_graphic <- data[, c("dateRep", "countriesAndTerritories", "cases_per_million", "cumulative_cases", "cases_weekly")]

## remove france

data_graphic <- subset(data_graphic, countriesAndTerritories != "France")

## add moving average

## 2 sided => backwards and forwards, 1 only backward
ma <- function(x, n = 3){ifelse(x > 3, stats::filter(x, rep(1 / n, n), sides = 1), 0)}

avg <- data_graphic %>% 
    group_by(countriesAndTerritories) %>%
    arrange(dateRep) %>%
    mutate(mov_avg_new = ma(cases_weekly)) %>%
    mutate(mov_avg_cum_cases = ma(cumulative_cases)) 


## only from march 2

## data_graphic <- subset(data_graphic, dateRep > "2020-03-01")

yt_highlight <- hchart(avg, "line", hcaes(x = log(mov_avg_cum_cases), y = log(mov_avg_new), group = countriesAndTerritories)) %>%
  hc_boost(enabled=FALSE) %>%
hc_plotOptions(
    series = list(
      events = list(
        mouseOver = JS("function() { if(this.options.color !== '#0066CC') {this.update({color: '#0066CC'})} }"),
        mouseOut = JS("function() { if(this.options.color === '#0066CC') {this.update({color: '#ddd'})} }")
      ),
      states = list(
        hover = list(
          enabled = TRUE,
          lineWidth = 10
        )
      )
    )) %>%
    hc_tooltip(enabled = FALSE) %>%
    hc_xAxis(title = list(text = "Cumulative Cases (in log)")) %>%
    hc_yAxis(title = list(text = "New Cases (in log)")) %>%
hc_title(
    text = "<a href='https://youtu.be/54XLXg4fYsc?t=210'>Detecting the End of the Exponential Growth in COVID-19 Cases by Country</a>",
    useHTML = TRUE) %>% 
  hc_colors("#dbdbdb")

yt_highlight

saveWidget(yt_highlight, "yt_highlight.html")

