library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(scales)
library(forecast)
library(dplyr)
library(plotly)

data <- readRDS(file="country.2020.rds")

# server
server <- function(input, output){
  
  observe( print(input$metric) )
  observe( print(input$country) )
  observe( print(input$date_range_country) )
  
  metric_names <- colnames(data)[3:ncol(data)]
  metric_names <- gsub("_", " ", metric_names)
  metric_names <- paste0(toupper(substr(metric_names,1,1)), substr(metric_names, 2, nchar(metric_names)))
  colnames(data)[3:ncol(data)] <- metric_names
  
  clean_data_country <- reactive({
    clean_data <- data %>%
      filter(country_name %in% input$country & date >= input$date_range_country[1] & date <= input$date_range_country[2]) %>%
      select(country_name, date, input$metric) %>%
      set_names( c("country_name","date","metric")) %>% 
      arrange(date)
  })
  
  plot <- function(){
    ifelse(input$moving_average == T & !is.null(input$moving_average_days),
           data_ma <- clean_data_country() %>%
             group_by(country_name) %>% 
             mutate(ma2=rollapply(metric, ma_days(), mean, align="right", fill=NA)),
           data_ma <- clean_data_country()
    )
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "black"
    )
    
    x <- list(
      title = "Date",
      titlefont = f
    )
    
    y <- list(
      title = "Metric",
      titlefont = f
    )
    
    plt <- plot_ly(data=data_ma, x=~date, color=~country_name, text=~country_name)
    plt <- plt %>% add_trace(y=~metric, type="scatter", mode="lines+markers",
                             hovertemplate = paste(
                               paste0('<extra>Actual Data</extra>Country Name: %{text}\n', input$metric, ": %{y}\nDate: %{x}")
                             ))
    if(input$moving_average == T & !is.null(input$moving_average_days)) {
      plt <- plt %>% add_trace(y=~ma2, type = "scatter", mode="lines", line=list(dash="dot"), showlegend=F,
                               hovertemplate = paste(
                                 paste0('<extra>Moving Average</extra>Country Name: %{text}\n', input$metric, ": %{y}\nDate: %{x}")
                               )) }
    plt <- plt %>% layout(plt, title="", yaxis=y, xaxis=x)
    highlight(plt)
  }
  
  ma_days <- eventReactive(input$moving_average_bttn,{
    req(input$moving_average_days) # must exist
    input$moving_average_days
  },ignoreNULL = FALSE)
  
  plot_forecast <- function(data){
    ifelse( input$moving_average == T & !is.null(input$moving_average_days),
            forecastData <- forecast_data() %>%
              group_by(country_name) %>% 
              mutate(ma2=rollapply(metric, ma_days(), mean, align="right", fill=NA)),
            forecastData <- forecast_data()
    )
    
    forecastData2 <- rbind(
      forecastData %>% filter(forecast == 0) %>%  filter(date==max(date)),
      forecastData %>%  filter(forecast == 1)
    )
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "black"
    )
    
    x <- list(
      title = "Date",
      titlefont = f
    )
    
    y <- list(
      title = "Metric",
      titlefont = f
    )
    
    plt <- plot_ly(data=forecastData %>% filter(forecast==0), x=~date, color=~country_name, text=~country_name)
    plt <- plt %>% add_trace(y=~metric, type="scatter", mode="lines+markers",
                             hovertemplate = paste(
                               paste0('<extra>Actual Data</extra>Country Name: %{text}\n', input$metric, ": %{y}\nDate: %{x}")
                             ))
    if(input$moving_average == T & !is.null(input$moving_average_days)) {
      plt <- plt %>% add_trace(y=~ma2, type = "scatter", mode="lines", line=list(dash="dot"), showlegend=F,
                               hovertemplate = paste(
                                 paste0('<extra>Moving Average</extra>Country Name: %{text}\n', input$metric, ": %{y}\nDate: %{x}")
                               ))
    }
    plt <- plt %>% add_trace(data = forecastData2, y=~metric, x=~date, color=~country_name, alpha = 0.4, showlegend=F,
                             type = "scatter", mode="lines",
                             hovertemplate = paste(
                               paste0('<extra>Forecast</extra>Country Name: %{text}\n', input$metric, ": %{y}\nDate: %{x}")
                             ))
    plt <- plt %>% layout(plt, title="", yaxis=y, xaxis=x)
    highlight(plt)
  }
  
  observeEvent(input$moving_average,{
    if(input$moving_average  == T)
      shinyjs::show(id="moving_average_days", anim = TRUE, animType = "slide")
    else {
      shinyjs::hide(id="moving_average_days", anim = TRUE, animType = "fade")
    }
  })
  
  observeEvent(input$moving_average,{
    if(input$moving_average  == T)
      shinyjs::show(id="moving_average_bttn", anim = TRUE, animType = "slide")
    else {
      shinyjs::hide(id="moving_average_bttn", anim = TRUE, animType = "fade")
    }
  })
  
  create_forecast <- function(data, num_forecasts) {
    name_country <- unique(data$country_name)
    auto_forecast <- forecast(auto.arima(data$metric), num_forecasts)$mean
    max_date <- max(data$date)
    new_dates <- max_date + c(1:num_forecasts)
    new_forecast <- tibble(country_name = name_country, date = new_dates, metric = as.vector(auto_forecast), forecast=1)
    return(new_forecast) }
  predictions_by_country <- reactive({
    clean_data_country() %>% 
      group_by(country_name) %>% 
      group_map(~ create_forecast(.x, num_forecasts = input$forecast), .keep=T)
  })
  
  forecast_data <- reactive({
    unforecasted_data <- clean_data_country()
    unforecasted_data$forecast <- 0
    forecasted_data <- predictions_by_country()
    forecasted_data <- do.call(rbind, forecasted_data)
    dplyr::bind_rows(unforecasted_data, forecasted_data)
  })
  
  make_forecast <- reactiveValues(value=0)
  observeEvent(input$forecast_bttn, {
    make_forecast$value <- 1
  })
  
  observeEvent(input$remove_forecast_bttn, {
    make_forecast$value <- 0
  })
  
  observe(print(make_forecast$value))
  
  # OUTPUT function
  output$plot <- renderPlotly({
    req(input$country)
    ifelse(make_forecast$value == 0, return(plot()), return(plot_forecast(clean_data_country())))
  })
  
}
