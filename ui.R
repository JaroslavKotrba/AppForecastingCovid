library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(scales)
library(forecast)
library(dplyr)
library(plotly)

# ui
ui <- dashboardPage(
  skin='red',
  dashboardHeader(
    title = "COVID-19 Country Comparison in 2020",
    titleWidth = 600
  ),
  
  dashboardSidebar(
    
    shinyjs::useShinyjs(),
    
    width = 300,
    br(),
    h4(strong("Your INPUTS"), style="padding-left: 15px"),
    
    selectInput(
      inputId = "metric",
      label = strong("Select Metric:", style = "font-family: 'arial', font-size: 14px"),
      choices = c("New confirmed", "New tested", "New deceased", "New recovered"),
      selected = "New confirmed"
    ),
    
    selectInput(
      inputId = "country",
      multiple = TRUE,
      label = strong("Select Countries to Compare:", style = "font-family: 'arial', font-size: 14px"),
      choices = sort(unique(data$country_name)),
      selected = c("Czech Republic", "Austria", "Slovakia")
    ),
    
    dateRangeInput(
      inputId = "date_range_country",
      label = strong("Date Range:", style = "font-family: 'arial', font-size: 14px"),
      start = "2020-01-01",
      end = "2020-12-31",
    ),
    
    checkboxInput(
      inputId = "moving_average",
      label = div("Include Moving Average", style = "font-family: 'arial', font-size: 14px"),
      value = TRUE
    ),
    
    div(
      numericInput(
        inputId = "moving_average_days",
        label = "Number of Days for Moving Average",
        value = 7,
        min = 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_average_bttn",
        label = "Update MA",
        class = "btn-success")
    )
  ),
  
  dashboardBody(
    tabsetPanel(
      # type = "pills", ### tabs look
      id = "tab_selected",
      # tab panel number 1
      tabPanel(
        title = strong("Country View & Prediction", style = "font-family: 'arial', font-size: 14px"),
        style="padding-top: 0px",
        plotlyOutput("plot"),
        p("\n"),
        div(  class = "jumbotron",
              div(
                class = "container bg-danger",
                h2("Forecast"),
                p("Select the number of days you would like to forecast using ", code("R")),
                numericInput(
                  inputId = "forecast",
                  label = "Number of Days to Forecast",
                  value = 14, min = 0, max = 100, step = 1
                ),
                actionButton(
                  inputId = "forecast_bttn",
                  label = "Forecast ARIMA",
                  style = "color: white;",
                  class = "btn btn-lg btn-primary",
                ),
                actionButton(
                  inputId = "remove_forecast_bttn",
                  label = "Remove",
                  icon = icon("eye-close", lib = "glyphicon"),
                  style = "color: white;",
                  class = "btn btn-sm btn-danger",
                )
              )
        )
      ),
      tabPanel(
        title = strong("About", style = "font-family: 'arial', font-size: 14px"),
        style="padding-top: 0px",
        div(class="jumbotron", style = "background-color: #FFFFFF", div(class = "container", h2("About the Model", code("ARIMA")), p("ARIMA is an acronym that stands for AutoRegressive Integrated Moving Average. It is a class of model that captures a suite of different standard temporal structures in time series data."),
                                                                        p("In statistics and econometrics, and in particular in time series analysis, an autoregressive integrated moving average model is a generalization of an autoregressive moving average model. Both of these models are fitted to time series data either to better understand the data or to predict future points in the series.")
        )
        )
      )
    )
  )
)
