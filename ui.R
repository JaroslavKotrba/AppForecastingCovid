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
      choices = c("Afghanistan", "Albania", "Algeria", "American Samoa", "Andorra", 
                  "Angola", "Anguilla", "Antarctica", "Antigua and Barbuda", "Argentina", 
                  "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", 
                  "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", 
                  "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina", 
                  "Botswana", "Bouvet Island", "Brazil", "British Indian Ocean Territory", 
                  "British Virgin Islands", "Brunei", "Bulgaria", "Burkina Faso", 
                  "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Caribbean Netherlands", 
                  "Cayman Islands", "Central African Republic", "Chad", "Chile", 
                  "China", "Christmas Island", "Cocos Islands", "Colombia", "Comoros", 
                  "Cook Islands", "Costa Rica", "Croatia", "Cuba", "Cura?ao", "Cyprus", 
                  "Czech Republic", "Democratic Republic of the Congo", "Denmark", 
                  "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador", 
                  "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", 
                  "Ethiopia", "Falkland Islands", "Faroe Islands", "Fiji", "Finland", 
                  "France", "French Guiana", "French Polynesia", "French Southern Territories", 
                  "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", 
                  "Greece", "Greenland", "Grenada", "Guam", "Guatemala", "Guernsey", 
                  "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard Island and McDonald Islands", 
                  "Honduras", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", 
                  "Iran", "Iraq", "Ireland", "Isle of Man", "Israel", "Italy", 
                  "Ivory Coast", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan", 
                  "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", 
                  "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", 
                  "Lithuania", "Luxembourg", "Macau", "Macedonia", "Madagascar", 
                  "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", 
                  "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", 
                  "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", 
                  "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", 
                  "Nauru", "Nepal", "Netherlands", "Netherlands Antilles", "New Caledonia", 
                  "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfolk Island", 
                  "North Korea", "Northern Mariana Islands", "Norway", "Oman", 
                  "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", 
                  "Paraguay", "Peru", "Philippines", "Pitcairn Islands", "Poland", 
                  "Portugal", "Puerto Rico", "Qatar", "Republic of the Congo", 
                  "R?union", "Romania", "Russia", "Rwanda", "Saint Helena", "Saint Kitts and Nevis", 
                  "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", 
                  "S?o Tom? and Pr?ncipe", "Saudi Arabia", "Senegal", "Serbia", 
                  "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten", "Slovakia", 
                  "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Georgia and the South Sandwich Islands", 
                  "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", 
                  "Suriname", "Svalbard and Jan Mayen", "Swaziland", "Sweden", 
                  "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", 
                  "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", 
                  "Turkey", "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", 
                  "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", 
                  "United States Minor Outlying Islands", "United States of America", 
                  "United States Virgin Islands", "Uruguay", "Uzbekistan", "Vanuatu", 
                  "Vatican City", "Venezuela", "Vietnam", "Wallis and Futuna", 
                  "Western Sahara", "Yemen", "Zambia", "Zimbabwe"),
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
