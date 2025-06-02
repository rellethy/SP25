# Install required packages if needed
# install.packages(c("shiny", "forecast", "ggplot2", "lubridate"))

library(shiny)
library(forecast)
library(ggplot2)
library(lubridate)

#this app is designed to show some variations in time series forcasting 
#I got this idea to incorporate some of the content i've learned in my 
#Econ 475 class. It will let you choose a couple of different datasets, choose what
#model to forecast, and then you will see the 1 year ahead forecast for that model
#it will also print the results of the regression, showing how each aspect of the model
#effects the data, it iwll also print the AIC which asses how well the model fit 
#the dataset, the smaller the AIC value, the better the model is for forecasting
#we'll take a look at a couple of datasets from the econmics package
#personal consumption (pce), personal savings rate (psavert), and unemployment rate (unemploy)
data("economics", package = "ggplot2")

# Define UI
ui <- fluidPage(
  titlePanel("Economic Time Series Forecaster"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable to Forecast:", 
                  choices = c("Personal Consumption Expenditures" = "pce",
                              "Personal Savings Rate" = "psavert",
                              "Unemployment" = "unemploy")),
      #these datasets have information going back into the 1900s
      #ideally we'd like to include all potential past information 
      #in our forcast, so w're going to use the max date range
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(economics$date),
                     end = max(economics$date)),
      
      #you'll choose from variations of time and seasonal trends. 
      selectInput("modelType", "Select Model Type:",
                  choices = c("Linear Time Trend" = "linear",
                              "Linear + Quadratic Time Trend" = "quadratic",
                              "Linear + Seasonal Trend" = "seasonal",
                              "Linear + Seasonal + Quadratic Trend" = "seasonal_quadratic")),
      
      actionButton("runModel", "Run Forecast")
    ),
    
    mainPanel(
      plotOutput("tsPlot"),
      verbatimTextOutput("modelSummary"),
      verbatimTextOutput("forecastOutput"),
      verbatimTextOutput("aicOutput")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  observeEvent(input$runModel, {
    # this will prepare our data for forcasting
    filtered_data <- subset(economics, 
                            date >= input$dateRange[1] & date <= input$dateRange[2])
    # we need to convert our data to a time series using the ts() function
    ts_data <- ts(filtered_data[[input$variable]], frequency = 12, 
                  start = c(year(min(filtered_data$date)), month(min(filtered_data$date))))
    #we want to initialize our models this will reset everytime we rerun our model
    model_fit <- NULL
    forecast_result <- NULL
    time <- 1:length(ts_data)
    #this is for our seasonal forecast
    month_factor <- factor(month(filtered_data$date))
    
    #the data records monthly steps, so to forecast one year ahead
    #we'd do the 12-step ahead forecast 
    future_steps <- 12
    new_time <- (length(ts_data) + 1):(length(ts_data) + future_steps)
    future_months <- as.factor(((month(max(filtered_data$date)) + 1:future_steps - 1) %% 12) + 1)
    
    # each model has a different version of the prediction function
    # we set model_fit to whatever input we get from our user
    
    #linear time trend
    if (input$modelType == "linear") {
      model_fit <- lm(ts_data ~ time)
      forecast_val <- predict(model_fit, 
                              newdata = data.frame(time = new_time), 
                              interval = "prediction", level = 0.95)
      #linear + quadratic 
    } else if (input$modelType == "quadratic") {
      model_fit <- lm(ts_data ~ time + I(time^2))
      forecast_val <- predict(model_fit, 
                              newdata = data.frame(time = new_time), 
                              interval = "prediction", level = 0.95)
      
      # purely seasonal model
    } else if (input$modelType == "seasonal") {
      model_fit <- lm(ts_data ~ month_factor)
      forecast_val <- predict(model_fit, 
                              newdata = data.frame(month_factor = future_months), 
                              interval = "prediction", level = 0.95)
      # seasonal + linear + quadratic model
    } else if (input$modelType == "seasonal_quadratic") {
      model_fit <- lm(ts_data ~ time + I(time^2) + month_factor)
      forecast_val <- predict(model_fit, 
                              newdata = data.frame(time = new_time, month_factor = future_months), 
                              interval = "prediction", level = 0.95)
    }
    
    # collect the results, the fit, upper and lower forecast interval
    # and AIC
    forecast_result <- list(
      point = forecast_val[, "fit"],
      lower = forecast_val[, "lwr"],
      upper = forecast_val[, "upr"],
      aic = AIC(model_fit)
    )
    
    # this is how we will visualize the time series data
    # ggplot2's autolayer helps us print our forecast onto the time series data
    output$tsPlot <- renderPlot({
      autoplot(ts_data) +
        autolayer(ts(forecast_result$point, 
                     start = end(ts_data) + c(0,1), frequency = 12), 
                  series = "Forecast", size = 1.2) +
        ggtitle("Time Series with 12-Month Ahead Forecast") +
        xlab("Time") + ylab(input$variable)
    })
    
    # we'll print a summary of the model 
    output$modelSummary <- renderPrint({
      summary(model_fit)
    })
    
    # we'll print the value of the forecast, as well as the 95% forecast interval
    output$forecastOutput <- renderPrint({
      cat("12-Month Ahead Forecast:\n")
      print(data.frame(
        Month = 1:12,
        Point_Forecast = round(forecast_result$point, 2),
        Lower_95 = round(forecast_result$lower, 2),
        Upper_95 = round(forecast_result$upper, 2)
      ))
    })
    
    # lastly, we'll print out the AIC value (remember lower aic is better!)
    output$aicOutput <- renderPrint({
      cat("Model AIC:", round(forecast_result$aic, 2))
    })
  })
}


shinyApp(ui = ui, server = server)

