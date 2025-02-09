# Install Required Packages (Only Run Once)
install.packages(c(
  "glmnet",       # Elastic Net regression
  "tseries",      # Time series functions like ADF test
  "randomForest", # Random Forest
  "rugarch",      # GARCH modeling
  "FinTS",        # ARCH/GARCH tests
  "auto.garch",   # Automated GARCH selection
  "tidymodels",   # For machine learning workflows
  "caret",        # For machine learning models and tuning
  "prophet",      # Facebook Prophet for forecasting
  "urca",         # Unit root and cointegration tests
  "forecast",     # ARIMA and forecasting models
  "stats",        # Base R statistics functions
  "lubridate",    # Date manipulation
  "zoo",          # Time series manipulation
  "ggplot2",      # Data visualization
  "dplyr",        # Data manipulation
  "timetk",       # Time series visualization and feature engineering
  "modeltime",    # Time series modeling
  "recipes",      # Preprocessing recipes for machine learning
  "rsample"       # Data splitting
))

# Load All Libraries
library(glmnet)        # Elastic Net regression
library(tseries)       # Time series analysis functions
library(randomForest)  # Machine learning with Random Forest
library(rugarch)       # GARCH models for volatility modeling
library(FinTS)         # ARCH/GARCH diagnostic tests
library(auto.garch)    # Automated GARCH model selection
library(tidymodels)    # Unified interface for machine learning workflows
library(caret)         # Easy model training and tuning
library(prophet)       # For trend-seasonality modeling in time series
library(urca)          # Advanced econometric tests like unit root and cointegration
library(forecast)      # ARIMA, SARIMA, and time series forecasting
library(stats)         # Base R functions like `ts` and statistical tests
library(lubridate)     # Simplify date and time operations
library(zoo)           # Handle irregular time series
library(ggplot2)       # Create professional data visualizations
library(dplyr)         # Grammar for data manipulation
library(timetk)        # Visualize and engineer time series data
library(modeltime)     # Use machine learning models for time series
library(recipes)       # Preprocessing steps for data pipelines
library(rsample)       # Splitting data for training/testing
library(lmtest)

# Upload Data sets
df_train <- read.csv("C:/Users/MahwishMalik/OneDrive/Desktop/train.csv", 
                  header=TRUE, sep=",", 
                  colClasses=c("integer","Date","factor","factor","numeric","numeric"))

df_test=read.csv("C:/Users/MahwishMalik/OneDrive/Desktop/test.csv", 
                 header=TRUE, sep=",",
                 colClasses=c("integer","Date","factor","factor","numeric"))

df_oil = read.csv("C:/Users/MahwishMalik/OneDrive/Desktop/oil.csv", 
                  header=TRUE, sep=",",
                  colClasses=c("Date","numeric"))

df_holidays = read.csv("C:/Users/MahwishMalik/OneDrive/Desktop/holidays_events.csv", 
                       header=TRUE, sep=",",
                       colClasses=c("Date","factor","factor","factor","character","factor"))

df_stores = read.csv("C:/Users/MahwishMalik/OneDrive/Desktop/stores.csv",
                     header=TRUE, sep=",",
                     colClasses =c("factor","factor","factor","factor","integer"))

df_transactions = read.csv("C:/Users/MahwishMalik/OneDrive/Desktop/transactions.csv", 
                           header=TRUE, sep=",",
                           colClasses=c("Date","factor","numeric"))

# Summary of All Datasets or Variables

# Summary of Train Data Set
summary(df_train)
str(df_train)

# Summary of Oil Data set
summary(df_oil)
str(df_oil)

# Missing values Treatment in Oil Dataset

df_oil$oil_NNA<-df_oil$dcoilwtico
df_oil[1,3]=df_oil[2,3]
for(i in 2:nrow(df_oil)){
  if(is.na(df_oil[i,3])){
    df_oil[i,3]=prev_val
  }else{
    prev_val=df_oil[i,3]
  }
}

# Summary of Holidays Data Set
summary(df_holidays)
str(df_holidays)

# Summary of Stores Data Set
summary(df_stores)
str(df_stores)

# Summary of Transactions Data Set
summary(df_transactions)
str(df_transactions)


# Summary of Test Data Set
summary(df_test)
str(df_test)



# Taking all these data sets as variables in a single table to do the analysis 
df_train <- left_join(x=df_train, y=df_stores, by="store_nbr")
df_train <- left_join(x=df_train, y=df_transactions, by=c("store_nbr","date"))
df_train <- left_join(x=df_train, y=df_holidays, by="date")
df_train <- left_join(x=df_train, y=df_oil, by="date")
head(df_train,n=20)

                                                 # Analysis
                                         # Daily Sales Report Plot
                                         
#Sales in Ecuador increased between 2013 and 2017 (in 2017 almost twice as much as in 2013). We noticed sales peaks in the end of each year.

plot1<-df_train %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales)
  ) %>%
  ggplot(aes(x=date,y=daily_sales,groups=1))+geom_line()+geom_smooth()+
  labs(title="Sales",subtitle="Ecuador (2013-2017)")+
  xlab("Date")+ylab("Sales")
ggsave("pics/plot1.png")  # Save Daily sales Plot
print(plot1)              # Print Daily sales Plot


                                             #Oil dependency Plots
# Oil price fluctuation have a big impact on economy, and Ecuador has a high dependency on oil.
plot_salesvsoil <-df_train %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE),
    daily_oil=mean(oil_NNA,na.rm=TRUE)
  ) %>%
  ggplot(aes(x=daily_oil,y=daily_sales))+geom_point()+geom_smooth()+
  ylim(c(300000,1200000))+
  labs(title="Impact of oil price",subtitle="Ecuador (2013-2017)")+
  xlab("Oil Price")+ylab("Daily sales")

ggsave("pics/plot_oil.png")  # Save Oil sales Plot
print(plot_salesvsoil)    # Print Oil sales Plot


# Holidays/events Data set Plots
# There are national (approx. 14), regional and local holidays in Ecuador.
# If we focus on national holidays, we can see that the sales volume is more important during holidays.

plot_holidays <-df_train %>%
  mutate(holidays_fact=ifelse(is.na(locale) | locale!="National","No","Yes")) %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE),
    holidays_fact=min(holidays_fact,na.rm=TRUE)
  )%>%
  mutate(holidays_fact=factor(holidays_fact,levels=c("No","Yes"))) %>%
  ggplot(aes(x=holidays_fact,y=daily_sales,fill=holidays_fact,group=holidays_fact))+geom_boxplot()+
  labs(title="Average sales",subtitle="Ecuador (2013-2017)")+xlab("Holidays ?")+ylab("Average daily sales")
ggsave("pics/plot_holidays.png")
print(plot_holidays)  # Print Holidays Plot


# Promotions Data set Plots
# Sales promotions can have a positive effect on business,
# we can see it in the next plot that sales volume was more significant during promotions.

Plot_promotions <- df_train %>%
  group_by(date) %>%
  summarise(
    sales=mean(sales,na.rm=TRUE),
    onprom=sum(onpromotion,na.rm=TRUE)
  ) %>%
  mutate(Promotions=ifelse(onprom==0,"No","Yes")) %>%
  ggplot(aes(x=Promotions,y=sales,fill=Promotions))+geom_boxplot()+
  labs("Influence of promotions on daily sales",subtitle="Ecuador (2013-2017)")+
  xlab("Promotions ?")+ylab("Daily sales")
ggsave("pics/plot_promotions.png")
print(Plot_promotions)

# Stationarity

# Stationarity is a very important aspect as our data involves time, it is important that the behavior of our data (mean, variance) remains constant to predict the future.
# We have to look into the sales variation in time to see any frequency.To do that, we can use stl function (Seasonal Decomposition of Time Series by Loss).

# Seasonal decomposition
max_date=max(df_train$date)
print(max_date)
min_date=min(df_train$date)
print(min_date)

dat_ts <- df_train %>%
  # Earth quake filter
  filter(!grepl("Terremoto", description, fixed = TRUE)) %>%
  select(date,sales) %>%
  group_by(date) %>%
  summarise(
    value=sum(sales,na.rm=TRUE)
  )

dat_ts <-ts(dat_ts$value,end=c(year(max_date), month(max_date)),
            start=c(year(min_date), month(min_date)),
            frequency = 30)

# Seasonal Decomposition Plot
png(file = "C:/Users/MahwishMalik/OneDrive/Desktop/stl_plot.png",
    width = 960, height = 960, units = "px", pointsize = 20, bg = "azure")

plot(stl(dat_ts,s.window = "periodic"), 
     main="Seasonal Decomposition of Time Series by Loess")

# Close the device to save the plot
dev.off()

# Unit Root Test: Augmented Dickey-Fuller(ADF) Test
# p-value <0.05 -> data is "stationary"
print(adf.test(dat_ts))



                                 # Forecasting Models (Machine Learning Models)


# training(splits) (training set)
# There are total 51 stores and here I am taking Store_nbr 51 for testing and training

# Store_nbr 51
# Replace 'description' with the actual column name, e.g., 'event_description'
data <- df_train %>%
  filter(store_nbr == 51) %>%
  group_by(date) %>%
  summarise(
    value = mean(sales, na.rm = TRUE)
  )


# testing(splits) (last three months as testing set)
splits <- data %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)


# Extract Training and Testing Data
train_data <- training(splits) %>%
  arrange(date)
test_data <- testing(splits) %>%
  arrange(date)

# Perform time series split
splits <- time_series_split(data, assess = "3 months", cumulative = TRUE)

# Extract testing data
test_data <- testing(splits)

# Convert to Train data to Time Series
train_ts <- ts(train_data$value, frequency = 365)  # Adjust frequency based on your data

# Define forecast horizon
horizon <- nrow(test_data)  # Adjusted to use rows instead of column length

# Fit auto ANN model
auto_ann_model <- nnetar(train_ts)

# Print model summary
summary(auto_ann_model)

# Convert to Test data to Time Series
test_ts <- ts(test_data$value, start = c(1, 1), frequency = 365)

# Forecast
auto_ann_forecast <- forecast(auto_ann_model, h = horizon)
print(auto_ann_forecast)
print(test_ts)

# Calculate accuracy metrics
accuracy_results <- accuracy(auto_ann_forecast$mean, test_ts)

# Print accuracy results
print(accuracy_results)


                                            # Econometrics Models For Forecasting

# ACF Plot
# Calculate ACF and Save the Plot
png(file = "C:/Users/MahwishMalik/OneDrive/Desktop/train_acf_plot.png",
    width = 960, height = 960, units = "px", pointsize = 20, bg = "azure")

# ACF Plot
acf_plot <- acf(train_ts, lag.max = 50, main = "Training Data: Autocorrelation Function (ACF)")
plot(acf_plot)

# Close the Graphics Device
dev.off()

# PACF (Partial Autocorrelation Function)
# Calculate PACF and Save the Plot
png(file = "C:/Users/MahwishMalik/OneDrive/Desktop/train_pacf_plot.png",
    width = 960, height = 960, units = "px", pointsize = 20, bg = "azure")

# PACF Plot
pacf_plot <- pacf(train_ts, lag.max = 50, main = "Training Data: Partial Autocorrelation Function (PACF)")
plot(pacf_plot)

# Close the Graphics Device
dev.off()


                                                          # ARIMA

model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits))

# Fit ARIMA Model Automatically
arima_model <- auto.arima(train_ts)

# Print ARIMA Model Summary
summary(arima_model)

# Residual Diagnostics
# Box-Pierce Test
box_pierce_test <- Box.test(residuals(arima_model), type = "Box-Pierce")
print(box_pierce_test)

# Box-Ljung Test
box_ljung_test <- Box.test(residuals(arima_model), type = "Ljung-Box")
print(box_ljung_test)

                                     # Sarima Model

# Fit SARIMA Model (manual selection of parameters)
sarima_model_auto <- auto.arima(train_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(sarima_model_auto)

# Forecast with Automated SARIMA Model
sarima_forecast_auto <- forecast(sarima_model_auto, h = 90)

# Plot Automated SARIMA Forecast
png(file = "C:/Users/MahwishMalik/OneDrive/Desktop/sarima_auto_forecast_plot.png",
    width = 960, height = 960, units = "px", pointsize = 20, bg = "azure")
plot(sarima_forecast_auto, main = "Automated SARIMA Forecast for Training Data")
dev.off()

# Extract Residuals from the Model
residuals_arima <- residuals(sarima_model_auto)  
print(residuals_arima)

# Basic plot for residuals
plot(residuals_arima, main = "Residuals of SARIMA Model", xlab = "Time", ylab = "Residuals", col = "blue", type = "o", pch = 20)

# Histogram of residuals
hist(residuals_arima, main = "Histogram of Arima Residuals", xlab = "Residuals", col = "lightblue", border = "white")


# Perform ARCH Test
library(FinTS)
arch_test_result <- ArchTest(residuals_arima, lags = 10)
print(arch_test_result)



                                                      # Garch Model  

# Residuals from SARIMA Model
residuals_clean <- residuals(sarima_model_auto)

# Clean Residuals
residuals_clean <- residuals_clean[!is.na(residuals_clean) & !is.infinite(residuals_clean)]

# Define GARCH(1,1) Specification
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

# Fit the GARCH(1,1) Model
garch_model <- ugarchfit(spec = garch_spec, data = residuals_clean)

# Print Model Summary
summary(garch_model)

coef(garch_model)

fitted_values <- fitted(garch_model)
head(fitted_values)

residuals_garch <- residuals(garch_model)
head(residuals_garch)

# Forecast 10 steps ahead
garch_forecast <- ugarchforecast(garch_model, n.ahead = 10)

# Extract forecasted mean and variance
forecast_mean <- fitted(garch_forecast)
forecast_variance <- sigma(garch_forecast)^2

# Print forecast
print(garch_forecast)

# Plot forecast
plot(garch_forecast, which = 1)  # Conditional mean
plot(garch_forecast, which = 3)  # Conditional variance





                                             # Fit and Specify E-GARCH Model
egarch_spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), # Order of ARCH and GARCH terms
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),   # Mean model settings
  distribution.model = "std"  # Use "std" for Student's t-distribution or "norm" for Gaussian
)

# Fit E-GARCH Model
egarch_model <- ugarchfit(
  spec = egarch_spec,
  data = residuals_clean,
  solver = "hybrid"  # Other options: "nlminb", "solnp"
)

# Model Summary
summary(egarch_model)

# Forecast with E-GARCH
egarch_forecast <- ugarchforecast(egarch_model, n.ahead = 10)
print(egarch_forecast)

# Plot Forecast
plot(egarch_forecast, which = 1)  # Conditional mean forecast
plot(egarch_forecast, which = 3)  # Conditional variance forecast


                                          # Specify F-GARCH Model with TGARCH Submodel
fgarch_spec <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"  # Use "std" for Student's t-distribution or "norm" for Gaussian
)

# Fit F-GARCH Model
fgarch_model <- ugarchfit(spec = fgarch_spec, data = residuals_clean)

# Display Model Summary
summary(fgarch_model)

# Forecast with F-GARCH Model
fgarch_forecast <- ugarchforecast(fgarch_model, n.ahead = 10)

# Print Forecast Results
print(fgarch_forecast)

# Plot Forecasts
plot(fgarch_forecast, which = 1)  # Conditional mean forecast
plot(fgarch_forecast, which = 3)  # Conditional variance forecast

                                                 # Prophet

# Using Prophet to combine trend component, seasonality, holidays and residuals.
model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE,seasonality_weekly =TRUE) %>%
  set_engine("prophet",holidays=df_holidays) %>%
  fit(value ~ date, training(splits))




                                                  #TBATS

# Fitting a TBATS (Trigonometric, Box-Cox, ARMA errors, Trend, and Seasonal components) model to a time series dataset.
model_fit_tbats<-seasonal_reg(mode="regression",
                              seasonal_period_1= "auto",
                              seasonal_period_2= "1 year",
                              seasonal_period_3= "1 month") %>%
  set_engine("tbats") %>%
  fit(value ~ date, training(splits))



                                                  #Naive Method

# Naive method set all forecasts to the value of previous observations, Seasonal Naive methods add seasonal factor.
model_snaive <- naive_reg() %>%
  set_engine("snaive") %>%
  fit(value ~ date, training(splits))




                                    # Feature Engineering Using Machine Learning Models

# Data preprocessing recipe for a time series forecasting task. The recipe object specifies the steps to transform
# the raw data (value ~ date) into a format suitable for modeling, including feature engineering for time series data.

recipe <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_fourier(date, period = c(365, 91.25, 30.42), K = 5) %>%
  step_rm(date) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes())

recipe %>% 
  prep() %>%
  juice()


                                                    
                                                       # Elastics Net

# Elastics Net: This model is designed to calculate a linear regression solution with regularization to prevent overfitting and improve generalization.
# It uses the Elastic Net Regularization method, which combines:
engine_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

model_glmnet <- workflow() %>%
  add_model(engine_glmnet) %>%
  add_recipe(recipe) %>%
  fit(training(splits))


                                                       
                                                         # Random forest

# Random forest is a supervised learning algorithm for regression and classification.
# Random Forest operates by constructing several decision trees and outputting the mean of the classes as the prediction of all the trees.
engine_rf <- rand_forest(mode="regression",trees = 50, min_n = 5) %>%
  set_engine("randomForest")

model_rf <- workflow() %>%
  add_model(engine_rf) %>%
  add_recipe(recipe) %>%
  fit(training(splits))



                              # Combine all models into a modeltime table (excluding GARCH)
models_table <- modeltime_table(
  model_fit_arima,          # ARIMA
  model_fit_prophet,        # Prophet
  model_fit_tbats,          # TBATS
  model_snaive,             # SNaive
  model_glmnet,             # Elastic Net
  model_rf                  # Random Forest
)

# Calibrate the models on the test data
calib_table <- models_table %>%
  modeltime_calibrate(testing(splits))

calib_table <- models_table %>%
  modeltime_calibrate(testing(splits), quiet = FALSE)


# Evaluate model performance
accuracy_results <- calib_table %>%
  modeltime_accuracy()

                                       # Add GARCH Metrics for Manual Comparison

# Ensure lengths are equal
length(forecast_mean) == length(testing(splits)$value)

common_length <- min(length(forecast_mean), length(testing(splits)$value))
forecast_mean <- forecast_mean[1:common_length]
observed_values <- testing(splits)$value[1:common_length]

# Plot using aligned observed values
plot(
  forecast_mean, 
  observed_values, 
  main = "GARCH Forecast vs Test Data",
  xlab = "Forecast Mean", 
  ylab = "Observed Values"
)

# garch accuracy

garch_accuracy <- data.frame(
  .model_id = "GARCH",
  .model_desc = "GARCH(1,1)",
  rmse = sqrt(mean((forecast_mean - testing(splits)$value)^2)),
  mae = mean(abs(forecast_mean - testing(splits)$value)),
  rsq = ifelse(var(forecast_mean) > 0 && var(testing(splits)$value) > 0,
               cor(forecast_mean, testing(splits)$value)^2,
               NA)  # Handle constant variance
)


# Print Garch Accuracy
print(garch_accuracy)


# Convert .model_id in garch_accuracy to integer
garch_accuracy$.model_id <- as.integer(garch_accuracy$.model_id)

# Combine Accuracy Results
combined_accuracy <- bind_rows(accuracy_results, garch_accuracy)

# Print Combined Accuracy
print(combined_accuracy)


# Create Modeltime Table with all models
models_table <- modeltime_table(
  model_fit_arima,          # ARIMA
  model_fit_prophet,        # Prophet
  model_fit_tbats,          # TBATS
  model_snaive,             # SNaive
  model_glmnet,             # Elastic Net
  model_rf                  # Random Forest
)

# Manually Add ANN, GARCH, E-GARCH, and F-GARCH Models


# ANN Model Accuracy
ann_accuracy <- data.frame(
  .model_id = max(models_table$.model_id) + 1,  # Increment model ID
  .model_desc = "ANN",
  rmse = accuracy(auto_ann_forecast$mean, test_ts)["Test set", "RMSE"],
  mae = accuracy(auto_ann_forecast$mean, test_ts)["Test set", "MAE"],
  rsq = cor(auto_ann_forecast$mean, test_ts)^2  # Ensure test_ts is aligned
)

# GARCH Accuracy
garch_accuracy <- data.frame(
  .model_id = max(models_table$.model_id) + 1,
  .model_desc = "GARCH(1,1)",
  rmse = sqrt(mean((forecast_mean - testing(splits)$value)^2)),
  mae = mean(abs(forecast_mean - testing(splits)$value)),
  rsq = ifelse(var(forecast_mean) > 0 && var(testing(splits)$value) > 0,
               cor(forecast_mean, testing(splits)$value)^2, NA)
)

# E-GARCH Accuracy
egarch_forecast_series <- egarch_forecast@forecast$seriesFor[, 1]  # Adjust slot or method
common_length <- min(length(egarch_forecast_series), length(testing(splits)$value))

egarch_forecast_series <- egarch_forecast_series[1:common_length]
observed_values <- testing(splits)$value[1:common_length]

egarch_accuracy <- data.frame(
  .model_id = max(models_table$.model_id) + 1,
  .model_desc = "E-GARCH",
  rmse = sqrt(mean((egarch_forecast_series - observed_values)^2)),
  mae = mean(abs(egarch_forecast_series - observed_values)),
  rsq = ifelse(var(egarch_forecast_series) > 0 && var(observed_values) > 0,
               cor(egarch_forecast_series, observed_values)^2, NA)
)

# F-GARCH Accuracy
# Check for zero standard deviation (constant values)
if (sd(fgarch_forecast_series) > 0 && sd(observed_values) > 0) {
  rsq <- cor(fgarch_forecast_series, observed_values)^2
} else {
  rsq <- NA  # Set R-squared to NA if there is no variation
}

# Calculate RMSE and MAE
fgarch_accuracy <- data.frame(
  .model_id = max(egarch_accuracy$.model_id) + 1,
  .model_desc = "F-GARCH",
  rmse = sqrt(mean((fgarch_forecast_series - observed_values)^2)),
  mae = mean(abs(fgarch_forecast_series - observed_values)),
  rsq = rsq
)

# Print the accuracy results
print(fgarch_accuracy)


# Combine All Accuracy Results
combined_accuracy <- bind_rows(
  models_table %>% modeltime_accuracy(),  # Existing models
  ann_accuracy,                          # ANN Model
  garch_accuracy,                        # GARCH Model
  egarch_accuracy,                       # E-GARCH Model
  fgarch_accuracy                        # F-GARCH Model
)

# Print Combined Accuracy Table
print(combined_accuracy)

# Plot RMSE Comparison
ggplot(combined_accuracy, aes(x = .model_desc, y = rmse, fill = .model_desc)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison (RMSE)", x = "Model", y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
