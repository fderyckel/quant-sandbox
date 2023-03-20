library(glue)
library(readr)
library(dplyr)
library(purrr)
library(roll)
library(gridExtra)        # to stack the charts
library(scales)           # again to deal with dates but on the x-axis
library(bdscale)          # to remove the weekends using the scale_x_bd
library(httr)
library(rvest)
library(tidyr)
library(lubridate)

########## Functions
#### 1. Download data - Fetching FMPR price
#### 2. conform_data - to fetch the data on our local drive from a specific provider 
#### 2. create_base_plot_df - to create a df with all the required variable
#### 3. quick_read
#### 4. relative_return - 
#### 5. reduce_df - select a few chosen variables used in the modeling phase
#### 6. 


### Function to harmonize the data.   
### Are the data from Yahoo or AV?  Are there intraday data or EoD data?  
######################################################################################
conform_data <- function(ticker, interval, provider){
  if (interval == "Daily" & provider == "Yahoo"){
    df <- read_csv(paste0(the_path, "/data_stock_ya/", ticker, ".csv"), show_col_types = FALSE) %>% 
      na.omit()
  } else if (interval == "Daily" & provider == "Tiingo"){
    df <- read_csv(paste0(the_path, "/data_stock_ti/", ticker, ".csv"), show_col_types = FALSE) %>% 
      na.omit() %>% 
      rename(index = date, open = adjOpen, high = adjHigh, low = adjLow, close = adjClose, 
             adjusted = adjClose, volume = adjVolume)    
  } else if (interval == "Daily" & provider == "fmpr"){
    df <- read_csv(paste0(the_path, "/data_stock_fmpr/", ticker, ".csv"), show_col_types = FALSE) %>% 
      rename(index = date, adjusted = adjClose)    
  }
  df <- df %>% arrange(index)
  return(df)
}


#######################################################################
## Using models for prediction. 
#######################################################################
source(paste0(the_path, "/models/functions/wrangle_data_v06.R"))
source(paste0(the_path, "/models/functions/wrangle_data_v07_2.R"))
source(paste0(the_path, "/models/functions/wrangle_data_v08_2.R"))

predict_v6_long <- function(ticker, model_ticker, mkt, sect){ 
  test0 <- conform_data(ticker, interval = "Daily", provider = "fmpr") %>% 
    create_base_fin_df_v6() %>% mutate(ord_class = as.factor(ntile(ret_5w, 3))) 
  test1 <- relative_returns_v6(ticker, mkt, sect) 
  test <- left_join(test0, test1, by = "index") %>% reduce_test_df_v6() 
  test_recent <- test %>% filter(index > today() - 100)
  fit_all <- fit(read_rds(glue(the_path, "/models/models_raw/xgboost_modelv62e_5wClassThirdile_", 
                               model_ticker,".rda")), 
                 data = (test %>% drop_na())) 
  predict_recent <- tibble(index = test_recent$index, 
                           predictions = predict(fit_all, new_data = test_recent, type = "prob"), 
                           pred_class = predict(fit_all, new_data = test_recent))
  yo_v6 <- left_join(test_recent, predict_recent, by = "index") %>% 
    select(index, ret_5w, ord_class, predictions, pred_class) %>% 
    mutate(ticker = ticker, model = "V62e Long")

  return(yo_v6)
}

library(embed)
predict_v7_long <- function(ticker, model_ticker, mkt, sect){ 
  test0 <- conform_data(ticker, interval = "Daily", provider = "fmpr") %>% 
    create_base_fin_df_v7() %>% mutate(ord_class = as.factor(ntile(ret_5w, 3))) 
  test1 <- relative_returns_v7(ticker, mkt, sect) 
  test <- left_join(test0, test1, by = "index") %>% reduce_test_df_v7()  
  test_recent <- test %>% filter(index > today() - 100)
  fit_all <- fit(read_rds(glue(the_path, "/models/models_raw/xgboost_modelv71e_5wClassThirdile_", model_ticker,".rda")), 
                 data = (test %>% drop_na())) 
  predict_recent <- tibble(index = test_recent$index, 
                           predictions = predict(fit_all, new_data = test_recent, type = "prob"), 
                           pred_class = predict(fit_all, new_data = test_recent))
  yo_v7 <- left_join(test_recent, predict_recent, by = "index") %>% 
    select(index, ret_5w, ord_class, predictions, pred_class) %>% 
    mutate(ticker = ticker, model = "V71e Long")
  
  return(yo_v7)
}


predict_v8_long <- function(ticker, model_ticker, mkt, sect){ 
  test0 <- conform_data(ticker, interval = "Daily", provider = "fmpr") %>% 
    create_base_fin_df_v8() %>% mutate(ord_class = as.factor(ntile(ret_5w, 3))) 
  test1 <- relative_returns_v8(ticker, mkt, sect) 
  test2 <- portion_above_ma(sect, ticker) 
  test <- left_join(test0, test1, by = "index") %>% left_join(., test2, by = "index") %>% reduce_test_df_v8() 
  test_recent <- test %>% filter(index > today() - 100)
  fit_all <- fit(read_rds(glue(the_path, "/models/models_raw/xgboost_modelv82e_5wClassThirdile_", model_ticker,".rda")), 
                 data = (test %>% drop_na())) 
  predict_recent <- tibble(index = test_recent$index, 
                           predictions = predict(fit_all, new_data = test_recent, type = "prob"), 
                           pred_class = predict(fit_all, new_data = test_recent))
  yo_v8 <- left_join(test_recent, predict_recent, by = "index") %>% 
    select(index, ret_5w, ord_class, predictions, pred_class) %>% 
    mutate(ticker = ticker, model = "V81e Long")
  return(yo_v8)
}
