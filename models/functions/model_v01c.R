# Model 01 A

library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(tidyr)
library(glue)
library(timetk)

the_path <- here::here()


sd_roll_31d <- slidify(.f = sd, .align = 'right', .period = 31)
mean_roll_3M <- slidify(.f = mean, .align = 'right', .period = 61)
sd_roll_3M <- slidify(.f = sd, .align = 'right', .period = 61)
sd_roll_63d <- slidify(.f = sd, .align = 'right', .period = 63)
mean_roll_7M <- slidify(.f = mean, .align = 'right', .period = 147)
sd_roll_7M <- slidify(.f = sd, .align = 'right', .period = 147)
mean_roll_1Y <- slidify(.f = mean, .align = 'right', .period = 251)
sd_roll_1Y <- slidify(.f = sd, .align = 'right', .period = 251)
corr_roll_199d <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 199)
corr_roll_1Y <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 251)

model01a <- function(ticker, num_days){ 
  df <- read_csv(glue(the_path, '/data_stock_fmpr/', ticker, '.csv')) |> 
    arrange(date) |> 
    select(date, open, high, low, close, volume) |>
    mutate(ret_1d = log(close / lag(close, n = 1)), 
           ret_5d = log(close / lag(close, n = 5)), 
           ret_21d = log(close / lag(close, 21)), 
           
           roll_sd_ret1d_63days = sd_roll_63d(ret_1d), 
           roll_mean_ret1d_3M = mean_roll_3M(ret_1d), 
           roll_sd_ret1d_3M = sd_roll_3M(ret_1d), 
           above_sd_ret1d_3M = (ret_1d - roll_mean_ret1d_3M) / roll_sd_ret1d_3M, 
           roll_mean_ret5d_3M = mean_roll_3M(ret_5d), 
           roll_sd_ret5d_3M = sd_roll_3M(ret_5d), 
           above_sd_ret5d_3M = (ret_5d - roll_mean_ret5d_3M) / roll_sd_ret5d_3M, 
           roll_mean_ret5d_7M = mean_roll_7M(ret_5d), 
           roll_sd_ret5d_7M = sd_roll_7M(ret_5d), 
           above_sd_ret5d_7M = (ret_5d - roll_mean_ret5d_7M) / roll_sd_ret5d_7M, 
           roll_mean_ret21d_1Y = mean_roll_1Y(ret_21d), 
           roll_sd_ret21d_1Y = sd_roll_1Y(ret_21d),          
           above_sd_ret21d_1Y = (ret_21d - roll_mean_ret21d_1Y) / roll_sd_ret21d_1Y,
           
           intraday = (high - low) / open, 
           roll_mean_intraday_1Y = mean_roll_1Y(intraday), 
           roll_sd_intraday_1Y = sd_roll_1Y(intraday), 
           roll_sd_intraday_3M = sd_roll_3M(intraday), 
           above_sd_intraday_1Y = (intraday - roll_mean_intraday_1Y) / roll_sd_intraday_1Y, 
           
           ## old parameters
           ema20 = TTR::EMA(close, 20), 
           sma50 = TTR::SMA(close, 50), 
           sma100 = TTR::SMA(close, 100), 
           sma200 = TTR::SMA(close, 200),
           volum_sma200 = TTR::SMA(volume,  n = 200), 
           volum200_perc = log(volume / volum_sma200), 
           
           ma_cross_l = sma50 / sma100, 
           corr_ema20_sma50_199d = corr_roll_199d(ema20, sma50), 
           corr_ema20_sma50_1Y = corr_roll_1Y(ema20, sma50), 
           corr_sma50_sma200_199d = corr_roll_199d(sma50, sma200), 
           corr_sma50_sma200_1Y = corr_roll_1Y(sma50, sma200), 
           roll_sd_volu200_31days = sd_roll_31d(volum200_perc), 
           roll_sd_volu200_251days = sd_roll_1Y(volum200_perc), 
           forw_ret = log(lead(close, n = num_days) / close), 
           ord_class = ntile(forw_ret, 5), 
           ord_class2 = case_when(ord_class == 1 ~ 1, 
                                  ord_class == 2 ~ 1, 
                                  ord_class == 3 ~ 2, 
                                  ord_class == 4 ~ 3, 
                                  ord_class == 5 ~ 3), 
           ord_class2 = as.factor(ord_class2)) |> 
    select(date, roll_sd_ret1d_63days, roll_sd_ret5d_3M, roll_sd_ret5d_7M, 
           above_sd_ret1d_3M, above_sd_ret5d_3M, above_sd_ret5d_7M, above_sd_ret21d_1Y,
           roll_sd_ret21d_1Y, above_sd_intraday_1Y, roll_sd_intraday_3M, 
           ma_cross_l, corr_ema20_sma50_199d, corr_ema20_sma50_1Y, 
           corr_sma50_sma200_199d, corr_sma50_sma200_1Y,  
           roll_sd_volu200_31days, roll_sd_volu200_251days, 
           ord_class = ord_class2) 
  
  return(df)
  }
        
