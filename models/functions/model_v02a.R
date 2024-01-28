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
mean_roll_61d <- slidify(.f = mean, .align = 'right', .period = 61)
sd_roll_61d <- slidify(.f = sd, .align = 'right', .period = 61)
sd_roll_63d <- slidify(.f = sd, .align = 'right', .period = 63)
mean_roll_147d <- slidify(.f = mean, .align = 'right', .period = 147)
sd_roll_147d <- slidify(.f = sd, .align = 'right', .period = 147)
mean_roll_251d <- slidify(.f = mean, .align = 'right', .period = 251)
sd_roll_251d <- slidify(.f = sd, .align = 'right', .period = 251)
corr_roll_101d <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 101)
corr_roll_199d <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 199)
corr_roll_229d <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 229)
corr_roll_251d <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 251)

model02a <- function(ticker, num_days){ 
  df <- read_csv(glue(the_path, '/data_stock_fmpr/', ticker, '.csv')) |> 
    arrange(date) |> 
    select(date, open, high, low, close, volume)
  yo <- tibble(atr11 = TTR::ATR(df[, 3:5], n = 11)[,2])
  df <- bind_cols(df, yo) |> mutate(atr11_open = atr11/open) |> select(-atr11)
  yo <- tibble(atr17 = TTR::ATR(df[, 3:5], n = 17)[,2]) 
  df <- bind_cols(df, yo) |> mutate(atr17_open = atr17/open) |> select(-atr17)
  
  df <- df |> 
    mutate(ret_1d = log(close / lag(close, n = 1)), 
           ret_5d = log(close / lag(close, n = 5)), 
           ret_21d = log(close / lag(close, 21)), 
           
           roll_sd_ret1d_61d = sd_roll_61d(ret_1d), 
           roll_mean_ret1d_61d = mean_roll_61d(ret_1d), 
           roll_sd_ret1d_147d = sd_roll_147d(ret_1d), 
           roll_mean_ret5d_61d = mean_roll_61d(ret_5d), 
           roll_sd_ret5d_61d = sd_roll_61d(ret_5d), 
           roll_mean_ret5d_147d = mean_roll_147d(ret_5d), 
           roll_sd_ret5d_147d = sd_roll_147d(ret_5d), 
           roll_sd_ret5d_147d_lag5 = lag(roll_sd_ret5d_147d, n = 5), 
           roll_sd_ret5d_147d_lag11 = lag(roll_sd_ret5d_147d, n = 11), 
           corr_rollSdRet5d3M_rollSdRet5d7M_229d = corr_roll_229d(roll_sd_ret5d_61d, roll_sd_ret5d_147d), 
           roll_sd_ret21d_251d = sd_roll_251d(ret_21d),     
           roll_sd_ret21d_251d_lag7 = lag(roll_sd_ret21d_251d, n = 7), 
           roll_sd_ret21d_251d_lag19 = lag(roll_sd_ret21d_251d, n = 11),
           
           intraday = (high - low) / open, 
           roll_sd_intraday_61d = sd_roll_61d(intraday), 
           roll_sd_intraday_251d = sd_roll_251d(intraday), 
           
           ## old parameters
           ema20 = TTR::EMA(close, 20), 
           sma50 = TTR::SMA(close, 50), 
           sma101 = TTR::SMA(close, 101), 
           sma199 = TTR::SMA(close, 199),
           roll_mean_sma199_251d = mean_roll_251d(sma199), 
           roll_sd_sma199_251d = sd_roll_251d(sma199), 
           above_sd_sma199_251d = (sma199 - roll_mean_sma199_251d) / roll_sd_sma199_251d, 
           sma200 = TTR::SMA(close, 200),
           ma_cross_l = sma50 / sma101, 
           corr_ema20_sma50_199d = corr_roll_199d(ema20, sma50), 
           corr_ema20_sma50_251d = corr_roll_251d(ema20, sma50), 
           corr_sma50_sma200_101d = corr_roll_101d(sma50, sma200), 
           corr_sma50_sma200_199d = corr_roll_251d(sma50, sma200), 
           
           volum_sma200 = TTR::SMA(volume,  n = 200), 
           volum200_perc = log(volume / volum_sma200), 
           roll_sd_volu200_31d = sd_roll_31d(volum200_perc), 
           roll_sd_volu200_251d = sd_roll_251d(volum200_perc), 
           roll_sd_volu200_251d_lag5 = lag(roll_sd_volu200_251d, n = 5), 
           roll_sd_volu200_251d_lag17 = lag(roll_sd_volu200_251d, n = 17), 
           
           forw_ret = log(lead(close, n = num_days) / close), 
           ord_class = ntile(forw_ret, 3)) |> 
    
    select(date, atr11_open, atr17_open, 
           roll_mean_ret1d_61d, roll_mean_ret5d_61d, roll_mean_ret5d_147d, 
           roll_sd_ret1d_61d, roll_sd_ret1d_147d, roll_sd_ret5d_147d, 
           roll_sd_ret5d_147d_lag5, roll_sd_ret5d_147d_lag11, 
           corr_rollSdRet5d3M_rollSdRet5d7M_229d, 
           roll_sd_ret21d_251d, roll_sd_intraday_61d, roll_sd_intraday_251d, 
           roll_sd_ret21d_251d_lag7, roll_sd_ret21d_251d_lag19, 
           above_sd_sma199_251d, 
           ma_cross_l, 
           corr_ema20_sma50_199d, corr_ema20_sma50_251d, 
           corr_sma50_sma200_199d, corr_sma50_sma200_101d,
           roll_sd_volu200_31d, roll_sd_volu200_251d, 
           roll_sd_volu200_251d_lag5, roll_sd_volu200_251d_lag17, 
           ord_class) 
  
  return(df)
  }
        
