# Model 02 A

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(timetk)

the_path <- here::here()

mean_roll_251d <- slidify(.f = mean, .align = 'right', .period = 251)
sd_roll_21d <- slidify(.f = sd, .align = 'right', .period = 21)
sd_roll_61d <- slidify(.f = sd, .align = 'right', .period = 61)
sd_roll_67d <- slidify(.f = sd, .align = 'right', .period = 67)
sd_roll_147d <- slidify(.f = sd, .align = 'right', .period = 147)
sd_roll_251d <- slidify(.f = sd, .align = 'right', .period = 251)
corr_roll_101d <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 101)
corr_roll_147d <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 147)
corr_roll_199d <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 199)
corr_roll_251d <- slidify(.f = ~cor(.x, .y), .align = 'right', .period = 251)


model03a <- function(ticker, num_days){ 
  df <- read_csv(glue(the_path, '/data_stock_fmpr/', ticker, '.csv')) |> 
    arrange(date) |> 
    select(date, open, high, low, close, volume) |> 
    mutate(ret_1d = log(close / lag(close, n = 1)), 
           ret_5d = log(close / lag(close, n = 5)), 
           ret_21d = log(close / lag(close, 21)), 
           intraday = (high - low) / open, 
           roll_sd_intraday_61d = sd_roll_61d(intraday), 
           roll_sd_ret5d_61d = sd_roll_61d(ret_5d), 
           roll_sd_ret1d_67d = sd_roll_67d(ret_1d),
           roll_sd_ret1d_147d = sd_roll_147d(ret_1d), 
           roll_sd_ret5d_147d = sd_roll_147d(ret_5d), 
           too_corr1 = corr_roll_147d(roll_sd_ret1d_147d, roll_sd_ret5d_147d), 
           roll_sd_intraday_251d = sd_roll_251d(intraday),
           roll_sd_ret1d_251d = sd_roll_251d(ret_1d), 
           too_corr2 = corr_roll_251d(roll_sd_intraday_251d, roll_sd_ret1d_251d), 
           roll_sd_ret21d_251d = sd_roll_251d(ret_21d),  
           too_corr3 = corr_roll_199d(roll_sd_ret21d_251d, roll_sd_intraday_251d), 
           
           ema20 = TTR::EMA(close, 20), 
           sma50 = TTR::SMA(close, 50), 
           sma101 = TTR::SMA(close, 101),
           sma200 = TTR::SMA(close, 199),
           roll_mean_sma200_251d = mean_roll_251d(sma200), 
           roll_sd_sma200_251d = sd_roll_251d(sma200), 
           ma_cross_l = sma50 / sma101, 
           above_sd_sma200_251d = (sma200 - roll_mean_sma200_251d) / roll_sd_sma200_251d,
           corr_ema20_sma50_199d = corr_roll_199d(ema20, sma50), 
           corr_ema20_sma50_251d = corr_roll_251d(ema20, sma50), 
           corr_sma50_sma200_101d = corr_roll_101d(sma50, sma200), 
           corr_sma50_sma200_199d = corr_roll_199d(sma50, sma200), 
           corr_sma50_sma200_251d = corr_roll_251d(sma50, sma200), 
           
           volum_sma50 = TTR::SMA(volume,  n = 53),
           volum53_perc = log(volume / volum_sma50), 
           roll_sd_volu50_21d = sd_roll_21d(volum53_perc), 
           volum_sma100 = TTR::SMA(volume,  n = 101),
           volum100_perc = log(volume / volum_sma100), 
           roll_sd_volu100_61d = sd_roll_61d(volum100_perc), 
           volum_sma200 = TTR::SMA(volume,  n = 199), 
           volum200_perc = log(volume / volum_sma200), 
           roll_sd_volu200_251d = sd_roll_251d(volum200_perc), 
           
           forw_ret = log(lead(close, n = parse_number(num_days)) / close), 
           ord_class = as.factor(ntile(forw_ret, 3))) |> 
    select(date, ord_class, 
           ma_cross_l, above_sd_sma200_251d, 
           roll_sd_intraday_61d, roll_sd_ret5d_61d, 
           roll_sd_ret1d_147d, too_corr1, 
           too_corr3, too_corr2, roll_sd_ret21d_251d, 
           corr_sma50_sma200_101d, 
           corr_ema20_sma50_199d, corr_ema20_sma50_251d, 
           corr_sma50_sma200_199d, corr_sma50_sma200_251d, 
           roll_sd_volu50_21d, roll_sd_volu100_61d, roll_sd_volu200_251d
    )
  return(df)
}
