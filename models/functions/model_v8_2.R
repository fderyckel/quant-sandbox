library(readr)
library(dplyr)
library(purrr)
library(timetk)
library(lubridate)

the_path <- here::here()

mean_roll_21 <- slidify(.f = mean, .align = 'right', .period = 21)
mean_roll_252 <- slidify(.f = mean, .align = 'right', .period = 252)
sd_roll_252 <- slidify(.f = sd, .align = 'right', .period = 252)


model_v82 <- function(ticker){ 
  df <- read_csv(paste0(the_path, "/data_stock_fmpr/", ticker, ".csv"), show_col_types = FALSE) |>  
    select(date, open, high, low, close, adjusted=adjClose, volume) |>  
    arrange(date) |> 
    filter(date < today() - 252) 
  
  df <- df |>   
    arrange(date) |>  
    mutate(roc21 = log(adjusted / lag(adjusted, n = 21)), 
           
           ema11 = TTR::EMA(adjusted, n = 11), 
           ema23 = TTR::EMA(adjusted, n = 23), 
           sma47 = TTR::SMA(adjusted, n = 47), 
           sma101 = TTR::SMA(adjusted, n = 101), 
           sma199 = TTR::SMA(adjusted, n = 199), 
           perc_abov_ema11 = log(adjusted / ema11), 
           perc_abov_ema23 = log(adjusted / ema23), 
           perc_abov_sma47 = log(adjusted / sma47), 
           perc_abov_sma101 = log(adjusted / sma101), 
           perc_abov_sma199 = log(adjusted / sma199), 
           sd_abov_ema11 = (ema11 - mean_roll_252(perc_abov_ema11)) / sd_roll_252(perc_abov_ema11), 
           sd_abov_ema23 = (ema23 - mean_roll_252(perc_abov_ema23)) / sd_roll_252(perc_abov_ema23), 
           sd_abov_sma47 = (sma47 - mean_roll_252(perc_abov_sma47)) / sd_roll_252(perc_abov_sma47), 
           sd_abov_sma101 = (sma101 - mean_roll_252(perc_abov_sma101)) / sd_roll_252(perc_abov_sma101),  
           sd_abov_sma199 = (sma199 - mean_roll_252(perc_abov_sma199)) / sd_roll_252(perc_abov_sma199),  
           
           volum_sma200 = TTR::SMA(volume,  n = 200), 
           volum200_perc = log(volume / volum_sma200), 
           
           volat_roc21_252d = sd_roll_252(roc21), 
           volat_volu200_252d = sd_roll_252(volum200_perc), 
           
           ret_7w = (lead(adjusted, n = 35) / adjusted) - 1)
  
  df <- df |> 
    select(date, 
           sd_abov_ema11, sd_abov_ema23, sd_abov_sma47, sd_abov_sma101, sd_abov_sma199,
           volat_roc21_252d, volat_volu200_252d, 
           ret_7w)
}