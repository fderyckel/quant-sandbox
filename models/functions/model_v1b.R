# model V1b

model_v1b <- function(ticker) {
  df <- read_csv(glue::glue(the_path, '/data_stock_fmpr/', ticker, '.csv')) |> 
    arrange(date) |> 
    select(date, open, high, low, close, volume) |>
    mutate(ret_1d = log(close / lag(close)), 
           roll_mean_ret1d_3M = roll_mean(ret_1d, 61), 
           roll_sd_ret1d_3M = roll_sd(ret_1d, 61), 
           above_sd_ret1d_3M = (ret_1d - roll_mean_ret1d_3M) / roll_sd_ret1d_3M, 
           
           ret_5d = log(close / lag(close, n = 5)), 
           roll_mean_ret5d_3M = roll_mean(ret_5d, 61), 
           roll_sd_ret5d_3M = roll_sd(ret_5d, 61), 
           above_sd_ret5d_3M = (ret_5d - roll_mean_ret5d_3M) / roll_sd_ret5d_3M, 
           roll_mean_ret5d_7M = roll_mean(ret_5d, 147), 
           roll_sd_ret5d_7M = roll_sd(ret_5d, 147), 
           above_sd_ret5d_7M = (ret_5d - roll_mean_ret5d_7M) / roll_sd_ret5d_7M, 
           
           ret_21d = log(close / lag(close, 21)), 
           roll_sd_ret21d_1Y = roll_sd(ret_21d, 251), 
           roll_mean_ret21d_1Y = roll_mean(ret_21d, 251), 
           above_sd_ret21d_1Y = (ret_21d - roll_mean_ret21d_1Y) / roll_sd_ret21d_1Y,
           
           intra_day = (high - low) / open, 
           roll_mean_intra_day_1Y = roll_mean(intra_day,  251), 
           roll_sd_intra_day_1Y = roll_sd(intra_day,  251), 
           roll_sd_intra_day_3M = roll_sd(intra_day,  61), 
           above_sd_intraday_1Y = (intra_day - roll_mean_intra_day_1Y) / roll_sd_intra_day_1Y, 
           
           ## old parameters
           ema20 = TTR::EMA(close, 20), 
           sma50 = TTR::SMA(close, 50), 
           sma100 = TTR::SMA(close, 100), 
           sma200 = TTR::SMA(close, 200),
           volum_sma200 = TTR::SMA(volume,  n = 200), 
           volum200_perc = log(volume / volum_sma200), 
           
           ma_cross_l = sma50 / sma100, 
           corr_ema20_sma50_1Y = roll_cor(ema20, sma50, width = 251), 
           corr_sma50_sma200_199d = roll_cor(sma50, sma200, 199), 
           volat_ret1d_63days = roll_sd(ret_1d, 63), 
           volat_volu200_31days = roll_sd(volum200_perc, width = 31), 
           volat_volu200_251days = roll_sd(volum200_perc, width = 251), 
           forw_ret = log(lead(close, n = num_days) / close), 
           ord_class = ntile(forw_ret, 5), 
           ord_class2 = case_when(ord_class == 1 ~ 1, 
                                  ord_class == 2 ~ 1, 
                                  ord_class == 3 ~ 2, 
                                  ord_class == 4 ~ 3, 
                                  ord_class == 5 ~ 3), 
           ord_class2 = as.factor(ord_class2))
  
  df2 <- df |> 
    select(date, above_sd_ret1d_3M, above_sd_ret5d_3M, above_sd_ret5d_7M, above_sd_intraday_1Y, roll_sd_intra_day_3M, 
           volat_volu200_31days, volat_volu200_251days, roll_sd_ret21d_1Y, above_sd_ret21d_1Y, 
           ma_cross_l, corr_ema20_sma50_1Y, corr_sma50_sma200_199d, volat_ret1d_63days, 
           ord_class2) |> 
    drop_na()
  
  return(df2)
}
