library(glue)
library(readr)
library(dplyr)
library(purrr)
library(roll)
library(gridExtra)        # to stack the charts
library(scales)           # again to deal with dates but on the x-axis
library(bdscale)          # to remove the weekends using the scale_x_bd
library(ggplot2)
library(tidyr)
library(lubridate)


### !!!! Change this ... don't avoid the problem ... do we have 0 or NA in the df???
create_plot_fin_df <- function(df) { 
  df <- df %>% filter(open != "null" & close != 0 & adjusted != 0) %>% 
    select(index, open, high, low, close, adjusted, volume) %>% arrange(index) %>% 
    mutate(open = as.numeric(open), high = as.numeric(high), low = as.numeric(low), 
           close = as.numeric(close), adjusted = as.numeric(adjusted), 
           Volume = if_else((is.na(volume) | volume == 0), lag(volume), volume)) %>% 
    select(-volume) %>% 
    rename(volume = Volume) %>% na.omit()
  yo <- TTR::ATR(df[,3:5], n = 13) %>% as_tibble() %>% select(atr13 = atr) 
  adx <- TTR::ADX(df[,3:5], n = 17) %>% as_tibble() %>% select(adx17 = ADX, din17 = DIn, dip17 = DIp)
  df <- bind_cols(df, adx)
  df <- bind_cols(df, yo)
  df <- df %>% 
    mutate(ema5 = TTR::EMA(adjusted, 5), 
           ema9 = TTR::EMA(adjusted, 9), 
           ema12 = TTR::EMA(adjusted, 12),
           ema13 = TTR::EMA(adjusted, 13),
           ema26 = TTR::EMA(adjusted, 26),
           ema43 = TTR::EMA(adjusted, 43), 
           row_num = row_number(), 
           cl43_sd = roll_sd(close, width = 43), 
           tti_atr_cl43sd = roll_cor(atr13,  cl43_sd, width=21), 
           cl_roll_corr = roll_cor(adjusted, row_num, width=41), 
           cl_roll_corr_long = roll_cor(adjusted, row_num, width=113), 
           sma50 = TTR::SMA(adjusted, 50), 
           sma200 = TTR::SMA(adjusted, 200), 
           rsi14 = TTR::RSI(close, 14), 
           rsi5 = TTR::RSI(close, 5), 
           ppo_line = ((ema12 - ema26) / ema26) * 100, 
           ppo_line_st = ((ema5 - ema13) / ema13) * 100,
           ppo_signal = TTR::EMA(ppo_line, n = 9), 
           ppo_signal_st = TTR::EMA(ppo_line_st, n = 7))
  return(df)
}



############################################################################################
########################### Plotting functions #############################################
############################################################################################

geom_candlestick <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = TRUE, show.legend = NA,
                             inherit.aes = TRUE,
                             colour_up = "gray30", colour_down = "gray30",
                             fill_up = "green3", fill_down = "red",
                             ...) {
  
  linerange <- ggplot2::layer(
    stat = StatLinerangeBC, geom = GeomLinerangeBC, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  colour_up = colour_up, colour_down = colour_down, ...)
  )
  
  rect <- ggplot2::layer(
    stat = StatRectCS, geom = GeomRectCS, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  colour_up = colour_up, colour_down = colour_down, ...)
  )
  
  list(linerange, rect)
}

StatLinerangeBC <- ggproto("StatLinerangeBC", Stat, required_aes = c("x", "open", "high", "low", "close"), 
                           compute_group = function(data, scales, params, fill_up, fill_down, colour_up, colour_down) { 
                             data <-  data %>% dplyr::mutate(colour = ifelse(open < close, colour_up, colour_down))
                             tibble(x = data$x, ymin = data$low, ymax = data$high, colour = data$colour)  })

StatRectCS <- ggproto("StatRectCS", Stat, required_aes = c("x", "open", "high", "low", "close"), 
                      compute_group = function(data, scales, params, fill_up, fill_down, colour_up, colour_down) { 
                        data <-  data %>%
                          mutate(fill = ifelse(open < close, fill_up, fill_down), 
                                 ymin = ifelse(open < close, open, close), ymax = ifelse(open < close, close, open)) 
                        tibble(xmin = data$x - 0.45, xmax = data$x + 0.45, ymin = data$ymin, 
                               ymax = data$ymax, fill = data$fill)  })

GeomRectCS <- ggproto("GeomRectCS", GeomRect,
                      default_aes = aes(colour = NA, size = 0.5, linetype = 1, alpha = NA))

GeomLinerangeBC <- ggproto("GeomLinerangeBC", GeomLinerange, default_aes = aes(size = 0.5, linetype = 1,  alpha = NA))

#####################################################
## Create standard Candlestick chart with PPO and RSI
create_candlestick_chart <- function(df, tickerss, start_date = today()-365, end_date = today()){
  df2 <- df %>% filter(index >= start_date & index <= end_date)
  
  # The main chart with the moving averages
  p1 <- ggplot(df2, aes(x=index, y = close)) + 
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) + 
    geom_line(aes(y = ema9), colour = "red", linewidth = 0.2) + 
    geom_line(aes(y = sma200), colour = "darkorchid1", linewidth = 0.3) + 
    # because I need to remember which chart is it (to which stock it belongs)
    annotate("text", x = df2$index[10], y = 1.1 * df2$close[10], label = tickerss, color = "white") + 
    geom_line(aes(y = sma50), colour = "Turquoise 1", linewidth = 0.3) + 
    scale_x_bd(business.dates=df2$index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.3)) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0.2, 0.2, 0.1, 0.4, "cm"),       # This is to shrink the padding at the 4 side of the graph
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.2),
          panel.grid.minor.y = element_line(colour = "white", linetype = "dotted", size = 0.15),
          panel.grid.minor.x = element_blank())
  
  # graphing of the ppo part.  
  p2 <- ggplot(df2, aes(x = index)) + 
    geom_line(aes(y = ppo_signal), colour = "darkorchid1",  linewidth = 0.4) + 
    geom_line(aes(y = ppo_line),  colour = "Royal Blue 1",  linewidth = 0.5) + 
    geom_hline(yintercept = 0, colour = "red", linetype = "dashed",  linewidth = 0.3) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    scale_x_bd(business.dates=df2$index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.1)) + 
    ylab("PPO MT") + 
    theme(legend.position = "none",  
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0, 0.2, 0.1, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.1), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  p3 <- ggplot(df2, aes(x = index)) + 
    geom_line(aes(y = ppo_signal_st), colour = "darkorchid1", linewidth = 0.4) + 
    geom_line(aes(y = ppo_line_st), colour = "Royal Blue 1", linewidth = 0.5) + 
    geom_hline(yintercept = 0, colour = "red", linetype = "dashed", linewidth = 0.3) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    scale_x_bd(business.dates=df2$index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.1)) + 
    ylab("PPO ST") +  
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90), 
          axis.text.x = element_text(angle = 30, vjust = 0.4), 
          plot.margin = margin(0.0, 0.2, 0.2, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.1), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  
  yo <- grid.arrange(p1, p2, p3, ncol = 1, heights = c(2.5, 1, 1))
  yo
}

#####################################################
## create candlestick chart with ATR and sd (EMA40)
create_vol_candlestick_chart <- function(df, tickerss, start_date = today()-365, end_date = today()){
  df2 <- df %>% filter(index >= start_date & index <= end_date)
  
  # The main chart with the moving averages
  p1 <- ggplot(df2, aes(x=index, y = close)) + 
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) + 
    geom_line(aes(y = ema43), colour = "Turquoise 1", linewidth = 0.3) + 
    geom_line(aes(y = ema43 + (1 * atr13)), colour = "lightpink2", linewidth = 0.2) + 
    geom_line(aes(y = ema43 + (2 * atr13)), colour = "lightpink2", linewidth = 0.2) + 
    geom_line(aes(y = ema43 - (1 * atr13)), colour = "lightpink2", linewidth = 0.2) +
    geom_line(aes(y = ema43 - (2 * atr13)), colour = "lightpink2", linewidth = 0.2) +
    geom_line(aes(y = ema43 + (1 * cl43_sd)), colour = "darkorchid1", linewidth = 0.2) + 
    geom_line(aes(y = ema43 + (2 * cl43_sd)), colour = "darkorchid1", linewidth = 0.2) + 
    geom_line(aes(y = ema43 - (1 * cl43_sd)), colour = "darkorchid1", linewidth = 0.2) +
    geom_line(aes(y = ema43 - (2 * cl43_sd)), colour = "darkorchid1", linewidth = 0.2) +
    # because I need to remember which chart is it (to which stock it belongs)
    annotate("text", x = df2$index[10], y = 1.1 * df2$close[10], label = tickerss, color = "white") + 
    #geom_line(aes(y = sma50), colour = "Turquoise 1", size = 0.3) + 
    #scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.3)) + 
    scale_x_bd(business.dates=df2$index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0.2, 0.2, 0.1, 0.4, "cm"),       # This is to shrink the padding at the 4 side of the graph
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.15), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.15),
          panel.grid.minor.y = element_line(colour = "white", linetype = "dotted", size = 0.15),
          panel.grid.minor.x = element_blank())
  
  p2 <- ggplot(df2, aes(x = index)) + 
    geom_line(aes(y = cl_roll_corr), colour = "Dark Orange") + 
    geom_line(aes(y = cl_roll_corr_long), colour = "Gray 80") + 
    geom_hline(yintercept = 0, colour = "red", linetype = "dashed", linewidth = 0.3) + 
    scale_x_bd(business.dates=df2$index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.)) + 
    ylab("Correlation 8w vs 26w") + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90), 
          axis.text.x = element_blank(), 
          plot.margin = margin(0.0, 0.2, 0.2, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.1), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  p3 <- ggplot(df2, aes(x = index)) + 
    #geom_line(aes(y = cl_roll_corr, colour = "Dark Orange")) + 
    geom_line(aes(y = adx17), colour = "blue") + 
    geom_line(aes(y = din17), colour = "red") + 
    geom_line(aes(y = dip17), colour = "green") + 
    geom_hline(yintercept = 21, colour = "red", linetype = "dashed", linewidth = 0.3) + 
    scale_x_bd(business.dates=df2$index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.)) + 
    ylab("ADX") + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90), 
          axis.text.x = element_text(angle = 30, vjust = 0.4), 
          plot.margin = margin(0.0, 0.2, 0.2, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.1), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  
  yo <- grid.arrange(p1, p2, p3,  ncol = 1, heights = c(3, 1, 1))
  yo
  #p1
}


#####################################################
## create candlestick chart with ETF1 (market) and etf2 (sector)
create_rel_candlestick_chart <- function(ticker, etf1, etf2, 
                                         tickerss, start_date = today()-365, end_date = today()) {
  df_ticker <- conform_data(ticker, interval = "Daily", provider = "fmpr") %>% 
    select(index, adjusted) %>% 
    mutate(adjusted_tic = as.numeric(adjusted)) 
  df_etf1 <- conform_data(etf1, interval = "Daily", provider = "fmpr") %>% 
    select(index, adjusted) %>% mutate(adjusted_etf1 = as.numeric(adjusted)) 
  df_etf2 <- conform_data(etf2, interval = "Daily", provider = "fmpr") %>% 
    select(index, adjusted) %>% mutate(adjusted_etf2 = as.numeric(adjusted)) 
  yo0 <- left_join(df_etf1, df_etf2, by = "index") %>% na.omit() %>% 
    mutate(adjusted_ms = adjusted_etf2 / adjusted_etf1, 
           ms_ema139 = TTR::EMA(adjusted_ms, n = 139), 
           ms_sma373 = TTR::SMA(adjusted_ms, n = 307)) %>% 
    select(index, adjusted_ms, ms_ema139, ms_sma373)
  yo1 <- left_join(df_ticker, df_etf1, by = "index") %>% na.omit() %>% 
    mutate(adjusted_tic_mark = adjusted_tic / adjusted_etf1, 
           tic_mark_ema139 = TTR::EMA(adjusted_tic_mark, n = 139), 
           tic_mark_sma373 = TTR::SMA(adjusted_tic_mark, n = 307)) %>% 
    select(index, adjusted_tic_mark, tic_mark_ema139, tic_mark_sma373)
  yo2 <- left_join(df_ticker, df_etf2, by = "index") %>% na.omit() %>% 
    mutate(adjusted_tic_sect = adjusted_tic / adjusted_etf2, 
           tic_sect_ema139 = TTR::EMA(adjusted_tic_sect, n = 139), 
           tic_sect_sma373 = TTR::SMA(adjusted_tic_sect, n = 307)) %>% 
    select(index, adjusted_tic_sect, tic_sect_ema139, tic_sect_sma373)
  
  df2 <- left_join(yo0, yo1) %>% left_join(., yo2) %>% 
    filter(index >= start_date & index <= end_date)
  
  # The main chart with the moving averages
  p1 <- ggplot(df2, aes(x=index, y = adjusted_ms)) + 
    geom_line(color = "Gray 70") + 
    geom_line(aes(y = ms_ema139), colour = "Turquoise 1", linewidth = 0.3) + 
    geom_line(aes(y = ms_sma373), colour = "darkorchid1", linewidth = 0.3) + 
    # because I need to remember which chart is it (to which stock it belongs)
    annotate("text", x = df2$index[20], y = 1.1 * df2$adjusted_ms[10], label = paste0(etf2, "/", etf1), color = "white") + 
    scale_x_bd(business.dates=df2$index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0.2, 0.2, 0.1, 0.4, "cm"), # This is to shrink the padding at the 4 side of the graph
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.15), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.15),
          panel.grid.minor.y = element_line(colour = "white", linetype = "dotted", size = 0.15),
          panel.grid.minor.x = element_blank())
  
  p2 <- ggplot(df2, aes(x = index, y = adjusted_tic_mark)) + 
    geom_line( colour = "Gray 80") + 
    geom_line(aes(y = tic_mark_ema139), colour = "Turquoise 1", linewidth = 0.3) + 
    geom_line(aes(y = tic_mark_sma373), colour = "darkorchid1", linewidth = 0.3) + 
    annotate("text", x = df2$index[20], y = 1.1 * df2$adjusted_tic_mark[10], label = paste0(ticker, "/", etf1), color = "white") + 
    scale_x_bd(business.dates=df2$index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.)) + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90), 
          axis.text.x = element_blank(), 
          plot.margin = margin(0.0, 0.2, 0.2, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", linewidth = 0.1), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", linewidth = 0.1),
          panel.grid.minor = element_blank())
  
  p3 <- ggplot(df2, aes(x = index, y = adjusted_tic_sect)) + 
    geom_line(colour = "Gray 80") + 
    geom_line(aes(y = tic_sect_ema139), colour = "Turquoise 1", linewidth = 0.3) + 
    geom_line(aes(y = tic_sect_sma373), colour = "darkorchid1", linewidth = 0.3) + 
    annotate("text", x = df2$index[20], y = 1.1 * df2$adjusted_tic_sect[10], label = paste0(ticker, "/", etf2), color = "white") + 
    scale_x_bd(business.dates=df2$index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.)) + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90), 
          axis.text.x = element_text(angle = 30, vjust = 0.4), 
          plot.margin = margin(0.0, 0.2, 0.2, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", linewidth = 0.1), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  
  yo <- grid.arrange(p1, p2, p3,  ncol = 1, heights = c(1, 1, 1))
  yo
  #p1
}


