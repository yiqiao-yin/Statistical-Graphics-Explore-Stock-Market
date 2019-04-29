############################### BEGIN SCRIPT ##############################

#install.packages(c("NLP", "SnowballC", "slam", "tm", "wordcloud", "xml2",
#                   "quantmod", "shiny", "shinythemes", "corrplot", "forecast",
#                   "xts", "dygraphs", "ggplot2", "reshape2", "DT", "gtools",
#                   "rnn", "plot3D", "plotly", "parcoords"))
library('quantmod')
library('plotly')
library('shiny')
library('shinysky')
library('shinythemes')
library('corrplot')
library('forecast')
library('xts')
library('dygraphs')
library('ggplot2')
library('reshape2')
library('gtools')
library('DT')
library('rnn')
library("plot3D")
library("plotly")
library("parcoords")
library("quadprog")
library("pROC")
library("matrixcalc")
library("XML")
library("beepr")
library('data.table')
library('scales')
library('ggplot2')
library('fPortfolio')
library('finreportr')
library('knitr')
library('treemap')
library('tidyquant')
library('gridExtra')
library('readxl')         # for reading in Excel data
library('dplyr')          # for data manipulation
library('tidyr')          # for data shaping
library('ggplot2')        # for generating the visualizations
library('gridExtra')      # for arrange grid plots


######################## DEFINE: FUNCTIONS ################################

# Download data for a stock if needed, and return the data
# Define function
require_symbol <- function(symbol, envir = parent.frame()) {
  if (is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(# src = "yahoo",
      #from = "2010-01-01"
      symbol,
      auto.assign = FALSE)
  }
  envir[[symbol]]
}

###################### TIME-SERIES PLOT #####################

# Define function:
DyPlot.Initial.with.100 <- function() {
  tickers <- c(
    "AAPL", "FB", "GOOGL", "WMT", "PEP", "SBUX", "GS", "BAC", "WFC"
  )
  getSymbols(tickers)
  closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
  dateWindow <- c(as.Date(Sys.time())-300, as.Date(Sys.time()))
  
  dygraph(closePrices, main = "Value (in USD)", group = "stock") %>%
    dyRebase(value = 100) %>% dyRangeSelector(dateWindow = dateWindow)
} # End of function

# Define function:
DePlot.Initial.with.Return <- function() {
  tickers <- c(
    "AAPL", "FB", "GOOGL", "WMT", "PEP", "SBUX", "GS", "BAC", "WFC"
  )
  getSymbols(tickers)
  closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
  dateWindow <- c(as.Date(Sys.time())-300, as.Date(Sys.time()))
  
  dygraph(closePrices, main = "Percent (Stock Returns)", group = "stock") %>%
    dyRebase(percent = TRUE) %>% dyRangeSelector(dateWindow = dateWindow)
} # End of function

# Define function:
Overview.Plot <- function() {
  tickers <- c(
    "AAPL", "FB", "GOOGL", "WMT", "PEP", "SBUX", "GS", "BAC", "WFC"
  )
  getSymbols(tickers)
  closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
  closePrices <- data.frame(na.omit(closePrices))
  data_new <- closePrices
  for (j in 1:ncol(closePrices)) {
    data_new <- cbind(
      data_new,
      closePrices[,j]/lag(closePrices[,j])-1,
      closePrices[,j]/lag(closePrices[,j], k=30)-1,
      closePrices[,j]/lag(closePrices[,j], k=250)-1
    )
  }
  data_new <- data.frame(na.omit(data_new))
  colnames(data_new) <- c(
    tickers,
    c(rbind( paste0(tickers, "_Past_1W"), paste0(tickers, "_Past_1M"), paste0(tickers, "_Past_1Y")))
    )
  data_new$Top_Week_Stocks_Max <- cbind(apply(data_new[, c(10,13,16, 19,22,25, 28,31,34)], 1, which.max))
  data_new$Top_Week_Stocks_Min <- cbind(apply(data_new[, c(10,13,16, 19,22,25, 28,31,34)], 1, which.min))
  data_new$Top_Month_Stocks_Max <- cbind(apply(data_new[, c(10,13,16, 19,22,25, 28,31,34)+1], 1, which.max))
  data_new$Top_Month_Stocks_Min <- cbind(apply(data_new[, c(10,13,16, 19,22,25, 28,31,34)+1], 1, which.min))
  data_new$Top_Year_Stocks_Max <- cbind(apply(data_new[, c(10,13,16, 19,22,25, 28,31,34)+2], 1, which.max))
  data_new$Top_Year_Stocks_Min <- cbind(apply(data_new[, c(10,13,16, 19,22,25, 28,31,34)+2], 1, which.min))
  data_new$Long_Run_Reversal <- sapply(
    1:nrow(data_new),
    function(i) { abs(- data_new[i, 10+as.numeric(data_new[1,42])*3] + data_new[i, 10+as.numeric(data_new[1,37])*1]) })
  data_new$Long_Run_Reversal_USD <- cuWFCum(data_new$Long_Run_Reversal+1)
  plot(data_new$Long_Run_Reversal_USD, type="l", ylim = c(0, 1000)); lines(data_new$AAPL)
} # End of plot

###################### TIME-SERIES ##########################

# ARIMA Forecast:
ARMA_Fit_D <- function(entry, ahead) {
  #entry <- SPY[,4]; ahead=10
  entry <- entry[, 4]
  fit <- auto.arima(
    entry,
    stationary = FALSE,
    max.p = 20,
    max.q = 20,
    max.P = 10,
    max.Q = 10,
    max.order = 5,
    max.d = 2,
    max.D = 1,
    start.p = 2,
    start.q = 2,
    start.P = 1,
    start.Q = 1,
    ic = c("aicc", "aic", "bic"),
    test = c("kpss", "adf", "pp"),
    approximation = TRUE
  )
  #summary(fit)
  results <- cbind(# fit$coef,
    fit$sigma ^ 2,
    fit$log,
    fit$aic,
    fit$aicc,
    fit$bic)
  #results
  forecast <- data.frame(forecast(fit, ahead))
  forecast.days <- 1:ahead
  forecast <- data.frame(cbind(forecast.days, forecast))
  colnames(forecast) <-
    c("Day + ?", "Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95")
  print(forecast, right = FALSE)
  #rownames(forecast) <- rownames('SPY')
  #plot(forecast,main="Forecast Daily Closing Price")
} # End function

# Forecast with time series
ARMA_Fit_W <- function(entry, ahead) {
  #entry <- SPY[,4]; ahead=10
  entry <- to.weekly(entry)[, 4]
  fit <- auto.arima(
    entry,
    stationary = FALSE,
    max.p = 5,
    max.q = 5,
    max.P = 2,
    max.Q = 2,
    max.order = 5,
    max.d = 2,
    max.D = 1,
    start.p = 2,
    start.q = 2,
    start.P = 1,
    start.Q = 1,
    ic = c("aicc", "aic", "bic"),
    test = c("kpss", "adf", "pp"),
    approximation = TRUE
  )
  #summary(fit)
  results <- cbind(# fit$coef,
    fit$sigma ^ 2,
    fit$log,
    fit$aic,
    fit$aicc,
    fit$bic)
  #results
  forecast <- data.frame(forecast(fit, ahead))
  forecast.days <- 1:ahead
  forecast <- data.frame(cbind(forecast.days, forecast))
  colnames(forecast) <-
    c("Week + ?",
      "Point.Forecast",
      "Lo.80",
      "Hi.80",
      "Lo.95",
      "Hi.95")
  print(forecast, right = FALSE)
  #rownames(forecast) <- rownames('SPY')
  #plot(forecast,main="Forecast Daily Closing Price")
} # End function

# Forecast with time series
ARMA_Fit_M <- function(entry, ahead) {
  #entry <- SPY[,4]; ahead=10
  entry <- to.monthly(entry)[, 4]
  fit <- auto.arima(
    entry,
    stationary = FALSE,
    max.p = 5,
    max.q = 5,
    max.P = 2,
    max.Q = 2,
    max.order = 5,
    max.d = 2,
    max.D = 1,
    start.p = 2,
    start.q = 2,
    start.P = 1,
    start.Q = 1,
    ic = c("aicc", "aic", "bic"),
    test = c("kpss", "adf", "pp"),
    approximation = TRUE
  )
  #summary(fit)
  results <- cbind(# fit$coef,
    fit$sigma ^ 2,
    fit$log,
    fit$aic,
    fit$aicc,
    fit$bic)
  #results
  forecast <- data.frame(forecast(fit, ahead))
  forecast.days <- 1:ahead
  forecast <- data.frame(cbind(forecast.days, forecast))
  colnames(forecast) <-
    c("Month + ?",
      "Point.Forecast",
      "Lo.80",
      "Hi.80",
      "Lo.95",
      "Hi.95")
  print(forecast, right = FALSE)
  #rownames(forecast) <- rownames('SPY')
  #plot(forecast,main="Forecast Daily Closing Price")
} # End function

# Forecast with time series
ARMA_Fit_Q <- function(entry, ahead) {
  #entry <- SPY[,4]; ahead=10
  entry <- to.quarterly(entry)[, 4]
  fit <- auto.arima(
    entry,
    stationary = FALSE,
    max.p = 5,
    max.q = 5,
    max.P = 2,
    max.Q = 2,
    max.order = 5,
    max.d = 2,
    max.D = 1,
    start.p = 2,
    start.q = 2,
    start.P = 1,
    start.Q = 1,
    ic = c("aicc", "aic", "bic"),
    test = c("kpss", "adf", "pp"),
    approximation = TRUE
  )
  #summary(fit)
  results <- cbind(# fit$coef,
    fit$sigma ^ 2,
    fit$log,
    fit$aic,
    fit$aicc,
    fit$bic)
  #results
  forecast <- data.frame(forecast(fit, ahead))
  forecast.days <- 1:ahead
  forecast <- data.frame(cbind(forecast.days, forecast))
  colnames(forecast) <-
    c("Quarter + ?",
      "Point.Forecast",
      "Lo.80",
      "Hi.80",
      "Lo.95",
      "Hi.95")
  print(forecast, right = FALSE)
  #rownames(forecast) <- rownames('SPY')
  #plot(forecast,main="Forecast Daily Closing Price")
} # End function

# Define function
ARMA_Plot_D <- function(ticker = AAPL,
                        ahead = 10) {
  # Day
  entry <- ticker[, 4]
  fit <- auto.arima(
    entry,
    stationary = FALSE,
    max.p = 20,
    max.q = 20,
    max.P = 10,
    max.Q = 10,
    max.order = 5,
    max.d = 2,
    max.D = 1,
    start.p = 2,
    start.q = 2,
    start.P = 1,
    start.Q = 1,
    ic = c("aicc", "aic", "bic"),
    test = c("kpss", "adf", "pp"),
    approximation = TRUE
  )
  #summary(fit)
  results <- cbind(# fit$coef,
    fit$sigma ^ 2,
    fit$log,
    fit$aic,
    fit$aicc,
    fit$bic)
  #results
  pred.table <- forecast(fit, ahead)
  pred.table
  Forecasted_Daily_Trace <- c(pred.table$mean)
  Forecasted_Daily_Trace_LB <- c(pred.table$lower[, 1])
  Forecasted_Daily_Trace_UB <- c(pred.table$upper[, 1])
  Entered_Stock <- c(1:ahead)
  data <-
    data.frame(
      Entered_Stock,
      Forecasted_Daily_Trace,
      Forecasted_Daily_Trace_LB,
      Forecasted_Daily_Trace_UB
    )
  p <- plot_ly(
    data,
    Entered_Stock = ~ Entered_Stock,
    y = ~ Forecasted_Daily_Trace,
    name = 'Forecasted Trace',
    type = 'scatter',
    mode = 'lines+markers'
  ) %>%
    add_trace(
      y = ~ Forecasted_Daily_Trace_LB,
      name = 'LB',
      mode = 'lines+markers'
    ) %>%
    add_trace(
      y = ~ Forecasted_Daily_Trace_UB,
      name = 'UB',
      mode = 'lines+markers'
    )
  p
} # End of function

# Define function
ARMA_Plot_W <- function(ticker = AAPL,
                        ahead = 10) {
  # Week
  entry <- to.weekly(ticker)[, 4]
  fit <- auto.arima(
    entry,
    stationary = FALSE,
    max.p = 20,
    max.q = 20,
    max.P = 10,
    max.Q = 10,
    max.order = 5,
    max.d = 2,
    max.D = 1,
    start.p = 2,
    start.q = 2,
    start.P = 1,
    start.Q = 1,
    ic = c("aicc", "aic", "bic"),
    test = c("kpss", "adf", "pp"),
    approximation = TRUE
  )
  #summary(fit)
  results <- cbind(# fit$coef,
    fit$sigma ^ 2,
    fit$log,
    fit$aic,
    fit$aicc,
    fit$bic)
  #results
  pred.table <- forecast(fit, ahead)
  pred.table
  Forecasted_Weekly_Trace <- c(pred.table$mean)
  Forecasted_Weekly_Trace_LB <- c(pred.table$lower[, 1])
  Forecasted_Weekly_Trace_UB <- c(pred.table$upper[, 1])
  Entered_Stock <- c(1:ahead)
  data <-
    data.frame(
      Entered_Stock,
      Forecasted_Weekly_Trace,
      Forecasted_Weekly_Trace_LB,
      Forecasted_Weekly_Trace_UB
    )
  p <- plot_ly(
    data,
    Entered_Stock = ~ Entered_Stock,
    y = ~ Forecasted_Weekly_Trace,
    name = 'Forecasted Trace',
    type = 'scatter',
    mode = 'lines+markers'
  ) %>%
    add_trace(
      y = ~ Forecasted_Weekly_Trace_LB,
      name = 'LB',
      mode = 'lines+markers'
    ) %>%
    add_trace(
      y = ~ Forecasted_Weekly_Trace_UB,
      name = 'UB',
      mode = 'lines+markers'
    )
  p
} # End of function

# Define function
ARMA_Plot_M <- function(ticker = AAPL,
                        ahead = 10) {
  # Month
  entry <- to.monthly(ticker)[, 4]
  fit <- auto.arima(
    entry,
    stationary = FALSE,
    max.p = 20,
    max.q = 20,
    max.P = 10,
    max.Q = 10,
    max.order = 5,
    max.d = 2,
    max.D = 1,
    start.p = 2,
    start.q = 2,
    start.P = 1,
    start.Q = 1,
    ic = c("aicc", "aic", "bic"),
    test = c("kpss", "adf", "pp"),
    approximation = TRUE
  )
  #summary(fit)
  results <- cbind(# fit$coef,
    fit$sigma ^ 2,
    fit$log,
    fit$aic,
    fit$aicc,
    fit$bic)
  #results
  pred.table <- forecast(fit, ahead)
  pred.table
  Forecasted_Monthly_Trace <- c(pred.table$mean)
  Forecasted_Monthly_Trace_LB <- c(pred.table$lower[, 1])
  Forecasted_Monthly_Trace_UB <- c(pred.table$upper[, 1])
  Entered_Stock <- c(1:ahead)
  data <-
    data.frame(
      Entered_Stock,
      Forecasted_Monthly_Trace,
      Forecasted_Monthly_Trace_LB,
      Forecasted_Monthly_Trace_UB
    )
  p <- plot_ly(
    data,
    Entered_Stock = ~ Entered_Stock,
    y = ~ Forecasted_Monthly_Trace,
    name = 'Forecasted Trace',
    type = 'scatter',
    mode = 'lines+markers'
  ) %>%
    add_trace(
      y = ~ Forecasted_Monthly_Trace_LB,
      name = 'LB',
      mode = 'lines+markers'
    ) %>%
    add_trace(
      y = ~ Forecasted_Monthly_Trace_UB,
      name = 'UB',
      mode = 'lines+markers'
    )
  p
} # End of function

# Define function
ARMA_Plot_Q <- function(ticker = AAPL,
                        ahead = 10) {
  # Quarter
  entry <- to.monthly(ticker)[, 4]
  fit <- auto.arima(
    entry,
    stationary = FALSE,
    max.p = 20,
    max.q = 20,
    max.P = 10,
    max.Q = 10,
    max.order = 5,
    max.d = 2,
    max.D = 1,
    start.p = 2,
    start.q = 2,
    start.P = 1,
    start.Q = 1,
    ic = c("aicc", "aic", "bic"),
    test = c("kpss", "adf", "pp"),
    approximation = TRUE
  )
  #summary(fit)
  results <- cbind(# fit$coef,
    fit$sigma ^ 2,
    fit$log,
    fit$aic,
    fit$aicc,
    fit$bic)
  #results
  pred.table <- forecast(fit, ahead)
  pred.table
  Forecasted_Quarterly_Trace <- c(pred.table$mean)
  Forecasted_Quarterly_Trace_LB <- c(pred.table$lower[, 1])
  Forecasted_Quarterly_Trace_UB <- c(pred.table$upper[, 1])
  Entered_Stock <- c(1:ahead)
  data <-
    data.frame(
      Entered_Stock,
      Forecasted_Quarterly_Trace,
      Forecasted_Quarterly_Trace_LB,
      Forecasted_Quarterly_Trace_UB
    )
  p <- plot_ly(
    data,
    Entered_Stock = ~ Entered_Stock,
    y = ~ Forecasted_Quarterly_Trace,
    name = 'Forecasted Trace',
    type = 'scatter',
    mode = 'lines+markers'
  ) %>%
    add_trace(
      y = ~ Forecasted_Quarterly_Trace_LB,
      name = 'LB',
      mode = 'lines+markers'
    ) %>%
    add_trace(
      y = ~ Forecasted_Quarterly_Trace_UB,
      name = 'UB',
      mode = 'lines+markers'
    )
  p
}

###################### PARCOORDS ##############################

# Library
library("quantmod"); library('DT')

# 3D Plot
All.Indice.3D <- function() {
  data <- getSymbols(c(
    "AAPL", "FB", "GOOGL", "WMT", "PEP", "SBUX", "GS", "BAC", "WFC"
  )); data
  data.list <- list(
    AAPL, FB, GOOGL, WMT, PEP, SBUX, GS, BAC, WFC
  )
  
  # Create data set:
  all <- matrix(NA,nrow=length(data),ncol=12)
  rownames(all) <- data
  
  # Update Price (Current, daily basis):
  for (i in c(1:nrow(all))){ all[i,2] <- data.frame(data.list[i])[nrow(data.frame(data.list[i])),4] }
  
  # Update Momentum:
  for (i in c(1:nrow(all))){ all[i,5] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-5),4])-1 }
  for (i in c(1:nrow(all))){ all[i,6] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-25),4])-1 }
  for (i in c(1:nrow(all))){ all[i,7] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-25*3),4])-1 }
  for (i in c(1:nrow(all))){ all[i,8] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-252),4])-1 }
  
  # Clearn up
  all <- all[,-c(1,3,4,9,10,11,12)]
  
  # Update column names:
  colnames(all) <- c("Last Price",
                     "Pre 5-Days",
                     "Pre 30-Days",
                     "Pre Quarter",
                     "Pre Year")
  
  # Quick vertical bar plot:
  #counts <- all[,2]
  #counts.std <- sd(all[,2])
  #barplot(counts, main="5-Day Return Bar Chart", #horiz=TRUE,
  #        names.arg=rownames(all), cex.names=0.35,
  #        col=ifelse(counts>counts.std,"green",ifelse(counts<(-1)*counts.std,"red","pink")))
  
  # Sortable table:
  # Present table:
  d = data.frame( round(all[,c(2,3,4,5)],4), stringsAsFactors = FALSE)
  d$Sector <- c(rep("Technology", 3), rep("Consumer", 3), rep("Financial", 3))
  
  # Output
  return(d)
} # End of function

# Slider Matplot
All.Indice.3D.Slide <- function() {
  d.parcoord <- All.Indice.3D()[, -5]
  parcoords(d.parcoord,
            #All.Indice.3D(),
            rownames = T,
            brushMode = "1d",
            #reorderable = T,
            #queue = F,
            color = list(colorBy = rownames(d.parcoord))
  )
} # End of function

####################### 3D BAR PLOT #############################

library(lattice)
library(latticeExtra)
library(gridExtra)

# Define function
Bar.Plot.3D <- function(DT = data.all, which.sector = "Technology") {
  ## Design Plot
  DF.temp <- DT
  DF <- DF.temp[DF.temp$Sector == which.sector, ][, -5]
  #data(VADeaths)
  #sub.data <- VADeaths[1:nrow(DF), 1:ncol(DF)]
  sub.data <- as.matrix(DF)
  colnames(sub.data) <- c(colnames(DF))
  rownames(sub.data) <- c(rownames(DF))
  #for (i in 1:5) { for (j in 1:4) { sub.data[i, j] <- DF[i, j] } }
  P1 <- cloud(
    sub.data,
    panel.3d.cloud = panel.3dbars,
    xbase = 0.4,
    ybase = 0.4,
    zlim = c(-0.1,0.5),
    scales = list(arrows = FALSE, just = "right"),
    xlab = list(label = "Stocks", cex = 1.1),
    ylab = list(label = "Time", cex = 1.1),
    zlab = list(
      label = "Returns",
      cex = 1.1,
      rot = 90
    ),
    cex.axis = 0.7,
    lwd = 0.5,
    main = list(label = paste0("Performance: ", which.sector), cex = 1.5),
    col.facet = level.colors(
      sub.data,
      at = do.breaks(range(sub.data), 20),
      col.regions = cm.colors,
      colors = TRUE
    ),
    colorkey = list(col = cm.colors, at = do.breaks(range(sub.data), 20))
  ); P1
} # End of function

##################### QQPLOT ######################

# Modern Portfolio Theory (MPT)
QQ_Comparison <- function(past_n_days = 30) {
  tickers <- c( "SPY", "AAPL", "FB", "GOOGL", "WMT", "PEP", "SBUX", "GS", "BAC", "WFC" )
  getSymbols(tickers)
  closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
  DF <- closePrices; N <- nrow(DF)
  data_new <- c()
  for (j in 1:ncol(closePrices)) {
    data_new <- cbind(
      data_new,
      as.numeric(closePrices[,j]/lag(closePrices[,j])-1)
      #closePrices[,j]/lag(closePrices[,j], k=30)-1,
      #closePrices[,j]/lag(closePrices[,j], k=250)-1
    )
  }; colnames(data_new) <- paste0(tickers, "_Daily_Return")
  data_new <- data.frame(na.omit(data_new))
  data_compare <- data.frame(cbind(Equal_Weight = apply(data_new[, -1], 1, mean), 
                                   Market = data_new$SPY_Daily_Return))
  data_compare <- data_compare[(nrow(data_compare)-past_n_days):nrow(data_compare), ]
  qqP1 <- ggplot(data_compare, aes(sample = Equal_Weight)) +
    stat_qq() +
    stat_qq_line() + 
    ggtitle(paste0("QQPlot: Past ",past_n_days+1," days of data"),
            subtitle = "Portfolio (Eq Weight) Return vs. Standard Normal")
  qqP2 <- ggplot(data_compare, aes(sample = Market)) +
    stat_qq() +
    stat_qq_line() + 
    ggtitle(paste0("QQPlot: Past ",past_n_days+1," days of data"),
            subtitle = "Market Return vs. Standard Normal")
  qqP3 <- ggplot(data_compare, aes(Equal_Weight, Market)) + 
    geom_line() + 
    ggtitle(paste0("QQPlot: Past ",past_n_days+1," days of data"),
            subtitle = "Portfolio (Eq Weight) Return vs. Market Return")
  #grid.arrange(qqP1,qqP2,qqP3, nrow=1)
  return(list(
    qqP1 = qqP1, qqP2 = qqP2, qqP3 = qqP3
  ))
} # End of function

#################### BAR PLOT ####################

# Library
library(quantmod)
library(ggplot2)
library(lattice)
library(latticeExtra)

# Define function
Gen_Data <- function(past_n_days = 30) {
  # List of names
  data <- getSymbols(c(
    "AAPL", "FB", "GOOGL", "WMT", "PEP", "SBUX", "GS", "BAC", "WFC"
  ))
  
  # Mutate data
  #past_n_days <- 30
  AAPL <- data.frame(cbind(
    data.frame(data.frame(AAPL) %>% mutate(Stock = "AAPL", Sector="Technology")),
    rownames(data.frame(AAPL))
  ))[(nrow(AAPL)-past_n_days):nrow(AAPL), ]; colnames(AAPL)[[9]] <- "Date"
  FB <- data.frame(cbind(
    data.frame(data.frame(FB) %>% mutate(Stock = "FB", Sector="Technology")),
    rownames(data.frame(FB))
  ))[(nrow(FB)-past_n_days):nrow(FB), ]; colnames(FB)[[9]] <- "Date"
  GOOGL <- data.frame(cbind(
    data.frame(data.frame(GOOGL) %>% mutate(Stock = "GOOGL", Sector="Technology")),
    rownames(data.frame(GOOGL))
  ))[(nrow(GOOGL)-past_n_days):nrow(GOOGL), ]; colnames(GOOGL)[[9]] <- "Date"
  WMT <- data.frame(cbind(
    data.frame(data.frame(WMT) %>% mutate(Stock = "WMT", Sector = "Consumer")),
    rownames(data.frame(WMT))
  ))[(nrow(WMT)-past_n_days):nrow(WMT), ]; colnames(WMT)[[9]] <- "Date"
  PEP <- data.frame(cbind(
    data.frame(data.frame(PEP) %>% mutate(Stock = "PEP", Sector = "Consumer")),
    rownames(data.frame(PEP))
  ))[(nrow(PEP)-past_n_days):nrow(PEP), ]; colnames(PEP)[[9]] <- "Date"
  SBUX <- data.frame(cbind(
    data.frame(data.frame(SBUX) %>% mutate(Stock = "SBUX", Sector = "Consumer")),
    rownames(data.frame(SBUX))
  ))[(nrow(SBUX)-past_n_days):nrow(SBUX), ]; colnames(SBUX)[[9]] <- "Date"
  GS <- data.frame(cbind(
    data.frame(data.frame(GS) %>% mutate(Stock = "GS", Sector = "Financial")),
    rownames(data.frame(GS))
  ))[(nrow(GS)-past_n_days):nrow(GS), ]; colnames(GS)[[9]] <- "Date"
  BAC <- data.frame(cbind(
    data.frame(data.frame(BAC) %>% mutate(Stock = "BAC", Sector = "Financial")),
    rownames(data.frame(BAC))
  ))[(nrow(BAC)-past_n_days):nrow(BAC), ]; colnames(BAC)[[9]] <- "Date"
  WFC <- data.frame(cbind(
    data.frame(data.frame(WFC) %>% mutate(Stock = "WFC", Sector = "Financial")),
    rownames(data.frame(WFC))
  ))[(nrow(WFC)-past_n_days):nrow(WFC), ]; colnames(WFC)[[9]] <- "Date"
  colnames(AAPL)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(FB)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(GOOGL)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(WMT)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(PEP)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(SBUX)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(GS)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(BAC)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(WFC)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  
  # Compile data
  data <- rbind(AAPL, FB, GOOGL, WMT, PEP, SBUX, GS, BAC, WFC)
  #Date1<-strsplit(data$Date,"-") %>% ldply()
  #colnames(Date1) <- c("Year", "Month", "Day")
  #data<-cbind(data,Date1)
  #data$Month <- as.numeric(data$Month)
  #data$Date <- as.Date(data$Date)
  data <- data.frame(na.omit(data))
  #head(data); dim(data)
  return(data.frame(data))
} # End of function

# Define function
grid_barplot <- function(data = data) {
  # Generate data
  # data <- Gen_Data(past_n_days = 50)
  
  # GGPlot
  bar_P1 <- ggplot(data,aes(factor(Stock),Open,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: Open', subtitle = "Past 50 Days", x = 'Stocks', y = 'Close Price')
  bar_P2 <- ggplot(data,aes(factor(Stock),High,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: High', subtitle = "Past 50 Days", x = 'Stocks', y = 'Close Price')
  bar_P3 <- ggplot(data,aes(factor(Stock),Low,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: Low', subtitle = "Past 50 Days",  x = 'Stocks', y = 'Close Price')
  bar_P4 <- ggplot(data,aes(factor(Stock),Close,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: Close', subtitle = "Past 50 Days", x = 'Stocks', y = 'Close Price')
  
  # Generate data
  data <- Gen_Data(past_n_days = 100)
  
  # GGPlot
  bar_P5 <- ggplot(data,aes(factor(Stock),Open,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: Open', subtitle = "Past 100 Days", x = 'Stocks', y = 'Close Price')
  bar_P6 <- ggplot(data,aes(factor(Stock),High,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: High', subtitle = "Past 100 Days", x = 'Stocks', y = 'Close Price')
  bar_P7 <- ggplot(data,aes(factor(Stock),Low,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: Low', subtitle = "Past 100 Days", x = 'Stocks', y = 'Close Price')
  bar_P8 <- ggplot(data,aes(factor(Stock),Close,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: Close', subtitle = "Past 100 Days", x = 'Stocks', y = 'Close Price')
  
  # Generate data
  data <- Gen_Data(past_n_days = 200)
  
  # GGPlot
  bar_P9 <- ggplot(data,aes(factor(Stock),Open,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: Open', subtitle = "Past 200 Days", x = 'Stocks', y = 'Close Price')
  bar_P10 <- ggplot(data,aes(factor(Stock),High,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: High', subtitle = "Past 200 Days", x = 'Stocks', y = 'Close Price')
  bar_P11 <- ggplot(data,aes(factor(Stock),Low,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: Low', subtitle = "Past 200 Days", x = 'Stocks', y = 'Close Price')
  bar_P12 <- ggplot(data,aes(factor(Stock),Close,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price PEPt: Close', subtitle = "Past 200 Days", x = 'Stocks', y = 'Close Price')
  
  # Plot in a grid
  grid.arrange(bar_P1,bar_P2,bar_P3,bar_P4, 
               bar_P5,bar_P6,bar_P7,bar_P8, 
               bar_P9,bar_P10,bar_P11,bar_P12,
               nrow=3)
} # End of function

####################### INCOME STATEMENT ###########################

# Selected iteWFC on income statement
library(finreportr)
library(ggplot2)
library(gridExtra)

# Define function
one_company_income_plot <- function(stock = "AAPL", YR = 2017) {
  #YR <- as.numeric(unlist(strsplit(as.character(Sys.time()),"-"))[[1]])
  temp <- GetIncome(stock, YR)
  temp <- temp[order(temp$endDate,temp$startDate), c(4,5,1,2,3)]
  temp$Amount <- as.numeric(as.character(temp$Amount))
  temp <- temp[, c(4,5,1,2,3)]; temp <- cbind(stock, temp)
  temp
  #head(temp)
  #ggplot(temp, aes(endDate, Amount, color = Metric)) + 
    #geom_point(size = 2) + 
    #theme(text = element_text(size = 10)) +
    #labs(y = "Metric")
} # End of function

# Define function
all_company_income_plot <- function() {
  income_P1 <- one_company_income_plot("AAPL")
  income_P2 <- one_company_income_plot("FB")
  income_P3 <- one_company_income_plot("GOOG")
  income_P4 <- one_company_income_plot("WMT")
  income_P5 <- one_company_income_plot("PEP")
  income_P6 <- one_company_income_plot("SBUX")
  income_P7 <- one_company_income_plot("GS")
  income_P8 <- one_company_income_plot("BAC")
  income_P9 <- one_company_income_plot("WFC")
  temp <- data.frame(rbind(
    income_P1,income_P2,income_P3,
    income_P4,income_P5,income_P6,
    income_P7,income_P8,income_P9
  ))
  ggplot(temp, aes(endDate, Amount, color = Metric, type = stock, shape = stock)) +
    geom_point(size=5)
} # End of function

##################### END SCRIPT ########################