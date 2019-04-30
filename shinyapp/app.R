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

######################## DEFINE: FUNCTIONS ################################

# Download data for a stock if needed, and return the data
require_symbol <- function(symbol, envir = parent.frame()) {
  if (is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(# src = "yahoo",
      #from = "2010-01-01"
      symbol,
      auto.assign = FALSE)
  }
  envir[[symbol]]
}

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
}

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
}

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
}

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

library(quantmod)
# 3D Plot
All.Indice.3D <- function() {
  data <- getSymbols(c(
    "AAPL", "FB", "GOOGL", "WMT", "DIS", "SBUX", "GS", "BAC", "MS"
  )); data
  data.list <- list(
    AAPL, FB, GOOGL, WMT, DIS, SBUX, GS, BAC, MS
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
  d.parcoord <- All.Indice.3D()
  parcoords(d.parcoord,
            #All.Indice.3D(),
            rownames = T,
            brushMode = "1d"#,
            #reorderable = T,
            #queue = F,
            #color = list(colorBy = rownames(d.parcoord))
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
    zlim = c(min(sub.data),max(sub.data)),
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
  tickers <- c( "SPY", "AAPL", "FB", "GOOGL", "WMT", "DIS", "SBUX", "GS", "BAC", "MS" )
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
    "AAPL", "FB", "GOOGL", "WMT", "DIS", "SBUX", "GS", "BAC", "MS"
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
  DIS <- data.frame(cbind(
    data.frame(data.frame(DIS) %>% mutate(Stock = "DIS", Sector = "Consumer")),
    rownames(data.frame(DIS))
  ))[(nrow(DIS)-past_n_days):nrow(DIS), ]; colnames(DIS)[[9]] <- "Date"
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
  MS <- data.frame(cbind(
    data.frame(data.frame(MS) %>% mutate(Stock = "MS", Sector = "Financial")),
    rownames(data.frame(MS))
  ))[(nrow(MS)-past_n_days):nrow(MS), ]; colnames(MS)[[9]] <- "Date"
  colnames(AAPL)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(FB)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(GOOGL)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(WMT)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(DIS)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(SBUX)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(GS)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(BAC)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  colnames(MS)[1:6] <- c("Open","High","Low", "Close", "Volume", "Adjusted")
  
  # Compile data
  data <- rbind(AAPL, FB, GOOGL, WMT, DIS, SBUX, GS, BAC, MS)
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
grid_barplot <- function() {
  # Generate data
  data <- Gen_Data(past_n_days = 50)
  
  # GGPlot
  bar_P1 <- ggplot(data,aes(factor(Stock),Open,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: Open', subtitle = "Past 50 Days", x = 'Stocks', y = 'Close Price')
  bar_P2 <- ggplot(data,aes(factor(Stock),High,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: High', subtitle = "Past 50 Days", x = 'Stocks', y = 'Close Price')
  bar_P3 <- ggplot(data,aes(factor(Stock),Low,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: Low', subtitle = "Past 50 Days",  x = 'Stocks', y = 'Close Price')
  bar_P4 <- ggplot(data,aes(factor(Stock),Close,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: Close', subtitle = "Past 50 Days", x = 'Stocks', y = 'Close Price')
  
  # Generate data
  data <- Gen_Data(past_n_days = 100)
  
  # GGPlot
  bar_P5 <- ggplot(data,aes(factor(Stock),Open,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: Open', subtitle = "Past 100 Days", x = 'Stocks', y = 'Close Price')
  bar_P6 <- ggplot(data,aes(factor(Stock),High,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: High', subtitle = "Past 100 Days", x = 'Stocks', y = 'Close Price')
  bar_P7 <- ggplot(data,aes(factor(Stock),Low,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: Low', subtitle = "Past 100 Days", x = 'Stocks', y = 'Close Price')
  bar_P8 <- ggplot(data,aes(factor(Stock),Close,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: Close', subtitle = "Past 100 Days", x = 'Stocks', y = 'Close Price')
  
  # Generate data
  data <- Gen_Data(past_n_days = 200)
  
  # GGPlot
  bar_P9 <- ggplot(data,aes(factor(Stock),Open,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: Open', subtitle = "Past 200 Days", x = 'Stocks', y = 'Close Price')
  bar_P10 <- ggplot(data,aes(factor(Stock),High,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: High', subtitle = "Past 200 Days", x = 'Stocks', y = 'Close Price')
  bar_P11 <- ggplot(data,aes(factor(Stock),Low,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: Low', subtitle = "Past 200 Days", x = 'Stocks', y = 'Close Price')
  bar_P12 <- ggplot(data,aes(factor(Stock),Close,color=Stock)) +
    geom_jitter(aes(size=Close)) +
    ylim(0,1000)+
    labs(title = 'Price Dist: Close', subtitle = "Past 200 Days", x = 'Stocks', y = 'Close Price')
  
  # Plot in a grid
  grid.arrange(bar_P1,bar_P2,bar_P3,bar_P4, 
               bar_P5,bar_P6,bar_P7,bar_P8, 
               bar_P9,bar_P10,bar_P11,bar_P12,
               nrow=3)
} # End of function

####################### INCOME STATEMENT ###########################

# Selected items on income statement
library(finreportr)
library(ggplot2)
library(gridExtra)

# Define function
one_company_income_plot <- function(stock = "AAPL") {
  YR <- as.numeric(unlist(strsplit(as.character(Sys.time()),"-"))[[1]])
  temp <- GetIncome(stock, YR - 2)
  temp <- temp[order(temp$endDate,temp$startDate), c(4,5,1,2,3)]
  temp$Amount <- as.numeric(as.character(temp$Amount))
  temp <- temp[c(
    which(temp$Metric == "Revenue, Net"),
    which(temp$Metric == "Cost of Goods and Services Sold"),
    which(temp$Metric == "Gross Profit"),
    which(temp$Metric == "Research and Development Expense"),
    which(temp$Metric == "Selling, General and Administrative Expense"),
    which(temp$Metric == "Operating Expenses"),
    which(temp$Metric == "Operating Income (Loss)"),
    which(temp$Metric == "Income Tax Expense (Benefit)"),
    which(temp$Metric == "Net Income (Loss) Attributable to Parent"),
    which(temp$Metric == "Earnings Per Share, Basic")
  ), c(4,5,1,2,3)]
  #head(temp)
  ggplot(temp, aes(Amount, reorder(Metric, -Amount), color = endDate)) + 
    geom_point(size = 3) + theme(text = element_text(size = 20)) +
    labs(y = "Metric")
} # End of function

# Define function
all_tech_company_income_plot <- function() {
  income_P1 <- one_company_income_plot("AAPL")
  income_P2 <- one_company_income_plot("FB")
  income_P3 <- one_company_income_plot("GOOGL")
  #income_P4 <- one_company_income_plot("WMT")
  #income_P5 <- one_company_income_plot("DIS")
  #income_P6 <- one_company_income_plot("SBUX")
  #income_P7 <- one_company_income_plot("GS")
  #income_P8 <- one_company_income_plot("BAC")
  #income_P9 <- one_company_income_plot("MS")
  grid.arrange(
    income_P1,income_P2,income_P3,
    #income_P4,income_P5,income_P6,
    #income_P7,income_P8,income_P9,
    nrow=1
  )
} # End of function

##################### AUTO LIST CONTENT #########################

# List for Selection
# This is for Navbar 1 and we store tickers here.
my_autocomplete_list <- c(
  # All Indices
  "SPY",
  "DIA",
  "QQQ",
  "IWM",
  "GLD",
  "XLB",
  "XLE",
  "XLK",
  "XLU",
  "XLI",
  "XLP",
  "XLY",
  "EWC",
  "EWG",
  "EWJ",
  "EWZ",
  "FEZ",
  "FXI",
  "GDX",
  "GLD",
  "IBB",
  "INDA",
  "IVV",
  "SPXL",
  "TLT",
  "TQQQ",
  "XBI",
  "ITA",
  "IYZ",
  "HACK",
  "KRE",
  "MOO",
  "SOCL",
  "XHB",
  "IAK",
  # DJIA
  "AXP",
  "AAPL",
  "BA",
  "CVX",
  "CSCO",
  "DIS",
  "XOM",
  "GE",
  "GS",
  "HD",
  "IBM",
  "INTC",
  "JNJ",
  "JPM",
  "MCD",
  "MRK",
  "MSFT",
  "NKE",
  "PFE",
  "PG",
  "UTX",
  "UNH",
  "V",
  "WMT",
  # QQQ
  "QQQ",
  "AAPL",
  "AMZN",
  "MSFT",
  "FB",
  "GOOGL",
  "INTC",
  "CSCO",
  "NFLX",
  "NVDA",
  "CMCSA",
  "AMGN",
  "ADBE",
  "TXN",
  # XLF
  "XLF",
  "JPM",
  "BAC",
  "WFC",
  "USB",
  "GS",
  "AXP",
  "MS",
  "PNC",
  "BLK",
  "SCHW",
  "BK",
  "AIG",
  # XLI
  "XLI",
  "BA",
  "GE",
  "MMM",
  "UNP",
  "HON",
  "UTX",
  "CAT",
  "LMT",
  "UPS",
  "FDX",
  "CSX",
  "RTN",
  # XLP
  "XLP",
  "PG",
  "KO",
  "PEP",
  "PM",
  "WMT",
  "COST",
  "MO",
  "MDLZ",
  "CL",
  "WBA",
  "KHC",
  "STZ",
  # XLU
  "XLU",
  "NEE",
  "DUK",
  "SO",
  "D",
  "EXC",
  "AEP",
  "SRE",
  "PEG",
  "ED",
  "XEL",
  "PCG",
  "EIX",
  # XLV
  "XLV",
  "JNJ",
  "UNH",
  "PFE",
  "MRK",
  "ABBV",
  "AMGN",
  "MDT",
  "ABT",
  "GILD",
  "BMY",
  "LLY",
  "BIIB",
  "CVS",
  # SEMI-CONDUCTOR
  "NVDA",
  "AMD",
  "AVGO",
  "IDTI",
  "MCHP",
  "MXIM",
  "TXN",
  "ADI",
  "XLNX",
  "MU"
)

####################### DESIGN SHINY ###################################
shinyApp(
  ####################### DEFINE: UI ##############################
  ui = tagList(
    navbarPage(
      "APP: EXPLORE STOCK MARKET",
      fluid = TRUE,
      #################### NAVBAR 1 #####################
      tabPanel(
        "Navbar 1: OVERVIEW",
        sidebarPanel(
          h4("--------- MAIN ---------"),
          #strong("Enter: a stock ticker"),
          #textInput(
          #  inputId = "stock_enter_1",
          #  label = "Enter Ticker",
          #  value = "AAPL",
          #  width = "100%"
          #),
          select2Input(
            "stock_enter_1",
            "",
            choices = c(my_autocomplete_list),
            type = c("input", "select"),
            selected = "AAPL",
            width = "100%"
          ),
          br(),
          br(),
          sliderInput(
            inputId = "past.n.days",
            label = "Forecast: Report n Days",
            min = 2,
            max = 100,
            value = 5,
            step = 1
          ),
          helpText("This is for Forecast tab only and it controls how many days the algorithm predicts."),
          submitButton("Run Script", width = "100%")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("BASIC",
                     fluidPage(fluidRow(
                       column(
                         12,
                         h4("Overview of Entered Stock"),
                         plotlyOutput("overview.stock")
                       )
                     ))),
            tabPanel("NEWS",
                     fluidPage(fluidRow(
                       column(
                         12,
                         h4("SEC Link of 10K Financials for Entered Company"),
                         uiOutput("tab10k")
                       ),
                       column(12,
                              h4("Finviz Site for Entered Company"),
                              uiOutput("tabfinviz")),
                       column(
                         12,
                         h4("MarketWatch Site for Entered Company"),
                         uiOutput("tabmarketwatch")
                       )
                     ))),
            tabPanel("FORECAST",
                     fluidPage(
                       h3("TIME-SERIES ANALYSIS:"),
                       h4("- Next Day/Week/Month/Quarter Forecast with 80% Percentile C.I."),
                       #fluidRow(column(5,
                       #                tableOutput(outputId = "ts_forecast_daily")),
                       #         column(5,
                       #                tableOutput(outputId = "ts_forecast_weekly"))),
                       #fluidRow(column(5,
                       #                tableOutput(outputId = "ts_forecast_monthly")),
                       #         column(5,
                       #                tableOutput(outputId = "ts_forecast_quarterly"))),
                       fluidRow(column(
                         6,
                         plotlyOutput(outputId = "ts_forecast_plot_D")
                       ),
                       column(
                         6,
                         plotlyOutput(outputId = "ts_forecast_plot_W")
                       )),
                       fluidRow(column(
                         6,
                         plotlyOutput(outputId = "ts_forecast_plot_M")
                       ),
                       column(
                         6,
                         plotlyOutput(outputId = "ts_forecast_plot_Q")
                       ))
                     ))
          ),
          tags$head(
            conditionalPanel(condition = "input.goButton > 0 | $('html').hasClass('shiny-busy')",
                             tags$div(
                               c(
                                 "Calculating... Please wait... Patience is the key to success.",
                                 "Calculating... Please wait... Patience is not simply the ability to wait - it's how we behave while we're waiting",
                                 "Calculating... Please wait... The two most powerful warriors are patience and time."
                               )[sample(3, 3)[1]]
                             ))
          )
        )
      ),
      #################### NAVBAR 2 #####################
      tabPanel(
        "Navbar 2: PORTFOLIO",
        sidebarPanel(
          h4("--------- MAIN ---------"),
          selectInput("selectsector", "Select Sector:",
                      c("Technology" = "Technology",
                        "Consumer" = "Consumer",
                        "Financial" = "Financial")),
          selectInput("selectday", "Number of Days:",
                      c("5" = 5,
                        "10" = 10,
                        "20" = 20,
                        "50" = 50,
                        "70" = 70,
                        "100" = 100)),
          submitButton("Run Script", width = "100%")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Data Set Preview",
              fluidPage(
                fluidRow(
                  column(12,
                         h4("Data"),
                         dataTableOutput("t1", width = '100%', height = 'auto'),
                         h5("This table previews data set. There are 9 stocks (9 observations) 
                            while each observation we compute previous 5 days return,
                            previous month return, previous quarter return, and previous 
                            year return. Each stock belongs to a sector. There are 
                            Technology, Consumer, and Financial."))
                  )
                )
              ),
            tabPanel(
              "3D Bar Plot by Sector",
              fluidPage(
                fluidRow(
                  column(12,
                         h4("View by 3D Bar Plot"),
                         plotOutput("P1"),
                         h5("This is three-dimensional visualization of the data on previous 
                            page. Each sector and each stock we plot previous 5 days, previous
                            30 days, previous quarter, and 
                            previous year returns while the height of the bar is returns."))
                )
              )
            ),
            tabPanel(
              "Overview of Data by QQPlot",
              fluidPage(
                fluidRow(
                  column(12,
                         h4("View by QQPlot"),
                         plotOutput("P2"),
                         h5("This is two-dimensional visualization of the data on previous 
                            page. Instead of using three-dimensional plot, we can generate 
                            two-dimensional plot with numerical values, i.e. returns, on 
                            x-axis and stocks on y-axis. While color represents different 
                            sector, shape of the dots represents different days of past data
                            used in printing this dot. "))
                  )
                )
              ),
            tabPanel(
              "Overview of Parcoords Filter",
              fluidPage(
                fluidRow(
                  column(8,
                         h4("View by Parcoords"),
                         parcoordsOutput("parcoords", width = '100%'),
                         h5("This is a filter, using parcoords plot, to allow users to drag and click 
                            to generate a window for each variable, i.e. a window for each vertical line. 
                            This allows algorithm to filter through stocks for this user."))
                )
              )
            ),
            tabPanel(
              "Grid of QQPlot",
              fluidPage(
                fluidRow(
                  column(12,
                         h4("View by QQPlot in Grid"),
                         plotOutput("qqplot_grid", width = '100%'),
                         h5("There are 1x3 = 3 plots in this page. This row generates 
                            graphs using past n days of information and it is difficult to 
                            observe correlation. That is, it is difficult to use five data 
                            points to conclude one distribution comes from another. However, 
                            as time moves on to using large number of days days in the past, we observe almost 
                            all dots fall on 45 degree line which indicates that one distribution 
                            may potentially come from another. This means that we can say that 
                            distribution of Equal Weight Portfolio may come from Standard Normal 
                            Distribution, and distribution of Market Return may come from Standard Normal 
                            Distribution. "))
                         )
                )
              )
            )
        )
      )
    )
  ),
  ####################### DEFINE: SERVER ##############################
  server = function(input, output, session) {
    #################### NAVBAR 1 #####################
    # Create an environment for storing data
    symbol_env <- new.env()
    # Make a chart for a symbol, with the settings from the inputs
    make_chart <- function(symbol) {
      symbol_data <- require_symbol(symbol, symbol_env)
      chartSeries(
        symbol_data,
        name      = symbol,
        type      = input$chart_type,
        subset    = paste(input$daterange, collapse = "::"),
        log.scale = input$log_y,
        theme     = "white"
      )
    }
    output$overview.stock <- renderPlotly({
      #Overview.Plot(
      #  x = require_symbol(input$stock_enter_1),
      #  r_day_plot = input$percentage[1],
      #  end_day_plot = input$percentage[2]
      #)
      # Get data
      # getSymbols("MSFT", auto.assign = F)
      stock <- require_symbol(input$stock_enter_1)
      dts <- index(stock)
      df <- data.frame(stock, row.names = NULL)
      df$dates <- dts
      names(df) <- c("Open",
                     "High",
                     "Low",
                     "Close",
                     "Volume",
                     "Adjusted",
                     "dates")
      # Subset to after Jan 2016
      df <- subset(df, dates > "2012-01-01")
      # Color or volume bars
      barcols <- c()
      for (i in 1:length(df$dates)) {
        if (i == 1) {
          barcols[i] <- "#F95959"
        }
        if (i > 1) {
          x <- ifelse(df$Close[i] > df$Close[i - 1], "#455D7A", "#F95959")
          barcols[i] <- x
        }
      }
      # Moving Avg line
      MA <- runMean(df$Close)
      # Range selector
      rangeselectorlist = list(
        x = 0,
        y = 0.9,
        bgcolor = "#0099cc",
        font = list(color = "white"),
        buttons = list(
          list(
            count = 1,
            label = "reset",
            step = "all"
          ),
          list(
            count = 1,
            label = "1yr",
            step = "year",
            stepmode = "backward"
          ),
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backward"
          ),
          list(
            count = 1,
            label = "1 mo",
            step = "month",
            stepmode = "backward"
          ),
          list(step = "all")
        )
      )
      # BASE CANDLESTICK CHART WITH MACD
      macd <- data.frame(TTR::MACD(df$Close, 12, 26, 9))
      macd$diff <- macd$macd - macd$signal
      BB <- as.data.frame(BBands(df$Close))
      plot_ly(
        df,
        type = "candlestick",
        x = ~ dates,
        open = ~ Open,
        high = ~ High,
        low = ~ Low,
        close = ~ Close,
        yaxis = "y",
        increasing = list(line = list(color = "#455D7A")),
        decreasing = list(line = list(color = "#F95959")),
        name = "Price",
        height = 600 # height = 600, width = 1024
      ) %>%
        # MA
        add_lines(
          x = df$dates,
          y = BB$mavg,
          line = list(
            width = 3,
            dash = "5px",
            color = "#33bbff"
          ),
          inherit = F,
          name = "Mov Avg"
        ) %>%
        # MACD
        add_lines(
          x = df$dates,
          y = macd$macd,
          yaxis = "y2",
          line = list(width = 1, color = "#8c8c8c"),
          inherit = FALSE
        ) %>%
        add_lines(
          x = df$dates,
          y = macd$signal,
          yaxis = "y2",
          line = list(width = 1, color = "#ff6666"),
          inherit = FALSE
        ) %>%
        add_bars(
          x = df$dates,
          y = macd$diff,
          marker = list(color = "#bfbfbf"),
          yaxis = "y2",
          inherit = FALSE
        ) %>%
        layout(
          plot_bgcolor = "rgb(250,250,250)",
          xaxis = list(
            title = "",
            domain = c(0, 0.95),
            rangeslider = list(visible = F),
            rangeselector = rangeselectorlist
          ),
          yaxis = list(domain = c(0.22, 0.9)),
          yaxis2 = list(domain = c(0, 0.18), side = "right"),
          showlegend = F,
          annotations = list(
            list(
              x = 0,
              y = 1,
              xanchor = "left",
              yanchor = "top",
              xref = "paper",
              yref = "paper",
              text = paste0("<b>", input$stock_enter_1, "</b>"),
              font = list(size = 30, family = "serif"),
              showarrow = FALSE
            ),
            list(
              x = 0.8,
              y = 0.95,
              xanchor = "left",
              yanchor = "top",
              xref = "paper",
              yref = "paper",
              text = paste0("[", paste(range(df$dates), collapse = " / "), "]"),
              font = list(size = 15, family = "serif"),
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 0.18,
              xanchor = "left",
              yanchor = "top",
              xref = "paper",
              yref = "paper",
              text = paste0("<b>MACD (12, 26, 9)</b>"),
              font = list(size = 15, family = "serif"),
              showarrow = FALSE
            )
          )
        )
    })
    
    # Source
    # Link 1
    url10k <- reactive({
      a(
        "here",
        href = paste0(
          "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=",
          input$stock_enter_1,
          "&type=10-K&dateb=&owner=exclude&count=40"
        )
      )
    })
    output$tab10k <- renderUI({
      tagList("Please click", url10k())
    })
    # Link 2
    urlfinviz <- reactive({
      a("here",
        href = paste0("https://finviz.com/quote.ashx?t=",
                      input$stock_enter_1))
    })
    output$tabfinviz <- renderUI({
      tagList("Please click", urlfinviz())
    })
    # Link 3
    urlmktwatch <- reactive({
      a(
        "here",
        href = paste0(
          "https://www.marketwatch.com/investing/stock/",
          input$stock_enter_1
        )
      )
    })
    output$tabmarketwatch <- renderUI({
      tagList("Please click", urlmktwatch())
    })
    
    # Time-series
    # FORECAST
    output$ts_forecast_daily <- renderTable({
      ARMA_Fit_D(entry = require_symbol(input$stock_enter_1),
                 ahead = input$past.n.days)
    })
    output$ts_forecast_weekly <- renderTable({
      ARMA_Fit_W(entry = require_symbol(input$stock_enter_1),
                 ahead = input$past.n.days)
    })
    output$ts_forecast_monthly <- renderTable({
      ARMA_Fit_M(entry = require_symbol(input$stock_enter_1),
                 ahead = input$past.n.days)
    })
    output$ts_forecast_quarterly <- renderTable({
      ARMA_Fit_Q(entry = require_symbol(input$stock_enter_1),
                 ahead = input$past.n.days)
    })
    output$ts_forecast_plot_D <- renderPlotly({
      ARMA_Plot_D(ticker = require_symbol(input$stock_enter_1),
                  ahead = input$past.n.days)
    })
    output$ts_forecast_plot_W <- renderPlotly({
      ARMA_Plot_W(ticker = require_symbol(input$stock_enter_1),
                  ahead = input$past.n.days)
    })
    output$ts_forecast_plot_M <- renderPlotly({
      ARMA_Plot_M(ticker = require_symbol(input$stock_enter_1),
                  ahead = input$past.n.days)
    })
    output$ts_forecast_plot_Q <- renderPlotly({
      ARMA_Plot_Q(ticker = require_symbol(input$stock_enter_1),
                  ahead = input$past.n.days)
    })
    
    #################### NAVBAR 2 #####################
    output$t1 <- renderDataTable({
      datatable(All.Indice.3D())
    })
    output$parcoords <- renderParcoords({
      # Data
      data.all <- All.Indice.3D()
      parcoords(data.all[, c(1:4)],
                rownames = T,
                brushMode = "1d",
                color = list(
                  colorScale = htmlwidgets::JS('d3.scale.category10()'),
                  colorby = "Sector"))
    })
    output$P1 <- renderPlot({
      # Data
      data.all <- All.Indice.3D()
      # ector: Technology, Consumer, Financial
      #if (input$selectsector == "Technology") {Bar.Plot.3D(DT = data.all[, -6], which.sector = "Technology")}
      #if (input$selectsector == "Consumer") {Bar.Plot.3D(DT = data.all[, -6], which.sector = "Consumer")}
      #if (input$selectsector == "Financial") {Bar.Plot.3D(DT = data.all[, -6], which.sector = "Financial")}
      ind <- ifelse(
        input$selectsector == "Technology",
        1, 
        ifelse(
          input$selectsector == "Consumer",
          2,
          3
        )
      )
      sector_vec <- c("Technology", "Consumer", "Financial")
      Bar.Plot.3D(DT = data.all[, -6], which.sector = sector_vec[ind])
    })
    output$P2 <- renderPlot({
      # Data
      data.all <- All.Indice.3D()
      tempdata <- data.all[, c(1:5)] %>% rownames_to_column("Name") %>% gather(key = "Type", value = "Return", -Name, -Sector)
      ggplot(tempdata, aes(Return, Name, color = Sector, type = Return, shape = Type)) + geom_point(size = 5) + 
        theme(text = element_text(size=20))
    })
    output$qqplot_grid <- renderPlot({
      #Day5 <- QQ_Comparison(past_n_days = 5-1)
      #Day30 <- QQ_Comparison(past_n_days = 30-1)
      #Day100 <- QQ_Comparison(past_n_days = 100-1)
      DayN <- QQ_Comparison(past_n_days = as.numeric(as.character(input$selectday))-1)
      grid.arrange(DayN$qqP1,DayN$qqP2, nrow=1)
    })
  }
)

########################### END SCRIPT ############################