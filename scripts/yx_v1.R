# Library
library(quantmod)
library(ggplot2)
library(gganimate)

# Define function
Gen_Data <- function(past_n_days = 30) {
  # List of names
  data <- getSymbols(c(
    "AAPL", "FB", "GOOGL", "WMT", "DIS", "SBUX", "GS", "BAC", "MS"
  ), to = "2019-04-09")
  
  # Mutate data
  #past_n_days <- 30
  AAPL <- data.frame(data.frame(AAPL) %>% mutate(Stock = "AAPL", Sector="Technology"))[(nrow(AAPL)-past_n_days):nrow(AAPL), ]
  FB <- data.frame(data.frame(FB) %>% mutate(Stock = "FB", Sector="Technology"))[(nrow(FB)-past_n_days):nrow(FB), ]
  GOOGL <- data.frame(data.frame(GOOGL) %>% mutate(Stock = "GOOGL", Sector="Technology"))[(nrow(GOOGL)-past_n_days):nrow(GOOGL), ]
  WMT <- data.frame(data.frame(WMT) %>% mutate(Stock = "WMT", Sector = "Consumer"))[(nrow(WMT)-past_n_days):nrow(WMT), ]
  DIS <- data.frame(data.frame(DIS) %>% mutate(Stock = "DIS", Sector = "Consumer"))[(nrow(DIS)-past_n_days):nrow(DIS), ]
  SBUX <- data.frame(data.frame(SBUX) %>% mutate(Stock = "SBUX", Sector = "Consumer"))[(nrow(SBUX)-past_n_days):nrow(SBUX), ]
  GS <- data.frame(data.frame(GS) %>% mutate(Stock = "GS", Sector = "Financial"))[(nrow(GS)-past_n_days):nrow(GS), ]
  BAC <- data.frame(data.frame(BAC) %>% mutate(Stock = "BAC", Sector = "Financial"))[(nrow(BAC)-past_n_days):nrow(BAC), ]
  MS <- data.frame(data.frame(MS) %>% mutate(Stock = "MS", Sector = "Financial"))[(nrow(MS)-past_n_days):nrow(MS), ]
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
  data <- rbind(AAPL, FB, GOOGL, WMT, DIS, SBUX, GS, BAC, MS) %>% rownames_to_column("Date")
  Date1<-strsplit(data$Date,"-") %>% ldply()
  colnames(Date1) <- c("Year", "Month", "Day")
  data<-cbind(data,Date1)
  data$Month <- as.numeric(data$Month)
  data$Date <- as.Date(data$Date)
  data <- data.frame(na.omit(data))
  head(data); dim(data)
  return(data)
} # End of function

# Define function
grid_barplot <- function() {
  # Generate data
  data <- Gen_Data(past_n_days <- 50)
  
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
  data <- Gen_Data(past_n_days <- 100)
  
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
  
  # Plot in a grid
  grid.arrange(bar_P1,bar_P2,bar_P3,bar_P4, 
               bar_P5,bar_P6,bar_P7,bar_P8, 
               nrow=2)
} # End of function

# Visualization of monthly prices
#ggplot(data,aes(factor(Stock),Close,color=Stock)) +
#  geom_jitter(aes(size=Close)) +
#  ylim(0,1000)+
#  labs(title = 'Month: {frame_time}', x = 'Stocks', y = 'Close Price') #+
  #transition_time(Month)


#Discovering the Relation between Total Traded Quantity vs Close Price
# ggplot(data, aes(x = Volume, y = Close, color = Stock)) +
#   geom_smooth(method='loess') +
#   xlim(0,400)+
#   facet_wrap(~ Stock, ncol = 3, scale = "free_y") +
#   scale_fill_tq(fill="green4",theme="light") +
#   labs(title = "Monthly: {frame_time} Traded Quantity vs Price", x = "Traded Quantity (Lacs)",y="Close Price") +
#   transition_time(Month)









