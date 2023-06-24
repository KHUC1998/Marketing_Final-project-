"/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/"
year_month <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/year_month.csv")
year_2020 <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/year_2020.csv")
year_2021 <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/yaer_2021.csv") 
mens_ath_foot <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/TimeSeries(Men's Athletic Footwear).csv")
mens_st_foot <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/TimeSeries(Men's Street Footwear).csv")
wom_ath_foot <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/TimeSeries(Men's Street Footwear).csv")
wom_st_foot <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/TimeSeries(Women's Street Footwear).csv")
mens_apparel <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/TimeSeries(Men's Apparel).csv")
wom_apparel <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/TImeSeries(Women's Apparel).csv")
instore <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/TimeSeries(In-store).csv")
online <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/TimeSeries(Online).csv")
outlet <- read.csv("/Users/akosuke/Desktop/MSBAAtDominican/Marketing/addidas_data/TimeSeries(Outlet).csv")

#year_month
library(TTR)
mean(year_month$sqerror,na.rm = T)
mean(year_month$sqerror4,na.rm = T)

sma_func <- function(data) {
  sma <- SMA(data$Total.Sales, n = 3)
  sma4 <- SMA(data$Total.Sales, n = 4)
  
  data$sma <- c(rep(NA, 3), sma[3:(nrow(data) - 1)])
  data$sqerror <- (data$Total.Sales - data$sma)^2
  
  data$sma4 <- c(rep(NA, 4), sma4[4:(nrow(data) - 1)])
  data$sqerror4 <- (data$Total.Sales - data$sma4)^2
  
  sqerror_mean <- mean(data$sqerror, na.rm = T)
  sqerror4_mean <- mean(data$sqerror4, na.rm = T)
  
  return(list(data = data,
              next_expected_sma = sma[nrow(data)],
              next_expected_sma4 = sma4[nrow(data)],
              sqerror_mean = sqerror_mean, 
              sqerror4_mean = sqerror4_mean))
}

sma_func(year_month)
sma_func(instore)
sma_func(online)
sma_func(outlet)
