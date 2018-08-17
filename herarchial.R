library(hts)
library(zoo)
library(xts)
library(lubridate)
library(reshape2)
library(data.table)

for_val<-24
dt$AQI[dt$AQI=="Atleast 3 inputs*"]<-0
dt$AQI<-as.numeric(dt$AQI)
dt[,"City_station"]<-paste0(dt$City,"-",dt$Station)
dt$From.Date<-as.POSIXct(dt$From.Date,format = "%d-%m-%Y %H:%M")
dcast(dt, From.Date ~City_station, value.var = "AQI", fill = 0)->dat
dat1<-as.matrix(dat[,-1])
rownames(dat1)<-as.character(as.Date(dat$From.Date))

df_ts = ts(dat1,start=c(year(dt$From.Date[1]),1,1), frequency = 365.25, 
           class= c("mts","ts","matrix"))

#df_ts <- as.xts(x = dat1, order.by = dat$From.Date,)
nodes<-list(2,c(3,3))
#dfts<-df_ts[,-1]
aqi.hts <- hts(df_ts, nodes = nodes)
aqi.hts %>% aggts(levels=0:1) %>%
  autoplot(facet=TRUE) +
  xlab("Year") + ylab("AQI") + ggtitle("AQI Index")



library(stringr)
library(tidyverse)
cols <- sample(scales::hue_pal(h=c(15,375),
                               c=100,l=65,h.start=0,direction = 1)(NCOL(df_ts)))
as_tibble(df_ts) %>%
  gather(Zone) %>%
  mutate(Date = rep(time(df_ts), NCOL(df_ts)),
         City = gsub("-.*","",(Zone))) %>%
  ggplot(aes(x=Date, y=value, group=Zone, colour=Zone)) +
  geom_line() +
  facet_grid(City~., scales="free_y") +
  xlab("Year") + ylab("AQI") +
  ggtitle("AQI score") +
  scale_colour_manual(values = cols)



# Forecast 10-step-ahead using the bottom-up method
aqiforecast <- forecast(aqi.hts, method='bu', fmethod='arima', h=for_val)
# plot the forecasts including the last ten historical years
autoplot(aggts(aqi.hts, level=0)) +
  autolayer(aggts(aqiforecast, level=0), lty=2)
autoplot(aggts(aqi.hts, level=1)) +
  autolayer(aggts(aqiforecast, level=1), lty=2)
autoplot(aggts(aqi.hts, level=2)) +
  autolayer(aggts(aqiforecast, level=2), lty=2)
