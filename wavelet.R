library(dplyr)
library(WaveletComp)
library(ggplot2)
data("weather.radiation.Mannheim")
weather.radiation.Mannheim <- as_tibble(weather.radiation.Mannheim)
head(weather.radiation.Mannheim)
weather.radiation.Mannheim$date <- as.Date(weather.radiation.Mannheim$date)
# поиск сезонности в данных о погоде

my.w <- analyze.wavelet(weather.radiation.Mannheim, 'temperature',
                        loess.span = 0, dt = 1, dj = 1/50, 
                        lowerPeriod = 200, upperPeriod = 600,
                        make.pval = T, n.sim = 10)

wt.image(my.w, color.key = 'interval', n.levels = 250, exponent = 0.5,
         legend.params = list(lab = 'wavelet power levels'),
         show.date = T, date.format = '%F', timelab = '')

ggplot(weather.radiation.Mannheim, aes(date, temperature))+
  geom_path()
ggplot(weather.radiation.Mannheim, aes(date, humidity))+
  geom_path()
ggplot(weather.radiation.Mannheim, aes(date, radiation))+
  geom_path()

# Причешем график для наглядности результатов

wt.image(my.w, color.key = 'interval', n.levels = 250,
         exponent = 0.5, maximum.level = 1.25, legend.params = list(label.digits = 2),
         show.date = T, date.format = "%F", timelab = '',
         spec.time.axis = list(at = c(paste(2005:2014, '-01-01', sep = '')),
                               labels = c(2005:2014)),
         timetcl = -0.5,
         spec.period.axis = list(at = c(32, 64, 128, 365, 1024)),
         periodtck = 1, periodtcl = NULL)

max.power <- max(my.w$Power)
wt.image(my.w, color.key = 'interval', n.levels = 250,
         maximum.level = 1.001 * max.power,
         show.date = T, date.format = '%F')


?analyze.wavelet
