library(readxl)
library(dplyr)
library(ggplot2)
library(WaveletComp)
library(biwavelet)

GrBar_1900_2018 <- read_excel("Arc_db/GrBar_1900_2018.xls")
# GrBar_1900_2018 <- GrBar_1900_2018 %>%  mutate(Year = (paste0('01/01/', Year)))
# as.Date(GrBar_1900_2018$Year, '%d/%m/%yyyy')
ggplot(GrBar_1900_2018, )+
  geom_line(aes(Year, GS), col = '#3182bd')+
  geom_line(aes(Year, BS), col = '#e34a33')+
  geom_point(aes(Year, GS), col = '#3182bd')+
  geom_point(aes(Year, BS), col = '#e34a33')

gs.w <- analyze.wavelet(GrBar_1900_2018, my.series = 'GS', loess.span = 0, dt = 1, dj = 1/100,
                        make.pval = T, upperPeriod = 70, lowerPeriod = 1, method = 'ARIMA')
gs.w <- wt(GrBar_1900_2018$GS)

gs.image <- wt.image(gs.w, color.key = 'interval', exponent = 0.5)
wt.sel.phases(gs.w)
bs.w <- analyze.wavelet(GrBar_1900_2018, my.series = 'BS', loess.span = 0, dt = 1, dj = 1/100,
                        make.pval = T)
bs.image <- wt.image(bs.w, color.key = 'interval', show.date = T)

gs.wb <- wt(cbind(1:dim(GrBar_1900_2018)[1], GrBar_1900_2018))
plot(gs.wb, type = 'power')
wt.image(gs.wb)
wt.avg(gs.w)
?analyze.wavelet()

