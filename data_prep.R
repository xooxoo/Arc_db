library(dplyr)
library(ggplot2)
library(ggbiplot)
library(spacetime)
library(WaveletComp)
library(readxl)

# Читаем данные из исходника, выбрасываем лишнее, переименовывавем
Data50_13 <- read_excel("Arc_db/Data50-13.xlsx", 
                        col_types = c("skip", "text", "text", 
                                      "text", "skip", "skip", "skip", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "skip", "skip", "skip", 
                                      "skip", "skip", "skip", "skip", 
                                      "skip"))
Data50_13 <- Data50_13 %>% 
  rename(Lat = `Latitude [degrees_north]`,
         Lon = `Longitude [degrees_east]`,
         Year = `Winter for Year:`,
         ULTh = `Upper Layer Thickness [m]`,
         ULS = `Upper Layer Mean Salinity [psu]`,
         ULT = `Upper Layer Mean Temperature [ｰC]`) %>% 
  mutate(Station = substring(Station, 1, 3))


# Удаляем выбросы в каждом узле (по времени), делим на 2 кластера
node.for.cluster <- Data50_13 %>% 
  group_by(Station) %>% 
  summarise(mULTh = mean(ULTh[!ULTh %in% boxplot.stats(ULTh)$out], na.rm = T),
            mULT = mean(ULT[!ULT %in% boxplot.stats(ULT)$out], na.rm = T),
            mULS = mean(ULS[!ULS %in% boxplot.stats(ULS)$out], na.rm = T)) %>% 
  mutate_each_(funs(scale(.) %>% as.vector),
                 vars = c('mULTh', 'mULT', 'mULS'))

dist.m.for.clust <- dist(node.for.cluster)
fit <- hclust(dist.m.for.clust)
plot(fit, labels = node.for.cluster$Station)
rect.hclust(fit, 2)
cutree(fit, 2)
node.for.cluster <- node.for.cluster %>% 
  mutate(clusters = as.factor(cutree(fit, 2)))
Data50_13 <- left_join(Data50_13, node.for.cluster[c("Station", 'clusters')], by = "Station")

ggplot(Data50_13, aes(Year, ULS, group = interaction(Year, clusters), color = clusters))+
  stat_boxplot(na.rm = T)

mean.by.clust.no.out <- Data50_13 %>%  
  group_by(Year, clusters) %>% 
  summarise(mULTh = mean(ULTh[!ULTh %in% boxplot.stats(ULTh)$out], na.rm = T),
            mULT = mean(ULT[!ULT %in% boxplot.stats(ULT)$out], na.rm = T),
            mULS = mean(ULS[!ULS %in% boxplot.stats(ULS)$out], na.rm = T))
mean.by.clust.no.out



Data50_13
dd <- Data50_13 %>% 
  mutate(clusters = as.factor(clusters)) %>% 
  group_by(Year, clusters) %>% 
  summarise(meanULS = mean(ULS, na.rm = T),
            meanULT = mean(ULT, na.rm = T),
            meanULTh = mean(ULTh, na.rm = T))
ggplot(dd, aes(Year, meanULT, col = clusters, group = clusters))+
  geom_point()+
  geom_line()

ggplot(dd, aes(meanULT, fill = clusters))+
  geom_density(alpha = 0.5)

ggplot(dd, aes(meanULS, fill = clusters))+
  geom_density(alpha = 0.5)

sd(dd$meanULT)

ggplot(mean.by.clust.no.out, aes(Year, mULT, col = clusters, group = clusters))+
  geom_point()+
  geom_line()

ggplot(mean.by.clust.no.out, aes(mULT, fill = clusters))+
  geom_density(alpha = 0.5)

ggplot(mean.by.clust.no.out, aes(mULS, fill = clusters))+
  geom_density(alpha = .5)

sd(mean.by.clust.no.out$mULT)

