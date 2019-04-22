library(foreign)
library(dplyr)
library(oce)
library(ggplot2)
library(readxl) 
library(cluster)

df <- as_tibble(read.dbf('Arc_db/fin_work_grd_katya_srf03_1994_2016_winter.DBF')[1:11])
Data50_13 <- read_excel("Arc_db/Data50-13.xlsx", 
                        col_types = c("skip", "text", "skip", 
                                      "skip", "skip", "skip", "skip", 
                                      "skip", "skip", "skip", "skip", 
                                      "skip", "skip", "skip", "skip", "skip", 
                                      "skip", "skip", "skip"))


bln_nodes <- substring(levels(as.factor(Data50_13$Station)), 1, 3)

df <- df %>% 
  mutate(NBD = substring(NBD, 6, 9)) %>% 
  filter(NBD %in% bln_nodes) %>% 
  mutate(NBD = as.factor(NBD))

df.with.zero <- as_tibble(matrix(vector(), 0, length(names(df)), 
                                 dimnames = list(c(),names(df))))


for(year in 1:length(levels(as.factor(df$GOD)))){
  year.arr <- df %>% filter(GOD == levels(as.factor(GOD))[year])
  print(levels(as.factor(df$GOD))[year])
  for(node in 1:length(levels(year.arr$NBD))){
    node.arr <- year.arr %>% filter(NBD == levels(NBD)[node])
    for(depth in 2:length(levels(as.factor(node.arr$H)))){
      if((node.arr$T[depth -1] < 0 & node.arr$T[depth] > 0) | 
         (node.arr$T[depth-1] > 0 & node.arr$T[depth] < 0)){
        node.string <- tibble(NBD = node.arr$NBD[depth],
                        GOD = node.arr$GOD[depth],
                        REG = node.arr$REG[depth],
                        SUBBAS = node.arr$SUBBAS[depth],
                        HMAX_ST = node.arr$HMAX_ST[depth],
                        XL = node.arr$XL[depth],
                        YL = node.arr$YL[depth],
                        H = oce.approx(x = c(node.arr$T[depth - 1], node.arr$T[depth]),
                                       y = c(node.arr$H[depth - 1], node.arr$H[depth]),
                                       0),
                        T = 0,
                        S = oce.approx(x = c(node.arr$T[depth - 1], node.arr$T[depth]),
                                       y = c(node.arr$S[depth - 1], node.arr$S[depth]),
                                       0),
                        SOURCE = node.arr$SOURCE[depth])
        node.arr <- rbind(node.arr, node.string)
      }
    }
    node.arr <- node.arr %>% arrange(H)
    df.with.zero <- rbind(df.with.zero, node.arr)
  }
}
rm('node.arr', 'Data50_13', 'year.arr', 'node.string', 'depth', 'year', 'node')
df.with.zero <- df.with.zero %>% 
  mutate(sigma = swSigma(S, T, H), SOURCE = NULL) %>% 
  filter(T > -5) %>% 
  group_by(GOD, NBD) %>% 
  mutate(Layer = ifelse(T >= 0, 'Atl',
                        ifelse(T < 0 & sigma < 29.7, 'Upper', 'Bottom'))) %>% 
  mutate(Th = ifelse(Layer == 'Upper', ifelse(length(H[Layer == 'Atl']) != 0, H[T == 0][1], 
                                       ifelse(length(H[Layer == 'Bottom']) != 0, H[Layer == 'Bottom'][1], max(H[Layer == 'Upper']))), 
              ifelse(Layer == 'Atl', ifelse(length(H[T == 0]) == 2, H[T == 0][2] - H[T == 0][1], 
                                     ifelse(length(H[Layer == 'Upper']) != 0, max(H[Layer == 'Atl']) - H[T == 0], max(H[Layer == 'Atl']))), 
              ifelse(Layer == 'Bottom', ifelse(length(H[Layer == 'Upper']) != 0 & length(H[Layer == 'Atl']) != 0, max(H) - H[T == 0][2], 
                                       ifelse(length(H[Layer == 'Upper']) == 0, max(H) - H[T == 0][1], max(H) - max(H[Layer == 'Upper']))), 1))))

mean.by.node <- df.with.zero %>% 
  group_by(GOD, NBD, Layer) %>% 
  summarise(S = mean(S[!S %in% boxplot.stats(S)$out]),
            Temp = mean(T[!T %in% boxplot.stats(T)$out]),
            Th = mean(Th[!Th %in% boxplot.stats(Th)$out]),
            sigma = mean(sigma[!sigma %in% boxplot.stats(sigma)$out]))

df.with.zero %>% filter(Layer == 'Upper', GOD < 2007) %>% 
ggplot(aes(GOD, S, group = NBD))+
  stat_summary(fun.y = mean, geom = 'line')

df.clust <- mean.by.node %>%
  group_by(NBD) %>%
  filter(Layer == 'Upper', !NBD %in% c('277', '255', '098', '099')) %>%
  mutate_at(.vars = c('Temp', 'Th', 'S'), .funs = scale) %>%
  summarise(Temp = mean(Temp), S = mean(S), Th = mean(Th))

clust_mat <- dist(df.clust[2:4])
clusters <- hclust(clust_mat, method = 'complete')
plot(clusters, labels = df.clust$NBD)
rect.hclust(clusters, 2)
plot(clusters, labels = df.clust$NBD)
rect.hclust(clusters, 3)
df.clust$cluster5 <- as.factor(cutree(clusters, 3))
df.clust$cluster2 <- as.factor(cutree(clusters, 2))

df.mean.upper.layer <- left_join(mean.by.node %>% filter(Layer == 'Upper', !NBD %in% c('277', '255', '098', '099')), 
                                 df.clust[c('NBD', 'cluster5', 'cluster2')], by = 'NBD')

df.mean.upper.layer %>% 
  group_by(GOD, cluster5) %>% 
  summarise(Temp = mean(Temp), Th = mean(Th), S = mean(S)) %>% 
  ggplot(aes(Temp, S, col = cluster5))+
  geom_point()



df.mean.upper.layer %>% 
  filter(GOD < 2007) %>% 
  ggplot(aes(GOD, S))+
  geom_point(size = 0.3, aes(col = cluster5))+
  stat_summary(fun.y = mean, geom = 'line', size = 0.2, aes(col = cluster5,group = NBD))+
  stat_summary(fun.y = mean, geom = 'line', size = 0.8, col = 'black')+
  facet_grid(.~cluster5)
   # 
# mean.by.node %>% 
#   group_by(NBD) %>% 
#   filter(Layer == 'Upper') %>% 
#   select(Temp, S, Th) %>% 
#   mutate_at(scale, .vars = c(Temp, S, Th)) %>% 
#   summarise_all(mean)
# mean.by.node %>% 
#   group_by(NBD) %>% 
#   filter(Layer == 'Bottom') %>% 
#   summarise(Th = is.na(Th))
# mean.by.node$Th


# df.without.atl <- df.with.zero %>% 
#   group_by(NBD, GOD) %>% 
#   summarise(Layers = length(H[Layer == 'Atl'] == 0))
# which(df.with.zero$H[df.with.zero$Layer == 'Atl'] == 0)
# df.with.zero$NBD[length(df.with.zero$Layer == 'Atl') == 0]
# length(df.with.zero$Layer == 'Atl')

df.full <- read.dbf('/home/sm/Downloads/Katya_Nodes_1950_2013_winter/Data_Katya_Nodes_1950_2013.dbf', as.is = T)
df.full$Station <- as.factor(substring(df.full$NBD,6,8))
df.with.zero[df.with.zero$NBD == df.with.zero$NBD[df.with.zero$H > 300 & df.with.zero$Layer == 'Upper'] & df.with.zero$GOD == 2006,]
df.with.zero[df.with.zero$NBD == df.with.zero$NBD[length(df.with.zero$H[df.with.zero$Layer == 'Atl']) == 0],]



library(data.table)
df.amer <- read.dta('post_Evro.dat')



df.nodes <- tibble(Node = levels(as.factor(substring(Data50_13$Station, 1, 3))),
                   XL = substring(Data50_13$Station[substring(Data50_13$Station, 1, 3) %in% df.nodes$Node][1:116], 7, 10),
                   YL = substring(Data50_13$Station[substring(Data50_13$Station, 1, 3) %in% df.nodes$Node][1:116], 14, 17),
                   Lon = Data50_13$`Longitude [degrees_east]`[1:116],
                   Lat = Data50_13$`Latitude [degrees_north]`[1:116],
                   Last.level = Data50_13$`Bot. Depth[m]`[1:116])

write.csv(df.nodes, 'nodes.csv', row.names = F)
