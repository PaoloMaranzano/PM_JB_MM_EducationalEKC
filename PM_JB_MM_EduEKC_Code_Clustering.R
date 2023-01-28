###############################################
########## SWIID Database (2018 ed.) ##########
###############################################

### Packages
library(tidyverse)
library(xts)
library(cluster)
library(stats)
library(fpc)
library(factoextra)
library(ggplot2)
library(eurostat)
library(tmap)

### Upload dataset SWIID 7.1
setwd("~/Ricerca/Educational EKC (with Joao Bento)/Clustering")
load("C:/Users/paulm/OneDrive/Documenti/Ricerca/Dati/SWIID Gini Index/swiid9_0/swiid9_0.rda")

annoA <- 1987
annoB <- 2017

### OECD Countries selection
countries_red <- c("Italy","France","Germany","Belgium", "Spain" ,"Portugal" ,"Greece" ,"Austria",
                   "Netherlands","Luxembourg","Norway", "Finland" ,"Sweden" ,"Denmark",
                   "Ireland","United Kingdom","Turkey", "Switzerland")

europe <- swiid_summary %>% 
  filter(country %in% countries_red) %>%
  mutate(gini_disp_c = gini_disp/gini_disp_se) %>% filter(year>=annoA, year<=annoB) %>%
  select(country, year, gini_disp, gini_mkt, gini_disp_c, gini_disp_se, gini_mkt_se)


#####################################
##### Gini on disposable income #####
#####################################

### Extract panel for clustering
panel <- europe %>% 
  filter(year>=annoA, year<=annoB) %>% 
  gather(., "Variable", "Value", 3:7) %>% 
  select(country,year, Variable, Value)  %>% 
  spread(., year, Value) %>%
  filter(Variable=="gini_disp")

europe2 <- europe %>% 
  select(country,year,gini_disp) %>% 
  spread(., country, gini_disp) %>% 
  filter(year>=annoA, year<=annoB)

DataClust <-  panel %>% column_to_rownames(.,'country')
DataClust <- DataClust[,-1]
DataClust <- DataClust %>% scale(.,center=T,scale=T) %>% data.frame(.)

distance <- get_dist(DataClust)
fviz_dist(distance, gradient = list(low="white", mid="grey", high="black"))

clust.k2 <- kmeans(DataClust, centers=2, nstart = 25)

## Valori medi o centrali delle variabili per ogni gruppo creato (originali)
Data <- cbind(panel,k2=clust.k2$cluster)

### Graph 1
aggr <- europe %>%
  filter(year <= 2018) %>%
  group_by(year) %>%
  summarise(avg_gini=mean(gini_disp), sd_gini=sd(gini_disp))
g1 <- ggplot(aggr, aes(x=year,y=avg_gini)) + 
  geom_line(colour="black",size=1.1) + 
  geom_ribbon(aes(ymin=avg_gini-1.96*sd_gini/sqrt(17),
                  ymax=avg_gini+1.96*sd_gini/sqrt(17),linetype=NA), alpha=0.5, show.legend=T) +
  theme_bw() + 
  theme(legend.position="right", legend.title=element_blank(), 
        axis.text=element_text(size=13), axis.title=element_text(size=20), 
        plot.title=element_text(size=20), plot.subtitle=element_text(size=15)) + 
  # labs(title= "Income inequality (1987-2018) on disposable income", 
  #      subtitle=("Sample mean and Gaussian confidence interval at 95%")) + 
  xlab("") + 
  ylab("") + 
  scale_x_continuous(breaks = c(1987,1990,1995,2000,2005,2010,2015))
pdf(file = "Avg_Gini_disposable.pdf", width = 8, height = 6)
print(g1)
dev.off()
### Graph 2
summ1 <- Data %>% 
  group_by(k2) %>%
  add_count() %>%
  summarise_all(mean) %>%
  select(-c(country,Variable)) %>% 
  mutate(Group=case_when(k2 == 2 ~ "Group 1",
                         k2 == 1 ~ "Group 2")) %>%
  pivot_longer(names_to = "year",values_to = "avg_gini",2:32)
summ2 <- Data %>% 
  group_by(k2) %>% 
  summarise_all(sd) %>% 
  select(-c(country,Variable)) %>% 
  mutate(Group=case_when(k2 == 2 ~ "Group 1",
                         k2 == 1 ~ "Group 2")) %>% 
  pivot_longer(names_to = "year",values_to = "sd_gini",2:32) %>%
  select(sd_gini)
summ <- bind_cols(summ1,summ2)
g2 <- ggplot(summ, aes(x=year,y=avg_gini, group=Group)) + 
  geom_line(aes(linetype=Group),size=1.1) +
  geom_ribbon(aes(ymin=avg_gini-1.96*sd_gini/sqrt(n),
                  ymax=avg_gini+1.96*sd_gini/sqrt(n),linetype=Group), alpha=0.5, show.legend=T) +
  theme_bw() + 
  theme(legend.position="bottom", legend.title=element_blank(),
        axis.text=element_text(size=15), axis.title=element_text(size=20),
        plot.title=element_text(size=20), plot.subtitle=element_text(size=15)) + 
  # labs(title= "Income inequality (1987-2018) on disposable income", 
  #      subtitle=("Sample mean and Gaussian confidence interval at 95%")) + 
  scale_x_discrete("", breaks=c(1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous("", breaks=c(25,27,30,33,35)) + 
  scale_linetype_manual(labels = c("Group 1 (High income inequality): 7 countries",
                                   "Group 2 (Low income inequality): 10 countries"), 
                        values = c("dotted","twodash")) +
  guides(linetype = guide_legend(nrow = 2))
pdf(file = "Avg_Gini_Clusters_disposable.pdf", width = 8, height = 6)
print(g2)
dev.off()
### Maps
europe <- swiid_summary %>% 
  filter(country %in% countries_red)
dati <- Data %>% 
  mutate(CNTR_CODE=case_when(country == "Italy" ~ "IT", country == "France" ~ "FR", country == "Germany" ~ "DE",
                             country == "Belgium" ~ "BE", country == "Spain" ~ "ES", country == "Portugal" ~ "PT",
                             country == "Greece" ~ "EL", country == "Austria" ~ "AT", country == "Netherlands" ~ "NL",
                             country == "Luxembourg" ~ "LU",
                             country == "Norway" ~ "NO", country == "Finland" ~ "FI", country == "Sweden" ~ "SE",
                             country == "Denmark" ~ "DK", country == "Ireland" ~ "IE", country == "United Kingdom" ~ "UK",
                             country == "Switzerland" ~ "CH", country == "Turkey" ~ "TR"))
geodata <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = 0)
map_data <- right_join(geodata, dati, by="CNTR_CODE")
map_data <- map_data %>% mutate(k2=case_when(k2==1 ~ "Low income inequality",
                                             k2==2 ~ "High income inequality"))
## Thematic maps
map1 <- tmap::tm_shape(map_data,xlim=c(-15,45),ylim=c(33,73)) +
  tmap::tm_fill("lightgrey") +
  tmap::tm_shape(map_data) +
  tmap::tm_grid() +
  tmap::tm_borders(lwd=3,col="black") + 
  tmap::tm_polygons(col="k2", title="",n=2, palette=c("#333333","#999999")) +
  tmap::tm_layout(
    #main.title="K-means clustering using Gini on disposable income",
    main.title="",
    main.title.position="center",
    legend.outside = F, legend.show=T, legend.text.size=1.05,
    legend.title.size=1.9, legend.position=c(0.62,0.44),
    inner.margins=c(0,0,0,0),asp=NA, main.title.size = 1.2) +
  tmap::tm_ylab("") + 
  tmap::tm_xlab("")
pdf(file = "Cluster_Map_Europe_Gini_disp_income.pdf", width = 8, height = 6)
print(map1)
dev.off()






#################################
##### Gini on market income #####
#################################

### Extract panel for clustering
panel <- europe %>% 
  filter(year>=annoA, year<=annoB) %>% 
  gather(., "Variable", "Value", 3:7) %>% 
  select(country,year, Variable, Value)  %>% 
  spread(., year, Value) %>%
  filter(Variable=="gini_mkt")

europe2 <- europe %>% 
  select(country,year,gini_mkt) %>% 
  spread(., country, gini_mkt) %>% 
  filter(year>=annoA, year<=annoB)

DataClust <-  panel %>% column_to_rownames(.,'country')
DataClust <- DataClust[,-1]
DataClust <- DataClust %>% scale(.,center=T,scale=T) %>% data.frame(.)

distance <- get_dist(DataClust)
fviz_dist(distance, gradient = list(low="white", mid="grey", high="black"))

clust.k2 <- kmeans(DataClust, centers=2, nstart = 25)

## Valori medi o centrali delle variabili per ogni gruppo creato (originali)
Data <- cbind(panel,k2=clust.k2$cluster)

### Graph 1
aggr <- europe %>%
  filter(year >= 1987, year <= 2018) %>%
  group_by(year) %>% 
  summarise(avg_gini=mean(gini_mkt), sd_gini=sd(gini_mkt))
g1 <- ggplot(aggr, aes(x=year,y=avg_gini)) + 
  geom_line(colour="black",size=1.1) + 
  geom_ribbon(aes(ymin=avg_gini-1.96*sd_gini/sqrt(17),
                  ymax=avg_gini+1.96*sd_gini/sqrt(17),linetype=NA), alpha=0.5, show.legend=T) +
  theme_bw() + 
  theme(legend.position="right", legend.title=element_blank(), 
        axis.text=element_text(size=13), axis.title=element_text(size=20), 
        plot.title=element_text(size=20), plot.subtitle=element_text(size=15)) + 
  # labs(title= "Income inequality (1987-2018) on market income", 
  #      subtitle=("Sample mean and Gaussian confidence interval at 95%")) + 
  xlab("") +
  ylab("") + 
  scale_x_continuous(breaks = c(1987,1990,1995,2000,2005,2010,2015))
pdf(file = "Avg_Gini_market.pdf", width = 8, height = 6)
print(g1)
dev.off()
### Graph 2
summ1 <- Data %>% 
  group_by(k2) %>%
  add_count() %>%
  summarise_all(mean) %>%
  select(-c(country,Variable)) %>% 
  mutate(Group=case_when(k2 == 1 ~ "Group 1",
                         k2 == 2 ~ "Group 2")) %>%
  pivot_longer(names_to = "year",values_to = "avg_gini",2:32)
summ2 <- Data %>% 
  group_by(k2) %>% 
  summarise_all(sd) %>% 
  select(-c(country,Variable)) %>% 
  mutate(Group=case_when(k2 == 1 ~ "Group 1",
                         k2 == 2 ~ "Group 2")) %>% 
  pivot_longer(names_to = "year",values_to = "sd_gini",2:32) %>%
  select(sd_gini)
summ <- bind_cols(summ1,summ2)
g2 <- ggplot(summ, aes(x=year,y=avg_gini, group=Group)) + 
  geom_line(aes(linetype=Group),size=1.1) +
  geom_ribbon(aes(ymin=avg_gini-1.96*sd_gini/sqrt(n),
                  ymax=avg_gini+1.96*sd_gini/sqrt(n),linetype=Group), alpha=0.5, show.legend=T) +
  theme_bw() + 
  theme(legend.position="bottom", legend.title=element_blank(),
        axis.text=element_text(size=15), axis.title=element_text(size=20),
        plot.title=element_text(size=20), plot.subtitle=element_text(size=15)) + 
  # labs(title= "Income inequality (1987-2018) on market income", 
  #      subtitle=("Sample mean and Gaussian confidence interval at 95%")) + 
  scale_x_discrete("", breaks=c(1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous("", breaks=c(25,27,30,33,35)) + 
  scale_linetype_manual(labels = c("Group 1 (High income inequality): 7 countries",
                                   "Group 2 (Low income inequality): 10 countries"), 
                        values = c("dotted","twodash")) +
  guides(linetype = guide_legend(nrow = 2))
pdf(file = "Avg_Gini_Clusters_market.pdf", width = 8, height = 6)
print(g2)
dev.off()
### Maps
europe <- swiid_summary %>% 
  filter(country %in% countries_red)
dati <- Data %>% 
  mutate(CNTR_CODE=case_when(country == "Italy" ~ "IT", country == "France" ~ "FR", country == "Germany" ~ "DE",
                             country == "Belgium" ~ "BE", country == "Spain" ~ "ES", country == "Portugal" ~ "PT",
                             country == "Greece" ~ "EL", country == "Austria" ~ "AT", country == "Netherlands" ~ "NL",
                             country == "Luxembourg" ~ "LU",
                             country == "Norway" ~ "NO", country == "Finland" ~ "FI", country == "Sweden" ~ "SE",
                             country == "Denmark" ~ "DK", country == "Ireland" ~ "IE", country == "United Kingdom" ~ "UK",
                             country == "Switzerland" ~ "CH", country == "Turkey" ~ "TR"))
geodata <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = 0)
map_data <- right_join(geodata, dati, by="CNTR_CODE")
map_data <- map_data %>% mutate(k2=case_when(k2==2 ~ "Low income inequality",
                                             k2==1 ~ "High income inequality"))
## Thematic maps
map1 <- tmap::tm_shape(map_data,xlim=c(-15,45),ylim=c(33,73)) +
  tmap::tm_fill("lightgrey") +
  tmap::tm_shape(map_data) +
  tmap::tm_grid() +
  tmap::tm_borders(lwd=3,col="black") + 
  tmap::tm_polygons(col="k2", title="",n=2, palette=c("#333333","#999999")) +
  tmap::tm_layout(
    #main.title="K-means clustering using Gini on market income", 
    main.title = "",
    main.title.position="center",
    legend.outside = F, legend.show=T, legend.text.size=1.05,
    legend.title.size=1.9, legend.position=c(0.62,0.44),
    inner.margins=c(0,0,0,0),asp=NA,main.title.size = 1.2) +
  tmap::tm_ylab("") + 
  tmap::tm_xlab("")
pdf(file = "Cluster_Map_Europe_Gini_market.pdf", width = 8, height = 6)
print(map1)
dev.off()