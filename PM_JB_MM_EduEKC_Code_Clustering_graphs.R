################################################
########## Foreign Direct Investments ##########
################################################

library(tidyverse)
library(ggplot2)

## Load the SWIID database
setwd("C:/Users/paulm/OneDrive/Documenti/Ricerca/Dati/SWIID Gini Index/swiid9_0")
load("swiid9_0.rda")

### OECD Countries selection
europe <- swiid_summary %>% filter(country=="Italy" | country=="France" | country=="Germany" | country=="Belgium" |
                                     country=="Spain" | country=="Portugal" | country=="Greece" | country=="Austria" |
                                     country=="Netherlands"  | country=="Norway" |
                                     country=="Finland" | country=="Sweden" | country=="Denmark" | 
                                     country=="Ireland" | country=="United Kingdom" | country=="Switzerland" | country=="Turkey") %>%
  mutate(gini_disp_c = gini_disp/gini_disp_se) %>% filter(year>=1987, year<=2015) %>%
  select(country, year, gini_disp, gini_mkt, gini_disp_c, gini_disp_se, gini_mkt_se)

### g0
ggplot(europe, aes(x=year,y=gini_disp)) + geom_line(aes(colour=country),size=1.1) + 
  theme_bw() + 
  theme(legend.position="bottom", legend.title=element_blank(),
        axis.text=element_text(size=13), axis.title=element_text(size=20), 
        plot.title=element_text(size=25), plot.subtitle=element_text(size=15)) + 
  guides(colour = guide_legend(nrow = 2)) + 
  labs(title= "Income distribution inequality", subtitle=("Full panel within 1987-2015")) + xlab("") + ylab("Gini Index")


### G1
aggr <- europe %>% group_by(year) %>% summarise(avg_gini=mean(gini_disp), sd_gini=sd(gini_disp))

pdf(file = "Avg_Gini_Europe.pdf", width = 8, height = 6)
print(ggplot(aggr, aes(x=year,y=avg_gini)) + 
        geom_line(colour="black",size=1.1) + 
        geom_ribbon(aes(ymin=avg_gini-1.96*sd_gini/sqrt(17), ymax=avg_gini+1.96*sd_gini/sqrt(17),linetype=NA), alpha=0.5, show.legend=T) +
        theme_bw() + 
        theme(legend.position="right", legend.title=element_blank(), 
              axis.text=element_text(size=13), axis.title=element_text(size=20), 
              plot.title=element_text(size=25), plot.subtitle=element_text(size=15)) + 
        labs(title= "Income inequality (1987-2015)", 
             subtitle=("Sample mean and Gaussian confidence interval at 95%")) + xlab("") + ylab("Gini Index"))
dev.off()