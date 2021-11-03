# authors: Pouria Derakhshanfar
source('effect-size.r')

library(ggplot2)
library(tidyverse)
library(gridExtra)

VD_MAGNITUDE_LEVELS = c("large","medium","small")
CONSIDERABLE_VD_MAGNITUDES = c("large","medium")
BBC_CONFIGS = c("WeightedSum+BBC","WeightedSum+OPTBBC")
# For WS
timeEffectSize <- getTimeEffectSizes("WeightedSum") 


timeEffectSize$cat.alg1 <- ifelse(timeEffectSize$cat.alg1 == "WeightedSum", "WeightedSum",
                                              ifelse(timeEffectSize$cat.alg1 == "WeightedSum-bb", "WeightedSum+BBC","WeightedSum+OPTBBC"))


timeEffectSize$cat.alg2 <- ifelse(timeEffectSize$cat.alg2 == "WeightedSum", "WeightedSum",
                                              ifelse(timeEffectSize$cat.alg2 == "WeightedSum-bb", "WeightedSum+BBC","WeightedSum+OPTBBC"))


efficiency_abstract <- data.frame('Winner'=NA, 'WeightedSum.large'=NA,'WeightedSum.medium'=NA,
                                  'WeightedSum+BBC.large'=NA,'WeightedSum+BBC.medium'=NA,
                                  'WeightedSum+OPTBBC.large'=NA, 'WeightedSum+OPTBBC.medium'=NA)
  
efficiency_abstract <- efficiency_abstract[0,]


configurations <- c("WeightedSum","WeightedSum+BBC","WeightedSum+OPTBBC")

for (ca1 in configurations){
  row = c(ca1)
  for(ca2 in configurations){
    for (mag in CONSIDERABLE_VD_MAGNITUDES){
      tempdf <- timeEffectSize %>%
        filter(cat.alg1 == ca1 & cat.alg2 == ca2 & VD.magnitude == mag & VD.estimate.category=="< 0.5")
      row <- c(row,nrow(tempdf))
    }
  }
  efficiency_abstract[nrow(efficiency_abstract) + 1,] = row
}
  

#timeEffectSize <- timeEffectSize %>% filter(VD.estimate.category=="< 0.5")

delta2 <- timeEffectSize%>% 
  mutate(delta = (avg.alg2 - avg.alg1)/avg.alg2) %>%
  group_by(cat.alg1) %>%
  summarise(avg_delta = mean(delta))
  


# p <- ggplot(data=efficiency_abstract, aes(x=Winner, y=count, fill=VD_magnitude)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   geom_text(aes(label=count), vjust=-1.0, color="black",
#             position = position_dodge(0.9), size=5.5)+
#   theme(text = element_text(size=14),axis.text.x = element_text(size=11)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   expand_limits( y=c(0, 20))+
#   scale_fill_manual(values=c('black','gray')) +
#   ylab("# of crashes") + xlab("Configurations") + 
#   theme(legend.position = "none", axis.text.x=element_text(size=14))
#   
# ggsave("figures/significant-efficiency-ws.pdf", width = 5.5, height = 3.45)

mutateTimeEffectSize <- timeEffectSize %>%
  mutate(improvement = 1- (avg.alg1/avg.alg2))


improvementSummary <- mutateTimeEffectSize %>%
  group_by(cat.alg1) %>%
  summarise(avg_improvement = mean(improvement), avg_es = mean(VD.estimate),
            min_improvement = min(improvement), min_es = min(VD.estimate),
            max_improvement = max(improvement), max_es = max(VD.estimate))




temp <- mutateTimeEffectSize %>% 
  filter(cat.alg1== "WeightedSum+OPTBBC" & cat.alg2 == "WeightedSum")



plot1 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "WeightedSum+OPTBBC" & cat.alg2 == "WeightedSum"), aes(x = paste(cat.alg1), y = VD.estimate)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("effect size") + xlab("")
#ggsave("figures/effectSize-efficiency-ws.pdf", width = 3.5, height = 4)

plot2 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "WeightedSum+OPTBBC" & cat.alg2 == "WeightedSum"), aes(x = paste(cat.alg1), y = improvement)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Improvement (perc)") + xlab("")

p  <- grid.arrange(plot1, plot2, ncol=2)
ggsave("figures/improvement-efficiency-ws-optvsnone.pdf", p, width = 7, height = 4)





plot1 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "WeightedSum+BBC" & cat.alg2 == "WeightedSum"), aes(x = paste(cat.alg1), y = VD.estimate)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("effect size") + xlab("")
#ggsave("figures/effectSize-efficiency-ws.pdf", width = 3.5, height = 4)

plot2 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "WeightedSum+BBC" & cat.alg2 == "WeightedSum"), aes(x = paste(cat.alg1), y = improvement)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Improvement (perc)") + xlab("")

p  <- grid.arrange(plot1, plot2, ncol=2)
ggsave("figures/improvement-efficiency-ws-bbcvsnone.pdf", p, width = 7, height = 4)





plot1 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "WeightedSum+OPTBBC" & cat.alg2 == "WeightedSum+BBC"), aes(x = paste(cat.alg1), y = VD.estimate)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("effect size") + xlab("")
#ggsave("figures/effectSize-efficiency-ws.pdf", width = 3.5, height = 4)

plot2 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "WeightedSum+OPTBBC" & cat.alg2 == "WeightedSum+BBC"), aes(x = paste(cat.alg1), y = improvement)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Improvement (perc)") + xlab("")

p  <- grid.arrange(plot1, plot2, ncol=2)
ggsave("figures/improvement-efficiency-ws-optvsbbc.pdf", p, width = 7, height = 4)
