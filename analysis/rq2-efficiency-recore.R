# authors: Pouria Derakhshanfar
source('effect-size.r')

library(ggplot2)
library(tidyverse)
library(gridExtra)

VD_MAGNITUDE_LEVELS = c("large","medium","small")
CONSIDERABLE_VD_MAGNITUDES = c("large","medium")

# For integ
timeEffectSize <- getTimeEffectSizes("IntegrationSingleObjective")


timeEffectSize$cat.alg1 <- ifelse(timeEffectSize$cat.alg1 == "IntegrationSingleObjective", "RecoreSTDistance",
                                              ifelse(timeEffectSize$cat.alg1 == "IntegrationSingleObjective-bb", "RecoreSTDistance+BBC","RecoreSTDistance+OPTBBC"))


timeEffectSize$cat.alg2 <- ifelse(timeEffectSize$cat.alg2 == "IntegrationSingleObjective", "RecoreSTDistance",
                                              ifelse(timeEffectSize$cat.alg2 == "IntegrationSingleObjective-bb", "RecoreSTDistance+BBC","RecoreSTDistance+OPTBBC"))



efficiency_abstract <- data.frame('Winner'=NA, 'RecoreSTDistance.large'=NA,'RecoreSTDistance.medium'=NA,
                                  'RecoreSTDistance+BBC.large'=NA,'RecoreSTDistance+BBC.medium'=NA,
                                  'RecoreSTDistance+OPTBBC.large'=NA, 'RecoreSTDistance+OPTBBC.medium'=NA)
efficiency_abstract <- efficiency_abstract[0,]


configurations <- c("RecoreSTDistance","RecoreSTDistance+BBC","RecoreSTDistance+OPTBBC")


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



delta <- timeEffectSize%>% 
  mutate(delta = (avg.alg2 - avg.alg1)/avg.alg2) %>%
  group_by(winner) %>%
  summarise(avg_delta = mean(delta))
  


# p <- ggplot(data=efficiency_abstract, aes(x=Winner, y=count, fill=VD_magnitude)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   geom_text(aes(label=count), vjust=-1.0, color="black",
#             position = position_dodge(0.9), size=5.5)+
#   theme(text = element_text(size=14),axis.text.x = element_text(size=11)) +
#   scale_y_discrete(expand = c(0, 0)) +
#    expand_limits( y=c(0, 20))+
#   scale_fill_manual(values=c('black','gray','lightgray')) +
#   ylab("# of crashes") + xlab("Configurations") +
#   theme(legend.position = "top", legend.text=element_text(size=14), axis.text.x=element_text(size=14))
# ggsave("figures/significant-efficiency-recore.pdf", width = 5.5, height = 4)

mutateTimeEffectSize <- timeEffectSize %>%
  mutate(improvement = ifelse(avg.alg2>avg.alg1,1- (avg.alg1/avg.alg2),(avg.alg2/avg.alg1)-1))



improvementSummary <- mutateTimeEffectSize %>%
  group_by(cat.alg1) %>%
  summarise(avg_improvement = mean(improvement), avg_es = mean(VD.estimate),
            min_improvement = min(improvement), min_es = min(VD.estimate),
            max_improvement = max(improvement), max_es = max(VD.estimate))


plot1 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+BBC" & cat.alg2 == "RecoreSTDistance"), aes(x = paste(cat.alg1), y = VD.estimate)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("effect size") + xlab("")

plot2 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+BBC" & cat.alg2 == "RecoreSTDistance"), aes(x = paste(cat.alg1), y = improvement)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Improvement (perc)") + xlab("")

p  <- grid.arrange(plot1, plot2, ncol=2)
ggsave("figures/improvement-efficiency-recore-bbcvsnone.pdf", p, width = 7, height = 4)


###########

plot1 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+OPTBBC" & cat.alg2 == "RecoreSTDistance"), aes(x = paste(cat.alg1), y = VD.estimate)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("effect size") + xlab("")

plot2 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+OPTBBC" & cat.alg2 == "RecoreSTDistance"), aes(x = paste(cat.alg1), y = improvement)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Improvement (perc)") + xlab("")

p  <- grid.arrange(plot1, plot2, ncol=2)
ggsave("figures/improvement-efficiency-recore-optvsnone.pdf", p, width = 7, height = 4)


########


plot1 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+OPTBBC" & cat.alg2 == "RecoreSTDistance+BBC"), aes(x = paste(cat.alg1), y = VD.estimate)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("effect size") + xlab("")

plot2 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+OPTBBC" & cat.alg2 == "RecoreSTDistance+BBC"), aes(x = paste(cat.alg1), y = improvement)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Improvement (perc)") + xlab("")

p  <- grid.arrange(plot1, plot2, ncol=2)
ggsave("figures/improvement-efficiency-recore-optvsbbc.pdf", p, width = 7, height = 4)
