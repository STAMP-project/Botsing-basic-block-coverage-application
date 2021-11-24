# authors: Pouria Derakhshanfar
source('effect-size.r')

library(ggplot2)
library(tidyverse)
library(gridExtra)

VD_MAGNITUDE_LEVELS = c("large","medium","small")
CONSIDERABLE_VD_MAGNITUDES = c("large","medium","small")

# For integ
timeEffectSize <- getTimeEffectSizes("IntegrationSingleObjective")


timeEffectSize$cat.alg1 <- ifelse(timeEffectSize$cat.alg1 == "IntegrationSingleObjective", "RecoreSTDistance",
                                              ifelse(timeEffectSize$cat.alg1 == "IntegrationSingleObjective-bb", "RecoreSTDistance+old-BBC","RecoreSTDistance+BBC"))


timeEffectSize$cat.alg2 <- ifelse(timeEffectSize$cat.alg2 == "IntegrationSingleObjective", "RecoreSTDistance",
                                              ifelse(timeEffectSize$cat.alg2 == "IntegrationSingleObjective-bb", "RecoreSTDistance+old-BBC","RecoreSTDistance+BBC"))



efficiency_abstract <- data.frame('Winner'=NA, 'count' = NA, 'VD_magnitude' =NA)
efficiency_abstract <- efficiency_abstract[0,]


configurations <- c("RecoreSTDistance","RecoreSTDistance+BBC")


for (ca1 in configurations){
  for(ca2 in configurations){
    if (ca1 == ca2){
      next
    }
    for (mag in CONSIDERABLE_VD_MAGNITUDES){
      row = c(ca1)
      tempdf <- timeEffectSize %>%
        filter(cat.alg1 == ca1 & cat.alg2 == ca2 & VD.magnitude == mag & VD.estimate.category=="< 0.5")
      row <- c(row,nrow(tempdf),mag)
      efficiency_abstract[nrow(efficiency_abstract) + 1,] = row
    }
  }
  
}

efficiency_abstract$count <- as.numeric(efficiency_abstract$count)
p <- ggplot(data=efficiency_abstract, aes(x=Winner, y=count, fill=VD_magnitude)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=count), vjust=-1.0, color="black",
            position = position_dodge(0.9), size=5.5)+
  theme(text = element_text(size=14),axis.text.x = element_text(size=11)) +
  scale_y_continuous(limits=c(0, 59), expand = c(0, 0), breaks = NULL) +
  scale_fill_grey(start = 0, end = .9) +
  ylab("# of crashes") + xlab("Configurations") +
  theme(legend.position = "top", legend.text=element_text(size=14), axis.text.x=element_text(size=14))
ggsave("figures/significant-efficiency-recore.pdf", width = 5.5, height = 4)


##### Appendix #####


efficiency_abstract <- data.frame('Winner'=NA, 'count' = NA, 'VD_magnitude' =NA)
efficiency_abstract <- efficiency_abstract[0,]


configurations <- c("RecoreSTDistance","RecoreSTDistance+old-BBC")


for (ca1 in configurations){
  for(ca2 in configurations){
    if (ca1 == ca2){
      next
    }
    for (mag in CONSIDERABLE_VD_MAGNITUDES){
      row = c(ca1)
      tempdf <- timeEffectSize %>%
        filter(cat.alg1 == ca1 & cat.alg2 == ca2 & VD.magnitude == mag & VD.estimate.category=="< 0.5")
      row <- c(row,nrow(tempdf),mag)
      efficiency_abstract[nrow(efficiency_abstract) + 1,] = row
    }
  }
  
}

efficiency_abstract$count <- as.numeric(efficiency_abstract$count)
p <- ggplot(data=efficiency_abstract, aes(x=Winner, y=count, fill=VD_magnitude)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=count), vjust=-1.0, color="black",
            position = position_dodge(0.9), size=5.5)+
  theme(text = element_text(size=14),axis.text.x = element_text(size=11)) +
  scale_y_continuous(limits=c(0, 59), expand = c(0, 0), breaks = NULL) +
  scale_fill_grey(start = 0, end = .9) +
  ylab("# of crashes") + xlab("Configurations") +
  theme(legend.position = "top", legend.text=element_text(size=14), axis.text.x=element_text(size=14))
ggsave("figures/significant-efficiency-recore-oldvsnone.pdf", width = 5.5, height = 4)





efficiency_abstract <- data.frame('Winner'=NA, 'count' = NA, 'VD_magnitude' =NA)
efficiency_abstract <- efficiency_abstract[0,]


configurations <- c("RecoreSTDistance+old-BBC","RecoreSTDistance+BBC")


for (ca1 in configurations){
  for(ca2 in configurations){
    if (ca1 == ca2){
      next
    }
    for (mag in CONSIDERABLE_VD_MAGNITUDES){
      row = c(ca1)
      tempdf <- timeEffectSize %>%
        filter(cat.alg1 == ca1 & cat.alg2 == ca2 & VD.magnitude == mag & VD.estimate.category=="< 0.5")
      row <- c(row,nrow(tempdf),mag)
      efficiency_abstract[nrow(efficiency_abstract) + 1,] = row
    }
  }
  
}

efficiency_abstract$count <- as.numeric(efficiency_abstract$count)
p <- ggplot(data=efficiency_abstract, aes(x=Winner, y=count, fill=VD_magnitude)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=count), vjust=-1.0, color="black",
            position = position_dodge(0.9), size=5.5)+
  theme(text = element_text(size=14),axis.text.x = element_text(size=11)) +
  scale_y_continuous(limits=c(0, 59), expand = c(0, 0), breaks = NULL) +
  scale_fill_grey(start = 0, end = .9) +
  ylab("# of crashes") + xlab("Configurations") +
  theme(legend.position = "top", legend.text=element_text(size=14), axis.text.x=element_text(size=14))
ggsave("figures/significant-efficiency-recore-oldvsbbc.pdf", width = 6.5, height = 4)

#################### 




mutateTimeEffectSize <- timeEffectSize %>%
  # filter(cat.alg1 == "RecoreSTDistance+BBC" & cat.alg2 == "RecoreSTDistance") %>%
  mutate(improvement =  ifelse(((avg.alg2 - avg.alg1) / avg.alg2) > 0, 
                               ((avg.alg2 - avg.alg1) / avg.alg2),
                               -((avg.alg1 - avg.alg2) / avg.alg1))) 




improvementSummary <- mutateTimeEffectSize %>%
  filter(cat.alg1 == "RecoreSTDistance+BBC" & cat.alg2 == "RecoreSTDistance") %>%
  group_by(cat.alg1) %>%
  summarise(avg_improvement = mean(improvement), avg_es = mean(VD.estimate),
            min_improvement = min(improvement), min_es = min(VD.estimate),
            max_improvement = max(improvement), max_es = max(VD.estimate))


plot1 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+old-BBC" & cat.alg2 == "RecoreSTDistance"), aes(x = paste(cat.alg1), y = VD.estimate)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("effect size") + xlab("")

plot2 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+old-BBC" & cat.alg2 == "RecoreSTDistance"), aes(x = paste(cat.alg1), y = improvement)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Improvement (perc)") + xlab("")

p  <- grid.arrange(plot1, plot2, ncol=2)
ggsave("figures/improvement-efficiency-recore-oldBBCvsnone.pdf", p, width = 7, height = 4)


###########

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
ggsave("figures/improvement-efficiency-recore.pdf", p, width = 7, height = 4)


########


plot1 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+BBC" & cat.alg2 == "RecoreSTDistance+old-BBC"), aes(x = paste(cat.alg1), y = VD.estimate)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("effect size") + xlab("")

plot2 <- ggplot(mutateTimeEffectSize %>% 
                  filter(cat.alg1== "RecoreSTDistance+BBC" & cat.alg2 == "RecoreSTDistance+old-BBC"), aes(x = paste(cat.alg1), y = improvement)) +
  theme(text = element_text(size=18),axis.text.x = element_text(size=15)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Improvement (perc)") + xlab("")

p  <- grid.arrange(plot1, plot2, ncol=2)
ggsave("figures/improvement-efficiency-recore-oldBBCvsbbc.pdf", p, width = 7, height = 4)
