# authors: Pouria Derakhshanfar
source('effect-size.r')

library(ggplot2)
library(tidyverse)


reproductionRateEffectSize <- getOddsRatioReproduction("WeightedSum") 

reproductionRateEffectSize$cat.alg1 <- ifelse(reproductionRateEffectSize$cat.alg1 == "WeightedSum", "WeightedSum",
                                              ifelse(reproductionRateEffectSize$cat.alg1 == "WeightedSum-bb", "WeightedSum+BBC","WeightedSum+OPTBBC"))


reproductionRateEffectSize$cat.alg2 <- ifelse(reproductionRateEffectSize$cat.alg2 == "WeightedSum", "WeightedSum",
                                              ifelse(reproductionRateEffectSize$cat.alg2 == "WeightedSum-bb", "WeightedSum+BBC","WeightedSum+OPTBBC"))

print(reproductionRateEffectSize)


effectiveness <- data.frame('Winner'=NA, 'WeightedSum'=NA, 'WeightedSum+BBC'=NA, 'WeightedSum+OPTBBC'=NA)


effectiveness <- effectiveness[0,]

configurations <- c("WeightedSum","WeightedSum+BBC","WeightedSum+OPTBBC")
for (ca1 in configurations){
  row = c(ca1)
  for(ca2 in configurations){
    tempdf <- reproductionRateEffectSize %>%
      filter(cat.alg1 == ca1 & cat.alg2 == ca2 & oddsratio > 1)
    row <- c(row,nrow(tempdf))
  }
  effectiveness[nrow(effectiveness) + 1,] = row
}

# 
# p <- ggplot(data=effectiveness, aes(x=Winner, y=count)) +
#   geom_bar(stat="identity") +
#   geom_text(aes(label=count), vjust=-1.0, color="black",
#             position = position_dodge(0.9), size=3.5)+
#   theme(text = element_text(size=13),axis.text.x = element_text(size=11)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   expand_limits( y=c(0, 6))+
#   scale_fill_manual(values=c('black','lightgray')) +
#   ylab("# of crashes") + xlab("Winner configuration")
# ggsave("figures/significant-reproduction-ws.pdf", width = 4, height = 2)


resultsForReproductionCount <- getReproductionRate("WeightedSum") %>%
  mutate(reproduction_perc=(count/TOTAL_RUNS))
resultsForReproductionCount$cat <- ifelse(resultsForReproductionCount$cat == "WeightedSum","WeightedSum",ifelse(resultsForReproductionCount$cat == "WeightedSum-bb", "WeightedSum+BBC", "WeightedSum+BBC-OPT"))

ReproductionratioSummary <- resultsForReproductionCount %>%
  group_by(cat) %>%
  summarise(avg = mean(count), med = median(count),avg_perc = mean(reproduction_perc), med_perc = median(reproduction_perc), InterProcedural_quantile = IQR(reproduction_perc), count = n())


onlyByBBC <- resultsForReproductionCount %>%
  inner_join(resultsForReproductionCount,by=c("application","case","exception_name","highest"), suffix = c('.alg1', '.alg2')) %>%
  filter((cat.alg1 == "WeightedSum" | cat.alg1 == "WeightedSum+BBC") & cat.alg2 == "WeightedSum+BBC-OPT" & count.alg1 == 0 & count.alg2 > 0)

print(onlyByBBC)

print(ReproductionratioSummary)

p <- ggplot(resultsForReproductionCount, aes(x = cat, y = reproduction_perc)) +
  theme(text = element_text(size=15),axis.text.x = element_text(size=11)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Reproduction Ratio (perc)") + xlab("Configurations")
ggsave("figures/reproduction-overall-ws.pdf", width = 4.5, height = 3)
