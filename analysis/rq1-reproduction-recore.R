# authors: Pouria Derakhshanfar
source('effect-size.R')

library(ggplot2)
library(tidyverse)


reproductionRateEffectSize <- getOddsRatioReproduction("IntegrationSingleObjective") %>%
  mutate(winner = ifelse(oddsratio > 1,"RecoreSTDistance+BBC","RecoreSTDistance"))

print(reproductionRateEffectSize)


effectiveness <- data.frame('Winner'=NA, 'count'=NA)
effectiveness <- effectiveness[0,]

for (ca in c("RecoreSTDistance","RecoreSTDistance+BBC")){
    tempdf <- reproductionRateEffectSize %>%
      filter(winner == ca)
    row =c(ca,nrow(tempdf))
    effectiveness[nrow(effectiveness) + 1,] = row
}


effectiveness$count <- as.numeric(as.vector(effectiveness$count))

p <- ggplot(data=effectiveness, aes(x=Winner, y=count)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-1.0, color="black",
            position = position_dodge(0.9), size=3.5)+
  theme(text = element_text(size=13),axis.text.x = element_text(size=11)) +
  scale_y_discrete(expand = c(0, 0)) +
  expand_limits( y=c(0, 6))+
  scale_fill_manual(values=c('black','lightgray')) +
  ylab("# of crashes") + xlab("Winner configuration")
ggsave("figures/significant-reproduction-recore.pdf", width = 4, height = 2)


resultsForReproductionCount <- getReproductionRate("IntegrationSingleObjective") %>%
  mutate(reproduction_perc=(count/TOTAL_RUNS))
resultsForReproductionCount$cat <- ifelse(resultsForReproductionCount$cat == "IntegrationSingleObjective","RecoreSTDistance","RecoreSTDistance+BBC")

ReproductionratioSummary <- resultsForReproductionCount %>%
  group_by(cat) %>%
  summarise(avg = mean(count), med = median(count),avg_perc = mean(reproduction_perc), med_perc = median(reproduction_perc), InterProcedural_quantile = IQR(reproduction_perc))


onlyByBBC <- resultsForReproductionCount %>%
  inner_join(resultsForReproductionCount,by=c("application","case","exception_name","highest"), suffix = c('.alg1', '.alg2')) %>%
  filter(cat.alg1 == "RecoreSTDistance" & cat.alg2 == "RecoreSTDistance+BBC" & count.alg1 == 0 & count.alg2 > 0)

print(onlyByBBC)

print(ReproductionratioSummary)

p <- ggplot(resultsForReproductionCount, aes(x = cat, y = reproduction_perc)) +
  theme(text = element_text(size=15),axis.text.x = element_text(size=11)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Reproduction Ratio (perc)") + xlab("Configurations")
ggsave("figures/reproduction-overall-recore.pdf", width = 4.5, height = 3)
