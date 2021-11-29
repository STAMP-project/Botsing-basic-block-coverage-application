# authors: Pouria Derakhshanfar
source('effect-size.r')

library(ggplot2)
library(tidyverse)


reproductionRateEffectSize <- getOddsRatioReproduction("WeightedSum") 

reproductionRateEffectSize$cat.alg1 <- ifelse(reproductionRateEffectSize$cat.alg1 == "WeightedSum", "WeightedSum",
                                              ifelse(reproductionRateEffectSize$cat.alg1 == "WeightedSum-bb", "WeightedSum+BBC","WeightedSum+OP-BBC"))


reproductionRateEffectSize$cat.alg2 <- ifelse(reproductionRateEffectSize$cat.alg2 == "WeightedSum", "WeightedSum",
                                              ifelse(reproductionRateEffectSize$cat.alg2 == "WeightedSum-bb", "WeightedSum+BBC","WeightedSum+OP-BBC"))

print(reproductionRateEffectSize)


effectiveness <- data.frame('Winner'=NA, 'count'=NA)


effectiveness <- effectiveness[0,]



configurations <- c("WeightedSum","WeightedSum+OP-BBC")
for (ca1 in configurations){
  row = c(ca1)
  for(ca2 in configurations){
    if (ca1 == ca2){
      next
    }
    tempdf <- reproductionRateEffectSize %>%
      filter(cat.alg1 == ca1 & cat.alg2 == ca2 & oddsratio > 1)
    row <- c(row,nrow(tempdf))
  }
  effectiveness[nrow(effectiveness) + 1,] = row
}

effectiveness$Winner <- ifelse(effectiveness$Winner == "WeightedSum+OP-BBC", "WeightedSum+BBC", effectiveness$Winner)


effectiveness$count <- as.numeric(effectiveness$count)
p <- ggplot(data=effectiveness, aes(x=Winner, y=count)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-1.0, color="black",
            position = position_dodge(0.9), size=3.5)+
  theme(text = element_text(size=13),axis.text.x = element_text(size=11)) +
  scale_y_continuous(limits=c(0, 5), expand = c(0, 0), breaks = NULL) +
  scale_fill_manual(values=c('black','lightgray')) +
  ylab("# of crashes") + xlab("Winner configuration")
ggsave("figures/significant-reproduction-ws.pdf", width = 5, height = 3)





##### Appendix #####



effectiveness <- data.frame('Winner'=NA, 'WeightedSum'=NA, 'WeightedSum+old-BBC'=NA,'WeightedSum+BBC'=NA )
effectiveness <- effectiveness[0,]

configurations <- c("WeightedSum","WeightedSum+BBC","WeightedSum+OP-BBC")
for (ca1 in configurations){
  row = c(ca1)
  for(ca2 in configurations){
    if (ca1 == ca2){
      row <- c(row,0)
    }
    
    tempdf <- reproductionRateEffectSize %>%
      filter(cat.alg1 == ca1 & cat.alg2 == ca2 & oddsratio > 1)
    row <- c(row,nrow(tempdf))
    
  }
  effectiveness[nrow(effectiveness) + 1,] = row
  
}

effectiveness$Winner <- ifelse(effectiveness$Winner == "WeightedSum+BBC", "WeightedSum+old-BBC", effectiveness$Winner)
effectiveness$Winner <- ifelse(effectiveness$Winner == "WeightedSum+OP-BBC", "WeightedSum+BBC", effectiveness$Winner)


# WeightedSum vs. WeightedSum+old-BBC

OR_table_df <-  reproductionRateEffectSize %>%
  filter((cat.alg1 == "WeightedSum" & cat.alg2 %in% c("WeightedSum+BBC")))

OR_table_df$cat.alg2 <- ifelse(OR_table_df$cat.alg2 == "WeightedSum+BBC","WeightedSum+old-BBC",OR_table_df$cat.alg2)




outputFile <- "tables/appendix-rq2.1-OR-ws-nonevsoldBBC.tex"
unlink(outputFile)
# Redirect cat outputs to file
sink(outputFile, append = TRUE, split = TRUE)


cat("\\begin{tabular}{ l | c c | c c}\n")
cat("\\hline", "\n")
cat("\\textbf{Crash}", "&",
    "\\multicolumn{2}{c|}{\\textbf{Reproduction ratio}}", "&",
    "\\textbf{OR}", "&",
    "\\textbf{p-value}")
cat(" \\\\", "\n")

cat ("& WeightedSum & WeightedSum+old-BBC & &")
cat(" \\\\", "\n")
cat("\\hline", "\n")

for (row in 1:nrow(OR_table_df)) {
  crash = OR_table_df[row,"case"]
  count_alg = OR_table_df[row,"count.alg1"]
  count_bbc = OR_table_df[row,"count.alg2"]
  oddsratio = formatC(OR_table_df[row, "oddsratio"], digits=1, format="f", big.mark = ',')
  p = formatC(OR_table_df[row, "pValue"],  format="e", big.mark = ',')
  
  cat(crash,"&",
      count_alg,"&",
      count_bbc,"&",
      oddsratio,"&",
      p)
  cat(" \\\\", "\n")
}
cat("\\end{tabular}")

# Restore cat outputs to console
sink()

# WeightedSum+old-BBC vs. WeightedSum+BBC

OR_table_df <-  reproductionRateEffectSize %>%
  filter((cat.alg1 == "WeightedSum+BBC" & cat.alg2 %in% c("WeightedSum+OP-BBC")))

OR_table_df$cat.alg1 <- "WeightedSum+old-BBC"
OR_table_df$cat.alg2 <- "WeightedSum+BBC"




outputFile <- "tables/appendix-rq2.1-OR-ws-oldBBCvsnewBBC.tex"
unlink(outputFile)
# Redirect cat outputs to file
sink(outputFile, append = TRUE, split = TRUE)


cat("\\begin{tabular}{ l | c c | c c}\n")
cat("\\hline", "\n")
cat("\\textbf{Crash}", "&",
    "\\multicolumn{2}{c|}{\\textbf{Reproduction ratio}}", "&",
    "\\textbf{OR}", "&",
    "\\textbf{p-value}")
cat(" \\\\", "\n")

cat ("& WeightedSum+old-BBC & WeightedSum+BBC & &")
cat(" \\\\", "\n")
cat("\\hline", "\n")

for (row in 1:nrow(OR_table_df)) {
  crash = OR_table_df[row,"case"]
  count_alg = OR_table_df[row,"count.alg1"]
  count_bbc = OR_table_df[row,"count.alg2"]
  oddsratio = formatC(OR_table_df[row, "oddsratio"], digits=1, format="f", big.mark = ',')
  p = formatC(OR_table_df[row, "pValue"],  format="e", big.mark = ',')
  
  cat(crash,"&",
      count_alg,"&",
      count_bbc,"&",
      oddsratio,"&",
      p)
  cat(" \\\\", "\n")
}
cat("\\end{tabular}")

# Restore cat outputs to console
sink()



##############





OR_table_df <-  reproductionRateEffectSize %>%
  filter((cat.alg1 == "WeightedSum" & cat.alg2 %in% c("WeightedSum+OP-BBC"))) 

outputFile <- "tables/rq2.1-OR-ws.tex"
unlink(outputFile)
# Redirect cat outputs to file
sink(outputFile, append = TRUE, split = TRUE)


cat("\\begin{tabular}{ l | c c | c c}\n")
cat("\\hline", "\n")
cat("\\textbf{Crash}", "&",
    "\\multicolumn{2}{c|}{\\textbf{Reproduction ratio}}", "&",
    "\\textbf{OR}", "&",
    "\\textbf{p-value}")
cat(" \\\\", "\n")

cat ("& WeightedSum & WeightedSum+BBC & &")
cat(" \\\\", "\n")
cat("\\hline", "\n")

for (row in 1:nrow(OR_table_df)) {
  crash = OR_table_df[row,"case"]
  count_alg = OR_table_df[row,"count.alg1"]
  count_bbc = OR_table_df[row,"count.alg2"]
  oddsratio = formatC(OR_table_df[row, "oddsratio"], digits=1, format="f", big.mark = ',')
  p = formatC(OR_table_df[row, "pValue"],  format="e", big.mark = ',')
  
  cat(crash,"&",
      count_alg,"&",
      count_bbc,"&",
      oddsratio,"&",
      p)
  cat(" \\\\", "\n")
}
cat("\\end{tabular}")

# Restore cat outputs to console
sink()



resultsForReproductionCount <- getReproductionRate("WeightedSum") %>%
  mutate(reproduction_perc=(count/TOTAL_RUNS))
resultsForReproductionCount$cat <- ifelse(resultsForReproductionCount$cat == "WeightedSum","WeightedSum",ifelse(resultsForReproductionCount$cat == "WeightedSum-bb", "WeightedSum+old-BBC", "WeightedSum+BBC"))

ReproductionratioSummary <- resultsForReproductionCount %>%
  group_by(cat) %>%
  summarise(avg = mean(count), med = median(count),avg_perc = mean(reproduction_perc), med_perc = median(reproduction_perc), InterProcedural_quantile = IQR(reproduction_perc), count = n(), sd = sd(reproduction_perc))


onlyByBBC <- resultsForReproductionCount %>%
  inner_join(resultsForReproductionCount,by=c("application","case","exception_name","highest"), suffix = c('.alg1', '.alg2')) %>%
  filter((cat.alg1 == "WeightedSum") & cat.alg2 == "WeightedSum+BBC" & count.alg1 == 0 & count.alg2 > 0)


notByBBC <- resultsForReproductionCount %>%
  inner_join(resultsForReproductionCount,by=c("application","case","exception_name","highest"), suffix = c('.alg1', '.alg2')) %>%
  filter((cat.alg1 == "WeightedSum") & cat.alg2 == "WeightedSum+BBC" & count.alg1 > 0 & count.alg2 == 0)


print(onlyByBBC)

print(ReproductionratioSummary)


##### Appendix #####

resultsForReproductionCount$cat <- factor(resultsForReproductionCount$cat,levels = c("WeightedSum", "WeightedSum+old-BBC", "WeightedSum+BBC"))

p <- ggplot(resultsForReproductionCount, aes(x = cat, y = reproduction_perc)) +
  theme(text = element_text(size=15),axis.text.x = element_text(size=11)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Reproduction Ratio (perc)") + xlab("Configurations")
ggsave("figures/appendix-reproduction-overall-ws.pdf", width = 7, height = 3)


###############


resultsForReproductionCount <- resultsForReproductionCount %>%
  filter(cat != "WeightedSum+old-BBC")

p <- ggplot(resultsForReproductionCount, aes(x = cat, y = reproduction_perc)) +
  theme(text = element_text(size=15),axis.text.x = element_text(size=11)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Reproduction Ratio (perc)") + xlab("Configurations")
ggsave("figures/reproduction-overall-ws.pdf", width = 4.5, height = 3)
