# authors: Pouria Derakhshanfar
source('effect-size.R')

library(ggplot2)
library(tidyverse)


reproductionRateEffectSize <- getOddsRatioReproduction("IntegrationSingleObjective")


reproductionRateEffectSize$cat.alg1 <- ifelse(reproductionRateEffectSize$cat.alg1 == "IntegrationSingleObjective", "RecoreSTDistance",
                                              ifelse(reproductionRateEffectSize$cat.alg1 == "IntegrationSingleObjective-bb", "RecoreSTDistance+BBC","RecoreSTDistance+OP-BBC"))


reproductionRateEffectSize$cat.alg2 <- ifelse(reproductionRateEffectSize$cat.alg2 == "IntegrationSingleObjective", "RecoreSTDistance",
                                              ifelse(reproductionRateEffectSize$cat.alg2 == "IntegrationSingleObjective-bb", "RecoreSTDistance+BBC","RecoreSTDistance+OP-BBC"))

print(reproductionRateEffectSize)


effectiveness <- data.frame('Winner'=NA, 'count'=NA)
effectiveness <- effectiveness[0,]

configurations <- c("RecoreSTDistance","RecoreSTDistance+OP-BBC")
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


effectiveness$count <- as.numeric(effectiveness$count)
effectiveness$Winner <- ifelse(effectiveness$Winner == "RecoreSTDistance+OP-BBC", "RecoreSTDistance+BBC", effectiveness$Winner)
p <- ggplot(data=effectiveness, aes(x=Winner, y=count)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-1.0, color="black",
            position = position_dodge(0.9), size=3.5)+
  theme(text = element_text(size=13),axis.text.x = element_text(size=11)) +
  scale_y_continuous(limits=c(0, 12), expand = c(0, 0), breaks = NULL) +
  scale_fill_manual(values=c('black','lightgray')) +
  ylab("# of crashes") + xlab("Winner configuration")
ggsave("figures/significant-reproduction-recore.pdf", width = 5, height = 3)



##### Appendix #####



effectiveness <- data.frame('Winner'=NA, 'RecoreSTDistance'=NA, 'RecoreSTDistance+old-BBC'=NA,'RecoreSTDistance+BBC'=NA )
effectiveness <- effectiveness[0,]

configurations <- c("RecoreSTDistance","RecoreSTDistance+BBC","RecoreSTDistance+OP-BBC")
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

effectiveness$Winner <- ifelse(effectiveness$Winner == "RecoreSTDistance+BBC", "RecoreSTDistance+old-BBC", effectiveness$Winner)
effectiveness$Winner <- ifelse(effectiveness$Winner == "RecoreSTDistance+OP-BBC", "RecoreSTDistance+BBC", effectiveness$Winner)


# RecoreSTDistance vs. RecoreSTDistance+old-BBC

OR_table_df <-  reproductionRateEffectSize %>%
  filter((cat.alg1 == "RecoreSTDistance" & cat.alg2 %in% c("RecoreSTDistance+BBC")))

OR_table_df$cat.alg2 <- ifelse(OR_table_df$cat.alg2 == "RecoreSTDistance+BBC","RecoreSTDistance+old-BBC",OR_table_df$cat.alg2)




outputFile <- "tables/appendix-rq2.1-OR-recore-nonevsoldBBC.tex"
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

cat ("& RecoreSTDistance & RecoreSTDistance+old-BBC & &")
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

# RecoreSTDistance+old-BBC vs. RecoreSTDistance+BBC

OR_table_df <-  reproductionRateEffectSize %>%
  filter((cat.alg1 == "RecoreSTDistance+BBC" & cat.alg2 %in% c("RecoreSTDistance+OP-BBC")))

OR_table_df$cat.alg1 <- "RecoreSTDistance+old-BBC"
OR_table_df$cat.alg2 <- "RecoreSTDistance+BBC"




outputFile <- "tables/appendix-rq2.1-OR-recore-oldBBCvsnewBBC.tex"
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

cat ("& RecoreSTDistance+old-BBC & RecoreSTDistance+BBC & &")
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


# outputFile <- "tables/appendix-rq2.1-OR-recore.tex"
# 
# 
# p <- ggplot(data=effectiveness, aes(x=Winner, y=count)) +
#   geom_bar(stat="identity") +
#   geom_text(aes(label=count), vjust=-1.0, color="black",
#             position = position_dodge(0.9), size=3.5)+
#   theme(text = element_text(size=13),axis.text.x = element_text(size=11)) +
#   scale_y_continuous(limits=c(0, 12), expand = c(0, 0), breaks = NULL) +
#   scale_fill_manual(values=c('black','lightgray')) +
#   ylab("# of crashes") + xlab("Winner configuration")
# ggsave("figures/appendix-significant-reproduction-recore.pdf", width = 5, height = 3)


##############


OR_table_df <-  reproductionRateEffectSize %>%
  filter((cat.alg1 == "RecoreSTDistance" & cat.alg2 %in% c("RecoreSTDistance+OP-BBC")))


OR_table_df$cat.alg2 <- "RecoreSTDistance+BBC"
  



# OR_table_df <-  reproductionRateEffectSize %>%
#   filter((cat.alg1 == "RecoreSTDistance" & cat.alg2 %in% c("RecoreSTDistance+BBC","RecoreSTDistance+OP-BBC") ) | 
#            (cat.alg1 == "RecoreSTDistance+BBC" & cat.alg2 == "RecoreSTDistance+OP-BBC")) %>%
#   mutate(comparison = paste0(cat.alg1," vs. ", cat.alg2))

# 
# comparison_cats <- unique(OR_table_df$comparison)
# index = c(1,3,2)
# comparison_cats <- comparison_cats[order(index)]

outputFile <- "tables/rq2.1-OR-recore.tex"
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

cat ("& RecoreSTDistance & RecoreSTDistance+BBC & &")
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


# cat("\\begin{tabular}{ l | c c c}\n")
# cat("\\hline", "\n")
# cat("\\textbf{Configs comparison}", "&", 
#     "\\textbf{Crash}", "&",
#     "\\textbf{OR}", "&",
#     "\\textbf{p-value}", "&")
# cat("\\hline", "\n")
# 
# for (comp in comparison_cats){
#   
#   temp_df <- OR_table_df %>%
#     filter(comparison == comp)
#   
#   print_comparison_counter = 1
#   
#   
#   for (row in 1:nrow(temp_df)) {
#     
#     if(print_comparison_counter == 1){
#       string <- temp_df[row,"cat.alg1"]
#       cat(string, "&")
#       print_comparison_counter = 2
#     } else if(print_comparison_counter == 2){
#       cat("vs. ", "&")
#       print_comparison_counter = 3
#     }else if(print_comparison_counter == 3){
#       string <- temp_df[row,"cat.alg2"]
#       cat(string, "&")
#       print_comparison_counter = 4
#     }else{
#       cat(" &")
#     }
#     crash <- temp_df[row, "case"]
#     oddsratio <- temp_df[row, "oddsratio"]
#     pv <- temp_df[row, "pValue"]
#     
#     
#     oddsratio <- formatC(oddsratio, digits=1, format="f", big.mark = ',')
#     pv <- formatC(pv,  format="e", big.mark = ',')
#     cat(crash,"&",
#         oddsratio,"&",
#         pv)
#     cat(" \\\\", "\n")
#     
#   }
#   cat("\\hline", "\n")
# }
# cat("\\end{tabular}")
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
# ggsave("figures/significant-reproduction-recore.pdf", width = 4, height = 2)


resultsForReproductionCount <- getReproductionRate("IntegrationSingleObjective") %>%
  mutate(reproduction_perc=(count/TOTAL_RUNS))
resultsForReproductionCount$cat <- ifelse(resultsForReproductionCount$cat == "IntegrationSingleObjective","RecoreSTDistance",
                                          ifelse(resultsForReproductionCount$cat == "IntegrationSingleObjective-bb", "RecoreSTDistance+old-BBC","RecoreSTDistance+BBC"))

ReproductionratioSummary <- resultsForReproductionCount %>%
  group_by(cat) %>%
  summarise(avg = mean(count), med = median(count),avg_perc = mean(reproduction_perc), med_perc = median(reproduction_perc), InterProcedural_quantile = IQR(reproduction_perc), count = n())


onlyByBBC <- resultsForReproductionCount %>%
  inner_join(resultsForReproductionCount,by=c("application","case","exception_name","highest"), suffix = c('.alg1', '.alg2')) %>%
  filter((cat.alg1 == "RecoreSTDistance" | cat.alg1 == "RecoreSTDistance+old-BBC") & cat.alg2 == "RecoreSTDistance+BBC" & count.alg1 == 0 & count.alg2 > 0)

print(onlyByBBC)

print(ReproductionratioSummary)

##### Appendix #####

resultsForReproductionCount$cat <- factor(resultsForReproductionCount$cat,levels = c("RecoreSTDistance", "RecoreSTDistance+old-BBC", "RecoreSTDistance+BBC"))

p <- ggplot(resultsForReproductionCount, aes(x = cat, y = reproduction_perc)) +
  theme(text = element_text(size=15),axis.text.x = element_text(size=11)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Reproduction Ratio (perc)") + xlab("Configurations")
ggsave("figures/appendix-reproduction-overall-recore.pdf", width = 7, height = 3)


###############

resultsForReproductionCount <- resultsForReproductionCount %>%
  filter(cat != "RecoreSTDistance+old-BBC")


p <- ggplot(resultsForReproductionCount, aes(x = cat, y = reproduction_perc)) +
  theme(text = element_text(size=15),axis.text.x = element_text(size=11)) +
  geom_boxplot() +
  stat_summary(fun.y=mean,pch=22, size = 3, geom='point') +
  ylab("Reproduction Ratio (perc)") + xlab("Configurations")
ggsave("figures/reproduction-overall-recore.pdf", width = 4.5, height = 3)
