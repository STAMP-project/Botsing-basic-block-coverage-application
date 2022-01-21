library(questionr)
library(effsize)


source('dataclean.r')

SIGNIFICANCE_LEVEL = 0.01




getOddsRatioReproduction <- function(fitnessFunction){
  computeReproductionOddsRatio <- Vectorize(function(count1, count2){
    m <- matrix(c(count1, TOTAL_RUNS - count1,
                  count2, TOTAL_RUNS - count2), ncol = 2, byrow = TRUE)
    dimnames(m) <- list('Algorithm' = c('alg1', 'alg2'),
                        'Reproduced' = c('yes', 'no'))
    or <- odds.ratio(m, level = 1.0 - SIGNIFICANCE_LEVEL)
    if(or$p <= SIGNIFICANCE_LEVEL){
      return(or$OR)
    } else {
      return(NA)
    }
  })
  
  getPValue <- Vectorize(function(count1, count2){
    m <- matrix(c(count1, TOTAL_RUNS - count1,
                  count2, TOTAL_RUNS - count2), ncol = 2, byrow = TRUE)
    dimnames(m) <- list('Algorithm' = c('alg1', 'alg2'),
                        'Reproduced' = c('yes', 'no'))
    or <- odds.ratio(m, level = 1.0 - SIGNIFICANCE_LEVEL)
    return(or$p)
  })
  
  
  df <- getReproductionRate(fitnessFunction)
  df2 <- df %>%
    inner_join(df, by=c('case'), suffix = c('.alg1', '.alg2')) %>%
    filter(cat.alg1 != cat.alg2) %>%
    mutate(oddsratio = computeReproductionOddsRatio(count.alg1, count.alg2)) %>%
    mutate(pValue = getPValue(count.alg1, count.alg2)) %>%
    select(case, cat.alg1, highest.alg1, cat.alg2, count.alg1, count.alg2, oddsratio, pValue)%>%
    filter(pValue < SIGNIFICANCE_LEVEL) %>%
    rename(highest_reproduced_frame = highest.alg1)

  return(df2)
}


getOddsRatioReproductionAll <- function(fitnessFunction){
  computeReproductionOddsRatio <- Vectorize(function(count1, count2){
    m <- matrix(c(count1, TOTAL_RUNS - count1,
                  count2, TOTAL_RUNS - count2), ncol = 2, byrow = TRUE)
    dimnames(m) <- list('Algorithm' = c('alg1', 'alg2'),
                        'Reproduced' = c('yes', 'no'))
    or <- odds.ratio(m)
    return(or$OR)
    # if(or$p <= SIGNIFICANCE_LEVEL){
    #   return(or$OR)
    # } else {
    #   return(NA)
    # }
  })
  
  getPValue <- Vectorize(function(count1, count2){
    m <- matrix(c(count1, TOTAL_RUNS - count1,
                  count2, TOTAL_RUNS - count2), ncol = 2, byrow = TRUE)
    dimnames(m) <- list('Algorithm' = c('alg1', 'alg2'),
                        'Reproduced' = c('yes', 'no'))
    or <- odds.ratio(m)
    return(or$p)
  })
  
  
  df <- getReproductionRate(fitnessFunction)
  df2 <- df %>%
    inner_join(df, by=c('case'), suffix = c('.alg1', '.alg2')) %>%
    filter(cat.alg1 != cat.alg2) %>%
    mutate(oddsratio = computeReproductionOddsRatio(count.alg1, count.alg2)) %>%
  mutate(pValue = getPValue(count.alg1, count.alg2)) %>%
  select(case, cat.alg1, highest.alg1, cat.alg2, count.alg1, count.alg2, oddsratio, pValue)%>%
  rename(highest_reproduced_frame = highest.alg1)
     
  return(df2)
}

getLineCoverageEffectSizesForWS <- function(){
  lineCoverageRate <- getLineCoverageRateForWS()
  df <- lineCoverageRate %>%
    inner_join(lineCoverageRate, by=c('case','frame_level'), suffix = c('.alg1', '.alg2')) %>%
    filter(cat.alg1 != cat.alg2) %>%
    mutate(oddsratio = computeReproductionOddsRatio(line_coverage_count.alg1, line_coverage_count.alg2)) %>%
    mutate(pValue = getPValue(line_coverage_count.alg1, line_coverage_count.alg2)) %>%
    filter(pValue < SIGNIFICANCE_LEVEL) %>%
    select(case,frame_level, cat.alg1, cat.alg2, line_coverage_count.alg1, line_coverage_count.alg2, oddsratio, pValue)
}


getLineCoverageEffectSizesForInteg <- function(){
  df <- getLineCoverageRateForInteg()
  
  
  coverageES<- df %>%
    inner_join(df, by=c('case', 'frame_level', 'execution_idx'), suffix = c('.alg1', '.alg2')) %>%
    filter(cat.alg1 != cat.alg2) %>%
    group_by(case,frame_level, cat.alg1, cat.alg2) %>%
    summarise(VD.magnitude = VD.A(line_covered.alg1, line_covered.alg2)$magnitude,
              VD.estimate = VD.A(line_covered.alg1, line_covered.alg2)$estimate,
              wilcox.test.pvalue = wilcox.test(line_covered.alg1, line_covered.alg2)$p.value) %>%
    filter(wilcox.test.pvalue <= SIGNIFICANCE_LEVEL) %>%
    mutate(VD.estimate.category = case_when(
      VD.estimate < 0.5 ~ '< 0.5',
      VD.estimate > 0.5 ~ '> 0.5',
      TRUE ~ '= 0.5'
    ))
  
  df2 <- getReproductionRate() %>%
    select(case,highest)
  df2 <- unique(df2)
  
  coverageES <- coverageES %>%
    inner_join(df2, by=c("case")) %>%
    filter(frame_level>= highest)
  
  return(coverageES)
  
}


getTimeEffectSizes <- function(fitnessFunction){
  rawResult <- getAllResults()
  reproduced <- getReproduceStatus(fitnessFunction) %>%
    rename(frame_level = highest)
  
  df <- rawResult %>%
    inner_join(reproduced, by = c("application", "case", "frame_level", "cat")) %>%
    mutate(time_spent = ifelse(fitness_function_value == 0, as.integer(time_spent), 300))
  
  timeAvg  <- df %>%
    inner_join(df, by=c('case', 'frame_level', 'execution_idx'), suffix = c('.alg1', '.alg2')) %>%
    filter(cat.alg1 != cat.alg2) %>%
    group_by(case,frame_level, cat.alg1, cat.alg2) %>%
    summarise(time1=mean(time_spent.alg1), time2=mean(time_spent.alg2)) %>%
    mutate(diff=time1-time2)
  
  timeES <- df %>%
    inner_join(df, by=c('case', 'frame_level', 'execution_idx'), suffix = c('.alg1', '.alg2')) %>%
    filter(cat.alg1 != cat.alg2) %>%
    group_by(case,frame_level, cat.alg1, cat.alg2) %>%
    summarise(VD.magnitude = VD.A(time_spent.alg1, time_spent.alg2)$magnitude,
              VD.estimate = VD.A(time_spent.alg1, time_spent.alg2)$estimate,
              wilcox.test.pvalue = wilcox.test(time_spent.alg1, time_spent.alg2)$p.value,
              avg.alg1 = mean(time_spent.alg1),
              avg.alg2 = mean(time_spent.alg2)) %>%
    filter(wilcox.test.pvalue <= SIGNIFICANCE_LEVEL) %>%
    mutate(VD.estimate.category = case_when(
      VD.estimate < 0.5 ~ '< 0.5',
      VD.estimate > 0.5 ~ '> 0.5',
      TRUE ~ '= 0.5'
    ))
  
  return(timeES)
}
