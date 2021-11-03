# Contains functions to produce a clean/easy to process dataframe from the input and 
# output csv files.
# authors: Pouria Derakhshanfar

library(dplyr)

TOTAL_RUNS = 30
STATUS_LEVELS = c("not reproduced", "reproduced")
CONFIGS <- c("WeightedSum","WeightedSum-bb","WeightedSum-bb-opt","IntegrationSingleObjective","IntegrationSingleObjective-bb")

getWS <- function(){
  csvFile='../crash-reproduction-ws/results/results.csv'
  df <- read.csv(csvFile, stringsAsFactors = FALSE)
  df$fitness_function_value[df$fitness_function_value<0] <- 6
  df$basic_block <- 0
  df$bbc_opt <- 0
  df <- df %>% filter(execution_idx <= TOTAL_RUNS)

  return(df)
}

getInteg <- function(){
  csvFile='../crash-reproduction-new-fitness/results/results.csv'
  df <- read.csv(csvFile, stringsAsFactors = FALSE)
  df <- df %>%
    filter(objectives == "IntegrationSingleObjective" & secondary_objective == "")
  df$fitness_function_value[df$fitness_function_value<0] <- 6
  df$basic_block <- 0
  df$bbc_opt <- 0
  df$secondary_objective <- NULL
  df <- df %>% filter(execution_idx <= TOTAL_RUNS)
  
  return(df)
}

getWSBB <- function(){
  csvFile='../crash-reproduction-new-fitness/results/results.csv'
  df <- read.csv(csvFile, stringsAsFactors = FALSE)
  df <- df %>%
    filter(objectives == "WeightedSum" & secondary_objective == "BasicBlockCoverage")
  df$fitness_function_value[df$fitness_function_value<0] <- 6
  df$basic_block <- 1
  df$bbc_opt <- 0
  df$secondary_objective <- NULL
  df <- df %>% filter(execution_idx <= TOTAL_RUNS)
  
  return(df)
}


getWSBBOPT <- function(){
  csvFile='../crash-reproduction-new-fitness/results/results.csv'
  df <- read.csv(csvFile, stringsAsFactors = FALSE)
  df <- df %>%
    filter(objectives == "WeightedSum" & secondary_objective == "BasicBlockCoverage-opt")
  df$fitness_function_value[df$fitness_function_value<0] <- 6
  df$basic_block <- 1
  df$bbc_opt <- 1
  df$secondary_objective <- NULL
  df <- df %>% filter(execution_idx <= TOTAL_RUNS)
  
  return(df)
}

getIntegBB <- function(){
  csvFile='../crash-reproduction-new-fitness/results/results.csv'
  df <- read.csv(csvFile, stringsAsFactors = FALSE)
  df <- df %>%
    filter(objectives == "IntegrationSingleObjective" & secondary_objective == "BasicBlockCoverage")
  df$fitness_function_value[df$fitness_function_value<0] <- 6
  df$basic_block <- 1
  df$bbc_opt <- 0
  df$secondary_objective <- NULL
  df <- df %>% filter(execution_idx <= TOTAL_RUNS)
  
  return(df)
}


getIntegBBOPT <- function(){
  csvFile='../crash-reproduction-new-fitness/results/results.csv'
  df <- read.csv(csvFile, stringsAsFactors = FALSE)
  df <- df %>%
    filter(objectives == "IntegrationSingleObjective" & secondary_objective == "BasicBlockCoverage-opt")
  df$fitness_function_value[df$fitness_function_value<0] <- 6
  df$basic_block <- 1
  df$bbc_opt <- 1
  df$secondary_objective <- NULL
  df <- df %>% filter(execution_idx <= TOTAL_RUNS)
  
  return(df)
}


getAllResults <- function(){
  df <- union( getWS(), getInteg())
  df <- union(df, getWSBB())
  df <- union(df, getIntegBB())
  df <- union(df,getWSBBOPT())
  df <- union(df,getIntegBBOPT())
  # Clean data
  df$time_spent[df$fitness_function_evolution ==""] <- 300
  df$number_of_fitness_evaluations[df$fitness_function_evolution==""] <- 250000
  df$fitness_function_value <- ifelse(df$fitness_function_evolution=="" & df$objectives == "IntegrationSingleObjective",df$frame_level+1,df$fitness_function_value)
  df <- df %>%
    mutate(cat=paste(objectives,ifelse(basic_block,"-bb",""),ifelse(bbc_opt,"-opt",""),sep = ""))
  
  df <- df %>%
    filter(execution_idx <= TOTAL_RUNS)
  return(df)
}


getResultsForTime <- function(){
  allResults <- getAllResults()
  
  
  df <- allResults %>%
    group_by(case,frame_level) %>%
    summarise(minFF = min(fitness_function_value)) %>%
    filter(minFF == 0)
  
  joined <- allResults %>%
    inner_join(df, by=c('case','frame_level'))
  
  
  joined$time_spent[joined$fitness_function_value>0] <- 300
  
  return(joined)
}


getResultsForFFEval <- function(){
  allResults <- getAllResults()
  df <- allResults %>%
    group_by(case,frame_level) %>%
    summarise(minFF = min(fitness_function_value)) %>%
    filter(minFF == 0)
  
  joined <- allResults %>%
    inner_join(df, by=c('case','frame_level'))
  
  
  joined$number_of_fitness_evaluations[joined$fitness_function_value>0] <- 250000
  
  
  return(joined)
}


getReproduceStatus <- function(fitnessFunction){
  
  rawResult <- getAllResults()
  
  df <- rawResult %>%
    filter(startsWith(cat,fitnessFunction)) %>%
    group_by(case, cat, fitness_function_value) %>%
    mutate(max_reproduced = ifelse(fitness_function_value == 0, max(frame_level), 0)) %>%
    ungroup() %>%
    group_by(case, cat) %>%
    mutate(max_reproduced = max(max_reproduced)) %>%
    ungroup() %>%
    distinct(application, case, cat, max_reproduced)
  
  df <- data.frame(df) %>%
    group_by(case) %>%
    mutate(highest = max(max_reproduced)) %>%
    ungroup() %>%
    mutate(status = ifelse(max_reproduced > 0 & max_reproduced >= highest, STATUS_LEVELS[2], STATUS_LEVELS[1]))
  
  df <- data.frame(df)
  df$status_factor <- factor(df$status, levels = STATUS_LEVELS, ordered = TRUE)
  
  return(df)
}

getHighestReproduced <- function(){
  df <- getReproduceStatus() %>%
    select(case,highest)
  df <- unique(df) %>%
    filter(highest >0)
  return(df)
}

getReproductionRate <- function(fitnessFunction){
  rawResult <- getAllResults()
  
  df <- getReproduceStatus(fitnessFunction) %>%
    inner_join(rawResult, by = c("application","case","cat")) %>%
    filter(frame_level == highest) %>%
    group_by(application,case,exception_name,cat,highest,status,status_factor) %>%
    filter(fitness_function_value <= min(fitness_function_value)) %>%
    summarise(count = ifelse(status[1] == "reproduced", n(), 0) , 
              reproduction_rate = count/TOTAL_RUNS,
              avg_ff_evals = mean(number_of_fitness_evaluations),
              sd_ff_evals = sd(number_of_fitness_evaluations)) %>%
    data.frame()
  
  
  return(df)
}


getLineCoverageRateForWS <- function(){
  rawResult <- getAllResults() %>%
    filter(startsWith(cat, 'WeightedSum')) %>%
    mutate(line_covered = ifelse(fitness_function_value <= 3,1,0)) 
  
  df <- rawResult %>%
    group_by(application,case,exception_name,frame_level,cat) %>%
    summarise(line_coverage_ratio = sum(line_covered)/TOTAL_RUNS, 
              line_coverage_count = sum(line_covered))
  
  return(df)
}


getLineCoverageRateForInteg <- function(){
  df <- getAllResults() %>%
    filter(startsWith(cat, 'IntegrationSingleObjective')) %>%
    mutate(line_covered = ceiling(frame_level-fitness_function_value))
  
  df$line_covered <- ifelse(rawResult$line_covered == -1,0,rawResult$line_covered)
  
  
  return(df)
}