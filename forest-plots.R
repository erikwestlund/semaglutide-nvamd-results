library(ggplot2)
library(dplyr)

# Data schema used for forest plot
dataSchema <- tribble(~database_id, ~database_name, ~target_cohort_id, ~target_name, ~comparator_cohort_id, ~comparator_name, ~group_n, ~target_days, ~comparator_days, ~target_outcomes, ~comparator_outcomes, ~hr, ~hr_lb, ~hr_ub, )
data <- dataSchema

# Eventually I'll get this to pull data from the shiny DB to provide real life database values

databases <- c(
  "CUMC" = 1,
  "JHME" = 2,
  "CCAE" = 3,
  "MDCD" = 4,
  "MDCR" = 5,
  "OHSU" = 6,
  "Optum EHR" = 7,
  "Clinformatics" = 8,
  "PharMetrics" = 9,
  "STARR" = 10,
  "USC" = 11,
  "VA" = 12,
  "WU" = 13
)

cohortIds <- c(
   "Semaglutide" = 201,
   "Dulaglutide" = 211,
   "Exenatide" = 212,
   "Empagliflozin" = 213,
   "Sitagliptin" = 214,
   "Glipizide" = 215
)

# Expand out all T/C combinations and their names
cohortCombinations <- expand.grid(cohortIds, cohortIds) |> as.data.frame()
names(cohortCombinations) <- c("target_cohort_id", "comparator_cohort_id")

cohortCombinations$target_name <- cohortCombinations$target_cohort_id |> map_chr(~names(cohortIds)[.x]) |> names()
cohortCombinations$comparator_name <- cohortCombinations$comparator_cohort_id |> map_chr(~names(cohortIds)[.x]) |> names()

# Remove T=C combinations
cohortCombinations <- cohortCombinations |> 
  filter(target_cohort_id != comparator_cohort_id) 

# Simulate data. Later we can get this pulled in from the shiny DB
for(i in 1:length(databases)) {
  # Make up cohort Ns for database
  cohortData <- tribble(~database_id, ~cohort_id, ~n, ~ott) |> 
    add_row(cohort_id = cohortIds)
  
  cohortData$database_id <- databases[i]
  cohortData$n <- runif(nrow(cohortData), 0, 5500000)
  cohortData$t_ott <- cohortData$n * runif(nrow(cohortData), 1, 2)
  cohortData$c_ott <- cohortData$n * runif(nrow(cohortData), 1, 2)
  
  # Add rows for all cohort combinations
  dbData <- dataSchema |> add_row(cohortCombinations)
  
  dbData$database_id <- databases[i]
  dbData$database_name <- databases[i] |> names() |> as.character()

  # on treatment time in person-years, rnadom between 0 and 11000
  dbData$group_n <- cohortData$n[match(dbData$target_cohort_id, cohortData$cohort_id)]
  dbData$target_days <- cohortData$t_ott[match(dbData$target_cohort_id, cohortData$cohort_id)]
  dbData$comparator_days <- cohortData$c_ott[match(dbData$target_cohort_id, cohortData$cohort_id)]
  dbData$hr <- rnorm(nrow(dbData), mean = 1, sd = 0.1)
  dbData$hr_lb <- dbData$hr - runif(nrow(dbData), 0.1, 0.2)
  dbData$hr_ub <- dbData$hr + runif(nrow(dbData), 0.1, 0.2)

  dbData$target_outcomes = round(runif(nrow(dbData), 0, 100))
  dbData$comparator_outcomes = round(dbData$target_outcomes * (1/dbData$hr))
  
  
  data <- data |> rbind(dbData)
}

# Make the graphs