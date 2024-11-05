{
  library(ggplot2)
  library(ggdist)
  library(tidyverse)
  library(hrbrthemes)
  library(patchwork)
  library(ggforce)
  library(scales)
}


# Simulation Variables ----------------------------------------------------

#Trial sample sizes: 30, 100, 300

#Cohort size: 5, 30

#Response rate: 50%, 75%, 90%
#Rebate percentage: 0% to 100% in increments of 10%

# Inputs ----------------------

nSim <- 5000

# trial sample sizes
sampleSizes <- c(30, 100, 500)

# cohort sizes
# cohortSizes <- c(30, 100)
cohortSizes <- c(15, 100)



responseRates <- c(0.50, 0.75, 0.90)  
rebatePercentage <- seq(0, 100, 10) / 100  

drugCost <- 10000  
bestFDR <- 0.23

results <- data.frame()

set.seed(20240904)

# Simulation function ----------------------
for (sampleSize in sampleSizes) {
   for (cohortSize in cohortSizes) {
      for (rr in responseRates) {
         for (rebate in rebatePercentage) {
            equivalentFDRs <- double(nSim)
            
            # Simulate the trial
            trialResponses <- rbinom(n = nSim, size = sampleSize, prob = rr)
            estimatedFailureRates <- 1 - trialResponses / sampleSize
            
            numFailureInCohort <- sapply(estimatedFailureRates,
                                         function(prob) rbinom(n = 1, size = cohortSize, prob = prob))
            
            equivalentFDRs <- drugCost * numFailureInCohort * rebate / (drugCost * cohortSize)
            
            results <- rbind(results, data.frame(
               SampleSize = sampleSize,
               CohortSize = cohortSize,
               ResponseRate = sprintf("%.2f", rr),
               RebatePercentage = rebate,
               MeanFDR = mean(equivalentFDRs),
               LowerCI = quantile(equivalentFDRs, 0.025),
               UpperCI = quantile(equivalentFDRs, 0.975)
            ))
         }
      }
   }
}

# Saving ----------------------

rownames(results) <- seq(1, nrow(results))
results[, c("MeanFDR", "LowerCI", "UpperCI")] <- round(results[, c("MeanFDR", "LowerCI", "UpperCI")], digits = 3)
# results

# write.csv(results, "/Users/richard.yan/Downloads/results.csv", row.names = FALSE)
write.csv(results, "./results5000cohortsize15and100.csv", row.names = FALSE)

