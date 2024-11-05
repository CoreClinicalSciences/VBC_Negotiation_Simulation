# VBCAM Graded Error Bars -------------------------------------------------------

## Load Packages ---------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
# library(gridExtra)


# Read Data ---------------------------------------------------------------
data <- read.csv("results5000cohortsize15and100.csv")

## Format Data ---------------------------------------------------------------
data$RebatePercentage <- factor(data$RebatePercentage, levels = unique(data$RebatePercentage))   

data$RebatePercentage <- as.numeric(as.character(data$RebatePercentage))


# Dataframes for figures --------------------------------------------------

## Cohort 5 -----------------------------------------
filtered_data_5_50 <- data %>%filter(CohortSize == 15, ResponseRate == 0.50)
filtered_data_5_75 <- data %>%filter(CohortSize == 15, ResponseRate == 0.75)
filtered_data_5_90 <- data %>%filter(CohortSize == 15, ResponseRate == 0.90)

## Cohort 30 -----------------------------------------
filtered_data_30_50 <- data %>%filter(CohortSize == 100, ResponseRate == 0.50)
filtered_data_30_75 <- data %>%filter(CohortSize == 100, ResponseRate == 0.75)
filtered_data_30_90 <- data %>%filter(CohortSize == 100, ResponseRate == 0.90)


# Figure -------------------------------------------------------------------------
source("02-FunctionPlot.R")

## Cohort 5 ---------------------------------------------------------------

figureCohort5Response50 <- FunctionPlot(filteredData = filtered_data_5_50, responseRate = 50) + 
   theme(legend.position = "none", axis.title.x = element_blank()) +
   labs(title = "\nTop row illustrates the performance of a value-based contract with the beneficiary count of 30", subtitle = "a) Response rate of 50%") +
   theme(plot.title = element_text(hjust = 0, face = "bold", size = 16), # Left-align the title
         plot.subtitle = element_text(hjust = 0.5, size = 14)  # Center-align the subtitle
         )  


figureCohort5Response75 <- FunctionPlot(filteredData = filtered_data_5_75, responseRate = 75) + 
   theme(legend.position = "none", axis.title.y = element_blank()) +
   labs(title = "", subtitle = "b) Response rate of 75%") + 
   theme(plot.title = element_text(hjust = 0, face = "bold", size = 16), # Left-align the title
      plot.subtitle = element_text(hjust = 0.5, size = 14)  # Center-align the subtitle
      )  

figureCohort5Response90 <- FunctionPlot(filteredData = filtered_data_5_90, responseRate = 90) + 
   theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
   labs(title = "", subtitle = "c) Response rate of 90%") + 
   theme(plot.title = element_text(hjust = 0, face = "bold", size = 16), # Left-align the title
         plot.subtitle = element_text(hjust = 0.5, size = 14)  # Center-align the subtitle
         )  
# + 
#    geom_text(aes(x = Inf, y = 0.23, label = "Medicaid Best Price Threshold"), hjust = 1.2, vjust = -1, show.legend = FALSE)


## Cohort 30 ---------------------------------------------------------------

figureCohort30Response50 <- FunctionPlot(filteredData = filtered_data_30_50, responseRate = 50) + 
   theme(legend.position = "none", axis.title.x = element_blank()) +
   labs(title = "\nBottom row illustrates the performance of a value-based contract with beneficiary count of 100", subtitle = "d) Response rate of 50%") +
   theme(plot.title = element_text(hjust = 0, face = "bold", size = 16), # Left-align the title
         plot.subtitle = element_text(hjust = 0.5, size = 14)  # Center-align the subtitle
   )  

figureCohort30Response75 <- FunctionPlot(filteredData = filtered_data_30_75, responseRate = 75) + 
   theme(legend.position = "none", axis.title.y = element_blank()) +
   labs(title = "", subtitle = "e) Response rate of 75%") + 
   theme(plot.title = element_text(hjust = 0, face = "bold", size = 16), # Left-align the title
         plot.subtitle = element_text(hjust = 0.5, size = 14)  # Center-align the subtitle
   )  


figureCohort30Response90 <- FunctionPlot(filteredData = filtered_data_30_90, responseRate = 90) +
   theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
   labs(title = "", subtitle = "f) Response rate of 90%") + 
   theme(plot.title = element_text(hjust = 0, face = "bold", size = 16), # Left-align the title
         plot.subtitle = element_text(hjust = 0.5, size = 14)  # Center-align the subtitle
   )  
# +
#    geom_text(aes(x = Inf, y = 0.23, label = "Medicaid Best Price Threshold"), hjust = 1.2, vjust = -1, show.legend = FALSE)


# Patchwork ---------------------------------------------------------------

# ## work 1 - doesn't work
# combinedPlotUpperRow <-
#    (figureCohort5Response50 + figureCohort5Response75 + figureCohort5Response90) +
#    patchwork::plot_layout(ncol = 3, nrow = 1, guides = "auto")
# 
# combinedPlotLowerRow <-
#    (figureCohort30Response50 + figureCohort30Response75 + figureCohort30Response90) +
#    patchwork::plot_layout(ncol = 3, nrow = 1, guides = "auto")
# 
# combinedPlots <- (combinedPlotUpperRow + combinedPlotLowerRow) +
#    patchwork::plot_layout(ncol = 3 , nrow = 2, guides = "collect") &
#    theme(legend.position = "bottom", plot.title = element_text(hjust = 0))
# 
# combinedPlots

# work 2
combinedPlots <-
   (figureCohort5Response50 + figureCohort5Response75 + figureCohort5Response90 +
   figureCohort30Response50 + figureCohort30Response75 + figureCohort30Response90) +
   patchwork::plot_layout(ncol = 3, nrow = 2, guides = "collect", 
                          heights = c(2, 2) # Adjust the second value to increase spacing between rows
   ) &
   theme(
      legend.position = "bottom",
         plot.title = element_text(hjust = 0), 
         legend.title = element_text(face = "bold"), 
         legend.justification = c(0.5, 2)        
         )

   # theme(
   #    legend.position = c(1, 1),  # Adjust legend position inside the plot area
   #  ) +


combinedPlots


