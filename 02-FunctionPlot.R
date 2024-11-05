# Define the function for creating the plot
FunctionPlot <- function(filteredData, responseRate) {

   # Create the plot using ggplot
   ggplot(filteredData, 
          aes(x = RebatePercentage, 
              y = MeanFDR, 
              color = factor(SampleSize))) +
      # Add confidence intervals using geom_ribbon
      geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = factor(SampleSize)), 
                  alpha = 0.7, linetype = "blank") +
                  
                  # Teal            
      # alpha = 1, linetype = "blank") +
      
      geom_smooth(method = "lm",
                  se = FALSE,
                  size = 0,
                  aes(group = SampleSize, color = factor(SampleSize))) +  # Line of best fit for each SampleSize
      
      # Custom colors for lines
      scale_color_manual(name = "Trial Sample Size",
                         # values = c("30" = "#FFBB56", "100" = "#B03C5A", "300" = "#462A79")) +
                         values = c("30" = "#B03C5A", "100" = "#FFBB56", "500" = "#462A79")) +
      
                        # Teal
                        # values = c("30" = "#FCAF38", "100" = "#f95335", "500" = "#50a3a4")) +
   
   
                         # values = c("30" = "green", "100" = "red", "300" = "blue")) +
                         
      
      # Custom colors for ribbons - D74E6F
      scale_fill_manual(name = "Trial Sample Size", 
                        # values = c("30" = "#FFBB56", "100" = "#B03C5A", "300" = "#462A79")) +
                        values = c("30" = "#B03C5A", "100" = "#FFBB56", "500" = "#462A79")) +
      
                        # Teal
                        # values = c("30" = "#FCAF38", "100" = "#f95335", "500" = "#50a3a4")) +
   
                        # values = c("30" = "green", "100" = "red", "300" = "blue")) + 
      theme_minimal() +  # Minimal theme

      labs(title = paste("Response Rate:", responseRate,"%"),
           x = "Rebate Percentage",
           y = "Equivalent Flat Discount Rate") +
      
      # theme(
      #    legend.position = "top",
      #    plot.title = element_text(hjust = 0.5)) +
      
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + 
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) 
      
      # geom_hline(yintercept = 0.23, linetype = "dashed", color = "black", size = 1)
   
}

