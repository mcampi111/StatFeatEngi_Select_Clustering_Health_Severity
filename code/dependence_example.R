# Required libraries
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Function to generate different types of dependent data
generate_dependent_data <- function(n, type) {
  if(type == "strong_linear") {
    x <- runif(n)
    y <- x + rnorm(n, 0, 0.1)
  } else if(type == "moderate_linear") {
    x <- runif(n)
    y <- x + rnorm(n, 0, 0.3)
  } else if(type == "upper_tail") {
    x <- runif(n)
    y <- ifelse(x > 0.7,
                x + rnorm(n, 0, 0.05),
                x + rnorm(n, 0, 0.4))
  } else if(type == "lower_tail") {
    x <- runif(n)
    y <- ifelse(x < 0.3,
                x + rnorm(n, 0, 0.05),
                x + rnorm(n, 0, 0.4))
  } else if(type == "threshold") {
    x <- runif(n)
    y <- ifelse(x > 0.5,
                runif(n, 0.7, 1),
                runif(n, 0, 0.3))
  } else if(type == "nonlinear") {
    x <- runif(n)
    y <- x^2 + rnorm(n, 0, 0.1)
  } else if(type == "asymmetric") {
    x <- runif(n)
    y <- x^1.5 + rnorm(n, 0, 0.1)
  } else if(type == "joint_extremes") {
    extreme <- rbinom(n, 1, 0.2)
    x <- ifelse(extreme == 1,
                runif(n, 0.8, 1),
                runif(n))
    y <- ifelse(extreme == 1,
                runif(n, 0.8, 1),
                runif(n))
  }
  
  data.frame(x = x, y = y, type = type)
}

# Generate data for each type
n <- 500  # number of points per plot
types <- c("strong_linear", "moderate_linear", "upper_tail", "lower_tail",
           "threshold", "nonlinear", "asymmetric", "joint_extremes")

# Combine all data
all_data <- do.call(rbind, lapply(types, function(t) generate_dependent_data(n, t)))

# Create better labels for the plots
all_data$type_label <- factor(all_data$type,
                              levels = types,
                              labels = c("Strong Linear", "Moderate Linear",
                                         "Upper Tail", "Lower Tail",
                                         "Threshold Effect", "Nonlinear",
                                         "Asymmetric", "Joint Extremes"))

# Create the plot
ggplot(all_data, aes(x = x, y = y)) +
  geom_point(alpha = 0.5, color = "darkblue", size = 1) +
  facet_wrap(~type_label, ncol = 4) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 8)) +
  labs(x = "Variable 1", y = "Variable 2",
       #title = "Different Patterns of Statistical Dependence"
       ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

# Save the plot
ggsave("dependence_patterns.pdf", width = 12, height = 8)



# Function to generate bootstrap samples and calculate confidence intervals
calculate_measures_with_ci <- function(x, y, n_boot = 100) {
  n <- length(x)
  measures_boot <- matrix(NA, nrow = n_boot, ncol = 6)
  colnames(measures_boot) <- c("Pearson", "Spearman", "Kendall", 
                               "Upper Tail", "Lower Tail", "Concordance")
  
  for(i in 1:n_boot) {
    idx <- sample(1:n, n, replace = TRUE)
    x_boot <- x[idx]
    y_boot <- y[idx]
    
    # Pearson correlation
    measures_boot[i, 1] <- cor(x_boot, y_boot, method = "pearson")
    
    # Spearman correlation
    measures_boot[i, 2] <- cor(x_boot, y_boot, method = "spearman")
    
    # Kendall's tau
    measures_boot[i, 3] <- cor(x_boot, y_boot, method = "kendall")
    
    # Upper tail dependence
    q <- 0.9
    upper_joint <- mean((x_boot > quantile(x_boot, q)) & 
                          (y_boot > quantile(y_boot, q)))
    measures_boot[i, 4] <- upper_joint / (1 - q)
    
    # Lower tail dependence
    q <- 0.1
    lower_joint <- mean((x_boot < quantile(x_boot, q)) & 
                          (y_boot < quantile(y_boot, q)))
    measures_boot[i, 5] <- lower_joint / q
    
    # Median concordance
    med_x <- median(x_boot)
    med_y <- median(y_boot)
    measures_boot[i, 6] <- mean(sign(x_boot - med_x) * sign(y_boot - med_y))
  }
  
  # Calculate mean and confidence intervals
  results <- data.frame(
    measure = colnames(measures_boot),
    value = colMeans(measures_boot),
    lower_ci = apply(measures_boot, 2, quantile, 0.025),
    upper_ci = apply(measures_boot, 2, quantile, 0.975)
  )
  
  return(results)
}

# Generate data for one type and visualize with confidence intervals
generate_and_plot_measures <- function(type, n = 1000) {
  data <- generate_dependent_data(n, type)
  measures <- calculate_measures_with_ci(data$x, data$y)
  
  # Create plot
  ggplot(measures, aes(x = measure, y = value)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Dependence Measure", y = "Value",
         title = paste("Dependence Measures for", type),
         subtitle = "With 95% bootstrap confidence intervals") +
    coord_cartesian(ylim = c(-1, 1))
}

# Generate and save individual plots for each type
types <- c("strong_linear", "moderate_linear", "upper_tail", "lower_tail",
           "threshold", "nonlinear", "asymmetric", "joint_extremes")

plots <- lapply(types, generate_and_plot_measures)

# Arrange plots in a grid
grid_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 2))

# Save the grid
ggsave("dependence_measures_comparison.pdf", grid_plot, width = 15, height = 20)





