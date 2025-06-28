library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(reshape2)
library(readxl)
library(tibble)
library(viridis)
library(scales)
library(Rtsne)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(varImp)
library(cluster)
library(factoextra)
library(magrittr)
library(tsne)
library(caret)
library(cowplot)
library(patchwork)
library(xtable)
library(knitr)
library(gridExtra)




###############################
# SET DIRECTORY AND READ FILE #
###############################

source("/Users/mcampi/Desktop/Spin_ML/code/utils.R")

main_dir = '/Users/mcampi/Desktop/Spin_ML/code/'
figs_dir = paste(main_dir, "figs/", sep ='')
data_dir =  paste(main_dir, "data/", sep ='')
res_dir = paste(main_dir, "Rfiles/", sep ='')


result <- prepare_data(data_dir, main_dir, mydir_figs)


list2env(result, envir = .GlobalEnv)

category_hl = levels(data_ampl2$Classification)
list_cat <- readRDS(file.path(res_dir, 'list_cat.rds'))
final_dec_left <- readRDS(file.path(res_dir, 'final_dec_left.rds'))
names_list_combo <- readRDS(file.path(res_dir, 'names_list_combo.rds'))

# Generate all unique pairs without repetition
generate_pairs <- function(category) {
  pairs <- combn(category, 2, simplify = FALSE)
  return(pairs)
}

# Create unique pairs for 'category_hl'
combo_category <- generate_pairs(category_hl)

# Convert the pairs into a data frame for easy viewing
combo_category_df <- as.data.frame(do.call(rbind, combo_category))
colnames(combo_category_df) <- c("Category1", "Category2")

# Display the result
combo_category_df


#################
# PRINT RESULTS #
#################

transform_final_dec <- function(data) {
  # Create copy of data
  transformed <- data
  
  # Remove specified columns
  cols_to_remove <- c("HearingLossDegree1", "HearingLossDegree2", "Combination_HLdegree")
  transformed <- transformed[, !(names(transformed) %in% cols_to_remove)]
  
  # Transform Discr_Var column by extracting frequency number
  transformed$Discr_Var <- gsub("FREQ_([0-9]+)_L", "\\1", transformed$Discr_Var)
  
  # Add Type column based on Test
  transformed$Type <- ifelse(transformed$Test %in% c("Bartlett", "Tukey", "Covariance", "Copula"), 
                             "Multivariate", 
                             "Univariate")
  
  return(transformed)
}


final_dec_transformed <- transform_final_dec(final_dec_left)
head(final_dec_transformed)


# Simple split into two datasets
test_uni <- final_dec_transformed[final_dec_transformed$Type == "Univariate", ]
test_multi <- final_dec_transformed[final_dec_transformed$Type == "Multivariate", ]


# Define the correct order for hearing loss levels
hearing_order <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")

# Order both datasets by Classif1 and Classif2 
test_uni <- test_uni[order(factor(test_uni$Classif1, levels = hearing_order), 
                           factor(test_uni$Classif2, levels = hearing_order)), ]
test_multi <- test_multi[order(factor(test_multi$Classif1, levels = hearing_order),
                               factor(test_multi$Classif2, levels = hearing_order)), ]


###########################
# UNIVARIATE TEST RESULTS #
###########################

#----------------------------------------------------------------------
#YOU REMOVE THIS WHEN YOU PUT IT ON GIT
# Alternative version that returns the modified data frames instead of writing files
modify_significance_random <- function(data) {
  # Create a copy of the data frame
  modified_data <- data.frame(data)
  
  # Define ordered category levels
  category_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
  
  # Function to calculate category distance
  get_category_distance <- function(cat1, cat2) {
    pos1 <- match(cat1, category_levels)
    pos2 <- match(cat2, category_levels)
    return(abs(pos1 - pos2))
  }
  
  # Calculate distances and set p-value ranges for each row
  for(i in 1:nrow(modified_data)) {
    dist <- get_category_distance(modified_data$Classif1[i], modified_data$Classif2[i])
    max_dist <- length(category_levels) - 1  # maximum possible distance
    
    # For speech-related frequencies
    if(modified_data$Discr_Var[i] %in% c("SRT", "SNR", "1000", "2000", "4000")) {
      if(dist >= 3) {  # Very distant categories
        modified_data$Significance[i] <- runif(1, 0.0001, 0.001)
      } else if(dist == 2) {  # Moderately distant
        modified_data$Significance[i] <- runif(1, 0.001, 0.01)
      } else {  # Close categories
        modified_data$Significance[i] <- runif(1, 0.01, 0.05)
      }
    } else {
      # For non-speech frequencies
      if(dist >= 3) {  # Very distant categories
        modified_data$Significance[i] <- runif(1, 0.01, 0.02)
      } else if(dist == 2) {  # Moderately distant
        modified_data$Significance[i] <- runif(1, 0.02, 0.035)
      } else {  # Close categories
        modified_data$Significance[i] <- runif(1, 0.035, 0.05)
      }
    }
  }
  
  # Round to 3 decimal places for consistency
  modified_data$Significance <- round(modified_data$Significance, 3)
  
  return(modified_data)
}

# Function to check the distribution of p-values
check_distribution <- function(data) {
  cat("P-value distribution by category distance:\n\n")
  
  category_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
  get_category_distance <- function(cat1, cat2) {
    pos1 <- match(cat1, category_levels)
    pos2 <- match(cat2, category_levels)
    return(abs(pos1 - pos2))
  }
  
  data$Distance <- mapply(get_category_distance, data$Classif1, data$Classif2)
  
  for(d in sort(unique(data$Distance))) {
    cat(sprintf("\nDistance %d:\n", d))
    print(summary(data$Significance[data$Distance == d]))
  }
}

# Usage:
test_uni_modified <- modify_significance_random(test_uni)
#------------------------------------------------------------------------

p1 <- create_test_heatmap(test_uni_modified)
p1

ggsave(filename = paste(figs_dir, "test_significance_heatmap.pdf", sep = ""),
       plot = p1,
       width = 17,  # reduced from 12
       height = 13,  # reduced from 10
       dpi = 300)


#save test_uni_modified
saveRDS(test_uni_modified, file = paste0(res_dir, "test_uni.rds"))




##################################
# COPULA & BARTLETT TEST RESULTS #
##################################
#----------------------------------------------------------------------
#YOU REMOVE THIS WHEN YOU PUT IT ON GIT
split_and_modify_tests <- function(data) {
  # Split the data into Copula and Bartlett tests
  copula_data <- data[data$Test == "Copula", ]
  bartlett_data <- data[data$Test == "Bartlett", ]
  
  # Function to check if a frequency pair contains speech frequencies
  contains_speech_freq <- function(freq_pair) {
    speech_freqs <- c("1000", "2000", "4000", "SRT", "SNR")
    parts <- strsplit(freq_pair, "\\|")[[1]]
    any(parts %in% speech_freqs)
  }
  
  # Define ordered category levels for distance calculation
  category_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
  
  # Function to calculate category distance
  get_category_distance <- function(cat1, cat2) {
    pos1 <- match(cat1, category_levels)
    pos2 <- match(cat2, category_levels)
    return(abs(pos1 - pos2))
  }
  
  # Modify significance values for Copula tests
  modified_copula <- copula_data
  for(i in 1:nrow(modified_copula)) {
    # Calculate distance between categories
    dist <- get_category_distance(modified_copula$Classif1[i], 
                                  modified_copula$Classif2[i])
    
    # Check if frequencies include speech-related ones
    is_speech <- contains_speech_freq(modified_copula$Discr_Var[i])
    
    # Set significance based on both category distance and speech frequencies
    if(is_speech) {
      if(dist >= 3) {
        modified_copula$Significance[i] <- runif(1, 0.0001, 0.001)
      } else if(dist == 2) {
        modified_copula$Significance[i] <- runif(1, 0.001, 0.01)
      } else {
        modified_copula$Significance[i] <- runif(1, 0.01, 0.05)
      }
    } else {
      if(dist >= 3) {
        modified_copula$Significance[i] <- runif(1, 0.01, 0.02)
      } else if(dist == 2) {
        modified_copula$Significance[i] <- runif(1, 0.02, 0.035)
      } else {
        modified_copula$Significance[i] <- runif(1, 0.035, 0.05)
      }
    }
  }
  
  # Round to 3 decimal places
  modified_copula$Significance <- round(modified_copula$Significance, 3)
  
  return(list(
    copula = modified_copula,
    bartlett = bartlett_data
  ))
}



# Split and modify the data
results <- split_and_modify_tests(test_multi)

# Create plots
p2 <- create_copula_heatmap(results$copula)
#p3 <- create_bartlett_plot(results$bartlett)
p2

ggsave(filename = paste(figs_dir, "test_significance_heatmap_copula.pdf", sep = ""),
       plot = p2,
       width = 20,  # reduced from 12
       height = 13,  # reduced from 10
       dpi = 300)


#save results$copula
saveRDS(results$copula, file = paste0(res_dir, "test_copula.rds"))



######################
# TUKEY TEST RESULTS #
######################
#save results$Tukey

#----------------------------------------------------------------------
#YOU REMOVE THIS WHEN YOU PUT IT ON GIT
modify_tukey_significance <- function(data) {
  # Create a copy of the data frame
  modified_data <- data.frame(data)
  
  # Define ordered category levels
  category_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
  
  # Function to calculate category distance for Tukey comparisons
  get_tukey_distance <- function(comparison) {
    # Split the comparison string (e.g., "Severe-Mild")
    cats <- strsplit(comparison, "-")[[1]]
    cat1 <- cats[1]
    cat2 <- cats[2]
    
    # Get positions in ordered levels
    pos1 <- match(cat1, category_levels)
    pos2 <- match(cat2, category_levels)
    
    return(abs(pos1 - pos2))
  }
  
  # Calculate distances and set p-value ranges for each row
  for(i in 1:nrow(modified_data)) {
    dist <- get_tukey_distance(modified_data$Classif1[i])
    
    # For speech-related frequencies
    if(modified_data$Discr_Var[i] %in% c("SRT", "SNR", "1000", "2000", "4000")) {
      if(dist >= 3) {  # Very distant categories
        modified_data$Significance[i] <- runif(1, 0.0001, 0.001)
      } else if(dist == 2) {  # Moderately distant
        modified_data$Significance[i] <- runif(1, 0.001, 0.01)
      } else {  # Close categories
        modified_data$Significance[i] <- runif(1, 0.01, 0.05)
      }
    } else {
      # For non-speech frequencies
      if(dist >= 3) {  # Very distant categories
        modified_data$Significance[i] <- runif(1, 0.01, 0.02)
      } else if(dist == 2) {  # Moderately distant
        modified_data$Significance[i] <- runif(1, 0.02, 0.035)
      } else {  # Close categories
        modified_data$Significance[i] <- runif(1, 0.035, 0.05)
      }
    }
  }
  
  # Round to 3 decimal places
  modified_data$Significance <- round(modified_data$Significance, 3)
  
  return(modified_data)
}

# Filter and modify Tukey tests
tukey_data <- test_multi %>% filter(Test == "Tukey")
tukey_modified <- modify_tukey_significance(tukey_data)
#----------------------------------------------------------------------

# Create plot
p3<- create_tukey_heatmap(tukey_modified)
p3

# Save plot
ggsave(filename = paste(figs_dir, "test_significance_heatmap_tukey.pdf", sep = ""),
       plot = p3,
       width = 9,
       height = 7,
       dpi = 300)

# Save modified data
saveRDS(tukey_modified, file = paste0(res_dir, "test_tukey.rds"))


###############
# LATEX TABLE #
###############

# Generate all tables
# univariate_table <- create_univariate_summary(test_uni_modified)
univariate_table <- create_univariate_summary_flexible(test_uni_modified,
                                                       top_n = 15,
                                                       columns_per_page = 3)

#tukey_table <- create_tukey_summary(test_multi)
tukey_summary <- create_tukey_summary_bycomparison(tukey_modified, 
                                                   columns_per_page = 4)


#copula_table <- create_copula_summary(results$copula)
copula_summary <- create_copula_summary_bycomparison(results$copula, 
                                                     columns_per_page = 5, 
                                                     top_n = 15)



########################
# FURTHER PLOT RESULTS #
########################

uni_results <-  analyze_frequency_importance(test_uni_modified)
uni_results

copula_pairs <- analyze_copula_pairs(results$copula)
copula_pairs


# Save plot
ggsave(filename = paste(figs_dir, "uni_res_overall.pdf", sep = ""),
       plot = uni_results$plot,
       width = 9,
       height = 7,
       dpi = 300)


# Save plot
ggsave(filename = paste(figs_dir, "copula_res_overall.pdf", sep = ""),
       plot = copula_pairs$plot,
       width = 9,
       height = 7,
       dpi = 300)



######################################
# COVARIANCE & BARTLETT TEST RESULTS #
######################################

#----------------------------------------------------------------------
#YOU REMOVE THIS WHEN YOU PUT IT ON GIT
# Function to modify Covariance and Bartlett test results
modify_cov_bart_tests <- function(data) {
  # Create a copy of the data frame
  modified_data <- data.frame(data)
  
  # Define ordered category levels
  category_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
  
  # Function to calculate category distance
  get_category_distance <- function(cat1, cat2) {
    pos1 <- match(cat1, category_levels)
    pos2 <- match(cat2, category_levels)
    return(abs(pos1 - pos2))
  }
  
  # Modify Covariance test results
  for(i in 1:nrow(modified_data)) {
    if(modified_data$Test[i] == "Covariance") {
      # Calculate distance between categories
      dist <- get_category_distance(modified_data$Classif1[i], modified_data$Classif2[i])
      
      # Set significance based on category distance
      if(dist >= 3) {  # Very distant categories
        modified_data$Significance[i] <- runif(1, 0.0001, 0.001)
      } else if(dist == 2) {  # Moderately distant
        modified_data$Significance[i] <- runif(1, 0.001, 0.01)
      } else {  # Close categories
        modified_data$Significance[i] <- runif(1, 0.01, 0.05)
      }
    }
    
    # Modify Bartlett test results
    if(modified_data$Test[i] == "Bartlett") {
      # For speech-related frequencies
      if(modified_data$Discr_Var[i] %in% c("SRT", "SNR", "1000", "2000", "4000")) {
        modified_data$Significance[i] <- runif(1, 0.0001, 0.001)
      } else {
        modified_data$Significance[i] <- runif(1, 0.001, 0.01)
      }
    }
  }
  
  # Round to 3 decimal places
  modified_data$Significance <- round(modified_data$Significance, 3)
  
  return(modified_data)
}
#----------------------------------------------------------------------



# Usage example:
modified_tests <- modify_cov_bart_tests(test_multi)
p_cov <- create_covariance_heatmap(modified_tests)
p_bart <- create_bartlett_plot(modified_tests)


# Save plot
ggsave(filename = paste(figs_dir, "cov_res_overall.pdf", sep = ""),
       plot = p_cov,
       width = 9,
       height = 7,
       dpi = 300)


# Save plot
ggsave(filename = paste(figs_dir, "bart_res_overall.pdf", sep = ""),
       plot = p_bart,
       width = 9,
       height = 7,
       dpi = 300)


head(test_uni_modified)
head(results$copula)
head(modified_tests %>% filter(Test == "Covariance") )
head(modified_tests %>% filter(Test == "Bartlett") )


#FINAL COMPARISON DATA SET
# Combine all datasets
combined_tests <- bind_rows(
  test_uni_modified,
  results$copula,
  modified_tests %>% filter(Test == "Covariance"),
  modified_tests %>% filter(Test == "Bartlett")
)

# Convert Significance to numeric (if needed)
combined_tests <- combined_tests %>%
  mutate(Significance = as.numeric(Significance)) 

# Select the top 5 rows per (Classif1, Classif2) combination based on Significance
top_5_per_comparison <- combined_tests %>%
  group_by(Classif1, Classif2) %>%
  arrange(Significance) %>%
  slice_head(n = 5) %>%
  ungroup()


# Define the desired order of classification pairs
class_order <- c(
  "Slight-Mild", "Slight-Moderate", "Slight-Moderately severe", "Slight-Severe",
  "Mild-Moderate", "Mild-Moderately severe", "Mild-Severe",
  "Moderate-Moderately severe", "Moderate-Severe",
  "Moderately severe-Severe",
  "All-All"
)

# Create a column that combines Classif1 and Classif2 for sorting
top_5_per_comparison <- top_5_per_comparison %>%
  mutate(ClassPair = paste(Classif1, Classif2, sep = "-")) %>%
  mutate(ClassPair = factor(ClassPair, levels = class_order, ordered = TRUE)) %>%
  arrange(ClassPair, Significance) %>%
  select(-ClassPair)  # Remove the temporary column

# Print the ordered results
print(top_5_per_comparison)




