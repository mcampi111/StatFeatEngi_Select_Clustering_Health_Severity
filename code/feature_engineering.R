library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(proxy)
library(pracma)
library(reshape2)
library(readxl)
library(tibble)
library(viridis)
library(scales)
library(blandr)
library(irr)
library(Rtsne)
library(varImp)
library(cluster)
library(magrittr)
library(tsne)
library(NbClust)
library(RcppAlgos)
library(boot)
library(caret)
library(twosamples)
library(RoDiCE)
library(doParallel)
library(sLED)
library(grid)
library(gridExtra)
library(patchwork)
library(copula)



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


list_cat <- readRDS(file.path(res_dir, 'list_cat.rds'))
final_dec_left <- readRDS(file.path(res_dir, 'final_dec_left.rds'))
names_list_combo <- readRDS(file.path(res_dir, 'names_list_combo.rds'))


#######################
#######################
# FEATURE ENGINEERING #
#######################
#######################

prova = final_dec_left %>%
  distinct(Discr_Var, Test)

prova=  final_dec_left %>%
  distinct(Discr_Var, Test, .keep_all = TRUE)

final_dec_left_feat =  final_dec_left %>% 
  filter(Test != "Bartlett", 
         Test != "Welch",
         Test != "Covariance",
         Test != "CMV",
         Test != "Tukey")

unique(final_dec_left_feat$Test)


final_dec_left_feat <- final_dec_left_feat %>%
  mutate(Feat_Type = case_when(
    Test %in% c("T-test") ~ "Mean",
    Test == "Variance" ~ "Variance",
    Test == "Kolmogorov" ~ "Distribution",
    Test == "Copula" ~ "Rank",
    TRUE ~ NA_character_  # Optional: Handle any unexpected values
  ))

################
################
# BOOTSTRAPING #
################
################

# First, let's run both types of bootstrapping
columns_to_bootstrap <- colnames(list_cat[[1]])[c(3:13, 25, 26)]

# Run parametric bootstrap (t-copula)
bootstrap_results_param <- lapply(list_cat, function(df) {
  parametric_bootstrap_columns(df, columns_to_bootstrap, n_boot = 100)
})

# Run nonparametric bootstrap (Bernstein copula)
bootstrap_results_nonparam <- lapply(list_cat, function(df) {
  nonparametric_bootstrap_columns(df, columns_to_bootstrap, n_boot = 100)
})


# Process parametric results
feat_left_param = lapply(1:length(bootstrap_results_param), function(i) {
  results <- extract_statistics_modified(bootstrap_results_param[[i]])
  df <- data.frame(results)
  df$HL <- names_list_combo[i]
  return(df)
})
feat_left_param <- do.call(rbind, feat_left_param)

# Process nonparametric results
feat_left_nonparam = lapply(1:length(bootstrap_results_nonparam), function(i) {
  results <- extract_statistics_modified(bootstrap_results_nonparam[[i]])
  df <- data.frame(results)
  df$HL <- names_list_combo[i]
  return(df)
})
feat_left_nonparam <- do.call(rbind, feat_left_nonparam)

# Split columns for both parametric and nonparametric results
split_features <- function(feat_df) {
  # Identify column types
  univariate_cols <- grep("(mean|variance|median|iqr|p5|p95|rank\\d+)$", 
                          colnames(feat_df), value = TRUE)
  copula_cols <- grep("(tau_cop|rho_cop|gamma_cop|multi_rho|beta_cop|gini_alt|local_gauss|tail_dep)$", 
                      colnames(feat_df), value = TRUE)
  
  # Create separate dataframes
  feat_uni <- feat_df[, c("HL", univariate_cols)]
  feat_copula <- feat_df[, c("HL", copula_cols)]
  
  return(list(
    univariate = feat_uni,
    copula = feat_copula
  ))
}

# Split parametric results
param_split <- split_features(feat_left_param)
feat_left_uni_param <- param_split$univariate
feat_left_copula_param <- param_split$copula

# Split nonparametric results
nonparam_split <- split_features(feat_left_nonparam)
feat_left_uni_nonparam <- nonparam_split$univariate
feat_left_copula_nonparam <- nonparam_split$copula

# Save results
saveRDS(feat_left_uni_param, paste(res_dir, "/feat_left_uni_param_100.rds", sep = ''))
saveRDS(feat_left_copula_param, paste(res_dir, "/feat_left_copula_param_100.rds", sep = ''))
saveRDS(feat_left_uni_nonparam, paste(res_dir, "/feat_left_uni_nonparam_100.rds", sep = ''))
saveRDS(feat_left_copula_nonparam, paste(res_dir, "/feat_left_copula_nonparam_100.rds", sep = ''))

# Print summary statistics
print("Parametric Bootstrap Results:")
print(paste("Univariate features:", ncol(feat_left_uni_param) - 1))
print(paste("Copula features:", ncol(feat_left_copula_param) - 1))

print("\nNonparametric Bootstrap Results:")
print(paste("Univariate features:", ncol(feat_left_uni_nonparam) - 1))
print(paste("Copula features:", ncol(feat_left_copula_nonparam) - 1))

# Display sample of column names
print("\nSample of parametric copula features:")
head(colnames(feat_left_copula_param))
print("\nSample of nonparametric copula features:")
head(colnames(feat_left_copula_nonparam))

##############################################
# Robust t-SNE Analysis for Split Features   #
##############################################

############################
# Overall Type of Features #
############################

# Set random seed for reproducibility
set.seed(42)

results_param <- run_complete_analysis(feat_left_uni_param,
                                       feat_left_copula_param, 
                                       perplexity = 5)

results_nonparam <- run_complete_analysis(feat_left_uni_nonparam,
                                          feat_left_copula_nonparam, 
                                          perplexity = 5)

# Display results
cat("\nResults Summary:\n")
cat("\nUnivariate Analyses:\n")
print(results_param$summary_stats$univariate)
print(results_nonparam$summary_stats$univariate)
cat("\nCopula Analyses:\n")
print(results_param$summary_stats$copula)
print(results_nonparam$summary_stats$copula)


# Display visualizations if available
if(!is.null(results_param$visualizations$univariate$type)) {
  cat("\nDisplaying univariate visualizations...\n")
  print(results_param$visualizations$univariate$type)
}

if(!is.null(results_param$visualizations$copula$measure)) {
  cat("\nDisplaying copula visualizations...\n")
  print(results_param$visualizations$copula$measure)
}

# Display visualizations if available
if(!is.null(results_nonparam$visualizations$univariate$type)) {
  cat("\nDisplaying univariate visualizations...\n")
  print(results_nonparam$visualizations$univariate$type)
}

if(!is.null(results_nonparam$visualizations$copula$measure)) {
  cat("\nDisplaying copula visualizations...\n")
  print(results_nonparam$visualizations$copula$measure)
}



# Print summary of results
print(results_param$summary_stats)
print(results_param$univariate_results)

print(results_nonparam$summary_stats)
print(results_nonparam$univariate_results)


# Display key visualizations
gg_tsne_univariate_overall_param = print(results_param$visualizations$univariate$type)
gg_tsne_copula_overall_param = print(results_param$visualizations$copula$measure)

gg_tsne_univariate_overall_nonparam = print(results_nonparam$visualizations$univariate$type)
gg_tsne_copula_overall_nonparam = print(results_nonparam$visualizations$copula$measure)


ggsave(paste(figs_dir, "gg_tsne_univariate_overall_50_param.pdf", sep = ""), 
       gg_tsne_univariate_overall_param, width = 11, height = 8)

ggsave(paste(figs_dir,"gg_tsne_copula_overall_50_param.pdf", sep = ""),
       gg_tsne_copula_overall_param, width = 11, height = 8)


ggsave(paste(figs_dir, "gg_tsne_univariate_overall_50_nonparam.pdf", sep = ""), 
       gg_tsne_univariate_overall_nonparam, width = 20, height = 8)

ggsave(paste(figs_dir,"gg_tsne_copula_overall_50_nonparam.pdf", sep = ""),
       gg_tsne_copula_overall_nonparam, width = 20, height = 8)


################################
# Detailed Univariate Features #
################################

# Run the analysis
univariate_analysis_param <- analyze_individual_univariate_organized(feat_left_uni_param)
univariate_analysis_nonparam <- analyze_individual_univariate_organized(feat_left_uni_nonparam)

# Means only
means_plot_param <- wrap_plots(univariate_analysis_param$plots_mean, 
                               ncol = 4) + 
  plot_annotation(title = "Mean Features") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

means_plot_nonparam <- wrap_plots(univariate_analysis_nonparam$plots_mean, 
                                  ncol = 4) + 
  plot_annotation(title = "Mean Features") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")


# Variance only
variance_plot_param <- wrap_plots(univariate_analysis_param$plots_variance, 
                                  ncol = 4) + 
  plot_annotation(title = "Variance Features") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

variance_plot_nonparam <- wrap_plots(univariate_analysis_nonparam$plots_variance, 
                                     ncol = 4) + 
  plot_annotation(title = "Variance Features") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Distribution only
distribution_plot_param <- wrap_plots(univariate_analysis_param$plots_distribution, 
                                      ncol = 4) + 
  plot_annotation(title = "Distribution Features") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

distribution_plot_nonparam <- wrap_plots(univariate_analysis_nonparam$plots_distribution, 
                                         ncol = 4) + 
  plot_annotation(title = "Distribution Features") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Ranks only
ranks_plot_param <- wrap_plots(univariate_analysis_param$plots_ranks, ncol = 4) + 
  plot_annotation(title = "Rank Features") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ranks_plot_nonparam <- wrap_plots(univariate_analysis_nonparam$plots_ranks, ncol = 4) + 
  plot_annotation(title = "Rank Features") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Display individual feature type plots
print(means_plot_param)
print(variance_plot_param)
print(distribution_plot_param)
print(ranks_plot_param)

print(means_plot_nonparam)
print(variance_plot_nonparam)
print(distribution_plot_nonparam)
print(ranks_plot_nonparam)

ggsave(paste(figs_dir, "means_features_param_50.pdf", sep = ""),
       means_plot_param, width = 20, height = 8)
ggsave(paste(figs_dir, "variance_features_param_50.pdf",  sep = ""),
       variance_plot_param, width = 20, height = 8)
ggsave(paste(figs_dir, "distribution_features_param_50.pdf",  sep = ""),
       distribution_plot_param, width = 20, height = 8)
ggsave(paste(figs_dir, "ranks_features_param_50.pdf", sep = ""),
       ranks_plot_param, width = 20, height = 8)



ggsave(paste(figs_dir, "means_features_nonparam_50.pdf", sep = ""),
       means_plot_nonparam, width = 20, height = 8)
ggsave(paste(figs_dir, "variance_features_nonparam_50.pdf",  sep = ""),
       variance_plot_nonparam, width = 20, height = 8)
ggsave(paste(figs_dir, "distribution_features_nonparam_50.pdf",  sep = ""),
       distribution_plot_nonparam, width = 20, height = 8)
ggsave(paste(figs_dir, "ranks_features_nonparam_50.pdf", sep = ""),
       ranks_plot_nonparam, width = 20, height = 8)

###################################
# Detailed Multivariate Features #
###################################

# Run the analysis
copula_analysis_param <- analyze_copula_detailed(feat_left_copula_param)
copula_analysis_nonparam <- analyze_copula_detailed(feat_left_copula_nonparam)


# Display the plots
gg_copula_freq_pairs_param = copula_analysis_param$combined_freq_pairs 
gg_copula_combined_ranges_param = copula_analysis_param$combined_ranges
gg_copula_speech_pairs_param = copula_analysis_param$combined_speech

gg_copula_freq_pairs_nonparam = copula_analysis_nonparam$combined_freq_pairs 
gg_copula_combined_ranges_nonparam = copula_analysis_nonparam$combined_ranges
gg_copula_speech_pairs_nonparam = copula_analysis_nonparam$combined_speech


ggsave(paste(figs_dir, "freq_pairs_50_param.pdf",  sep = ""),
       gg_copula_freq_pairs_param, width = 20, height = 18)
ggsave(paste(figs_dir, "combined_ranges_50_param.pdf", sep = ""),
       gg_copula_combined_ranges_param, width = 20, height = 8)
ggsave(paste(figs_dir, "speech_pairs_50_param.pdf", sep = ""),
       gg_copula_speech_pairs_param, width = 20, height = 18)


ggsave(paste(figs_dir, "freq_pairs_50_nonparam.pdf",  sep = ""),
       gg_copula_freq_pairs_nonparam, width = 20, height = 18)
ggsave(paste(figs_dir, "combined_ranges_50_nonparam.pdf", sep = ""),
       gg_copula_combined_ranges_nonparam, width = 20, height = 8)
ggsave(paste(figs_dir, "speech_pairs_50_nonparam.pdf", sep = ""),
       gg_copula_speech_pairs_nonparam, width = 20, height = 18)



#########################
# TSNE on ORIGINAL DATA #
#########################

# Assuming data_ampl2 is your dataframe
set.seed(42)

# Function to sample 50 cases per HL category
sample_by_hl <- function(data) {
  data %>%
    group_by(Classification) %>%
    slice_sample(n = 50, replace = FALSE) %>%
    ungroup()
}

# Sample data
sampled_data <- sample_by_hl(data_ampl2)

# Extract feature sets
audiogram_cols <- grep("FREQ_.*_L", colnames(data_ampl2), value = TRUE)
speech_cols <- c("SNR", "SRT")
all_cols <- c(audiogram_cols, speech_cols)

# Prepare data for t-SNE
prepare_tsne <- function(data, cols) {
  data_scaled <- scale(data[, cols])
  # Remove duplicates
  unique_data <- unique(data_scaled)
  # Add small random noise if needed
  if(nrow(unique_data) < nrow(data_scaled)) {
    data_scaled <- data_scaled + matrix(rnorm(nrow(data_scaled) * ncol(data_scaled), 0, 1e-10), 
                                        nrow(data_scaled), ncol(data_scaled))
  }
  Rtsne(data_scaled, perplexity = 5, theta = 0.0)
}

# Run t-SNE for each feature set
tsne_audio <- prepare_tsne(sampled_data, audiogram_cols)
tsne_speech <- prepare_tsne(sampled_data, speech_cols)
tsne_all <- prepare_tsne(sampled_data, all_cols)

# Create plots
plot_tsne <- function(tsne_obj, title) {
  data.frame(
    V1 = tsne_obj$Y[,1],
    V2 = tsne_obj$Y[,2],
    HL = sampled_data$Classification
  ) %>%
    ggplot(aes(x = V1, y = V2, color = HL)) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_viridis_d(direction = -1) +
    theme_bw() +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          legend.text = element_text(size = 12))
}

# Generate plots
p1 <- plot_tsne(tsne_audio, "Audiogram")
p2 <- plot_tsne(tsne_speech, "Speech") 
p3 <- plot_tsne(tsne_all, "Audiogram and Speech")

# Combine with shared legend
combined_plot <- (p1 + p2 + p3) + 
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "bottom")

combined_plot

# Save plot
ggsave(paste(figs_dir, "tsne_raw.pdf", sep= ""),
       combined_plot, width = 10, height = 5)

