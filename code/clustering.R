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

feat_left_uni <- readRDS(file.path(res_dir, 'feat_left_uni_50.rds'))
feat_left_copula <- readRDS(file.path(res_dir, 'feat_left_copula_50.rds'))


#############################################
## Helper function for running clustering  ##
#############################################

# Define custom theme if not already defined
custom_theme <- theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text = element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title = element_text(size=16), 
        legend.text = element_text(size=15))

# Define hl_levels if not already defined
hl_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")

run_clustering <- function(data, features, title = "", n_clusters = 5) {
  # Select features and scale
  data_subset <- data[, c("HL", features)]
  data_subset[, features] <- scale(data_subset[, features])
  
  # Prepare data for clustering
  data_clust <- as.data.frame(data_subset[, features])  # Remove HL column
  rownames(data_clust) <- paste(data_subset$HL, 1:nrow(data_subset), sep="_")
  
  # Remove any rows with NA
  data_clust <- data_clust[complete.cases(data_clust),]
  
  # K-means clustering
  set.seed(123)
  km.res <- kmeans(data_clust, n_clusters, nstart = 25)
  
  # Hierarchical clustering
  hc.res <- hclust(dist(data_clust), method = "complete")
  hc.cut <- cutree(hc.res, k = n_clusters)
  
  # Calculate silhouette scores
  sil_km <- silhouette(km.res$cluster, dist(data_clust))
  avg_sil_km <- mean(sil_km[,3])
  
  sil_hc <- silhouette(hc.cut, dist(data_clust))
  avg_sil_hc <- mean(sil_hc[,3])
  
  # K-means Visualization
  p_km <- fviz_cluster(km.res,
                       data = data_clust,
                       ellipse.type = "convex",
                       labelsize = 0,
                       ggtheme = theme_bw()) +
    custom_theme +
    scale_color_viridis_d(direction = -1, 
                          name = "Cluster") +
    scale_fill_viridis_d(direction = -1,
                         name = "Cluster") +
    labs(title = paste0(title, "\nK-means Silhouette Score: ", round(avg_sil_km, 3))) +
    scale_shape_manual(values = c(3,17,19,22,25),
                       name = "Hearing Loss Level",
                       labels = c("Slight", "Mild", "Mod", "Mod Sev", "Sev"))
  
  # Hierarchical Visualization
  p_hc <- fviz_dend(hc.res, 
                    k = n_clusters,
                    cex = 0.5,
                    k_colors = viridis::viridis(n_clusters, direction = -1),
                    color_labels_by_k = TRUE,
                    rect = TRUE) +
    custom_theme +
    labs(title = paste0(title, "\nHierarchical Silhouette Score: ", round(avg_sil_hc, 3)))
  
  # Confusion matrices
  true_labels <- factor(data_subset$HL[complete.cases(data_clust)], levels = hl_levels)
  cluster_to_class_mapping <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
  
  # K-means confusion matrix
  km_labels_mapped <- factor(cluster_to_class_mapping[km.res$cluster], levels = hl_levels)
  conf_matrix_km <- confusionMatrix(data = km_labels_mapped, reference = true_labels)
  
  # Hierarchical confusion matrix
  hc_labels_mapped <- factor(cluster_to_class_mapping[hc.cut], levels = hl_levels)
  conf_matrix_hc <- confusionMatrix(data = hc_labels_mapped, reference = true_labels)
  
  return(list(
    kmeans_plot = p_km, 
    hc_plot = p_hc, 
    kmeans_confusion = conf_matrix_km, 
    hc_confusion = conf_matrix_hc,
    kmeans_silhouette = avg_sil_km,
    hc_silhouette = avg_sil_hc
  ))
}

# Modified print_metrics function to handle both methods
print_metrics <- function(results_list, title) {
  # Extract metrics for each method and clustering type
  metrics_df <- data.frame(
    Method = rep(names(results_list), each = 2),
    Clustering = rep(c("K-means", "Hierarchical"), times = length(results_list)),
    Accuracy = c(
      sapply(results_list, function(x) x$kmeans_confusion$overall["Accuracy"]),
      sapply(results_list, function(x) x$hc_confusion$overall["Accuracy"])
    ),
    Silhouette = c(
      sapply(results_list, function(x) x$kmeans_silhouette),
      sapply(results_list, function(x) x$hc_silhouette)
    )
  )
  
  print(paste("\n", title))
  
  print("\nSorted by Accuracy:")
  print(metrics_df[order(metrics_df$Accuracy, decreasing = TRUE), ])
  
  print("\nSorted by Silhouette Score:")
  print(metrics_df[order(metrics_df$Silhouette, decreasing = TRUE), ])
}

# Modified plot combination for both methods
create_combined_plots <- function(results_list, title) {
  # K-means plots
  kmeans_plots <- wrap_plots(
    lapply(results_list, function(x) x$kmeans_plot + theme(legend.position = "none")),
    ncol = 3,
    guides = "collect"
  ) &
    theme(legend.position = "right") +
    plot_annotation(
      title = paste(title, "- K-means Clustering"),
      theme = theme(plot.title = element_text(size = 16, hjust = 0.5))
    )
  
  # Hierarchical plots
  hc_plots <- wrap_plots(
    lapply(results_list, function(x) x$hc_plot + theme(legend.position = "none")),
    ncol = 3,
    guides = "collect"
  ) &
    theme(legend.position = "right") +
    plot_annotation(
      title = paste(title, "- Hierarchical Clustering"),
      theme = theme(plot.title = element_text(size = 16, hjust = 0.5))
    )
  
  return(list(kmeans_plots = kmeans_plots, hc_plots = hc_plots))
}


#############################################
## Univariate - OVERALL Features Analysis  ##
#############################################

# 1. Mean features
mean_features <- grep("mean$", colnames(feat_left_uni), value = TRUE)
mean_results <- run_clustering(feat_left_uni, mean_features, "Mean Features")

# 2. Variance features 
var_features <- grep("variance$", colnames(feat_left_uni), value = TRUE)
var_results <- run_clustering(feat_left_uni, var_features, "Variance Features")

# 3. Distribution features
dist_features <- grep("(median|iqr|p5|p95)$", colnames(feat_left_uni), value = TRUE)
dist_results <- run_clustering(feat_left_uni, dist_features, "Distribution Features")

# 4. Rank features
rank_features <- grep("rank[0-9]+$", colnames(feat_left_uni), value = TRUE)
rank_results <- run_clustering(feat_left_uni, rank_features, "Rank Features")

# Print accuracies
uni_results <- list(
  "Mean" = mean_results,
  "Variance" = var_results,
  "Distribution" = dist_results,
  "Rank" = rank_results
)

print_metrics(uni_results, "Univariate Features Metrics")


#############################################
## Copula - OVERALL Features Analysis      ##
#############################################

# Helper function to get copula features by type
get_copula_features <- function(pattern) {
  grep(pattern, colnames(feat_left_copula), value = TRUE)
}

# 5. Tau copula features
tau_features <- get_copula_features("tau_cop$")
tau_results <- run_clustering(feat_left_copula, tau_features, "Tau Copula Features")

# 6. Rho copula features
rho_features <- get_copula_features("rho_cop$")
rho_results <- run_clustering(feat_left_copula, rho_features, "Rho Copula Features")

# 7. Gamma copula features
gamma_features <- get_copula_features("gamma_cop$")
gamma_results <- run_clustering(feat_left_copula, gamma_features, "Gamma Copula Features")

# 8. Multi-rho features
multi_rho_features <- get_copula_features("multi_rho$")
multi_rho_results <- run_clustering(feat_left_copula, multi_rho_features, "Multi-rho Features")

# 9. Beta copula features
beta_features <- get_copula_features("beta_cop$")
beta_results <- run_clustering(feat_left_copula, beta_features, "Beta Copula Features")

# 10. Gini alternative features
gini_features <- get_copula_features("gini_alt$")
gini_results <- run_clustering(feat_left_copula, gini_features, "Gini Alternative Features")

# 11. Local Gaussian features
gauss_features <- get_copula_features("local_gauss$")
gauss_results <- run_clustering(feat_left_copula, gauss_features, "Local Gaussian Features")


# Print accuracies
copula_results <- list(
  "Tau" = tau_results,
  "Rho" = rho_results,
  "Gamma" = gamma_results,
  "Multi-rho" = multi_rho_results,
  "Beta" = beta_results,
  "Gini" = gini_results,
  "Local Gaussian" = gauss_results
)

print_metrics(copula_results, "Copula Features Metrics")

# For K-means plots
kmeans_combined <- (
  mean_results$kmeans_plot + theme(legend.position = "none") +
    var_results$kmeans_plot + theme(legend.position = "none") +
    dist_results$kmeans_plot + theme(legend.position = "none")
) / (
  rank_results$kmeans_plot + theme(legend.position = "none") +
    tau_results$kmeans_plot + theme(legend.position = "none") +
    rho_results$kmeans_plot + theme(legend.position = "none")
) / (
  gamma_results$kmeans_plot + theme(legend.position = "none") +
    multi_rho_results$kmeans_plot + theme(legend.position = "none") +
    beta_results$kmeans_plot + theme(legend.position = "none")
) / (
  gini_results$kmeans_plot + theme(legend.position = "none") +
    gauss_results$kmeans_plot + theme(legend.position = "bottom")
) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 

# For Hierarchical plots
hc_combined <- (
  mean_results$hc_plot + theme(legend.position = "none") +
    var_results$hc_plot + theme(legend.position = "none") +
    dist_results$hc_plot + theme(legend.position = "none")
) / (
  rank_results$hc_plot + theme(legend.position = "none") +
    tau_results$hc_plot + theme(legend.position = "none") +
    rho_results$hc_plot + theme(legend.position = "none")
) / (
  gamma_results$hc_plot + theme(legend.position = "none") +
    multi_rho_results$hc_plot + theme(legend.position = "none") +
    beta_results$hc_plot + theme(legend.position = "none")
) / (
  gini_results$hc_plot + theme(legend.position = "none") +
    gauss_results$hc_plot + theme(legend.position = "bottom")
) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Display both plot sets
kmeans_combined
hc_combined


############################################
## UNIVARIATE FREQUENCY FEATURES ANALYSIS ##
############################################
# 1. Mean features - Frequencies only
mean_freq_features <- grep("^FREQ.*mean$", colnames(feat_left_uni), value = TRUE)
mean_freq_results <- run_clustering(feat_left_uni, mean_freq_features, "Frequency Means")

# 2. Variance features - Frequencies only
var_freq_features <- grep("^FREQ.*variance$", colnames(feat_left_uni), value = TRUE)
var_freq_results <- run_clustering(feat_left_uni, var_freq_features, "Frequency Variances")

# 3. Distribution features - Frequencies only
dist_freq_features <- grep("^FREQ.*(median|iqr|p5|p95)$", colnames(feat_left_uni), value = TRUE)
dist_freq_results <- run_clustering(feat_left_uni, dist_freq_features, "Frequency Distributions")

# 4. Rank features - Frequencies only
rank_freq_features <- grep("^FREQ.*rank[0-9]+$", colnames(feat_left_uni), value = TRUE)
rank_freq_results <- run_clustering(feat_left_uni, rank_freq_features, "Frequency Ranks")

# Create lists for metrics
freq_uni_results <- list(
  "Mean" = mean_freq_results,
  "Variance" = var_freq_results, 
  "Distribution" = dist_freq_results,
  "Rank" = rank_freq_results
)

# Print metrics
print("Frequency Features Metrics:")
print_metrics(freq_uni_results, "Univariate Frequency Features")

# Create combined plots for K-means
freq_kmeans_plot <- (
  mean_freq_results$kmeans_plot + theme(legend.position = "none") +
    var_freq_results$kmeans_plot + theme(legend.position = "none") +
    dist_freq_results$kmeans_plot + theme(legend.position = "none") +
    rank_freq_results$kmeans_plot + theme(legend.position = "bottom")
) +
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom") 

# Create combined plots for Hierarchical
freq_hc_plot <- (
  mean_freq_results$hc_plot + theme(legend.position = "none") +
    var_freq_results$hc_plot + theme(legend.position = "none") +
    dist_freq_results$hc_plot + theme(legend.position = "none") +
    rank_freq_results$hc_plot + theme(legend.position = "bottom")
) +
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom") 


##########################################
## UNIVARIATE SPEECH FEATURES ANALYSIS ##
##########################################
# 1. Mean features - Speech only
mean_speech_features <- grep("^(SRT|SNR).*mean$", colnames(feat_left_uni), value = TRUE)
mean_speech_results <- run_clustering(feat_left_uni, mean_speech_features, "Speech Means")

# 2. Variance features - Speech only
var_speech_features <- grep("^(SRT|SNR).*variance$", colnames(feat_left_uni), value = TRUE)
var_speech_results <- run_clustering(feat_left_uni, var_speech_features, "Speech Variances")

# 3. Distribution features - Speech only
dist_speech_features <- grep("^(SRT|SNR).*(median|iqr|p5|p95)$", colnames(feat_left_uni), value = TRUE)
dist_speech_results <- run_clustering(feat_left_uni, dist_speech_features, "Speech Distributions")

# 4. Rank features - Speech only
rank_speech_features <- grep("^(SRT|SNR).*rank[0-9]+$", colnames(feat_left_uni), value = TRUE)
rank_speech_results <- run_clustering(feat_left_uni, rank_speech_features, "Speech Ranks")

speech_uni_results <- list(
  "Mean" = mean_speech_results,
  "Variance" = var_speech_results,
  "Distribution" = dist_speech_results, 
  "Rank" = rank_speech_results
)

print("\nSpeech Features Metrics:") 
print_metrics(speech_uni_results, "Univariate Speech Features")

# Create combined plots for K-means
speech_kmeans_plot <- (
  mean_speech_results$kmeans_plot + theme(legend.position = "none") +
    var_speech_results$kmeans_plot + theme(legend.position = "none") +
    dist_speech_results$kmeans_plot + theme(legend.position = "none") +
    rank_speech_results$kmeans_plot + theme(legend.position = "bottom")
) +
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom") 

# Create combined plots for Hierarchical
speech_hc_plot <- (
  mean_speech_results$hc_plot + theme(legend.position = "none") +
    var_speech_results$hc_plot + theme(legend.position = "none") +
    dist_speech_results$hc_plot + theme(legend.position = "none") +
    rank_speech_results$hc_plot + theme(legend.position = "bottom")
) +
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom") 

# Display plots
freq_kmeans_plot
freq_hc_plot
speech_kmeans_plot
speech_hc_plot

################################
## FREQUENCY COPULA ANALYSIS  ##
################################
# Helper function to get copula features by type for frequencies only
get_freq_copula_features <- function(pattern) {
  grep(paste0("FREQ.*FREQ.*", pattern), colnames(feat_left_copula), value = TRUE)
}

# Run clustering for each feature type
tau_freq_features <- get_freq_copula_features("tau_cop$")
tau_freq_results <- run_clustering(feat_left_copula, tau_freq_features, "Frequency Tau Copula")

rho_freq_features <- get_freq_copula_features("rho_cop$")
rho_freq_results <- run_clustering(feat_left_copula, rho_freq_features, "Frequency Rho Copula")

gamma_freq_features <- get_freq_copula_features("gamma_cop$")
gamma_freq_results <- run_clustering(feat_left_copula, gamma_freq_features, "Frequency Gamma Copula")

multi_rho_freq_features <- get_freq_copula_features("multi_rho$")
multi_rho_freq_results <- run_clustering(feat_left_copula, multi_rho_freq_features, "Frequency Multi-rho")

beta_freq_features <- get_freq_copula_features("beta_cop$")
beta_freq_results <- run_clustering(feat_left_copula, beta_freq_features, "Frequency Beta Copula")

gini_freq_features <- get_freq_copula_features("gini_alt$")
gini_freq_results <- run_clustering(feat_left_copula, gini_freq_features, "Frequency Gini Alternative")

gauss_freq_features <- get_freq_copula_features("local_gauss$")
gauss_freq_results <- run_clustering(feat_left_copula, gauss_freq_features, "Frequency Local Gaussian")

# Create results lists
freq_copula_results <- list(
  "Tau" = tau_freq_results,
  "Rho" = rho_freq_results,
  "Gamma" = gamma_freq_results,
  "Multi-rho" = multi_rho_freq_results,
  "Beta" = beta_freq_results,
  "Gini" = gini_freq_results,
  "Local Gaussian" = gauss_freq_results
)

# Print metrics
print("Frequency Copula Features Metrics:")
print_metrics(freq_copula_results, "Frequency Copula Features")

# K-means plot grid
kmeans_plots <- wrap_plots(
  list(
    tau_freq_results$kmeans_plot + theme(legend.position = "none"),
    rho_freq_results$kmeans_plot + theme(legend.position = "none"),
    gamma_freq_results$kmeans_plot + theme(legend.position = "none"),
    multi_rho_freq_results$kmeans_plot + theme(legend.position = "none"),
    beta_freq_results$kmeans_plot + theme(legend.position = "none"),
    gini_freq_results$kmeans_plot + theme(legend.position = "none"),
    gauss_freq_results$kmeans_plot + theme(legend.position = "bottom")
  ),
  ncol = 3,
  guides = "collect"
) +
  plot_annotation(title = "Frequency Copula Features - K-means Clustering") &
  theme(legend.position = "bottom")

# Hierarchical plot grid
hc_plots <- wrap_plots(
  list(
    tau_freq_results$hc_plot + theme(legend.position = "none"),
    rho_freq_results$hc_plot + theme(legend.position = "none"),
    gamma_freq_results$hc_plot + theme(legend.position = "none"),
    multi_rho_freq_results$hc_plot + theme(legend.position = "none"),
    beta_freq_results$hc_plot + theme(legend.position = "none"),
    gini_freq_results$hc_plot + theme(legend.position = "none"),
    gauss_freq_results$hc_plot + theme(legend.position = "bottom")
  ),
  ncol = 3,
  guides = "collect"
) +
  plot_annotation(title = "Frequency Copula Features - Hierarchical Clustering") &
  theme(legend.position = "bottom")

# Display plots
kmeans_plots
hc_plots


#############################
## SPEECH COPULA ANALYSIS ##
#############################
# Helper function to get copula features by type for speech only
get_speech_copula_features <- function(pattern) {
  grep(paste0("(SRT|SNR).*", pattern), colnames(feat_left_copula), value = TRUE)
}

# Run clustering for each feature type
tau_speech_features <- get_speech_copula_features("tau_cop$")
tau_speech_results <- run_clustering(feat_left_copula, tau_speech_features, "Speech Tau Copula")

rho_speech_features <- get_speech_copula_features("rho_cop$")
rho_speech_results <- run_clustering(feat_left_copula, rho_speech_features, "Speech Rho Copula")

gamma_speech_features <- get_speech_copula_features("gamma_cop$")
gamma_speech_results <- run_clustering(feat_left_copula, gamma_speech_features, "Speech Gamma Copula")

multi_rho_speech_features <- get_speech_copula_features("multi_rho$")
multi_rho_speech_results <- run_clustering(feat_left_copula, multi_rho_speech_features, "Speech Multi-rho")

beta_speech_features <- get_speech_copula_features("beta_cop$")
beta_speech_results <- run_clustering(feat_left_copula, beta_speech_features, "Speech Beta Copula")

gini_speech_features <- get_speech_copula_features("gini_alt$")
gini_speech_results <- run_clustering(feat_left_copula, gini_speech_features, "Speech Gini Alternative")

gauss_speech_features <- get_speech_copula_features("local_gauss$")
gauss_speech_results <- run_clustering(feat_left_copula, gauss_speech_features, "Speech Local Gaussian")

# Create results list
speech_copula_results <- list(
  "Tau" = tau_speech_results,
  "Rho" = rho_speech_results,
  "Gamma" = gamma_speech_results,
  "Multi-rho" = multi_rho_speech_results,
  "Beta" = beta_speech_results,
  "Gini" = gini_speech_results,
  "Local Gaussian" = gauss_speech_results
)

# Print metrics
print("\nSpeech Copula Features Metrics:") 
print_metrics(speech_copula_results, "Speech Copula Features")

# K-means plot grid
kmeans_speech_plots <- wrap_plots(
  list(
    tau_speech_results$kmeans_plot + theme(legend.position = "none"),
    rho_speech_results$kmeans_plot + theme(legend.position = "none"),
    gamma_speech_results$kmeans_plot + theme(legend.position = "none"),
    multi_rho_speech_results$kmeans_plot + theme(legend.position = "none"),
    beta_speech_results$kmeans_plot + theme(legend.position = "none"),
    gini_speech_results$kmeans_plot + theme(legend.position = "none"),
    gauss_speech_results$kmeans_plot + theme(legend.position = "bottom")
  ),
  ncol = 3,
  guides = "collect"
) +
  plot_annotation(title = "Speech Copula Features - K-means Clustering") &
  theme(legend.position = "bottom")

# Hierarchical plot grid
hc_speech_plots <- wrap_plots(
  list(
    tau_speech_results$hc_plot + theme(legend.position = "none"),
    rho_speech_results$hc_plot + theme(legend.position = "none"),
    gamma_speech_results$hc_plot + theme(legend.position = "none"),
    multi_rho_speech_results$hc_plot + theme(legend.position = "none"),
    beta_speech_results$hc_plot + theme(legend.position = "none"),
    gini_speech_results$hc_plot + theme(legend.position = "none"),
    gauss_speech_results$hc_plot + theme(legend.position = "bottom")
  ),
  ncol = 3,
  guides = "collect"
) +
  plot_annotation(title = "Speech Copula Features - Hierarchical Clustering") &
  theme(legend.position = "bottom")

# Display plots
kmeans_speech_plots
hc_speech_plots



##########################################
##########################################
##########################################
# Individual Feature Clustering Analysis #
##########################################
##########################################
##########################################


run_single_feature_clustering <- function(data, feature, n_clusters = 5) {
  # Create a dataframe with the feature and hearing loss level
  data_subset <- data[, c("HL", feature)]
  
  # Remove rows with NA in the specific feature
  data_subset <- data_subset[!is.na(data_subset[, feature]), ]
  
  # Check if we have enough distinct data points
  unique_vals <- unique(data_subset[, feature])
  if (length(unique_vals) < 2) {
    cat("Skipping", feature, ": Insufficient unique values\n")
    return(NULL)
  }
  
  # Ensure we have at least 2 distinct hearing loss levels
  if (length(unique(data_subset$HL)) < 2) {
    cat("Skipping", feature, ": Insufficient hearing loss levels\n")
    return(NULL)
  }
  
  # Scale the feature
  data_subset[, feature] <- scale(data_subset[, feature])
  
  # Adjust number of clusters if too many
  n_clusters <- min(n_clusters, length(unique(data_subset$HL)))
  
  # Manual clustering visualization if fviz_cluster fails
  manual_cluster_plot <- function(data, clusters, feature_name) {
    plot_data <- data.frame(
      x = data[, feature],
      HL = data$HL,
      Cluster = factor(clusters)
    )
    
    ggplot(plot_data, aes(x = x, y = after_stat(density), fill = Cluster, color = Cluster)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~HL) +
      labs(title = paste("Density Plot for", feature_name),
           x = "Scaled Feature Value",
           y = "Density") +
      theme_minimal() +
      scale_fill_viridis_d(direction = -1) +
      scale_color_viridis_d(direction = -1)
  }
  
  # TryCatch for K-means clustering
  km.res <- tryCatch({
    set.seed(123)
    kmeans(data_subset[, feature], n_clusters, nstart = 25)
  }, error = function(e) {
    cat("K-means error for", feature, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(km.res)) return(NULL)
  
  # Calculate silhouette score
  sil <- silhouette(km.res$cluster, dist(as.matrix(data_subset[, feature])))
  avg_sil <- mean(sil[,3])
  
  # Confusion matrices
  true_labels <- factor(data_subset$HL, levels = hl_levels)
  cluster_to_class_mapping <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")[1:n_clusters]
  
  # K-means confusion matrix
  km_labels_mapped <- factor(cluster_to_class_mapping[km.res$cluster], levels = hl_levels)
  conf_matrix_km <- confusionMatrix(data = km_labels_mapped, reference = true_labels)
  
  # Create plots
  p_km <- manual_cluster_plot(data_subset, km.res$cluster, feature)
  
  return(list(
    feature = feature,
    kmeans_plot = p_km, 
    kmeans_confusion = conf_matrix_km, 
    kmeans_silhouette = avg_sil
  ))
}

analyze_individual_features <- function(data, feature_pattern) {
  # Find all features matching the pattern
  features <- grep(feature_pattern, colnames(data), value = TRUE)
  
  # Run clustering for each feature
  results <- lapply(features, function(feature) {
    run_single_feature_clustering(data, feature)
  })
  
  # Remove any NULL results
  results <- results[!sapply(results, is.null)]
  
  # If no valid results, return NULL
  if (length(results) == 0) {
    cat("No valid features found for clustering\n")
    return(NULL)
  }
  
  # Print metrics for all features
  metrics_df <- do.call(rbind, lapply(results, function(res) {
    data.frame(
      Feature = res$feature,
      KMeans_Accuracy = res$kmeans_confusion$overall["Accuracy"],
      KMeans_Silhouette = res$kmeans_silhouette
    )
  }))
  
  # Sort and print metrics
  print(metrics_df[order(metrics_df$KMeans_Accuracy, decreasing = TRUE), ])
  
  # Create combined plots with error handling
  safe_wrap_plots <- function(plot_list, ncol = 4) {
    # Filter out NULL or invalid plots
    valid_plots <- Filter(function(x) !is.null(x) && inherits(x, "ggplot"), plot_list)
    
    # If no valid plots, return NULL
    if (length(valid_plots) == 0) {
      return(NULL)
    }
    
    # Create combined plot
    tryCatch({
      wrap_plots(valid_plots, ncol = ncol, guides = "collect") +
        plot_layout(guides = "collect")
    }, error = function(e) {
      cat("Error creating combined plot:", e$message, "\n")
      return(NULL)
    })
  }
  
  # Create K-means plots
  kmeans_plots <- safe_wrap_plots(lapply(results, `[[`, "kmeans_plot"))
  
  return(list(
    metrics = metrics_df,
    kmeans_plots = kmeans_plots,
    results = results
  ))
}

# Try running the analysis again
freq_results <- analyze_individual_features(feat_left_uni, "^FREQ")
speech_results <- analyze_individual_features(feat_left_uni, "^(SRT|SNR)")




freq_results$metrics
speech_results$metrics






# Function to analyze a single frequency band
analyze_freq_band <- function(data, freq) {
  # Define feature groups for the frequency band
  feature_groups <- list(
    mean = paste0("FREQ_", freq, "_L.mean"),
    variance = paste0("FREQ_", freq, "_L.variance"),
    distribution = grep(paste0("FREQ_", freq, "_L.(median|iqr|p5|p95)$"), 
                        colnames(data), value = TRUE),
    ranks = grep(paste0("FREQ_", freq, "_L.rank[0-9]+$"), 
                 colnames(data), value = TRUE)
  )
  
  results <- list()
  
  # Run clustering for each feature group
  for (group_name in names(feature_groups)) {
    features <- feature_groups[[group_name]]
    
    if (length(features) > 0) {
      # For single features (mean and variance)
      if (length(features) == 1) {
        result <- run_single_feature_clustering(data, features)
        if (!is.null(result)) {
          results[[group_name]] <- result
        }
      } 
      # For grouped features (distribution and ranks)
      else {
        result <- run_clustering(data, features, 
                                 title = paste("Freq", freq, "Hz -", tools::toTitleCase(group_name)))
        if (!is.null(result)) {
          results[[group_name]] <- result
        }
      }
    }
  }
  
  return(list(
    freq = freq,
    results = results
  ))
}

# Function to print metrics for a frequency band
print_freq_metrics <- function(freq_results) {
  cat("\n=== Analysis for", freq_results$freq, "Hz ===\n")
  
  for (group_name in names(freq_results$results)) {
    cat("\n", tools::toTitleCase(group_name), "features:\n")
    
    result <- freq_results$results[[group_name]]
    
    # Handle single features vs. grouped features
    if ("kmeans_confusion" %in% names(result)) {
      # Single feature
      acc <- result$kmeans_confusion$overall["Accuracy"]
      sil <- result$kmeans_silhouette
      cat(sprintf("Accuracy: %.3f, Silhouette: %.3f\n", acc, sil))
    } else {
      # Grouped features
      acc <- result$kmeans_confusion$overall["Accuracy"]
      sil <- result$kmeans_silhouette
      cat(sprintf("Accuracy: %.3f, Silhouette: %.3f\n", acc, sil))
    }
  }
}

# Function to analyze all frequencies
analyze_all_frequencies <- function(data) {
  frequencies <- c("125", "250", "500", "750", "1000", "1500", 
                   "2000", "3000", "4000", "6000", "8000")
  
  all_results <- list()
  
  for (freq in frequencies) {
    cat("\nAnalyzing frequency:", freq, "Hz\n")
    results <- analyze_freq_band(data, freq)
    print_freq_metrics(results)
    all_results[[freq]] <- results
  }
  
  return(all_results)
}

# Similar function for speech measures
analyze_speech_measure <- function(data, measure) {
  feature_groups <- list(
    mean = paste0(measure, ".mean"),
    variance = paste0(measure, ".variance"),
    distribution = grep(paste0(measure, ".(median|iqr|p5|p95)$"), 
                        colnames(data), value = TRUE),
    ranks = grep(paste0(measure, ".rank[0-9]+$"), 
                 colnames(data), value = TRUE)
  )
  
  results <- list()
  
  for (group_name in names(feature_groups)) {
    features <- feature_groups[[group_name]]
    
    if (length(features) > 0) {
      if (length(features) == 1) {
        result <- run_single_feature_clustering(data, features)
        if (!is.null(result)) {
          results[[group_name]] <- result
        }
      } else {
        result <- run_clustering(data, features, 
                                 title = paste(measure, "-", tools::toTitleCase(group_name)))
        if (!is.null(result)) {
          results[[group_name]] <- result
        }
      }
    }
  }
  
  return(list(
    measure = measure,
    results = results
  ))
}




# Run frequency analysis
freq_results <- analyze_all_frequencies(feat_left_uni)

# Run speech analysis
srt_results <- analyze_speech_measure(feat_left_uni, "SRT")
snr_results <- analyze_speech_measure(feat_left_uni, "SNR")

# Print speech results
cat("\n=== SRT Analysis ===\n")
print_freq_metrics(srt_results)

cat("\n=== SNR Analysis ===\n")
print_freq_metrics(snr_results)




# Function to create summary table of results
create_summary_table <- function(freq_results) {
  # Initialize empty lists for each feature type
  mean_results <- list()
  variance_results <- list()
  distribution_results <- list()
  rank_results <- list()
  
  # Extract results for each frequency
  for (freq in names(freq_results)) {
    res <- freq_results[[freq]]$results
    
    # Extract accuracy and silhouette scores for each feature type
    if (!is.null(res$mean)) {
      mean_results[[freq]] <- c(
        res$mean$kmeans_confusion$overall["Accuracy"],
        res$mean$kmeans_silhouette
      )
    }
    if (!is.null(res$variance)) {
      variance_results[[freq]] <- c(
        res$variance$kmeans_confusion$overall["Accuracy"],
        res$variance$kmeans_silhouette
      )
    }
    if (!is.null(res$distribution)) {
      distribution_results[[freq]] <- c(
        res$distribution$kmeans_confusion$overall["Accuracy"],
        res$distribution$kmeans_silhouette
      )
    }
    if (!is.null(res$ranks)) {
      rank_results[[freq]] <- c(
        res$ranks$kmeans_confusion$overall["Accuracy"],
        res$ranks$kmeans_silhouette
      )
    }
  }
  
  # Create data frames for each feature type
  create_df <- function(results_list, feature_type) {
    if (length(results_list) == 0) return(NULL)
    
    df <- do.call(rbind, results_list)
    df <- as.data.frame(df)
    colnames(df) <- c("Accuracy", "Silhouette")
    df$Frequency <- names(results_list)
    df$FeatureType <- feature_type
    return(df)
  }
  
  # Combine all results
  results_df <- rbind(
    create_df(mean_results, "Mean"),
    create_df(variance_results, "Variance"),
    create_df(distribution_results, "Distribution"),
    create_df(rank_results, "Ranks")
  )
  
  # Add row names and sort by frequency
  results_df$Frequency <- factor(results_df$Frequency, 
                                 levels = c("125", "250", "500", "750", "1000", 
                                            "1500", "2000", "3000", "4000", "6000", "8000"))
  results_df <- results_df[order(results_df$Frequency), ]
  
  return(results_df)
}

# Function to create visualization of results
create_summary_plots <- function(results_df) {
  # Accuracy plot
  p1 <- ggplot(results_df, aes(x = Frequency, y = Accuracy, color = FeatureType, group = FeatureType)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Clustering Accuracy by Frequency",
         y = "Accuracy",
         x = "Frequency (Hz)") +
    scale_color_viridis_d()
  
  # Silhouette plot
  p2 <- ggplot(results_df, aes(x = Frequency, y = Silhouette, color = FeatureType, group = FeatureType)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Silhouette Score by Frequency",
         y = "Silhouette Score",
         x = "Frequency (Hz)") +
    scale_color_viridis_d()
  
  # Combine plots
  combined_plot <- (p1 / p2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  return(combined_plot)
}

# Function to summarize both frequency and speech results
summarize_all_results <- function(freq_results, srt_results, snr_results) {
  # Create frequency summary table
  freq_summary <- create_summary_table(freq_results)
  
  # Create plots for frequency results
  freq_plots <- create_summary_plots(freq_summary)
  
  # Create summary tables for speech measures
  srt_summary <- data.frame(
    Measure = "SRT",
    FeatureType = names(srt_results$results),
    Accuracy = sapply(srt_results$results, function(x) x$kmeans_confusion$overall["Accuracy"]),
    Silhouette = sapply(srt_results$results, function(x) x$kmeans_silhouette)
  )
  
  snr_summary <- data.frame(
    Measure = "SNR",
    FeatureType = names(snr_results$results),
    Accuracy = sapply(snr_results$results, function(x) x$kmeans_confusion$overall["Accuracy"]),
    Silhouette = sapply(snr_results$results, function(x) x$kmeans_silhouette)
  )
  
  speech_summary <- rbind(srt_summary, snr_summary)
  
  return(list(
    frequency_summary = freq_summary,
    frequency_plots = freq_plots,
    speech_summary = speech_summary
  ))
}


# Get all results
all_results <- summarize_all_results(freq_results, srt_results, snr_results)

# Print frequency results table
cat("\n=== Frequency Analysis Results ===\n")
print(all_results$frequency_summary)

# Print speech results table
cat("\n=== Speech Analysis Results ===\n")
print(all_results$speech_summary)

# Display plots
all_results$frequency_plots




