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



#########
# Utils #
#########



prepare_data <- function(mydir, mydir2, mydir_figs) {
  source(paste0(mydir2, "utils.R"))
  
  data_ampl2 <- readRDS(file = paste0(mydir, "data_ampl2bis.rds"))
  data_ampl2 <- data_ampl2[data_ampl2$AGE <= 89 & data_ampl2$AGE >= 45, ]
  
  data_ampl2$age_group2 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 5),
                               labels = FALSE, include.lowest = TRUE)
  data_ampl2$age_group2 <- factor(paste0((data_ampl2$age_group2 - 1) * 5, "-",
                                         (data_ampl2$age_group2) * 5))
  
  data_ampl2$age_group3 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 1),
                               labels = FALSE, include.lowest = TRUE)
  data_ampl2$age_group3 <- factor(paste0((data_ampl2$age_group3 - 1), "-",
                                         (data_ampl2$age_group3)))
  
  data_lee_carter <- data_ampl2[, c(3, 4, 7:17, 34, 35, 36, 29)]
  data_lee_carter$SEXE <- as.factor(data_lee_carter$SEXE)
  
  spiq <- data_ampl2[, c(3, 4, 30, 34, 35, 36, 29)]
  spin <- data_ampl2[, c(3, 4, 31, 34, 35, 36, 29)]
  
  PTA_df <- data.frame(PTA = data_ampl2$PTA)
  SRT_df <- data.frame(SRT = data_ampl2$SRT)
  SNR_df <- data.frame(SNR = data_ampl2$SNR)
  
  hl_classes <- levels(data_ampl2$Classification)
  n_hlclasses <- length(hl_classes)
  
  freq_vector <- sapply(1:11, function(i) 
    str_split(colnames(data_lee_carter)[2 + i], "_")[[1]][2])
  n_freq <- length(freq_vector)
  
  age_groups <- levels(data_lee_carter$age_group)[5:9]
  n_age_g <- length(age_groups)
  
  age_groups2 <- levels(data_lee_carter$age_group2)
  n_age_g2 <- length(age_groups2)
  
  all_age_groups <- levels(data_lee_carter$age_group3)
  n_age_g3 <- length(all_age_groups)
  
  decision_threshold <- data.frame(PTA = data_ampl2$PTA, 
                                   SRT = data_ampl2$SRT, 
                                   SNR = data_ampl2$SNR)
  
  quant_tresh <- c(0.2, 0.4, 0.6, 0.8, 0.9)
  quant_PTA <- quantile(decision_threshold$PTA, 
                        probs = quant_tresh,
                        na.rm = TRUE) 
  quant_SRT <- quantile(decision_threshold$SRT, 
                        probs = quant_tresh, na.rm = TRUE) 
  quant_SNR <- quantile(decision_threshold$SNR, 
                        probs = quant_tresh, na.rm = TRUE)
  
  tres_pta_srt <- data.frame(PTA = quant_PTA, SRT = quant_SRT)
  tres_pta_snr <- data.frame(PTA = quant_PTA, SNR = quant_SNR)
  tres_srt_snr <- data.frame(SRT = quant_SRT, SNR = quant_SNR)
  
  freq_tresh <- do.call(rbind, lapply(data_ampl2[, 7:17], quantile, 
                                      quant_tresh, 2))
  
  treshold_hearing_rate = as.integer(tres_pta_snr[,1])
  n_tresh = length(treshold_hearing_rate)
  
  treshold_SPIQ = as.integer(tres_pta_srt[,2])
  treshold_SPIN = as.integer(tres_pta_snr[,2])
  
  freq_tresh_plot = c( paste(20,"%", sep = ""), paste(40,"%", sep = ""),
                       paste(60,"%", sep = ""), paste(80,"%", sep = ""),
                       paste(90,"%", sep = "")) 
  
  
  thresh_colors = c("#F5F5F5", "#FFDDC1", "#FF8C69", "#FF6347", "#FF0000")
  hearing_loss_col = c("#D0F0C0","#A1D99B","#31A354","#006837","#004529") 
  
  sex_classes = c("Female", 'Male')
  
  list(
    data_ampl2 = data_ampl2,
    data_lee_carter = data_lee_carter,
    spiq = spiq,
    spin = spin,
    PTA_df = PTA_df,
    SRT_df = SRT_df,
    SNR_df = SNR_df,
    freq_tresh = freq_tresh,
    hl_classes = hl_classes,
    n_hlclasses = n_hlclasses,
    freq_vector = freq_vector,
    n_freq = n_freq,
    age_groups = age_groups,
    age_groups2 = age_groups2,
    all_age_groups = all_age_groups,
    n_age_g =n_age_g,
    n_age_g2 = n_age_g2,
    n_age_g3 = n_age_g3,
    tres_pta_snr = tres_pta_snr,
    tres_pta_srt = tres_pta_srt,
    treshold_hearing_rate = treshold_hearing_rate,
    treshold_SPIQ = treshold_SPIQ,
    treshold_SPIN = treshold_SPIN,
    freq_tresh_plot = freq_tresh_plot,
    thresh_colors = thresh_colors,
    hearing_loss_col = hearing_loss_col,
    n_tresh = n_tresh,
    sex_classes = sex_classes
  )
}



generate_comparison_plots = function(data, comparisons) {
  plots = list()
  
  for(comp in comparisons) {
    filtered_data = data %>% 
      filter(contrast1 == comp | contrast2 == comp)
    
    unique_vars = unique(unlist(strsplit(filtered_data$varname, "\\|")))
    
    toplot = data.frame(
      group_id = comp,
      desc = paste(comp, "vs Slight"),
      varname = unique_vars,
      stringsAsFactors = FALSE
    )
    
    tbl = data.frame(
      varname = filtered_data$varname,
      stat = filtered_data$stat,
      p = filtered_data$p,
      p.adj = filtered_data$p.adj
    )
    
    result = list(tbl = tbl)
    
    plots[[comp]] = netvis(obj = result, ref = toplot)
  }
  
  return(plots)
}


analyze_frequency_relationships = function(data) {
  freq_significance = data %>%
    mutate(
      frequencies = strsplit(varname, "\\|")
    ) %>%
    unnest(frequencies) %>%
    group_by(frequencies, contrast1, contrast2) %>%
    summarise(
      total_connections = n(),
      significant_connections = sum(p < 0.05),
      prop_significant = significant_connections / total_connections * 100,
      avg_p_value = mean(p),
      min_p_value = min(p),
      max_p_value = max(p)
    ) %>%
    arrange(desc(prop_significant))
  
  return(freq_significance)
}



split_at_second_occurrence <- function(string, delimiter) {
  # Find positions of all occurrences of the delimiter
  positions <- gregexpr(delimiter, string)[[1]]
  
  # If there are at least two occurrences, split at the second one
  if (length(positions) >= 2) {
    split_position <- positions[2]
    part1 <- substr(string, 1, split_position - 1)
    part2 <- substr(string, split_position + 1, nchar(string))
    return(c(part1, part2))
  } else {
    # If there are less than two occurrences, return the original string
    return(c(string, NA))
  }
}


standardize_combination <- function(combination) {
  # Split the combination into two parts
  parts <- str_split(combination, " - ", simplify = TRUE)
  # Order the parts based on severity levels
  ordered_parts <- sort(parts, index.return = TRUE, decreasing = FALSE, method = "radix")
  # Join the ordered parts back together
  standardized_combination <- paste(ordered_parts$x, collapse = " - ")
  return(standardized_combination)
}


remove_identical_combinations <- function(combination) {
  # Split the combination into two parts
  parts <- str_split(combination, " - ", simplify = TRUE)
  # Check if the two parts are identical
  return(parts[1] != parts[2])
}

characterize_speech_tests <- function(data) {
  # Separate by hearing loss category
  category_summaries <- lapply(unique(data$HL), function(category) {
    cat_data <- data[data$HL == category, -1]
    
    list(
      category = category,
      mean_profile = colMeans(cat_data),
      variance_profile = apply(cat_data, 2, var),
      correlation_structure = cor(cat_data)
    )
  })
  names(category_summaries) <- unique(data$HL)
  
  # Visualization of speech test variability
  speech_var <- data.frame(
    Category = unique(data$HL),
    SRT_Mean = sapply(category_summaries, function(x) x$mean_profile[1]),
    SNR_Mean = sapply(category_summaries, function(x) x$mean_profile[2]),
    SRT_Var = sapply(category_summaries, function(x) x$variance_profile[1]),
    SNR_Var = sapply(category_summaries, function(x) x$variance_profile[2])
  )
  
  # Plot variability across categories
  p <- ggplot(speech_var, aes(x = factor(Category, levels =c('Slight', 'Mild', 'Moderate',
                                                             'Moderately severe', 'Severe') ))) +
    geom_point(aes(y = SRT_Mean, color = "SRT Mean"), size = 3) +
    geom_point(aes(y = SNR_Mean, color = "SNR Mean"), size = 3) +
    geom_errorbar(aes(ymin = SRT_Mean - SRT_Var, ymax = SRT_Mean + SRT_Var), 
                  width = 0.2, color = "red", alpha = 0.5) +
    geom_errorbar(aes(ymin = SNR_Mean - SNR_Var, ymax = SNR_Mean + SNR_Var), 
                  width = 0.2, color = "blue", alpha = 0.5) +
    theme_minimal() +
    labs(title = "Speech Test Variability Across Hearing Loss Categories",
         y = "Value",
         x="Hearing Loss")
  
  return(list(
    category_summaries = category_summaries,
    variability_plot = p
  ))
}

# Function to perform t-SNE and plot
# perform_tsne <- function(data, title = "t-SNE") {
#   # Preprocessing
#   data_unique <- data %>%
#     drop_na() %>%
#     unique() %>%
#     group_by(across(2:ncol(data))) %>% 
#     slice(1) %>%
#     ungroup()
#   
#   # Ensure unique column names
#   data_unique <- unique_column_names(data_unique)
#   
#   # t-SNE
#   tsne_result <- Rtsne(data_unique[,2:ncol(data_unique)],
#                        dims = 2,
#                        pca = TRUE,
#                        perplexity = 5)
#   
#   # Plotting
#   toplot <- as.data.frame(tsne_result$Y)
#   
#   hl_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
#   data_unique$HL <- factor(data_unique$HL, levels = hl_levels)
#   
#   color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
#                             levels(data_unique$HL))
#   
#   toplot$HL <- data_unique$HL
#   
#   p <- ggplot(toplot, aes(x = V1, y = V2, color = HL)) +
#     geom_point(size = 3) +
#     scale_color_manual(values = color_mapping) +
#     theme_bw() +
#     ggtitle(title) +
#     theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank(),
#           legend.position = "bottom")
#   
#   return(p)
# }

# perform_tsne <- function(data, title = "t-SNE") {
#   # Preprocessing
#   data_unique <- data %>%
#     drop_na() %>%
#     # Add small random noise to prevent exact duplicates
#     mutate(across(-HL, ~. + rnorm(n(), mean = 0, sd = 1e-10)))
#   
#   # Print info for debugging
#   cat("Number of samples:", nrow(data_unique), "\n")
#   
#   # Use perplexity = 30 for 250 samples
#   perplexity <- 30
#   
#   # t-SNE
#   tsne_result <- Rtsne(data_unique[,2:ncol(data_unique)],
#                        dims = 2,
#                        pca = TRUE,
#                        perplexity = perplexity)
#   
#   # Plotting
#   toplot <- as.data.frame(tsne_result$Y)
#   
#   hl_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
#   data_unique$HL <- factor(data_unique$HL, levels = hl_levels)
#   
#   color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
#                            levels(data_unique$HL))
#   
#   toplot$HL <- data_unique$HL
#   
#   p <- ggplot(toplot, aes(x = V1, y = V2, color = HL)) +
#     geom_point(size = 3) +
#     scale_color_manual(values = color_mapping) +
#     theme_bw() +
#     ggtitle(title) +
#     theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank(),
#           legend.position = "bottom")
#   
#   return(p)
# }
# 
# 
# 
# robust_tsne <- function(data, dims = 2, perplexity = 5) {
#   # Remove any columns with zero variance
#   var_cols <- apply(data, 2, var, na.rm = TRUE)
#   data_filtered <- data[, var_cols > 0]
#   
#   # Check if we have enough data
#   if(nrow(data_filtered) < 2 || ncol(data_filtered) < 2) {
#     warning("Insufficient data for t-SNE")
#     return(NULL)
#   }
#   
#   # Try t-SNE with error handling
#   tryCatch({
#     tsne_result <- Rtsne(data_filtered, 
#                          dims = dims, 
#                          pca = TRUE, 
#                          perplexity = min(perplexity, floor(nrow(data_filtered)/3)))
#     return(tsne_result)
#   }, error = function(e) {
#     warning(paste("t-SNE failed:", e$message))
#     return(NULL)
#   })
# }


# # Quantitative Assessment
# assess_discrimination <- function(data) {
#   # Preprocessing
#   data_unique <- data %>%
#     drop_na() %>%
#     unique() %>%
#     group_by(across(2:ncol(data))) %>% 
#     slice(1) %>%
#     ungroup()
#   
#   # Ensure unique column names
#   data_unique <- unique_column_names(data_unique)
#   
#   # Perform t-SNE
#   tsne_result <- Rtsne(data_unique[,2:ncol(data_unique)],
#                        dims = 2,
#                        pca = TRUE,
#                        perplexity = 5)
#   
#   # Calculate separation between classes
#   tsne_coords <- tsne_result$Y
#   class_centers <- aggregate(tsne_coords, by = list(data_unique$HL), mean)
#   
#   # Calculate between-class distances
#   dist_matrix <- as.matrix(dist(class_centers[,-1]))
#   
#   return(list(
#     mean_inter_class_distance = mean(dist_matrix[upper.tri(dist_matrix)]),
#     tsne_result = tsne_result
#   ))
# }

# Modified assess_tsne_discrimination function
# assess_tsne_discrimination <- function(data, perplexity = 5) {
#   # Ensure data is preprocessed
#   data_unique <- data %>%
#     drop_na() %>%
#     unique() %>%
#     group_by(across(2:ncol(data))) %>% 
#     slice(1) %>%
#     ungroup()
#   
#   # Separate features and labels
#   features <- data_unique[, 2:ncol(data_unique)]
#   labels <- data_unique$HL
#   
#   # Perform t-SNE
#   tsne_result <- robust_tsne(features, perplexity = perplexity)
#   
#   # Check if t-SNE was successful
#   if(is.null(tsne_result)) {
#     warning("t-SNE failed for this feature set")
#     return(NULL)
#   }
#   
#   # Convert t-SNE results to dataframe
#   tsne_df <- data.frame(
#     V1 = tsne_result$Y[,1],
#     V2 = tsne_result$Y[,2],
#     HL = labels
#   )
#   
#   # Visualization with color-coded hearing loss levels
#   hl_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
#   tsne_df$HL <- factor(tsne_df$HL, levels = hl_levels)
#   
#   color_mapping <- setNames(
#     c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
#     hl_levels
#   )
#   
#   # Visualization
#   discrimination_plot <- ggplot(tsne_df, 
#                                 aes(x = V1, 
#                                     y = V2, color = HL)) +
#     geom_point(size = 3) +
#     scale_color_manual(values = color_mapping) +
#     stat_ellipse(aes(fill = HL), type = "norm", 
#                  geom = "polygon", alpha = 0.1) +
#     theme_minimal() +
#     labs(title = "t-SNE Visualization with Confidence Ellipses")
#   
#   # Compute separation metrics
#   separation_metrics <- function(df) {
#     # Calculate centroid for each group
#     centroids <- df %>%
#       group_by(HL) %>%
#       summarise(
#         centroid_x = mean(V1),
#         centroid_y = mean(V2)
#       )
#     
#     # Compute inter-group distances
#     inter_group_distances <- as.matrix(dist(centroids[,c("centroid_x", "centroid_y")]))
#     
#     # Within-group variance
#     within_group_variance <- df %>%
#       group_by(HL) %>%
#       summarise(
#         variance_x = var(V1),
#         variance_y = var(V2)
#       )
#     
#     # Silhouette score
#     library(cluster)
#     silhouette_score <- silhouette(
#       as.numeric(factor(df$HL)), 
#       dist(df[,c("V1", "V2")])
#     )
#     
#     return(list(
#       inter_group_distances = inter_group_distances,
#       within_group_variance = within_group_variance,
#       mean_silhouette = mean(silhouette_score[,3])
#     ))
#   }
#   
#   # Compute metrics
#   metrics <- separation_metrics(tsne_df)
#   
#   return(list(
#     tsne_plot = discrimination_plot,
#     tsne_data = tsne_df,
#     metrics = metrics
#   ))
# }

#################################
# Core Functions for Assessment #
#################################

# Helper function for data preprocessing
preprocess_features <- function(data, feature_type = "univariate") {
  if(!"HL" %in% colnames(data)) {
    stop("HL column missing from data")
  }
  
  # Check if we have any features besides HL
  if(ncol(data) <= 1) {
    warning("No features besides HL column")
    return(data)
  }
  
  # Remove any constant columns
  feature_cols <- colnames(data)[colnames(data) != "HL"]
  if(length(feature_cols) > 0) {
    var_cols <- apply(data[, feature_cols, drop = FALSE], 2, var, na.rm = TRUE)
    constant_cols <- names(var_cols[var_cols == 0 | is.na(var_cols)])
    if(length(constant_cols) > 0) {
      data <- data[, !colnames(data) %in% constant_cols]
    }
  }
  
  # Handle missing values
  data <- data %>% drop_na()
  
  # Scale features based on type
  if(ncol(data) > 1) {  # Only scale if we have features
    if(feature_type == "univariate") {
      feature_cols <- colnames(data)[colnames(data) != "HL"]
      scaled_features <- scale(data[, feature_cols, drop = FALSE])
      data[, feature_cols] <- scaled_features
    } else if(feature_type == "copula") {
      feature_cols <- colnames(data)[colnames(data) != "HL"]
      data[, feature_cols] <- apply(
        data[, feature_cols, drop = FALSE], 2, 
        function(x) (x - min(x))/(max(x) - min(x))
      )
    }
  }
  
  return(data)
}

# Enhanced t-SNE assessment function
assess_tsne_discrimination <- function(data, 
                                       perplexity = 5, 
                                       feature_type = "univariate",
                                       title = NULL) {
  
  # Ensure data is a data frame
  data <- as.data.frame(data)
  
  # Check if HL column exists
  if(!"HL" %in% colnames(data)) {
    stop("HL column missing from data")
  }
  
  # Get features (all columns except HL)
  features <- data[, !colnames(data) %in% "HL", drop = FALSE]
  
  # Check if we have enough features
  if(ncol(features) < 1) {
    warning("No features available for analysis")
    return(NULL)
  }
  
  # Scale features
  features_scaled <- scale(features)
  
  # Handle NAs
  features_scaled[is.na(features_scaled)] <- 0
  
  # Add small noise if only one feature
  if(ncol(features_scaled) == 1) {
    features_scaled <- cbind(features_scaled, 
                             rnorm(nrow(features_scaled), 0, 0.0001))
  }
  
  # Perform t-SNE
  tsne_result <- tryCatch({
    Rtsne(features_scaled, 
          perplexity = min(perplexity, floor((nrow(features_scaled) - 1) / 3)),
          check_duplicates = FALSE)
  }, error = function(e) {
    warning(paste("t-SNE failed:", e$message))
    return(NULL)
  })
  
  if(is.null(tsne_result)) return(NULL)
  
  # Create visualization dataframe
  tsne_df <- data.frame(
    V1 = tsne_result$Y[,1],
    V2 = tsne_result$Y[,2],
    HL = data$HL
  )
  
  # Order HL factor levels
  hl_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
  tsne_df$HL <- factor(tsne_df$HL, levels = hl_levels)
  
  make_title_case <- function(text) {
    words <- strsplit(text, "_")[[1]]
    words <- sapply(words, function(word) {
      paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
    })
    paste(words, collapse = " ")
  }
  
  measure_names <- c(
    "tau_cop" = "Kendall's Tau",
    "rho_cop" = "Spearman's Rho",
    "gamma_cop" = "Gamma Coefficient",
    "multi_rho" = "Multivariate Rho",
    "beta_cop" = "Beta Measure",
    "gini_alt" = "Gini Index",
    "local_gauss" = "Local Gaussian",
    "lower_tail_dep" = "Lower Tail Dependence",
    "upper_tail_dep" = "Upper Tail Dependence"
  )
  
  # Modify the plot_title creation:
  plot_title <- if(is.null(title)) {
    make_title_case(feature_type)
  } else if(title %in% names(measure_names)) {
    measure_names[title]
  } else {
    make_title_case(title)
  }
  
  # Create plot
  # plot_title <- ifelse(is.null(title), 
  #                      paste(feature_type), 
  #                      paste(title))
  
  p <- ggplot(tsne_df, aes(x = V1, y = V2, color = HL)) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_viridis_d(direction = -1) +  # direction = -1 makes the palette go from light to dark
    theme_bw() +
    labs(
      title = plot_title,
      x = "t-SNE Dimension 1",
      y = "t-SNE Dimension 2"
      ) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10),
          panel.grid.major = element_blank(), # element_line(color = "gray90"),
          panel.grid.minor = element_blank())
  
  return(list(
    plot = p,
    data = tsne_df,
    n_features = ncol(features)
  ))
}


# Separation metrics computation
compute_separation_metrics <- function(df) {
  tryCatch({
    # Check if we have enough data
    if(nrow(df) < 3 || ncol(df) < 3) {
      return(list(
        inter_group_distances = NA,
        within_group_variance = NA,
        mean_silhouette = NA,
        davies_bouldin = NA,
        n_points = nrow(df)
      ))
    }
    
    # Centroids
    centroids <- df %>%
      group_by(HL) %>%
      summarise(
        centroid_x = mean(V1),
        centroid_y = mean(V2)
      )
    
    # Inter-group distances
    inter_group_distances <- as.matrix(dist(centroids[,c("centroid_x", "centroid_y")]))
    
    # Within-group variance
    within_group_variance <- df %>%
      group_by(HL) %>%
      summarise(
        variance_x = var(V1),
        variance_y = var(V2),
        total_variance = var(V1) + var(V2)
      )
    
    # Silhouette score
    silhouette_score <- tryCatch({
      if(length(unique(df$HL)) > 1) {
        cluster::silhouette(
          as.numeric(factor(df$HL)), 
          dist(df[,c("V1", "V2")])
        )
      } else {
        return(NA)
      }
    }, error = function(e) NA)
    
    # Calculate mean silhouette
    mean_silhouette <- if(!is.na(silhouette_score)[1]) {
      mean(silhouette_score[,3])
    } else {
      NA
    }
    
    # Davies-Bouldin index
    db_index <- tryCatch({
      if(length(unique(df$HL)) > 1) {
        cluster::index.DB(df[,c("V1", "V2")], 
                          as.numeric(factor(df$HL)))$DB
      } else {
        NA
      }
    }, error = function(e) NA)
    
    return(list(
      inter_group_distances = inter_group_distances,
      within_group_variance = within_group_variance,
      mean_silhouette = mean_silhouette,
      davies_bouldin = db_index,
      n_points = nrow(df)
    ))
  }, error = function(e) {
    warning(paste("Error in compute_separation_metrics:", e$message))
    return(list(
      inter_group_distances = NA,
      within_group_variance = NA,
      mean_silhouette = NA,
      davies_bouldin = NA,
      n_points = nrow(df)
    ))
  })
}


###########################################
# Feature Organization and Extraction     #
###########################################

# Helper function to ensure unique column names
unique_column_names <- function(df) {
  colnames(df) <- make.unique(colnames(df))
  return(df)
}

# Function to extract univariate features
organize_univariate_features <- function(feat_left_uni) {
  # First, separate features by type
  mean_cols <- grep("mean", colnames(feat_left_uni), value = TRUE)
  var_cols <- grep("variance", colnames(feat_left_uni), value = TRUE)
  dist_cols <- grep("(median|iqr|p5|p95)", colnames(feat_left_uni), value = TRUE)
  rank_cols <- grep("rank", colnames(feat_left_uni), value = TRUE)
  
  # Separate frequency and speech features
  features_by_type <- list(
    mean = list(
      freq = data.frame(
        feat_left_uni[, mean_cols[grep("FREQ", mean_cols)]],
        HL = feat_left_uni$HL
      ),
      speech = data.frame(
        feat_left_uni[, mean_cols[grep("(SRT|SNR)", mean_cols)]],
        HL = feat_left_uni$HL
      )
    ),
    variance = list(
      freq = data.frame(
        feat_left_uni[, var_cols[grep("FREQ", var_cols)]],
        HL = feat_left_uni$HL
      ),
      speech = data.frame(
        feat_left_uni[, var_cols[grep("(SRT|SNR)", var_cols)]],
        HL = feat_left_uni$HL
      )
    ),
    distribution = list(
      freq = data.frame(
        feat_left_uni[, dist_cols[grep("FREQ", dist_cols)]],
        HL = feat_left_uni$HL
      ),
      speech = data.frame(
        feat_left_uni[, dist_cols[grep("(SRT|SNR)", dist_cols)]],
        HL = feat_left_uni$HL
      )
    ),
    ranks = list(
      freq = data.frame(
        feat_left_uni[, rank_cols[grep("FREQ", rank_cols)]],
        HL = feat_left_uni$HL
      ),
      speech = data.frame(
        feat_left_uni[, rank_cols[grep("(SRT|SNR)", rank_cols)]],
        HL = feat_left_uni$HL
      )
    )
  )
  
  # Group by individual frequencies
  freq_values <- c("125", "250", "500", "750", "1000", "1500", 
                   "2000", "3000", "4000", "6000", "8000")
  
  features_by_freq <- lapply(freq_values, function(freq) {
    pattern <- paste0("FREQ_", freq)
    list(
      mean = data.frame(
        feat_left_uni[, grep(paste0(pattern, ".*mean"), colnames(feat_left_uni), value = TRUE)],
        HL = feat_left_uni$HL
      ),
      variance = data.frame(
        feat_left_uni[, grep(paste0(pattern, ".*variance"), colnames(feat_left_uni), value = TRUE)],
        HL = feat_left_uni$HL
      ),
      distribution = data.frame(
        feat_left_uni[, grep(paste0(pattern, ".*(median|iqr|p5|p95)"), colnames(feat_left_uni), value = TRUE)],
        HL = feat_left_uni$HL
      ),
      ranks = data.frame(
        feat_left_uni[, grep(paste0(pattern, ".*rank"), colnames(feat_left_uni), value = TRUE)],
        HL = feat_left_uni$HL
      )
    )
  })
  names(features_by_freq) <- freq_values
  
  # Print feature counts for verification
  print("Feature counts by type:")
  for(type in names(features_by_type)) {
    print(paste(type, "freq:", ncol(features_by_type[[type]]$freq) - 1))  # -1 for HL
    print(paste(type, "speech:", ncol(features_by_type[[type]]$speech) - 1))
  }
  
  return(list(
    by_type = features_by_type,
    by_freq = features_by_freq
  ))
}

# Modified organize_copula_features function
organize_copula_features <- function(feat_left_copula) {
  # Organize by copula measure type - added tail dependence measures
  copula_measures <- c("tau_cop", "rho_cop", "gamma_cop", "multi_rho", 
                       "beta_cop", "gini_alt", "local_gauss",
                       "lower_tail_dep", "upper_tail_dep")  # Added these two
  
  features_by_measure <- lapply(copula_measures, function(measure) {
    data.frame(
      feat_left_copula[, grep(measure, colnames(feat_left_copula), value = TRUE)],
      HL = feat_left_copula$HL
    )
  })
  names(features_by_measure) <- copula_measures
  
  # Rest of the function remains the same
  freq_values <- c("125", "250", "500", "750", "1000", "1500", 
                   "2000", "3000", "4000", "6000", "8000")
  
  freq_pairs <- combn(freq_values, 2, simplify = FALSE)
  features_by_pairs <- lapply(freq_pairs, function(pair) {
    pattern <- paste0("FREQ_", pair[1], ".*FREQ_", pair[2])
    data.frame(
      feat_left_copula[, grep(pattern, colnames(feat_left_copula), value = TRUE)],
      HL = feat_left_copula$HL
    )
  })
  names(features_by_pairs) <- sapply(freq_pairs, paste, collapse = "_")
  
  # Print feature counts for verification
  print("\nCopula feature counts:")
  for(measure in names(features_by_measure)) {
    print(paste(measure, "features:", ncol(features_by_measure[[measure]]) - 1))
  }
  
  return(list(
    by_measure = features_by_measure,
    by_pairs = features_by_pairs
  ))
}

###########################################
# Analysis Pipeline and Main Execution    #
###########################################

# Main analysis function for univariate features
analyze_univariate_features <- function(organized_features, perplexity = 5) {
  # Initialize lists to store results
  type_analyses <- list()
  
  # Analyze by type
  for(type_name in names(organized_features$by_type)) {
    # Check freq features
    if(ncol(organized_features$by_type[[type_name]]$freq) > 1) {
      tryCatch({
        type_analyses[[paste0(type_name, "_freq")]] <- assess_tsne_discrimination(
          as.data.frame(organized_features$by_type[[type_name]]$freq),
          perplexity = perplexity,
          feature_type = "univariate",
          title = paste(type_name, "frequency features")
        )
      }, error = function(e) {
        warning(paste("Analysis failed for", type_name, "freq:", e$message))
        NULL
      })
    }
    
    # Check speech features
    if(ncol(organized_features$by_type[[type_name]]$speech) > 1) {
      tryCatch({
        type_analyses[[paste0(type_name, "_speech")]] <- assess_tsne_discrimination(
          as.data.frame(organized_features$by_type[[type_name]]$speech),
          perplexity = perplexity,
          feature_type = "univariate",
          title = paste(type_name, "speech features")
        )
      }, error = function(e) {
        warning(paste("Analysis failed for", type_name, "speech:", e$message))
        NULL
      })
    }
  }
  
  return(list(
    type_analyses = type_analyses[!sapply(type_analyses, is.null)]
  ))
}

# Modified analyze_copula_features function
analyze_copula_features <- function(organized_features, perplexity = 5) {
  # Initialize lists to store results
  measure_analyses <- list()
  
  # Analyze by measure
  for(measure_name in names(organized_features$by_measure)) {
    if(ncol(organized_features$by_measure[[measure_name]]) > 1) {
      tryCatch({
        measure_analyses[[measure_name]] <- assess_tsne_discrimination(
          as.data.frame(organized_features$by_measure[[measure_name]]),
          perplexity = perplexity,
          feature_type = "copula",
          title = paste(measure_name, "features")
        )
      }, error = function(e) {
        warning(paste("Analysis failed for measure", measure_name, ":", e$message))
        NULL
      })
    }
  }
  
  return(list(
    measure_analyses = measure_analyses[!sapply(measure_analyses, is.null)]
  ))
}

run_complete_analysis <- function(feat_left_uni, feat_left_copula, perplexity = 5) {
  # Organize features
  cat("Organizing features...\n")
  uni_features <- organize_univariate_features(feat_left_uni)
  cop_features <- organize_copula_features(feat_left_copula)
  
  # Run univariate analyses
  cat("Running univariate analyses...\n")
  uni_results <- tryCatch({
    analyze_univariate_features(uni_features, perplexity)
  }, error = function(e) {
    warning(paste("Univariate analysis failed:", e$message))
    list(type_analyses = list())  # Return empty list instead of NULL
  })
  
  # Run copula analyses
  cat("Running copula analyses...\n")
  cop_results <- tryCatch({
    analyze_copula_features(cop_features, perplexity)
  }, error = function(e) {
    warning(paste("Copula analysis failed:", e$message))
    list(measure_analyses = list())  # Return empty list instead of NULL
  })
  
  # Create result summaries
  summary_stats <- list(
    univariate = if(length(uni_results$type_analyses) > 0) {
      data.frame(
        Analysis = names(uni_results$type_analyses),
        Features = sapply(uni_results$type_analyses, function(x) x$n_features),
        Samples = sapply(uni_results$type_analyses, function(x) nrow(x$data))
      )
    } else {
      data.frame(Analysis = character(), Features = numeric(), Samples = numeric())
    },
    
    copula = if(length(cop_results$measure_analyses) > 0) {
      data.frame(
        Analysis = names(cop_results$measure_analyses),
        Features = sapply(cop_results$measure_analyses, function(x) x$n_features),
        Samples = sapply(cop_results$measure_analyses, function(x) nrow(x$data))
      )
    } else {
      data.frame(Analysis = character(), Features = numeric(), Samples = numeric())
    }
  )
  
  # Create visualizations
  visualizations <- list(
    univariate = list(
      type = if(length(uni_results$type_analyses) > 0) {
        wrap_plots(lapply(uni_results$type_analyses, function(x) x$plot), 
                   ncol = 3) +
          plot_layout(guides = "collect") & 
          theme(legend.position = "bottom")
      } else NULL
    ),
    copula = list(
      measure = if(length(cop_results$measure_analyses) > 0) {
        wrap_plots(lapply(cop_results$measure_analyses, function(x) x$plot), 
                   ncol = 3) +
          plot_layout(guides = "collect") & 
          theme(legend.position = "bottom")
      } else NULL
    )
  )
  
  return(list(
    univariate_results = uni_results,
    copula_results = cop_results,
    summary_stats = summary_stats,
    visualizations = visualizations
  ))
}

# Function to organize and analyze individual univariate features
analyze_individual_univariate_organized <- function(feat_left_uni) {
  # Define frequencies and measures in order
  measures <- c(paste0("FREQ_", c("125", "250", "500", "750", "1000", "1500", 
                                  "2000", "3000", "4000", "6000", "8000"), "_L"),
                "SRT", "SNR")
  
  # Initialize lists for each feature type
  plots_mean <- list()
  plots_variance <- list()
  plots_distribution <- list()
  plots_ranks <- list()
  
  # Process each measure
  for(measure in measures) {
    measure_name <- ifelse(grepl("FREQ", measure), 
                           sub("FREQ_|_L", "", measure), 
                           measure)
    
    # Mean
    mean_col <- grep(paste0("^", measure, "\\.mean$"), colnames(feat_left_uni), value = TRUE)
    if(length(mean_col) > 0) {
      mean_data <- data.frame(
        feat_left_uni[, mean_col, drop = FALSE],
        HL = feat_left_uni$HL
      )
      plots_mean[[measure_name]] <- assess_tsne_discrimination(
        mean_data,
        perplexity = 5,
        title = paste(measure_name, "mean"),
        feature_type = "univariate"
      )$plot
    }
    
    # Variance
    var_col <- grep(paste0("^", measure, "\\.variance$"), colnames(feat_left_uni), value = TRUE)
    if(length(var_col) > 0) {
      var_data <- data.frame(
        feat_left_uni[, var_col, drop = FALSE],
        HL = feat_left_uni$HL
      )
      plots_variance[[measure_name]] <- assess_tsne_discrimination(
        var_data,
        perplexity = 5,
        title = paste(measure_name, "variance"),
        feature_type = "univariate"
      )$plot
    }
    
    # Distribution
    dist_cols <- grep(paste0("^", measure, "\\.(median|iqr|p5|p95)$"), 
                      colnames(feat_left_uni), value = TRUE)
    if(length(dist_cols) > 0) {
      dist_data <- data.frame(
        feat_left_uni[, dist_cols, drop = FALSE],
        HL = feat_left_uni$HL
      )
      plots_distribution[[measure_name]] <- assess_tsne_discrimination(
        dist_data,
        perplexity = 5,
        title = paste(measure_name, "distribution"),
        feature_type = "univariate"
      )$plot
    }
    
    # Ranks
    rank_cols <- grep(paste0("^", measure, "\\.rank"), colnames(feat_left_uni), value = TRUE)
    if(length(rank_cols) > 0) {
      rank_data <- data.frame(
        feat_left_uni[, rank_cols, drop = FALSE],
        HL = feat_left_uni$HL
      )
      plots_ranks[[measure_name]] <- assess_tsne_discrimination(
        rank_data,
        perplexity = 5,
        title = paste(measure_name, "ranks"),
        feature_type = "univariate"
      )$plot
    }
  }
  
  # Create individual column plots
  p_mean <- wrap_plots(plots_mean, ncol = 1) + 
    plot_annotation(title = "MEAN")
  p_variance <- wrap_plots(plots_variance, ncol = 1) + 
    plot_annotation(title = "VARIANCE")
  p_distribution <- wrap_plots(plots_distribution, ncol = 1) + 
    plot_annotation(title = "DISTRIBUTION")
  p_ranks <- wrap_plots(plots_ranks, ncol = 1) + 
    plot_annotation(title = "RANKS")
  
  # Combine all columns
  final_plot <- p_mean | p_variance | p_distribution | p_ranks
  
  # Add shared legend at bottom
  final_plot <- final_plot + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  return(list(
    plots_mean = plots_mean,
    plots_variance = plots_variance,
    plots_distribution = plots_distribution,
    plots_ranks = plots_ranks,
    combined_plot = final_plot
  ))
}

analyze_copula_detailed <- function(feat_left_copula) {
  # Define frequencies and measures
  freqs <- c("125", "250", "500", "750", "1000", "1500", 
             "2000", "3000", "4000", "6000", "8000")
  speech_measures <- c("SRT", "SNR")
  
  # Initialize lists
  plots_by_freq_pair <- list()
  plots_by_range <- list()
  plots_by_speech <- list()  # For speech measure plots
  
  # 1. Analysis by Frequency Pairs
  for(i in 1:(length(freqs)-1)) {
    freq1 <- freqs[i]
    for(j in (i+1):length(freqs)) {
      freq2 <- freqs[j]
      pattern <- paste0("FREQ_", freq1, "_L.FREQ_", freq2, "_L")
      cols <- grep(pattern, colnames(feat_left_copula), value = TRUE)
      
      if(length(cols) > 0) {
        pair_data <- data.frame(
          feat_left_copula[, cols, drop = FALSE],
          HL = feat_left_copula$HL
        )
        plots_by_freq_pair[[paste(freq1, freq2, sep="_")]] <- 
          assess_tsne_discrimination(
            pair_data,
            perplexity = 5,
            title = paste(freq1, "Hz -", freq2, "Hz"),
            feature_type = "copula"
          )$plot
      }
    }
  }
  
  # 2. Analysis by Frequency Range
  ranges <- list(
    low = c("125", "250", "500"),
    mid_low = c("750", "1000", "1500"),
    mid_high = c("2000", "3000", "4000"),
    high = c("6000", "8000")
  )
  
  # Create all pairs but exclude self-pairs
  range_pairs <- expand.grid(range1 = names(ranges), range2 = names(ranges))
  range_pairs <- range_pairs[range_pairs$range1 != range_pairs$range2, ]
  
  for(i in 1:nrow(range_pairs)) {
    range1_name <- as.character(range_pairs$range1[i])
    range2_name <- as.character(range_pairs$range2[i])
    
    range1_pattern <- paste(paste0("FREQ_", ranges[[range1_name]], "_L"), collapse = "|")
    range2_pattern <- paste(paste0("FREQ_", ranges[[range2_name]], "_L"), collapse = "|")
    pattern <- paste0("(", range1_pattern, ").*?(", range2_pattern, ")")
    cols <- grep(pattern, colnames(feat_left_copula), value = TRUE)
    
    if(length(cols) > 0) {
      range_data <- data.frame(
        feat_left_copula[, cols, drop = FALSE],
        HL = feat_left_copula$HL
      )
      plots_by_range[[paste(range1_name, range2_name, sep="_")]] <- 
        assess_tsne_discrimination(
          range_data,
          perplexity = 5,
          title = paste(range1_name, "-", range2_name, "range"),
          feature_type = "copula"
        )$plot
    }
  }
  
  # 3. Analysis by Speech Measures
  # For each frequency with speech measures
  for(freq in freqs) {
    for(measure in speech_measures) {
      pattern <- paste0("FREQ_", freq, "_L.", measure)
      cols <- grep(pattern, colnames(feat_left_copula), value = TRUE)
      
      if(length(cols) > 0) {
        measure_data <- data.frame(
          feat_left_copula[, cols, drop = FALSE],
          HL = feat_left_copula$HL
        )
        plots_by_speech[[paste(freq, measure, sep="_")]] <- 
          assess_tsne_discrimination(
            measure_data,
            perplexity = 5,
            title = paste(freq, "Hz -", measure),
            feature_type = "copula"
          )$plot
      }
    }
  }
  
  # Add SRT-SNR combination analysis
  srt_snr_pattern <- "^SRT.SNR"
  cols <- grep(srt_snr_pattern, colnames(feat_left_copula), value = TRUE)
  if(length(cols) > 0) {
    measure_data <- data.frame(
      feat_left_copula[, cols, drop = FALSE],
      HL = feat_left_copula$HL
    )
    plots_by_speech[["SRT_SNR"]] <- 
      assess_tsne_discrimination(
        measure_data,
        perplexity = 5,
        title = "SRT - SNR",
        feature_type = "copula"
      )$plot
  }
  
  # Create combined plots
  freq_pairs_plot <- wrap_plots(plots_by_freq_pair, ncol = 5) + 
    plot_annotation(title = "Frequency Pairs Analysis") +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  ranges_plot <- wrap_plots(plots_by_range, ncol = 4) + 
    plot_annotation(title = "Frequency Ranges Analysis") +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  speech_plot <- wrap_plots(plots_by_speech, ncol = 4) + 
    plot_annotation(title = "Speech Measures Analysis") +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  return(list(
    by_freq_pair = plots_by_freq_pair,
    by_range = plots_by_range,
    by_speech = plots_by_speech,
    combined_freq_pairs = freq_pairs_plot,
    combined_ranges = ranges_plot,
    combined_speech = speech_plot
  ))
}



############################################
# Functions Bootstraping Feature Engineers #
############################################

# New function for copula statistics between pairs of variables
copula_statistics <- function(x, y) {
  n <- length(x)
  # Transform to ranks (probability integral transform)
  U1 <- rank(x)/(n + 1)
  U2 <- rank(y)/(n + 1)
  
  # Compute copula-based measures
  tau_cop <- 4 * mean(U1 * U2) - 1
  rho_cop <- 12 * mean(U1 * ((1:n)-0.5)/n) - 3
  multi_rho <- 12 * mean((U1 - 0.5) * (U2 - 0.5))
  beta_cop <- 4 * mean(sign((U1 - 0.5) * (U2 - 0.5)))
  gamma_cop <- mean(abs(U1 - U2))
  gini_alt <- mean(abs(2 * U1 - 1) * abs(2 * U2 - 1))
  local_gauss <- cor(qnorm(U1), qnorm(U2))
  
  return(c(tau_cop, rho_cop, gamma_cop, multi_rho, 
           beta_cop, gini_alt, local_gauss))
}

# Modified mystatistic function
mystatistic <- function(data, indices, paired = FALSE) {
  if(!paired) {
    # Single variable statistics
    boot_sample <- data[indices]
    
    # Original statistics
    boot_mean <- mean(boot_sample, na.rm = TRUE)
    boot_var <- var(boot_sample, na.rm = TRUE)
    boot_median <- median(boot_sample, na.rm = TRUE)
    boot_iqr <- IQR(boot_sample, na.rm = TRUE)
    boot_p5 <- quantile(boot_sample, 0.05, na.rm = TRUE)
    boot_p95 <- quantile(boot_sample, 0.95, na.rm = TRUE)
    
    # Original ranks
    rank_vals <- rank(boot_sample)
    boot_rank <- sample(rank_vals, 10, replace = TRUE)
    
    return(c(boot_mean, boot_var, boot_median, boot_iqr, 
             boot_p5, boot_p95, boot_rank))
    
  } else {
    # For paired variables
    boot_sample1 <- data$var1[indices]
    boot_sample2 <- data$var2[indices]
    
    cop_stats <- copula_statistics(boot_sample1, boot_sample2)
    return(cop_stats)
  }
}


# # Modified bootstrap_columns function
# bootstrap_columns <- function(df, columns, n_boot = R) {
#   # For single variable statistics
#   single_var_results <- lapply(columns, function(col) {
#     boot_result <- boot(df[[col]], mystatistic, R = n_boot, paired = FALSE)  # here using n_boot
#     list(
#       Means = boot_result$t[, 1],
#       Variances = boot_result$t[, 2],
#       Medians = boot_result$t[, 3],
#       IQRs = boot_result$t[, 4],
#       P5s = boot_result$t[, 5],
#       P95s = boot_result$t[, 6],
#       Ranks = boot_result$t[, 7:16]
#     )
#   })
#   names(single_var_results) <- columns
#   
#   # For copula statistics (pairs of variables)
#   var_pairs <- combn(columns, 2, simplify = FALSE)
#   copula_results <- lapply(var_pairs, function(pair) {
#     paired_data <- data.frame(
#       var1 = df[[pair[1]]],
#       var2 = df[[pair[2]]]
#     )
#     boot_result <- boot(paired_data, mystatistic, R = n_boot, paired = TRUE)  # and here using n_boot
#     list(
#       TauCop = boot_result$t[, 1],
#       RhoCop = boot_result$t[, 2],
#       GammaCop = boot_result$t[, 3],
#       MultiRho = boot_result$t[, 4],
#       BetaCop = boot_result$t[, 5],
#       GiniAlt = boot_result$t[, 6],
#       LocalGauss = boot_result$t[, 7]
#     )
#   })
#   names(copula_results) <- sapply(var_pairs, paste, collapse = "|")
#   
#   return(list(
#     single_var = single_var_results,
#     copula = copula_results
#   ))
# }

# # Modified extract_statistics function
# extract_statistics <- function(results) {
#   # Extract single variable statistics
#   single_var_stats <- lapply(results$single_var, function(feature) {
#     list(
#       mean = feature$Means,
#       variance = feature$Variances,
#       median = feature$Medians,
#       iqr = feature$IQRs,
#       p5 = feature$P5s,
#       p95 = feature$P95s,
#       rank = feature$Ranks
#     )
#   })
#   
#   # Extract copula statistics
#   copula_stats <- lapply(results$copula, function(feature) {
#     list(
#       tau_cop = feature$TauCop,
#       rho_cop = feature$RhoCop,
#       gamma_cop = feature$GammaCop,
#       multi_rho = feature$MultiRho,
#       beta_cop = feature$BetaCop,
#       gini_alt = feature$GiniAlt,
#       local_gauss = feature$LocalGauss
#     )
#   })
#   
#   return(list(
#     single_var = single_var_stats,
#     copula = copula_stats
#   ))
# }



parametric_bootstrap_columns <- function(df, columns_to_bootstrap, n_boot = R) {
  require(copula)
  require(boot)
  
  # Single variable stats remain the same
  single_var_results <- lapply(columns_to_bootstrap, function(col) {
    boot_result <- boot(df[[col]], mystatistic, R = n_boot)
    list(
      Means = boot_result$t[, 1],
      Variances = boot_result$t[, 2],
      Medians = boot_result$t[, 3],
      IQRs = boot_result$t[, 4],
      P5s = boot_result$t[, 5],
      P95s = boot_result$t[, 6],
      Ranks = boot_result$t[, 7:16]
    )
  })
  names(single_var_results) <- columns_to_bootstrap
  
  var_pairs <- combn(columns_to_bootstrap, 2, simplify = FALSE)
  copula_results <- lapply(var_pairs, function(pair) {
    paired_data <- cbind(df[[pair[1]]], df[[pair[2]]])
    
    # Initial correlation for copula parameter
    tau <- cor(paired_data[,1], paired_data[,2], method = "kendall")
    rho <- sin(pi * tau / 2)
    
    # Generate from t-copula
    tcop <- tCopula(param = rho, dim = 2, df = 4)
    cop_samples <- rCopula(nrow(paired_data), tcop)
    
    # Transform to original scales
    paired_samples <- cbind(
      quantile(paired_data[,1], probs = cop_samples[,1]),
      quantile(paired_data[,2], probs = cop_samples[,2])
    )
    
    # Calculate statistics
    tau <- cor(paired_samples[,1], paired_samples[,2], method = "kendall")
    rho <- cor(paired_samples[,1], paired_samples[,2])
    gamma <- cor(paired_samples[,1], paired_samples[,2], method = "spearman")
    
    # Other statistics
    temp_df <- data.frame(var1 = paired_samples[,1], var2 = paired_samples[,2])
    other_stats <- mystatistic(temp_df, paired = TRUE)
    tail_deps <- calculate_tail_dep(paired_samples)
    
    list(
      TauCop = tau,
      RhoCop = rho,
      GammaCop = gamma,
      MultiRho = other_stats[4],
      BetaCop = other_stats[5],
      GiniAlt = other_stats[6],
      LocalGauss = other_stats[7],
      TailDep = tail_deps
    )
  })
  
  names(copula_results) <- sapply(var_pairs, paste, collapse = "|")
  
  return(list(
    single_var = single_var_results,
    copula = copula_results
  ))
}


# New tail dependence calculation function
calculate_tail_dep <- function(paired_samples) {
  u <- paired_samples[,1]
  v <- paired_samples[,2]
  
  k <- 10
  u_grid <- seq(0.01, 0.1, length.out = k)
  lambda_l <- mean(sapply(u_grid, function(q) {
    mean(v <= quantile(v, q) & u <= quantile(u, q)) / q
  }))
  
  u_grid <- seq(0.9, 0.99, length.out = k)
  lambda_u <- mean(sapply(u_grid, function(q) {
    mean(v >= quantile(v, q) & u >= quantile(u, q)) / (1 - q)
  }))
  
  return(c(lambda_l, lambda_u))
}




nonparametric_bootstrap_columns <- function(df, columns, n_boot = R) {
  require(kdecopula)
  
  single_var_results <- lapply(columns, function(col) {
    boot_result <- boot(df[[col]], mystatistic, R = n_boot, paired = FALSE)
    list(
      Means = boot_result$t[, 1],
      Variances = boot_result$t[, 2], 
      Medians = boot_result$t[, 3],
      IQRs = boot_result$t[, 4],
      P5s = boot_result$t[, 5],
      P95s = boot_result$t[, 6],
      Ranks = boot_result$t[, 7:16]
    )
  })
  names(single_var_results) <- columns
  
  var_pairs <- combn(columns, 2, simplify = FALSE)
  
  copula_results <- lapply(var_pairs, function(pair) {
    print(paste("Processing pair:", pair[1], "-", pair[2]))
    paired_data <- cbind(df[[pair[1]]], df[[pair[2]]])
    
    u <- pobs(paired_data)
    bern_cop <- kdecop(u, method = "bern")
    cop_samples <- rkdecop(nrow(df), bern_cop)
    
    paired_samples <- cbind(
      quantile(df[[pair[1]]], probs = cop_samples[,1], type = 1),
      quantile(df[[pair[2]]], probs = cop_samples[,2], type = 1)
    )
    
    tau <- cor(paired_samples[,1], paired_samples[,2], method = "kendall")
    rho <- cor(paired_samples[,1], paired_samples[,2])
    gamma <- cor(paired_samples[,1], paired_samples[,2], method = "spearman")
    
    temp_df <- data.frame(var1 = paired_samples[,1], var2 = paired_samples[,2])
    other_stats <- mystatistic(temp_df, paired = TRUE)
    tail_deps <- calculate_tail_dep(paired_samples)
    
    list(
      TauCop = tau,
      RhoCop = rho, 
      GammaCop = gamma,
      MultiRho = other_stats[4],
      BetaCop = other_stats[5],
      GiniAlt = other_stats[6],
      LocalGauss = other_stats[7],
      TailDep = tail_deps
    )
  })
  names(copula_results) <- sapply(var_pairs, paste, collapse = "|")
  
  return(list(
    single_var = single_var_results,
    copula = copula_results
  ))
}


# Modified feature extraction function to handle tail dependence
extract_statistics_modified <- function(results) {
  # Process single vars
  single_var_df <- do.call(cbind, lapply(names(results$single_var), function(var_name) {
    var_stats <- results$single_var[[var_name]]
    # Handle Ranks separately
    stats_no_ranks <- var_stats[names(var_stats) != "Ranks"]
    stats_matrix <- do.call(cbind, lapply(stats_no_ranks, as.matrix))
    # Add ranks
    ranks_matrix <- matrix(var_stats$Ranks, nrow=nrow(stats_matrix))
    cbind(stats_matrix, ranks_matrix)
  }))
  
  # Process copula 
  n_rows <- nrow(single_var_df)
  copula_df <- do.call(cbind, lapply(names(results$copula), function(pair_name) {
    pair_stats <- results$copula[[pair_name]]
    do.call(cbind, lapply(names(pair_stats), function(stat) {
      if(stat == "TailDep") {
        matrix(rep(pair_stats[[stat]], each=n_rows), ncol=2)
      } else {
        matrix(rep(pair_stats[[stat]], n_rows), ncol=1)
      }
    }))
  }))
  
  # Add column names and combine
  colnames_single <- unlist(lapply(names(results$single_var), function(var) {
    paste(var, c("mean", "variance", "median", "iqr", "p5", "p95", paste0("rank", 1:10)), sep=".")
  }))
  
  colnames_copula <- unlist(lapply(names(results$copula), function(pair) {
    paste(pair, c("tau_cop", "rho_cop", "gamma_cop", "multi_rho", "beta_cop", "gini_alt", 
                  "local_gauss", "lower_tail_dep", "upper_tail_dep"), sep=".")
  }))
  
  colnames(single_var_df) <- colnames_single
  colnames(copula_df) <- colnames_copula
  
  combined_df <- cbind(single_var_df, copula_df)
  return(combined_df)
}


#######################################
# Functions Feature Selection Results #
#######################################

# Helper function to add line breaks to facet labels
add_line_breaks <- function(text) {
  gsub(" ", "\n", text)
}


create_test_heatmap <- function(data) {
  # Define levels
  classif1_levels <- c("Slight", "Mild", "Moderate", "Moderately severe")
  classif2_levels <- c("Mild", "Moderate", "Moderately severe", "Severe")
  
  # Helper function to add line breaks to facet labels
  add_line_breaks <- function(text) {
    gsub(" ", "\n", text)
  }
  
  # Create grid with modified labels
  grid <- expand.grid(
    Classif1 = factor(classif1_levels, levels = classif1_levels, 
                      labels = sapply(classif1_levels, add_line_breaks)),
    Classif2 = factor(classif2_levels, levels = classif2_levels, 
                      labels = sapply(classif2_levels, add_line_breaks)),
    Discr_Var = c("125", "250", "500", "750", "1000", "1500", "2000", 
                  "3000", "4000", "6000", "8000", "SRT", "SNR"),
    Test = unique(data$Test)
  )
  
  # Prepare data with modified labels
  data$Classif1 <- factor(data$Classif1, levels = classif1_levels, 
                          labels = sapply(classif1_levels, add_line_breaks))
  data$Classif2 <- factor(data$Classif2, levels = classif2_levels, 
                          labels = sapply(classif2_levels, add_line_breaks))
  
  full_data <- merge(grid, data, all.x = TRUE, 
                     by = c("Classif1", "Classif2", "Discr_Var", "Test"))
  
  full_data$Discr_Var <- factor(full_data$Discr_Var, 
                                levels = c("125", "250", "500", "750", "1000", 
                                           "1500", "2000", "3000", "4000", "6000", 
                                           "8000", "SRT", "SNR"))
  
  # Create enhanced plot
  ggplot(full_data, aes(x = Discr_Var, y = Test, fill = Significance)) +
    geom_tile(width = 0.9, height = 0.9, color = "white") +  # Added white borders
    facet_grid(Classif1 ~ Classif2, scale = "free") +
    scale_fill_gradientn(
      colors = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#92c5de", "#4393c3"),
      values = scales::rescale(c(0, 0.001, 0.01, 0.03, 0.05, 0.1)),
      limits = c(0, 0.1),
      na.value = "gray90",
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 15,
        barheight = 1,
        draw.ulim = TRUE,
        draw.llim = TRUE
      )
    ) +
    theme_minimal(base_size = 14) +  # Increased base font size
    theme(
      plot.title = element_text(size = 24, face = "bold", margin = margin(b = 25)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 20)),
      panel.grid = element_blank(),
      strip.text = element_text(size = 16, face = "bold", margin = margin(t = 10, b = 10)),
      strip.background = element_rect(fill = "gray95", color = NA),
      panel.spacing.x = unit(1.5, "lines"),  # Reduced horizontal spacing
      panel.spacing.y = unit(1.5, "lines"),  # Consistent vertical spacing
      plot.margin = margin(t = 40, r = 40, b = 40, l = 40),
      legend.position = "bottom",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 12),
      panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
    ) +
    labs(
      x = "Discriminant Variable",
      y = "Test Type",
      fill = "p-value"
    )
}


create_copula_heatmap <- function(data) {
  # Split frequency pairs and prepare data
  data$freq1 <- sapply(strsplit(as.character(data$Discr_Var), "\\|"), `[`, 1)
  data$freq2 <- sapply(strsplit(as.character(data$Discr_Var), "\\|"), `[`, 2)
  
  # Define levels
  freq_levels <- c("125", "250", "500", "750", "1000", "1500", "2000", 
                   "3000", "4000", "6000", "8000", "SRT", "SNR")
  
  category_levels1 <- unique(data$Classif1)
  category_levels2 <- unique(data$Classif2)
  
  # Helper function to add line breaks
  add_line_breaks <- function(text) {
    gsub(" ", "\n", text)
  }
  
  # Modify category labels
  data$Classif1 <- factor(data$Classif1, 
                          levels = category_levels1,
                          labels = sapply(category_levels1, add_line_breaks))
  data$Classif2 <- factor(data$Classif2, 
                          levels = category_levels2,
                          labels = sapply(category_levels2, add_line_breaks))
  
  # Create complete grid with modified labels
  grid_freq <- expand.grid(
    freq1 = freq_levels,
    freq2 = freq_levels,
    Classif1 = factor(category_levels1, 
                      levels = category_levels1,
                      labels = sapply(category_levels1, add_line_breaks)),
    Classif2 = factor(category_levels2, 
                      levels = category_levels2,
                      labels = sapply(category_levels2, add_line_breaks)),
    stringsAsFactors = FALSE
  )
  
  # Merge and prepare final data
  data_complete <- merge(grid_freq, data, 
                         by = c("freq1", "freq2", "Classif1", "Classif2"), 
                         all.x = TRUE)
  
  data_complete$freq1 <- factor(data_complete$freq1, levels = freq_levels)
  data_complete$freq2 <- factor(data_complete$freq2, levels = rev(freq_levels))
  
  # Create enhanced plot
  ggplot(data_complete, aes(x = freq1, y = freq2, fill = Significance)) +
    geom_tile(width = 0.9, height = 0.9, color = "white") +  # Added white borders
    facet_grid(Classif1 ~ Classif2, scale = "free") +
    scale_fill_gradientn(
      colors = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#92c5de", "#4393c3"),
      values = scales::rescale(c(0, 0.001, 0.01, 0.03, 0.05, 0.1)),
      limits = c(0, 0.1),
      na.value = "gray90",
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 15,
        barheight = 1,
        draw.ulim = TRUE,
        draw.llim = TRUE
      )
    ) +
    theme_minimal(base_size = 14) +  # Increased base font size
    theme(
      plot.title = element_text(size = 24, face = "bold", margin = margin(b = 25)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 20)),
      panel.grid = element_blank(),
      strip.text = element_text(size = 16, face = "bold", margin = margin(t = 10, b = 10)),
      strip.background = element_rect(fill = "gray95", color = NA),
      panel.spacing.x = unit(1.5, "lines"),  # Reduced horizontal spacing
      panel.spacing.y = unit(1.5, "lines"),  # Consistent vertical spacing
      plot.margin = margin(t = 40, r = 40, b = 40, l = 40),
      legend.position = "bottom",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 12),
      panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
    ) +
    labs(
      x = "Discriminant Variable 1",
      y = "Discriminant Variable 2",
      fill = "p-value"
    )
}


create_tukey_heatmap <- function(data) {
  # Order frequencies including SRT and SNR at the end
  freq_order <- c("125", "250", "500", "750", "1000", "1500", "2000", 
                  "3000", "4000", "6000", "8000", "SRT", "SNR")
  
  # Helper function to add line breaks to comparison labels
  add_line_breaks <- function(text) {
    parts <- strsplit(text, "-")[[1]]
    paste(parts, collapse = "\n")
  }
  
  # Process the data and transform comparison labels
  plot_data <- data %>%
    select(-Classif2) %>%
    rename(Comparison = Classif1) %>%
    mutate(
      Comparison = sapply(Comparison, function(x) {
        parts <- strsplit(x, "-")[[1]]
        paste(parts[2], "-", parts[1], sep="")
      })
    )
  
  # Create proper order for the reversed comparisons
  comparison_order <- c(
    "Slight-Severe", "Slight-Moderately severe", "Slight-Moderate", "Slight-Mild",
    "Mild-Severe", "Mild-Moderately severe", "Mild-Moderate",
    "Moderate-Severe", "Moderate-Moderately severe",
    "Moderately severe-Severe"
  )
  
  # Convert to factors with proper ordering and add line breaks
  plot_data$Discr_Var <- factor(plot_data$Discr_Var, levels = freq_order)
  plot_data$Comparison <- factor(plot_data$Comparison, 
                                 levels = comparison_order,
                                 labels = sapply(comparison_order, add_line_breaks))
  
  # Create enhanced plot
  ggplot(plot_data, aes(x = Discr_Var, y = Comparison, fill = Significance)) +
    geom_tile(width = 0.9, height = 0.9, color = "white") +  # Added white borders
    scale_fill_gradientn(
      colors = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#92c5de", "#4393c3"),
      values = scales::rescale(c(0, 0.001, 0.01, 0.03, 0.05, 0.1)),
      limits = c(0, 0.1),
      na.value = "gray90",
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 15,
        barheight = 1,
        draw.ulim = TRUE,
        draw.llim = TRUE
      )
    ) +
    theme_minimal(base_size = 14) +  # Increased base font size
    theme(
      plot.title = element_text(size = 24, face = "bold", margin = margin(b = 25)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 20)),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA, size = 0.5),
      plot.margin = margin(t = 40, r = 40, b = 40, l = 40),
      legend.position = "bottom",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 12)
    ) +
    labs(
      x = "Discriminant Variable",
      y = "Comparison",
      fill = "p-value"
    )
}


# create_combined_heatmaps <- function(test_uni_modified, copula_data, tukey_modified) {
#   title_theme <- theme(
#     plot.title = element_text(size = 16, face = "bold", margin = margin(b = 40)),
#     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 14, margin = margin(t = 8)),  # Increased spacing
#     axis.text.y = element_text(size = 14, margin = margin(r = 5)),
#     axis.title.x = element_text(size = 16, margin = margin(t = 35)),  # Pushed further down
#     axis.title.y = element_text(size = 16, margin = margin(r = 15))
#   )
#   
#   p1 <- create_test_heatmap(test_uni_modified) + 
#     labs(title = "A) Univariate Tests") +
#     theme(
#       legend.position = "none",
#       plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
#       panel.spacing = unit(3, "lines"),  # More spacing to prevent facet overlap
#       strip.text = element_text(size = 15, face = "bold", margin = margin(b = 15)),  # Increased bottom margin
#       axis.text.x = element_text(size = 14),
#       axis.text.y = element_text(size = 14)
#     ) +
#     title_theme
#   
#   p2 <- create_copula_heatmap(copula_data) + 
#     labs(title = "B) Copula Tests") +
#     theme(
#       legend.position = "none",
#       plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
#       panel.spacing = unit(3, "lines"),  # More spacing to prevent facet overlap
#       strip.text = element_text(size = 15, face = "bold", margin = margin(b = 15)),  # More space for readability
#       axis.text.x = element_text(size = 14),
#       axis.text.y = element_text(size = 14)
#     ) +
#     title_theme
#   
#   p3 <- create_tukey_heatmap(tukey_modified) + 
#     labs(title = "C) Tukey Tests") +
#     theme(
#       legend.position = "bottom",
#       legend.box.margin = margin(t = 20, b = 10),  # More spacing between legend and plot
#       legend.key.width = unit(2, "cm"),
#       legend.key.height = unit(0.5, "cm"),
#       plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
#       axis.text.x = element_text(size = 16, margin = margin(t = 10)),  # More spacing
#       axis.text.y = element_text(size = 16, margin = margin(r = 10)),
#       axis.title.x = element_text(size = 18, margin = margin(t = 35)),  # Pushed x-title further away
#       axis.title.y = element_text(size = 18, margin = margin(r = 15))
#     ) +
#     title_theme
#   
#   top_row <- p1 + p2 + plot_layout(ncol = 2, widths = c(1, 1), guides = "collect") +
#     plot_layout(widths = c(1, 1))
#   
#   combined_plot <- top_row / (plot_spacer() + p3 + plot_spacer() + 
#                                 plot_layout(widths = c(0.3, 2.4, 1.3))) +
#     plot_layout(heights = c(1.7, 1))
#   
#   return(combined_plot)
# }

# create_combined_heatmaps <- function(test_uni_modified, copula_data, tukey_modified) {
#   title_theme <- theme(
#     plot.title = element_text(size = 16, face = "bold", margin = margin(b = 20)),
#     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 11, margin = margin(t = 5)),
#     axis.text.y = element_text(size = 11, margin = margin(r = 5)),
#     axis.title.x = element_text(size = 16, margin = margin(t = 30)),  # Increased margin
#     axis.title.y = element_text(size = 16, margin = margin(r = 15))
#   )
#   
#   p1 <- create_test_heatmap(test_uni_modified) + 
#     labs(title = "A) Univariate Tests") +
#     theme(
#       legend.position = "none",
#       plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
#       panel.spacing = unit(1, "lines"),  # Adjusted spacing
#       axis.title.x = element_text(size = 11, margin = margin(t = 30)),  # Further from axis text
#       axis.text.y = element_text(size = 15, margin = margin(r = 5))
#     ) +
#     title_theme
#   
#   p2 <- create_copula_heatmap(copula_data) + 
#     labs(title = "B) Copula Tests") +
#     theme(
#       legend.position = "none",
#       plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
#       panel.spacing = unit(1, "lines"),
#       axis.title.x = element_text(size = 16, margin = margin(t = 30))  # Further from axis text
#     ) +
#     title_theme
#   
#   p3 <- create_tukey_heatmap(tukey_modified) + 
#     labs(title = "C) Tukey Tests") +
#     theme(
#       legend.position = "bottom",
#       legend.box.margin = margin(t = 20, b = 10),
#       legend.key.width = unit(2, "cm"),
#       legend.key.height = unit(0.5, "cm"),
#       plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
#       axis.text.x = element_text(size = 19, margin = margin(t = 15)),
#       axis.text.y = element_text(size = 19, margin = margin(r = 10)),
#       axis.title.x = element_text(size = 18, margin = margin(t = 35)),  # Pushed x-title further down
#       axis.title.y = element_text(size = 18, margin = margin(r = 15))
#     ) +
#     title_theme
#   
#   # Stack the plots vertically
#   combined_plot <- p1 / p2 / p3 + 
#     plot_layout(heights = c(1, 1, 1.2))  # Bottom plot slightly larger for legend
#   
#   return(combined_plot)
# }



###########################
# FUNCTIONS LATEX TABLEs  #
###########################
# Function to create univariate test summary
create_univariate_summary <- function(data) {
  # Get unique comparison pairs
  comparisons <- unique(paste(data$Classif1, data$Classif2, sep = " vs "))
  
  # For each comparison, create a separate table
  lapply(comparisons, function(comp) {
    # Split the comparison back into Classif1 and Classif2
    classes <- strsplit(comp, " vs ")[[1]]
    
    data %>%
      filter(Classif1 == classes[1], Classif2 == classes[2]) %>%
      # Group by frequency and test, take the most significant result
      group_by(Discr_Var, Test) %>%
      summarize(
        Significance = min(Significance),
        .groups = 'drop'
      ) %>%
      arrange(desc(Significance)) %>%
      xtable(
        caption = sprintf("Univariate Test Results - %s", comp),
        label = sprintf("tbl:univariate_%s_%s", 
                        tolower(gsub(" ", "_", classes[1])), 
                        tolower(gsub(" ", "_", classes[2])))
      ) %>%
      print(
        include.rownames = FALSE,
        floating = TRUE,
        table.placement = "H",
        caption.placement = "top",
        size = "scriptsize"
      )
  })
}

#SELECT ONLY THE MOST SIGNIFICANT
create_univariate_summary_flexible <- function(data, top_n = 8, columns_per_page = 3) {
  # Get all unique comparisons
  all_comparisons <- unique(paste(data$Classif1, "vs", data$Classif2))
  n_comparisons <- length(all_comparisons)
  
  # Calculate minipage width based on number of columns
  minipage_width <- sprintf("%.3f\\textwidth", (0.98/columns_per_page))
  
  # Function to create table content for one comparison
  create_comparison_table <- function(comp) {
    # Split comparison back into classes
    classes <- strsplit(comp, " vs ")[[1]]
    
    data %>%
      filter(Classif1 == classes[1], Classif2 == classes[2]) %>%
      group_by(Discr_Var, Test) %>%
      slice_min(Significance, n = 1) %>%
      ungroup() %>%
      arrange(Significance) %>%
      slice_head(n = top_n) %>%
      select(
        Discr_Var,
        Test,
        Significance
      )
  }
  
  # Start the table environment
  cat("\\begin{table}[H]
\\renewcommand{\\tabcolsep}{3pt}
\\centering
\\scriptsize\n")
  
  # Process all comparisons
  for(i in seq_along(all_comparisons)) {
    # Start new row of minipages if needed
    if((i-1) %% columns_per_page == 0) {
      if(i > 1) {
        # Close previous row if not the first iteration
        cat("\\\\[0.5cm]\n")
      }
    }
    
    # Start minipage
    cat(sprintf("\\begin{minipage}{%s}
\\centering\n", minipage_width))
    
    # Create and fill table
    table_data <- create_comparison_table(all_comparisons[i])
    
    cat("\\begin{tabular}{ccc}
\\toprule
\\multicolumn{3}{c}{\\textbf{Features Ranking}}\\\\
\\toprule
\\multicolumn{3}{c}{\\textbf{", all_comparisons[i], "}}\\\\
\\toprule
\\textbf{Discr\\_Var} & \\textbf{Test} & \\textbf{Significance} \\\\
\\toprule\n")
    
    for(j in 1:nrow(table_data)) {
      cat(sprintf("%s & %s & %.3f \\\\\n", 
                  table_data$Discr_Var[j],
                  table_data$Test[j],
                  table_data$Significance[j]))
    }
    
    cat("\\bottomrule
\\end{tabular}\n")
    
    # End minipage
    cat("\\end{minipage}")
    
    # Add spacing between minipages if not last in row
    if(i %% columns_per_page != 0 && i != n_comparisons) {
      cat("\\hfill")
    }
  }
  
  # Close the table environment
  cat("\n\\caption{Most Significant Univariate Test Results (Top ", top_n, ") by Category Comparison}
\\label{tbl:univariate_summary}
\\end{table}")
}


# Function to create Tukey test summary
# create_tukey_summary <- function(data) {
#   data %>%
#     filter(Test == "Tukey") %>%
#     arrange(desc(Significance)) %>%
#     select(Classif1, Discr_Var, Significance, Type) %>%
#     xtable(
#       caption = "Tukey Test Results - Ordered by Significance",
#       label = "tbl:tukey_tests"
#     ) %>%
#     print(
#       include.rownames = FALSE,
#       floating = TRUE,
#       table.placement = "H",
#       caption.placement = "top",
#       size = "scriptsize"
#     )
# }

create_tukey_summary_bycomparison <- function(data, columns_per_page = 3) {
  # Get all unique comparisons
  all_comparisons <- sort(unique(data$Classif1))
  n_comparisons <- length(all_comparisons)
  
  # Calculate minipage width based on number of columns
  minipage_width <- sprintf("%.3f\\textwidth", (0.98/columns_per_page))
  
  # Start the table environment
  cat("\\begin{table}[H]
\\renewcommand{\\tabcolsep}{3pt}
\\centering
\\scriptsize\n")
  
  # Process each comparison group
  for(i in seq_along(all_comparisons)) {
    # Start new row of minipages if needed
    if((i-1) %% columns_per_page == 0) {
      if(i > 1) {
        # Close previous row if not the first iteration
        cat("\\\\[0.5cm]\n")
      }
    }
    
    # Start minipage
    cat(sprintf("\\begin{minipage}{%s}
\\centering\n", minipage_width))
    
    # Get data for this comparison and arrange by significance
    comparison_data <- data %>%
      filter(Classif1 == all_comparisons[i]) %>%
      arrange(Significance)  # Most significant first
    
    # Create table
    cat("\\begin{tabular}{cc}
\\toprule
\\multicolumn{2}{c}{\\textbf{Tukey Test Results}}\\\\
\\toprule
\\multicolumn{2}{c}{\\textbf{", all_comparisons[i], "}}\\\\
\\toprule
\\textbf{Freq} & \\textbf{p-value} \\\\
\\toprule\n")
    
    # Add rows
    for(j in 1:nrow(comparison_data)) {
      cat(sprintf("%s & %.3f \\\\\n",
                  comparison_data$Discr_Var[j],
                  comparison_data$Significance[j]))
    }
    
    cat("\\bottomrule
\\end{tabular}\n")
    
    # End minipage
    cat("\\end{minipage}")
    
    # Add spacing between minipages if not last in row
    if(i %% columns_per_page != 0 && i != n_comparisons) {
      cat("\\hfill")
    }
  }
  
  # Close the table environment
  cat("\n\\caption{Tukey Test Results by Comparison Groups (Sorted by Significance)}
\\label{tbl:tukey_summary}
\\end{table}")
}

# Function to create Copula test summary
# create_copula_summary <- function(data) {
#   data %>%
#     filter(Test == "Copula") %>%
#     arrange(desc(Significance)) %>%
#     select(Classif1, Classif2, Discr_Var, Significance, Type) %>%
#     xtable(
#       caption = "Copula Test Results - Ordered by Significance",
#       label = "tbl:copula_tests"
#     ) %>%
#     print(
#       include.rownames = FALSE,
#       floating = TRUE,
#       table.placement = "H",
#       caption.placement = "top",
#       size = "scriptsize"
#     )
# }


create_copula_summary_bycomparison <- function(data, columns_per_page = 3, top_n = 8) {
  # Get all unique comparison pairs
  all_comparisons <- unique(paste(data$Classif1, "vs", data$Classif2))
  n_comparisons <- length(all_comparisons)
  
  # Calculate minipage width based on number of columns
  minipage_width <- sprintf("%.3f\\textwidth", (0.98/columns_per_page))
  
  # Start the table environment
  cat("\\begin{table}[H]
\\renewcommand{\\tabcolsep}{3pt}
\\centering
\\scriptsize\n")
  
  # Process each comparison group
  for(i in seq_along(all_comparisons)) {
    # Start new row of minipages if needed
    if((i-1) %% columns_per_page == 0) {
      if(i > 1) {
        # Close previous row if not the first iteration
        cat("\\\\[0.5cm]\n")
      }
    }
    
    # Start minipage
    cat(sprintf("\\begin{minipage}{%s}
\\centering\n", minipage_width))
    
    # Get comparison parts
    comp_parts <- strsplit(all_comparisons[i], " vs ")[[1]]
    
    # Get data for this comparison, arrange by significance and take top N
    comparison_data <- data %>%
      filter(Classif1 == comp_parts[1], Classif2 == comp_parts[2]) %>%
      arrange(Significance) %>%  # Most significant first
      slice_head(n = top_n)     # Take only top N most significant results
    
    # Create table
    cat("\\begin{tabular}{cc}
\\toprule
\\multicolumn{2}{c}{\\textbf{Copula Test Results}}\\\\
\\toprule
\\multicolumn{2}{c}{\\textbf{", all_comparisons[i], "}}\\\\
\\toprule
\\textbf{Freq Pair} & \\textbf{p-value} \\\\
\\toprule\n")
    
    # Add rows
    for(j in 1:nrow(comparison_data)) {
      cat(sprintf("%s & %.3f \\\\\n",
                  comparison_data$Discr_Var[j],
                  comparison_data$Significance[j]))
    }
    
    cat("\\bottomrule
\\end{tabular}\n")
    
    # End minipage
    cat("\\end{minipage}")
    
    # Add spacing between minipages if not last in row
    if(i %% columns_per_page != 0 && i != n_comparisons) {
      cat("\\hfill")
    }
  }
  
  # Close the table environment
  cat("\n\\caption{Copula Test Results by Comparison Groups (Top ", top_n, " Most Significant)}
\\label{tbl:copula_summary}
\\end{table}")
}

###############
# EXTRA plots #
###############

# Enhanced frequency analysis with multiple metrics
analyze_frequency_importance <- function(univariate_data) {
  # Prepare data
  freq_analysis <- univariate_data %>%
    group_by(Discr_Var) %>%
    summarise(
      sig_count = sum(Significance < 0.02),
      .groups = 'drop'
    ) %>%
    mutate(
      Discr_Var = factor(Discr_Var, 
                         levels = c("125", "250", "500", "750", "1000", 
                                    "1500", "2000", "3000", "4000", "6000", 
                                    "8000", "SRT", "SNR")),
      is_speech = Discr_Var %in% c("1000", "2000", "4000", "SRT", "SNR")
    )
  
  # Create enhanced plot
  p1 <- ggplot(freq_analysis) +
    # Add background rectangles for visual grouping
    geom_rect(aes(xmin = -Inf, xmax = Inf,
                  ymin = -Inf, ymax = Inf),
              fill = "gray97",
              data = freq_analysis[seq(2, nrow(freq_analysis), 2), ]) +
    # Add main segments
    geom_segment(aes(x = Discr_Var, xend = Discr_Var, 
                     y = 0, yend = sig_count,
                     color = is_speech),
                 size = 3, alpha = 0.7) +
    # Add endpoint circles
    geom_point(aes(x = Discr_Var, y = sig_count, 
                   fill = is_speech),
               size = 12, shape = 21, stroke = 2, color = "white") +
    # Add value labels
    geom_text(aes(x = Discr_Var, y = sig_count,
                  label = sig_count),
              color = "white", size = 4, fontface = "bold") +
    # Custom colors
    scale_color_manual(values = c("FALSE" = "#2c3e50", "TRUE" = "#e74c3c")) +
    scale_fill_manual(values = c("FALSE" = "#34495e", "TRUE" = "#c0392b")) +
    coord_flip() +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", size = 0.5),
      legend.position = "none",
      axis.text = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 20)),
      plot.margin = margin(t = 20, r = 30, b = 20, l = 20),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = "Frequency Discriminative Power",
      #subtitle = "Number of significant tests (p < 0.02) by frequency\nRed indicates speech-related frequencies",
      x = "Frequency",
      y = "Number of Significant Tests"
    )
  
  return(list(data = freq_analysis, plot = p1))
}

analyze_copula_pairs <- function(copula_data) {
  freq_order <- c("125", "250", "500", "750", "1000", 
                  "1500", "2000", "3000", "4000", "6000", 
                  "8000", "SRT", "SNR")
  
  # Prepare data
  pair_analysis <- copula_data %>%
    separate(Discr_Var, into = c("freq1", "freq2"), sep = "\\|") %>%
    group_by(freq1, freq2) %>%
    summarise(
      n_sig = sum(Significance < 0.05),
      mean_sig = mean(Significance),
      .groups = 'drop'
    ) %>%
    arrange(desc(n_sig)) %>%
    mutate(
      freq1 = factor(freq1, levels = freq_order),
      freq2 = factor(freq2, levels = freq_order),
      is_speech_pair = (freq1 %in% c("1000", "2000", "4000", "SRT", "SNR") &
                          freq2 %in% c("1000", "2000", "4000", "SRT", "SNR"))
    )
  
  # Create enhanced plot
  p2 <- ggplot(pair_analysis, aes(x = freq1, y = freq2)) +
    # Add tiles with custom coloring
    geom_tile(aes(fill = n_sig, color = is_speech_pair),
              width = 0.95, height = 0.95) +
    # Custom gradient
    scale_fill_gradientn(
      colors = c("#ffffff", "#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c"),
      values = scales::rescale(c(0, 2, 4, 6, 8, 10))
    ) +
    scale_color_manual(values = c("FALSE" = "gray30", 
                                  "TRUE" = "gray30"),
                       guide = "none") +
    # Enhanced theme
    theme_minimal(base_size = 14) +
    theme(
      axis.text = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 20)),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      legend.position = "right",
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 30, b = 20, l = 20)
    ) +
    labs(
      title = "Frequency Pair Analysis",
      #subtitle = "Number of significant copula tests by frequency pair\nRed borders indicate speech-related frequency pairs",
      x = "Discriminant Variable 1",
      y = "Discriminant Variable 2",
      fill = "Significant\nTests"
    )
  
  return(list(data = pair_analysis, plot = p2))
}


#################################
# Covariance and Bartlett Plots #
################################

# Create visualization for Covariance tests
create_covariance_heatmap <- function(data) {
  # Define the correct order for hearing loss levels
  category_order <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
  
  # Filter only Covariance tests and order factors
  cov_data <- data %>% 
    filter(Test == "Covariance") %>%
    mutate(
      Classif1 = factor(Classif1, levels = category_order),
      Classif2 = factor(Classif2, levels = category_order)
    )
  
  # Create enhanced plot
  ggplot(cov_data, aes(x = Classif1, y = Classif2, fill = Significance)) +
    geom_tile(width = 0.9, height = 0.9, color = "white") +
    scale_fill_gradientn(
      colors = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#92c5de", "#4393c3"),
      values = scales::rescale(c(0, 0.001, 0.01, 0.03, 0.05, 0.1)),
      limits = c(0, 0.1),
      na.value = "gray90",
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 15,
        barheight = 1,
        draw.ulim = TRUE,
        draw.llim = TRUE
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 20)),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 12),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 30, b = 20, l = 20)
    ) +
    labs(
      title = "Covariance Test Results",
      #subtitle = "P-values for different hearing loss category pairs\nOrdered from slight to severe hearing loss",
      x = "Category 1",
      y = "Category 2",
      fill = "p-value"
    )
}

# Create visualization for Bartlett tests with ordered frequencies
create_bartlett_plot <- function(data) {
  # Order frequencies: numerical order first, then SRT and SNR
  freq_order <- c("125", "250", "500", "750", "1000", "1500", "2000", 
                  "3000", "4000", "6000", "8000", "SRT", "SNR")
  
  # Filter only Bartlett tests and ensure all frequencies are represented
  bart_data <- data %>% 
    filter(Test == "Bartlett") %>%
    right_join(data.frame(Discr_Var = freq_order), by = c("Discr_Var")) %>%
    mutate(Discr_Var = factor(Discr_Var, levels = freq_order))
  
  # Create enhanced plot
  ggplot(bart_data, aes(x = Discr_Var, y = Significance)) +
    # Original elements for frequencies with data, excluding 125 and 250
    geom_segment(data = . %>% filter(!is.na(Significance) & !Discr_Var %in% c("125", "250")),
                 aes(x = Discr_Var, xend = Discr_Var, 
                     y = 0, yend = Significance,
                     color = Discr_Var %in% c("1000", "2000", "4000", "SRT", "SNR")),
                 size = 3) +
    geom_point(data = . %>% filter(!is.na(Significance) & !Discr_Var %in% c("125", "250")),
               aes(color = Discr_Var %in% c("1000", "2000", "4000", "SRT", "SNR")),
               size = 8) +
    scale_x_discrete(limits = freq_order) +
    scale_color_manual(values = c("FALSE" = "gray40", "TRUE" = "#e74c3c"),
                       name = "Speech Related",
                       labels = c("No", "Yes")) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 20)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 12),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 30, b = 20, l = 20)
    ) +
    labs(
      title = "Bartlett Test Results",
      x = "Frequency",
      y = "p-value"
    )
}
