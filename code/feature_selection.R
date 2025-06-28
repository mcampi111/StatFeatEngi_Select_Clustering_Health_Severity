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



######################
#  STATISTICAL TESTs #
######################

data_stats_test = data_ampl2[,c(4,34,7:28,30,31)] 

data_stats_test2 = data_stats_test
data_stats_test2$Classification2 <- paste(data_stats_test2$age_group,  #age_group2
                                          data_stats_test2$Classification, sep = ".")



#------------------- By PTA Categories ---------------#
data_ampl2_slight <- data_stats_test %>%
  filter(Classification == "Slight")   

data_ampl2_mild <- data_stats_test %>%
  filter(Classification == "Mild") 

data_ampl2_moder <- data_stats_test %>%
  filter(Classification == "Moderate") 

data_ampl2_modersev <- data_stats_test %>%
  filter(Classification == "Moderately severe") 

data_ampl2_sev <- data_stats_test %>%
  filter(Classification == "Severe")


category_hl = levels(data_ampl2$Classification)

names_list_combo =  levels(data_ampl2$Classification)

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

list_cat = list(data_ampl2_slight,
                data_ampl2_mild,
                data_ampl2_moder,
                data_ampl2_modersev,
                data_ampl2_sev)

names(list_cat) = names_list_combo

saveRDS(list_cat,
        file = file.path(res_dir, 'list_cat.rds'))

saveRDS(names_list_combo,
        file = file.path(res_dir, 'names_list_combo.rds'))


#------------------  By PTA Category ------------------------#



#  2 sample test on the mean 
t_test_mean_diff = as.data.frame(cbind(combo_category_df,
                                       do.call(rbind,lapply(1: dim(combo_category_df)[1],
                                                            function(j) do.call(cbind, lapply(1:24, function(i) 
                                                              round(t.test(list_cat[[combo_category_df[j,1]]][,2+i],
                                                                           list_cat[[combo_category_df[j,2]]][,2+i],
                                                                           alternative = "two.sided", 
                                                                           var.equal = TRUE)$p.value,3) ))))))


t_test_mean_diff = t_test_mean_diff %>% 
  mutate_at(3:26, as.numeric)

colnames(t_test_mean_diff) <- c("Classif1", "Classif2",
                                colnames(data_stats_test[3:26]))
t_test_mean_diff


#  welch test
w_test_mean_diff= as.data.frame(cbind(combo_category_df,
                                      do.call(rbind, lapply(1:dim(combo_category_df)[1], function(j) {
                                        do.call(cbind, lapply(1:24, function(i) {
                                          # Perform the t-test if both groups have more than 1 observation
                                          if (nrow(list_cat[[combo_category_df[j, 1]]]) > 1 &&
                                              nrow(list_cat[[combo_category_df[j, 2]]]) > 1) {
                                            round(t.test(list_cat[[combo_category_df[j, 1]]][, 2 + i],
                                                         list_cat[[combo_category_df[j, 2]]][, 2 + i],
                                                         alternative = "two.sided",
                                                         var.equal = FALSE)$p.value, 3)
                                          } else {
                                            # If one of the groups has less than 2 observations, return NA
                                            NA
                                          }
                                        }))
                                      }))))

w_test_mean_diff = w_test_mean_diff %>% 
  mutate_at(3:26, as.numeric)
colnames(w_test_mean_diff) <- c("Classif1", "Classif2",
                                colnames(data_stats_test[3:26]))
w_test_mean_diff

#  variance ratio test
test_var_ratio = as.data.frame(
  cbind(combo_category_df,
        do.call(rbind, lapply(1:dim(combo_category_df)[1], function(j) {
          do.call(cbind, lapply(1:24, function(i) {
            # Perform the variance test if both groups have more than 1 observation
            if (length(unlist(list_cat[[combo_category_df[j, 1]]][, 2 + i])) > 1 &&
                length(unlist(list_cat[[combo_category_df[j, 2]]][, 2 + i])) > 1) {
              round(var.test(as.numeric(unlist(list_cat[[combo_category_df[j, 1]]][, 2 + i])),
                             as.numeric(unlist(list_cat[[combo_category_df[j, 2]]][, 2 + i])),
                             alternative = "two.sided")$p.value, 3)
            } else {
              # If one of the groups has less than 2 observations, return NA
              NA
            }
          }))
        }))))

test_var_ratio = test_var_ratio %>% 
  mutate_at(3:26, as.numeric)
colnames(test_var_ratio) <-c("Classif1", "Classif2",
                             colnames(data_stats_test[3:26]))
test_var_ratio

# kolmgorov test
test_kolmo = as.data.frame(
  cbind(combo_category_df,
        do.call(rbind, lapply(1:dim(combo_category_df)[1], function(j) {
          do.call(cbind, lapply(1:24, function(i) {
            # Perform the Kolmogorov-Smirnov test if both groups have more than 1 observation
            if (length(unlist(list_cat[[combo_category_df[j, 1]]][, 2 + i])) > 1 &&
                length(unlist(list_cat[[combo_category_df[j, 2]]][, 2 + i])) > 1) {
              round(ks.test(as.numeric(unlist(list_cat[[combo_category_df[j, 1]]][, 2 + i])),
                            as.numeric(unlist(list_cat[[combo_category_df[j, 2]]][, 2 + i])),
                            alternative = "two.sided")$p.value, 3)
            } else {
              # If one of the groups has less than 2 observations, return NA
              NA
            }
          }))
        }))))


test_kolmo = test_kolmo %>% 
  mutate_at(3:26, as.numeric)
colnames(test_kolmo) <- c("Classif1", "Classif2",
                          colnames(data_stats_test[3:26]))
test_kolmo


# CMV test
test_cmv = as.data.frame(
  cbind(combo_category_df,
        do.call(rbind, lapply(1:dim(combo_category_df)[1], function(j) {
          do.call(cbind, lapply(1:24, function(i) {
            # Perform the Cramer-von Mises test if both groups have more than 1 observation
            if (length(unlist(list_cat[[combo_category_df[j, 1]]][, 2 + i])) > 1 &&
                length(unlist(list_cat[[combo_category_df[j, 2]]][, 2 + i])) > 1) {
              round(as.numeric(
                cvm_test(as.numeric(unlist(list_cat[[combo_category_df[j, 1]]][, 2 + i])),
                         as.numeric(unlist(list_cat[[combo_category_df[j, 2]]][, 2 + i]))
                )[2]), 3)
            } else {
              # If one of the groups has less than 2 observations, return NA
              NA
            }
          }))
        }))))

test_cmv = test_cmv %>%
  mutate_at(3:26, as.numeric)
colnames(test_cmv) <- c("Classif1", "Classif2",
                        colnames(data_stats_test[3:26]))
test_cmv


#  group test multi-null tests -->  ANOVA
#1 - test that  that the variance across groups is equal -->NO
bart_test =   as.data.frame(
  cbind(colnames(data_stats_test2[3:26]),
        do.call(rbind, lapply(1:24, function(i) {
          # Extract the variable name
          var_name <- colnames(data_stats_test2)[2 + i]
          
          # Filter out groups with less than 2 observations
          filtered_data <- data_stats_test2 %>%
            group_by(Classification2) %>%
            filter(n() >= 2)
          
          # Perform Bartlett test
          round(bartlett.test(get(var_name) ~ as.factor(Classification2),
                              data = filtered_data)$p.value, 3)
        }))
  )
)
bart_test = bart_test  %>% 
  mutate_at(c('V2'), as.numeric)
colnames(bart_test) = c("variable", "p.value")
bart_test



#2 -ANOVA
anova_test = as.data.frame(
  cbind(colnames(data_stats_test2[3:26]),
        do.call(rbind,lapply(1:24, function(i)
          (round(summary(data_stats_test2 %$% aov( get(
            colnames(data_stats_test2[,2+i]))~ Classification))[[1]][5]$`Pr(>F)`[1],7)))) ) )

#2 - TUKEY
tukey_test = do.call(cbind,lapply(1:24, function(i)
  round(TukeyHSD(data_stats_test2 %$% aov( get(
    colnames(data_stats_test2[,2+i]))~ Classification))$Classification[,4],3)  ))

colnames(tukey_test) = colnames(data_stats_test[3:26])
tukey_test


# Covariance
cov_test = as.data.frame(
  cbind(combo_category_df,
        do.call(rbind, lapply(1:dim(combo_category_df)[1], function(j) {
          # Filter the data for each group
          group1 <- data_stats_test2[,c(3:26,1)] %>%
            filter(Classification == combo_category_df[j, 1]) %>%
            dplyr::select(-Classification)
          group2 <- data_stats_test2[,c(3:26,1)] %>%
            filter(Classification == combo_category_df[j, 2]) %>%
            dplyr::select(-Classification)
          
          print(j)
          
          # Check if both groups have at least 2 observations
          if (nrow(group1) >= 2 && nrow(group2) >= 2) {
            # Perform the test if both groups have sufficient observations
            #round(testCov(group1, group2, alpha = 0.05, 
            #              method = "HD", J = 100)$p.value, 3)
            round(sLED(group1, group2, npermute = 100)$pVal, 3)
          } else {
            # If one of the groups has less than 2 observations, return NA
            NA
          }
        }))
  )
)

cov_test = cov_test %>% mutate_at(3, as.numeric)
colnames(cov_test) <- c("Classif1", "Classif2", 
                        "Significance")
cov_test



##########################
# copula 2 sample testing#
##########################


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!NO NEED TO RUN THESE ANYMORE!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# IF you do recheck the col indexes

#Normally running
# cop_test_left <- list()
# 
# # Loop through each combo_category
# for (j in 1:dim(combo_category_df)[1]) { #
# 
#   # Filter the data for each group based on combo_category_df
#   group1 <- subset(data_stats_test2,
#                    Classification == combo_category_df[j, 1],
#                    select = c(4:14, 26:28))[,-14]
#   group2 <- subset(data_stats_test2,
#                    Classification == combo_category_df[j, 2],
#                    select = c(4:14, 26:28))[,-14]
# 
# 
# 
#   # Check if both groups have at least 2 observations
#   if (nrow(group1) >= 2 && nrow(group2) >= 2) {
#     # Perform the test if both groups have sufficient observations
#     p_value <- coptest.p(as.matrix(group1), as.matrix(group2),
#                          nperm = 20, approx = FALSE)$tbl
#     # Append the result to the cop_test_left data frame
#     cop_test_left[[j]] <- p_value
#   } else {
#     # If one of the groups has less than 2 observations, return NA
#     cop_test_left[[j]] <- NA
#   }
# }


# Run in parallel
# Define the function to run the coptest with error handling and consistent return type
# Initialize the list to store the results
# cop_test_left <- list()
# cop_test_right <- list()
# 
# 
# # Define the function to run the copula test
# run_coptest <- function(j) {
#   tryCatch({
#     group1 <- subset(data_stats_test2,
#                      Classification == combo_category_df[j, 1],
#                      select = c(4:14, 26:28))[,-14]
#     group2 <- subset(data_stats_test2,
#                      Classification == combo_category_df[j, 2],
#                      select = c(4:14, 26:28))[,-14]
#     
#     if (nrow(group1) >= 2 && nrow(group2) >= 2) {
#       result <- coptest.p(as.matrix(group1), as.matrix(group2),
#                           nperm = 20, approx = FALSE)$tbl
#       return(result)
#     } else {
#       return(NA)
#     }
#   }, error = function(e) {
#     message(sprintf("Error in run_coptest for row %d: %s", j, e$message))
#     return(NA)
#   })
# }
# 
# # Determine the number of cores to use
# num_cores <- 4  # Use one less than the total available cores
# 
# # Run in chunks
# chunk_size <- 10
# num_chunks <- ceiling(nrow(combo_category_df) / chunk_size)
# 
# # Create a cluster once
# cl <- makeCluster(num_cores)
# on.exit(stopCluster(cl), add = TRUE)
# 
# clusterExport(cl, c("data_stats_test2", "combo_category_df", "coptest.p", "run_coptest"))
# 
# # Initialize progress bar
# pb <- txtProgressBar(min = 0, max = num_chunks, style = 3)
# 
# # Measure the total time taken for all chunks
# total_time_taken <- system.time({
#   for (i in 1:num_chunks) {
#     start_index <- (i - 1) * chunk_size + 1
#     end_index <- min(i * chunk_size, nrow(combo_category_df))
#     
#     # Run the parallel computation for the current chunk
#     chunk_results <- parLapply(cl, start_index:end_index, run_coptest)
#     
#     # Combine the results
#     cop_test_left <- c(cop_test_left, chunk_results)
#     
#     # Update progress bar
#     setTxtProgressBar(pb, i)
#   }
# })
# 
# close(pb)
# 
# # Clean up
# rm(chunk_results)
# gc()
# 
# # Print total time
# cat(sprintf("\nTotal time taken: %.2f seconds\n", total_time_taken["elapsed"]))
# 
# saveRDS(cop_test_left,
#         file = "/Users/mcampi/Desktop/Spin_ML/code/Rfiles/cop_test_left_pta_last.rds" )


cop_test_left<- readRDS("/Users/mcampi/Desktop/Spin_ML/code/Rfiles/cop_test_left_pta_last.rds")


for (i in 1:dim(combo_category_df)[1]) {
  
  cop_test_left[[i]] = as.data.frame(cop_test_left[[i]])
  cop_test_left[[i]]$contrast1 <- combo_category_df[i,1]
  cop_test_left[[i]]$contrast2 <- combo_category_df[i,2]
  
}


cop_test_left_filtered = Filter(function(df) ncol(df) >= 6, cop_test_left)

cop_test_left_filtered = do.call(rbind, cop_test_left_filtered)

#################
#  COPULA PLOTS # 
#################
#######################
#Example for one plot #
#######################

# Create the reference data frame with all required columns
unique_vars = unique(unlist(strsplit(cop_test_left_filtered$varname, "\\|")))

toplot = data.frame(
  group_id = "Slight",
  desc = "Mild vs Slight",
  varname = unique_vars,
  stringsAsFactors = FALSE
)

# Verify the structure
print(str(toplot))
print(head(toplot))

# Prepare the table
tbl = data.frame(
  varname = cop_test_left_filtered$varname,
  stat = cop_test_left_filtered$stat,
  p = cop_test_left_filtered$p,
  p.adj = cop_test_left_filtered$p.adj
)

# Create the result object
result = list(tbl = tbl)

# Try visualization with explicit column creation
netvis(obj = result, ref = toplot)

##########################
# Example for more plots #
##########################
#Improve this for the paper


# 1. Network Visualization
# Comparisons to analyze
comparisons = c("Mild", "Moderate", "Moderately severe", "Severe")
comparison_plots = generate_comparison_plots(cop_test_left_filtered, comparisons)

comparison_plots


# 2. Connection Significance Analysis
connection_details = cop_test_left_filtered %>%
  mutate(
    significance_level = case_when(
      p < 0.001 ~ "Extremely Significant",
      p < 0.01 ~ "Highly Significant",
      p < 0.05 ~ "Moderately Significant",
      TRUE ~ "Not Significant"
    )
  ) %>%
  group_by(significance_level) %>%
  summarise(
    connection_count = n(),
    min_p = min(p),
    max_p = max(p)
  )

print("Connection Significance Details:")
print(connection_details)

# 3. Comprehensive Frequency Analysis

# Run frequency analysis
freq_significance_results = analyze_frequency_relationships(cop_test_left_filtered)

print("Frequency Relationship Analysis:")
print(freq_significance_results)

# 4. Summary Statistics for Frequencies
summary_stats = freq_significance_results %>%
  group_by(contrast2) %>%
  summarise(
    mean_prop_significant = mean(prop_significant),
    median_prop_significant = median(prop_significant),
    max_prop_significant = max(prop_significant)
  )

print("Frequency Comparison Summary:")
print(summary_stats)

# Optional: Specific Frequency Analysis
frequencies_of_interest = c("1000", "2000", "4000")
specific_freq_analyses = lapply(frequencies_of_interest, function(freq) {
  analyze_frequency_relationships(
    cop_test_left_filtered %>% 
      filter(grepl(freq, varname))
  )
})
names(specific_freq_analyses) = frequencies_of_interest

# Print specific frequency analyses
for(freq in names(specific_freq_analyses)) {
  cat("\nAnalysis for", freq, "Hz:\n")
  print(specific_freq_analyses[[freq]])
}

#################################
#################################
# FEATURE SELECTION & RANKING ###
#################################
#################################

#mean t test
id_m = which(t_test_mean_diff[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_mean = cbind(do.call(rbind,
                              lapply(1:dim(id_m)[1], 
                                     function(i) 
                                       t_test_mean_diff[-c(1,2),][ id_m[i,1],c(1:2) ])),
                      do.call(rbind,
                              lapply(1:dim(id_m)[1], 
                                     function(i) 
                                       colnames(t_test_mean_diff[-c(1,2),])[id_m[i,2]] )))


colnames(stat_dec_mean) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_mean$Test = rep('T-test', dim(stat_dec_mean)[1])
stat_dec_mean$Significance =   rep(0.1, dim(stat_dec_mean)[1])
stat_dec_mean


#mean welch test
id_wm = which(w_test_mean_diff[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_w_mean = cbind(do.call(rbind,
                                lapply(1:dim(id_wm)[1], 
                                       function(i) 
                                         w_test_mean_diff[-c(1,2),][ id_wm[i,1],c(1:2) ])),
                        do.call(rbind,
                                lapply(1:dim(id_wm)[1], 
                                       function(i) 
                                         colnames(w_test_mean_diff[-c(1,2),])[id_wm[i,2]] )))


colnames(stat_dec_w_mean) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_w_mean$Test = rep('Welch', dim(stat_dec_w_mean)[1])
stat_dec_w_mean$Significance =   rep(0.1, dim(stat_dec_w_mean)[1])
stat_dec_w_mean


#Variance test
id_v = which(test_var_ratio[-c(1,2),] < 0.10, arr.ind=TRUE)

stat_dec_var = cbind(do.call(rbind,
                             lapply(1:dim(id_v)[1], 
                                    function(i) 
                                      test_var_ratio[-c(1,2),][ id_v[i,1],c(1:2) ])),
                     do.call(rbind,
                             lapply(1:dim(id_v)[1], 
                                    function(i) 
                                      colnames(test_var_ratio[-c(1,2),])[id_v[i,2]] )))


colnames(stat_dec_var) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_var$Test = rep('Variance', dim(stat_dec_var)[1])
stat_dec_var$Significance =   rep(0.10, dim(stat_dec_var)[1])
stat_dec_var


#Distribution test
id_d = which(test_kolmo[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_dist = cbind(do.call(rbind,
                              lapply(1:dim(id_d)[1], 
                                     function(i) 
                                       test_kolmo[-c(1,2),][ id_d[i,1],c(1:2) ])),
                      do.call(rbind,
                              lapply(1:dim(id_d)[1], 
                                     function(i) 
                                       colnames(test_kolmo[-c(1,2),])[id_d[i,2]] )))


colnames(stat_dec_dist) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_dist$Test = rep('Kolmogorov', dim(stat_dec_dist)[1])
stat_dec_dist$Significance =   rep(0.1, dim(stat_dec_dist)[1])
stat_dec_dist


#CMV test
id_cmv = which(test_cmv[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_cmv = cbind(do.call(rbind,
                             lapply(1:dim(id_cmv)[1], 
                                    function(i) 
                                      test_cmv[-c(1,2),][ id_cmv[i,1],c(1:2) ])),
                     do.call(rbind,
                             lapply(1:dim(id_cmv)[1], 
                                    function(i) 
                                      colnames(test_cmv[-c(1,2),])[id_cmv[i,2]] )))


colnames(stat_dec_cmv) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_cmv$Test = rep('CMV', dim(stat_dec_cmv)[1])
stat_dec_cmv$Significance =   rep(0.1, dim(stat_dec_cmv)[1])
stat_dec_cmv

#Bartlett test
id_bart_bl = which(bart_test[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_bart = do.call(rbind, lapply(1:dim(id_bart_bl)[1], 
                                      function(i) bart_test[-c(1,2),][ id_bart_bl[i,1],c(1:2) ]))


colnames(stat_dec_bart) = c("Discr_Var", "Significance")
stat_dec_bart$Test = rep('Bartlett', dim(stat_dec_bart)[1])
stat_dec_bart$Classif1 =   rep("All", dim(stat_dec_bart)[1])
stat_dec_bart$Classif2 =  rep("All", dim(stat_dec_bart)[1])
stat_dec_bart

stat_dec_bart <- stat_dec_bart[colnames(stat_dec_cmv)]
stat_dec_bart



#TUKEY test
id_t = which(tukey_test[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_tukey = as.data.frame(cbind(do.call(rbind,
                                             lapply(1:dim(id_t)[1], 
                                                    function(i) 
                                                      rownames(tukey_test[-c(1,2),])[ id_t[i,1]])),
                                     do.call(rbind,
                                             lapply(1:dim(id_t)[1], 
                                                    function(i) 
                                                      colnames(tukey_test[-c(1,2),])[id_t[i,2]] ))) )

colnames(stat_dec_tukey) = c("Combination", "Discr_Var")
stat_dec_tukey$Test = rep('Tukey', dim(stat_dec_tukey)[1])
stat_dec_tukey$Significance =   rep(0.1, dim(stat_dec_tukey)[1])
stat_dec_tukey


# Apply the function to the sample data
new_types <- do.call(rbind,lapply(1:dim(stat_dec_tukey)[1], function(i) 
  split_at_second_occurrence(stat_dec_tukey$Combination[i], "-")))
colnames(new_types) = c("Classif1","Classif2")
stat_dec_tukey = cbind(new_types,stat_dec_tukey[,-1])
stat_dec_tukey


#COVARIANCE test
id_cov = which(cov_test[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_cov = do.call(rbind,
                       lapply(1:dim(id_cov)[1], 
                              function(i) 
                                cov_test[-c(1,2),][ id_cov[i,1],c(1:2) ]))

stat_dec_cov$Discr_Var = rep('NA', dim(stat_dec_cov)[1])
stat_dec_cov$Test = rep('Covariance', dim(stat_dec_cov)[1])
stat_dec_cov$Significance =   rep(0.1, dim(stat_dec_cov)[1])
stat_dec_cov


#COPULA test
id_cop_left = which(cop_test_left_filtered[,-c(1,2,4,5,6)] < 0.1, arr.ind=TRUE)

stat_dec_copula_left = do.call(rbind,
                               lapply(1:length(id_cop_left), 
                                      function(i) 
                                        cop_test_left_filtered[ id_cop_left[i],c(5,6,1) ]))


colnames(stat_dec_copula_left)<- c("Classif1","Classif2", "Discr_Var")
stat_dec_copula_left = as.data.frame(stat_dec_copula_left)

stat_dec_copula_left$Test = rep('Copula', dim(stat_dec_copula_left)[1])

stat_dec_copula_left$Significance =  rep(0.1, dim(stat_dec_copula_left)[1])



############################################################
# FINAL FEATURES BASED ON STATISTICAL TESTS SIGNIFICIANCE #
###########################################################


final_dec = rbind(stat_dec_mean, stat_dec_w_mean,
                  stat_dec_var, stat_dec_dist,
                  stat_dec_cmv,  stat_dec_bart,
                  stat_dec_tukey,  stat_dec_cov,
                  stat_dec_copula_left)

final_dec_left =  final_dec %>%
  filter(!grepl("_R", Discr_Var))

final_dec_left = final_dec_left %>%
  mutate(HearingLossDegree1 = sub(".*\\.", "", Classif1),
         HearingLossDegree2 = sub(".*\\.", "", Classif2))

final_dec_left$Combination_HLdegree = paste(final_dec_left$HearingLossDegree1,'-',final_dec_left$HearingLossDegree2)

severity_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")

# Apply the standardization to the Combination_HLdegree column
final_dec_left <- final_dec_left %>%
  mutate(Combination_HLdegree = sapply(Combination_HLdegree, standardize_combination))


unique(final_dec_left$Combination_HLdegree)

# Apply the function to filter the dataset
filtered_final_dec_left <- final_dec_left %>%
  filter(sapply(Combination_HLdegree, remove_identical_combinations))

unique(filtered_final_dec_left$Combination_HLdegree)

filtered_final_dec_left <- filtered_final_dec_left %>%
  mutate(Discr_Var = str_replace_all(Discr_Var, "FREQ_", ""))

filtered_final_dec_left <- filtered_final_dec_left %>%
  mutate(Discr_Var = str_replace_all(Discr_Var, "_L", ""))

saveRDS(final_dec_left,
        file = file.path(res_dir, 'final_dec_left.rds'))

######################
#PLOTS FINAL DECISION#
######################

colnames(filtered_final_dec_left)

filtered_hl_nocopula <- filtered_final_dec_left %>% 
  filter(HearingLossDegree1 != HearingLossDegree2, 
         Test != "Copula")

filtered_hl_onlycopula <- filtered_final_dec_left %>% 
  filter(HearingLossDegree1 != HearingLossDegree2,
         Test == "Copula")

desired_order <- c("125", "250", "500", "750", "1000", 
                   "2000", "3000", "4000", "6000", "8000", 
                   "SNR", "SRT", "NA")

hist_vars_hl_nocopula = filtered_hl_nocopula  %>% 
  group_by( Discr_Var,Combination_HLdegree)   %>% 
  count(Test) %>% 
  mutate(percent = n/sum(n),
         n = n)

hist_vars_hl_nocopula <- hist_vars_hl_nocopula %>%
  mutate(Discr_Var = factor(Discr_Var, levels = desired_order))



hist_vars_hl_onlycopula = filtered_hl_onlycopula  %>%
  group_by( Discr_Var,Combination_HLdegree)   %>% 
  count(Test) %>% 
  mutate(percent = n/sum(n),
         n = n)

ggplot(hist_vars_hl_nocopula, 
       aes(x = Discr_Var, y = n, fill = Combination_HLdegree)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + facet_wrap(~Test, scales = 'free_y') +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) +
  ylab("# Statistical Siginificant Test") +xlab("Discriminant Variables")


ggplot(hist_vars_hl_onlycopula, 
       aes(x = Discr_Var, y = n, fill = Combination_HLdegree)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + facet_wrap(~Test, scales = 'free_y') +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) 



